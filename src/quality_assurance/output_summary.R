library(sf)
library(tidyverse)


#' Normalize EA code values for stable joins.
#'
#' @param x (vector) EA_CODE-like values from any source type.
#'
#' @return character vector with normalized EA keys.
normalize_ea_code <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- sub("\\.0$", "", x)
    x <- sub("^0+", "", x)
    x[x == ""] <- "0"
    x
}


#' Check duplicate normalized EA codes in current and baseline data.
#'
#' @param new_df (data.frame) current output dataset.
#' @param baseline_df (data.frame) baseline dataset.
#' @param dataset_name (character) label used in QA outputs.
#'
#' @return duplicate_report (tibble) duplicate-key counts and sample keys.
check_duplicate_ea_codes <- function(new_df, baseline_df, dataset_name) {
    key <- "EA_CODE"

    # Ensure expected key exists before running duplicate checks.
    if (!(key %in% names(new_df)) || !(key %in% names(baseline_df))) {
        stop(paste0("EA_CODE not found in ", dataset_name, " duplicate-key check."))
    }

    # Normalize keys so type or formatting differences do not mask duplicates.
    new_join_ea <- normalize_ea_code(new_df[[key]])
    baseline_join_ea <- normalize_ea_code(baseline_df[[key]])

    # Collect distinct duplicate keys on each side.
    new_duplicate_keys <- unique(new_join_ea[duplicated(new_join_ea)])
    baseline_duplicate_keys <- unique(baseline_join_ea[duplicated(baseline_join_ea)])

    # Track whether duplicate-key sets differ between current and baseline.
    duplicate_key_drift <- !setequal(new_duplicate_keys, baseline_duplicate_keys)

    # Keep counts plus a capped sample list for quick QA inspection.
    duplicate_report <- tibble::tibble(
        dataset = dataset_name,
        current_duplicate_key_count = length(new_duplicate_keys),
        baseline_duplicate_key_count = length(baseline_duplicate_keys),
        duplicate_key_drift = duplicate_key_drift,
        current_duplicate_keys = paste(head(new_duplicate_keys, 20), collapse = ";"),
        baseline_duplicate_keys = paste(head(baseline_duplicate_keys, 20), collapse = ";")
    )

    return(duplicate_report)
}


#' Compare current and baseline tabular datasets by EA code.
#'
#' @param new_df (data.frame) current output dataset.
#' @param baseline_df (data.frame) baseline dataset.
#' @param dataset_name (character) label used in QA outputs.
#' @param numeric_tol (numeric) tolerance for numeric comparisons.
#'
#' @return list with column_report and mismatch_rows tibbles.
compare_dataframes_by_ea <- function(new_df, baseline_df, dataset_name, numeric_tol = 1e-9) {
    key <- "EA_CODE"

    # Guard clause to fail early if key column is missing.
    if (!(key %in% names(new_df)) || !(key %in% names(baseline_df))) {
        stop(paste0("EA_CODE not found in ", dataset_name, " comparison data."))
    }

    # Build a canonical join key to handle type/casing/format drift.
    new_df$join_ea <- normalize_ea_code(new_df[[key]])
    baseline_df$join_ea <- normalize_ea_code(baseline_df[[key]])

    # Compare only columns that exist on both sides.
    common_cols <- intersect(names(new_df), names(baseline_df))
    common_cols <- setdiff(common_cols, c("join_ea", "geometry"))

    # Join current and baseline data side-by-side by normalized EA key.
    merged <- dplyr::full_join(
        new_df[, c("join_ea", common_cols), drop = FALSE],
        baseline_df[, c("join_ea", common_cols), drop = FALSE],
        by = "join_ea",
        relationship = "many-to-many",
        suffix = c("_new", "_baseline")
    )

    # Per-column comparator that tracks mismatch and NA deltas.
    compare_col <- function(cl) {
        a <- merged[[paste0(cl, "_new")]]
        b <- merged[[paste0(cl, "_baseline")]]

        both_na <- is.na(a) & is.na(b)
        # Use tolerance for numeric columns to avoid floating-point noise.
        same_non_na <- if (is.numeric(a) && is.numeric(b)) {
            dplyr::near(a, b, tol = numeric_tol)
        } else {
            a == b
        }

        same <- both_na | (!is.na(a) & !is.na(b) & same_non_na)
        same[is.na(same)] <- FALSE

        tibble::tibble(
            dataset = dataset_name,
            column = cl,
            total_rows = length(same),
            mismatches = sum(!same),
            pct_mismatch = round(100 * mean(!same), 6),
            na_new = sum(is.na(a)),
            na_baseline = sum(is.na(b))
        )
    }

    # Assemble and rank report by mismatch count.
    column_report <- dplyr::bind_rows(lapply(common_cols, compare_col))
    column_report <- column_report[order(-column_report$mismatches, column_report$column), ]

    mismatch_rows <- tibble::tibble(dataset = dataset_name, mismatched_columns = sum(column_report$mismatches > 0))

    list(column_report = column_report, mismatch_rows = mismatch_rows)
}


#' Compare summarized survey CSV output against baseline CSV.
#'
#' @param new_csv_path (character) path to current summarized CSV output.
#' @param baseline_csv_path (character) path to baseline summarized CSV output.
#' @param numeric_tol (numeric) tolerance for numeric comparisons.
#'
#' @return list with column_report, mismatch_rows, and duplicate_report.
compare_csv_outputs <- function(new_csv_path, baseline_csv_path, numeric_tol = 1e-9) {
    new_df <- read.csv(new_csv_path, stringsAsFactors = FALSE, check.names = FALSE)
    baseline_df <- read.csv(baseline_csv_path, stringsAsFactors = FALSE, check.names = FALSE)

    duplicate_report <- check_duplicate_ea_codes(new_df, baseline_df, "summarized_survey_data")
    attr_compare <- compare_dataframes_by_ea(new_df, baseline_df, "summarized_survey_data", numeric_tol)

    list(
        column_report = attr_compare$column_report,
        mismatch_rows = attr_compare$mismatch_rows,
        duplicate_report = duplicate_report
    )
}


#' Compare household-size GPKG output against baseline GPKG.
#'
#' @param new_gpkg_path (character) path to current household-size GPKG output.
#' @param baseline_gpkg_path (character) path to baseline household-size GPKG output.
#' @param numeric_tol (numeric) tolerance for numeric comparisons.
#'
#' @return list with column_report, mismatch_rows, duplicate_report, and geometry_meta.
compare_gpkg_outputs <- function(new_gpkg_path, baseline_gpkg_path, numeric_tol = 1e-9) {
    new_sf <- st_read(new_gpkg_path, quiet = TRUE)
    baseline_sf <- st_read(baseline_gpkg_path, quiet = TRUE)
    new_df <- st_drop_geometry(new_sf)
    baseline_df <- st_drop_geometry(baseline_sf)

    # Keep lightweight geometry metadata checks alongside attribute parity.
    geometry_meta <- tibble::tibble(
        dataset = "hh_size_data",
        new_rows = nrow(new_sf),
        baseline_rows = nrow(baseline_sf),
        new_crs = as.character(st_crs(new_sf)$epsg),
        baseline_crs = as.character(st_crs(baseline_sf)$epsg)
    )

    duplicate_report <- check_duplicate_ea_codes(new_df, baseline_df, "hh_size_data")

    # Attribute comparison is done on non-geometry fields only.
    attr_compare <- compare_dataframes_by_ea(
        new_df,
        baseline_df,
        "hh_size_data",
        numeric_tol
    )

    list(
        column_report = attr_compare$column_report,
        mismatch_rows = attr_compare$mismatch_rows,
        duplicate_report = duplicate_report,
        geometry_meta = geometry_meta
    )
}


#' Run parity QA and write script-scoped QA artifacts.
#'
#' @param output_csv_path (character) path to current summarized CSV output.
#' @param output_gpkg_path (character) path to current household-size GPKG output.
#' @param baseline_csv_path (character) path to baseline summarized CSV output.
#' @param baseline_gpkg_path (character) path to baseline household-size GPKG output.
#' @param qa_output_dir (character) directory for QA report outputs.
#' @param report_prefix (character) prefix used for QA output filenames.
#' @param numeric_tol (numeric) tolerance for numeric comparisons.
#'
#' @return list with summary, column_report, duplicate_key_report, and geometry_meta.
run_parity_qa <- function(
    output_csv_path,
    output_gpkg_path,
    baseline_csv_path,
    baseline_gpkg_path,
    qa_output_dir,
    report_prefix = "data_processing2",
    numeric_tol = 1e-9
) {
    # Ensure QA output folder exists for report files.
    dir.create(qa_output_dir, recursive = TRUE, showWarnings = FALSE)

    # Baselines are optional; missing files should not crash QA.
    csv_exists <- file.exists(baseline_csv_path)
    gpkg_exists <- file.exists(baseline_gpkg_path)

    csv_report <- tibble::tibble()
    gpkg_report <- tibble::tibble()
    duplicate_key_report <- tibble::tibble()
    geometry_meta <- tibble::tibble()

    # Run per-artifact comparisons only when baseline exists.
    if (csv_exists) {
        csv_compare <- compare_csv_outputs(output_csv_path, baseline_csv_path, numeric_tol)
        csv_report <- csv_compare$column_report
        duplicate_key_report <- dplyr::bind_rows(duplicate_key_report, csv_compare$duplicate_report)
    }

    if (gpkg_exists) {
        gpkg_compare <- compare_gpkg_outputs(output_gpkg_path, baseline_gpkg_path, numeric_tol)
        gpkg_report <- gpkg_compare$column_report
        duplicate_key_report <- dplyr::bind_rows(duplicate_key_report, gpkg_compare$duplicate_report)
        geometry_meta <- gpkg_compare$geometry_meta
    }

    # Consolidate both reports into a single parity table.
    combined_report <- dplyr::bind_rows(csv_report, gpkg_report)
    total_mismatches <- if (nrow(combined_report) == 0) 0 else sum(combined_report$mismatches)
    total_current_duplicate_keys <- if (nrow(duplicate_key_report) == 0) 0 else sum(duplicate_key_report$current_duplicate_key_count)
    total_baseline_duplicate_keys <- if (nrow(duplicate_key_report) == 0) 0 else sum(duplicate_key_report$baseline_duplicate_key_count)
    datasets_with_duplicate_key_drift <- if (nrow(duplicate_key_report) == 0) 0 else sum(duplicate_key_report$duplicate_key_drift)

    # Status semantics: SKIPPED (no baseline), PASS (zero value mismatches),
    # FAIL (data values differ from baseline). Duplicate keys are reported
    # as informational but do not affect pass/fail status.
    status <- if (!csv_exists && !gpkg_exists) {
        "SKIPPED"
    } else if (total_mismatches == 0) {
        "PASS"
    } else {
        "FAIL"
    }

    summary_df <- tibble::tibble(
        status = status,
        baseline_csv_found = csv_exists,
        baseline_gpkg_found = gpkg_exists,
        total_columns_compared = nrow(combined_report),
        total_mismatches = total_mismatches,
        total_current_duplicate_keys = total_current_duplicate_keys,
        total_baseline_duplicate_keys = total_baseline_duplicate_keys,
        datasets_with_duplicate_key_drift = datasets_with_duplicate_key_drift,
        numeric_tolerance = numeric_tol
    )

    # Persist machine-readable QA outputs for downstream reporting.
    write.csv(
        combined_report,
        file.path(qa_output_dir, paste0(report_prefix, "_parity_column_report.csv")),
        row.names = FALSE
    )
    write.csv(
        summary_df,
        file.path(qa_output_dir, paste0(report_prefix, "_parity_summary.csv")),
        row.names = FALSE
    )
    write.csv(
        duplicate_key_report,
        file.path(qa_output_dir, paste0(report_prefix, "_duplicate_ea_code_report.csv")),
        row.names = FALSE
    )

    if (nrow(geometry_meta) > 0) {
        write.csv(
            geometry_meta,
            file.path(qa_output_dir, paste0(report_prefix, "_parity_geometry_meta.csv")),
            row.names = FALSE
        )
    }

    # Return all outputs for programmatic use in main orchestration.
    return(list(
        summary = summary_df,
        column_report = combined_report,
        duplicate_key_report = duplicate_key_report,
        geometry_meta = geometry_meta
    ))
}