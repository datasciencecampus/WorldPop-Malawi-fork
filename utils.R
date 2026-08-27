library(sf)
library(yaml)

generate_buffered_country_boundary <- function(
    shape_path,
    file_name,
    buffer
){
  #' Produce buffered country boundary shapefile by dissolving EA geometries and
  #' adding 10km buffer
  #' 
  ea_geoms <- st_read(file.path(shape_path, "2018_MPHC_EAs_Final_for_Use.shp"))
  country_buffer <- ea_geoms %>% 
    st_union() %>% 
    st_buffer(buffer)
  st_write(country_buffer, file.path(shape_path, file_name))
  return(country_buffer)
}

rbind_zomba_csvs <- function(csv_dir, output_file){
  #' Produce rbind csv file for zomba from 14 files provided by NSO in "zomba_csv"
  #' folder. 
  #' Headers are normalised, NA rows are removed, and then rbound together to
  #' make "zomba_rbind_data.csv" file.
  if (!dir.exists(csv_dir)) {
    stop("Directory does not exist: ", csv_dir)
  }

  output_path <- file.path(output_file)
  csv_files <- list.files(
    csv_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  csv_files <- sort(csv_files)

  if (length(csv_files) == 0) {
    stop("No CSV files found in: ", csv_dir)
  }

  zomba_data_list <- lapply(
    csv_files,
    function(csv_file) {
      zomba_data <- read.csv(csv_file, stringsAsFactors = FALSE, check.names = FALSE)
      # change headers to lowercase and replace spaces to _ to normalise
      names(zomba_data) <- gsub("\\s+", "_", tolower(trimws(names(zomba_data))))
      # "Nkapita.csv" had "household_name" instead of "household_number" like
      # the rest
      names(zomba_data)[names(zomba_data) == "household_name"] <- "household_number"
      # remove rows where every field is blank or missing
      empty_rows <- apply(
        zomba_data,
        1,
        function(row) all(is.na(row) | trimws(as.character(row)) == "")
      )
      zomba_data <- zomba_data[!empty_rows, , drop = FALSE]
      zomba_data
    }
  )
  
  # Look at the first file and determine what columns are expected
  expected_columns <- names(zomba_data_list[[1]])
  
  matching_columns <- vapply(
    zomba_data_list,
    function(zomba_data) identical(names(zomba_data), expected_columns),
    logical(1)
  )

  # Check that every CSV has the same normalized columns as the first file and
  # stop with the filenames of any CSVs that still do not match.
  if (!all(matching_columns)) {
    mismatched_files <- basename(csv_files[!matching_columns])
    stop(
      "CSV files do not share identical columns and cannot be row-bound with rbind: ",
      paste(mismatched_files, collapse = ", ")
    )
  }

  zomba_rbind_data <- do.call(rbind, zomba_data_list)
  write.csv(zomba_rbind_data, output_path, row.names = FALSE)

  return()
}


#' Write a structured plain-text run log.
#'
#' @param run_id (character) timestamp-based unique run identifier.
#' @param log_file_path (character) path to the logger output file for this run.
#' @param config (list) loaded pipeline config.
#' @param output_files (character vector) paths of output files to summarise.
#' @param qa_summary (data.frame) QA summary table from run_parity_qa.
#' @param log_dir (character) directory to write the structured log file to.
#'
#' @return (invisible) path to the written log file.
write_run_log <- function(run_id, log_file_path, config, output_files, qa_summary, log_dir) {

    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(log_dir, paste0("pipeline_run_", run_id, ".log"))

    lines <- character(0)
    add <- function(...) lines <<- c(lines, paste0(...))

    add("===== PIPELINE RUN =====")
    add("Run ID:      ", run_id)
    add("Timestamp:   ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    add("Timepoint:   ", config$run$timepoint)
    github_user <- tryCatch(
        trimws(system("git config user.name", intern = TRUE, ignore.stderr = TRUE)[1]),
        error = function(e) ""
    )
    if (is.na(github_user) || !nzchar(github_user)) {
        github_user <- Sys.getenv("GITHUB_ACTOR",
                       unset = Sys.getenv("USERNAME",
                       unset = Sys.getenv("USER", unset = "unknown")))
    }
    add("User:        ", github_user)
    add("R version:   ", as.character(getRversion()))
    add("sf:          ", as.character(utils::packageVersion("sf")))
    add("tidyverse:   ", as.character(utils::packageVersion("tidyverse")))
    add("haven:       ", as.character(utils::packageVersion("haven")))
    add("")

    add("--- CONFIG ---")
    add("gps_accuracy_threshold_m: ", config$thresholds$gps_accuracy_threshold_m)
    add("dhs_max_distance_m:       ", config$thresholds$dhs_max_distance_m)
    add("drive_path:               ", config$paths$drive_path)
    add("")

    add("--- LOG ---")
    if (!is.null(log_file_path) && file.exists(log_file_path)) {
        raw_lines <- readLines(log_file_path, warn = FALSE)
        lines <- c(lines, raw_lines)
    } else {
        add("(no log file captured)")
    }
    add("")

    add("--- OUTPUTS ---")
    for (f in output_files) {
        if (file.exists(f)) {
            info <- file.info(f)
            size_mb <- round(info$size / 1e6, 2)
            mtime  <- format(info$mtime, "%H:%M:%S")
            # try to get row count for CSV
            row_count <- tryCatch({
                if (grepl("\\.csv$", f, ignore.case = TRUE)) {
                    nrow(read.csv(f, nrows = 1L, check.names = FALSE)) # just header check
                    length(readLines(f, warn = FALSE)) - 1L
                } else {
                    NA_integer_
                }
            }, error = function(e) NA_integer_)
            row_str <- if (!is.na(row_count)) paste0(row_count, " rows  ") else ""
            add(sprintf("%-52s %s%s MB  %s", basename(f), row_str, size_mb, mtime))
        } else {
            add(basename(f), "  NOT FOUND")
        }
    }
    add("")

    add("--- QA SUMMARY ---")
    if (!is.null(qa_summary) && nrow(qa_summary) > 0) {
        for (i in seq_len(nrow(qa_summary))) {
            r <- qa_summary[i, ]
            add(sprintf(
                "%-24s %s  mismatches=%-4s current_dup_keys=%-6s baseline_dup_keys=%s",
                r$report_prefix,
                r$status,
                r$total_mismatches,
                r$total_current_duplicate_keys,
                r$total_baseline_duplicate_keys
            ))
        }
    } else {
        add("(no QA results)")
    }
    add("")
    add("===== END =====")

    writeLines(lines, out_path)
    invisible(out_path)
}


#' Loads the yaml config
#'
#' @param config_path (str, pathlike) path to config file. If not provided will
#'     default to "./src/config.yaml"
#' 
#' @return config (??) file containing configuration options for the pipeline.
load_config <- function(config_path = NULL){

  if (is.null(config_path)) {
    config_path <- file.path("src", "config.yaml")
  }

  config <- yaml::read_yaml(config_path)
  print(typeof(config))
  return(config)
}

#' Construct a nationally unique EA_CODE from census component fields
#'
#' Replicates the EA_CODE construction used in the mphc census processing:
#' EA_CODE = district + zero-padded TA (2 digits) + zero-padded EA (3 digits).
#' This is the single source of truth for EA_CODE construction across the pipeline.
#'
#' @param district (vector) District code values.
#' @param ta (vector) Traditional Authority code values.
#' @param ea (vector) Enumeration Area code values (within TA).
#'
#' @return character vector of EA_CODE values.
make_ea_code <- function(district, ta, ea) {
  stringr::str_c(
    district,
    stringr::str_pad(ta, width = 2, pad = "0"),
    stringr::str_pad(ea, width = 3, pad = "0")
  )
}

#' Configure Pandoc for rmarkdown/knitr rendering
#'
#' Attempts to locate Pandoc in the following order:
#'   1. Check if RSTUDIO_PANDOC is already set
#'   2. Look for pandoc in system PATH (Sys.which)
#'   3. Check for Quarto's bundled Pandoc (C:/Program Files/Quarto/bin/tools on Windows)
#'   4. Check common Linux/Mac Quarto paths (/usr/local/bin, ~/.local/bin)
#'
#' If Pandoc is found, sets RSTUDIO_PANDOC environment variable.
#' If not found, issues a warning but allows rmarkdown to attempt default behavior.
#'
#' @return Invisibly returns the path to Pandoc if found, NULL otherwise.
#' @examples
#' setup_pandoc()  # Call once at the start of your script
#'
#' @export
setup_pandoc <- function() {
    # Check if already set
    existing_pandoc <- Sys.getenv("RSTUDIO_PANDOC")
    if (nzchar(existing_pandoc)) {
        return(invisible(existing_pandoc))
    }
    
    # Try to find pandoc in system PATH
    pandoc_which <- Sys.which("pandoc")
    if (nzchar(pandoc_which)) {
        pandoc_dir <- dirname(pandoc_which)
        Sys.setenv(RSTUDIO_PANDOC = pandoc_dir)
        return(invisible(pandoc_dir))
    }
    
    # Try Quarto's bundled Pandoc
    quarto_paths <- c(
        "C:/Program Files/Quarto/bin/tools",  # Windows lockdown machine
        "C:/Program Files (x86)/Quarto/bin/tools",  # Alternative Windows path
        "/usr/local/opt/quarto/bin/tools",    # macOS
        "/usr/lib/quarto/bin/tools",          # Linux
        "/opt/quarto/bin/tools"               # Alternative Linux
    )
    
    for (path in quarto_paths) {
        if (dir.exists(path)) {
            Sys.setenv(RSTUDIO_PANDOC = path)
            return(invisible(path))
        }
    }
    
    # If nothing found, warn but don't fail
    warning(
        "Could not locate Pandoc. rmarkdown may fail unless Pandoc is available in your system PATH. ",
        "Consider installing Pandoc (https://pandoc.org) or Quarto (https://quarto.org)."
    )
    invisible(NULL)
}