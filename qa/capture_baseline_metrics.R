args <- commandArgs(trailingOnly = TRUE)

# Parse simple --flag value pairs while preserving a default when a flag is absent.
get_arg_value <- function(flag, default_value) {
  flag_index <- match(flag, args)
  if (is.na(flag_index) || flag_index == length(args)) {
    return(default_value)
  }
  args[[flag_index + 1]]
}

# Format scalar values consistently for the human-readable Markdown summary.
format_scalar <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("NA")
  }

  if (is.numeric(value) || is.integer(value)) {
    return(format(value, scientific = FALSE, trim = TRUE))
  }

  as.character(value)
}

# Build a small summary pack for numeric columns that are useful in QA review.
build_numeric_summary <- function(values) {
  numeric_values <- suppressWarnings(as.numeric(values))
  valid_values <- numeric_values[!is.na(numeric_values) & is.finite(numeric_values)]

  if (length(valid_values) == 0) {
    return(list(
      non_na_count = sum(!is.na(values)),
      min = NULL,
      median = NULL,
      mean = NULL,
      max = NULL,
      sum = NULL
    ))
  }

  list(
    non_na_count = sum(!is.na(values)),
    min = unname(min(valid_values)),
    median = unname(median(valid_values)),
    mean = unname(mean(valid_values)),
    max = unname(max(valid_values)),
    sum = unname(sum(valid_values))
  )
}

# Build a tidy table that records missingness for every column in the baseline CSV.
build_missing_values_table <- function(data_frame) {
  missing_value_count <- colSums(is.na(data_frame))
  non_missing_value_count <- nrow(data_frame) - missing_value_count

  output <- data.frame(
    column_name = names(data_frame),
    missing_value_count = unname(missing_value_count),
    non_missing_value_count = unname(non_missing_value_count),
    missing_value_rate = if (nrow(data_frame) == 0) {
      rep(NA_real_, length(missing_value_count))
    } else {
      unname(missing_value_count) / nrow(data_frame)
    },
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  output[order(output$missing_value_count, decreasing = TRUE), , drop = FALSE]
}

# Build a flat table of numeric summaries that can be reviewed without JSON parsing.
build_numeric_summary_table <- function(data_frame, column_names) {
  if (length(column_names) == 0) {
    return(data.frame(
      column_name = character(0),
      non_na_count = numeric(0),
      min = numeric(0),
      median = numeric(0),
      mean = numeric(0),
      max = numeric(0),
      sum = numeric(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  output <- lapply(column_names, function(column_name) {
    summary_values <- build_numeric_summary(data_frame[[column_name]])
    data.frame(
      column_name = column_name,
      non_na_count = summary_values$non_na_count,
      min = summary_values$min,
      median = summary_values$median,
      mean = summary_values$mean,
      max = summary_values$max,
      sum = summary_values$sum,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  do.call(rbind, output)
}

# Render the metrics object into a reviewer-friendly Markdown report.
build_markdown_summary <- function(metrics, missing_values_table, numeric_summary_table, source_columns) {
  lines <- c(
    "# Baseline Metrics Summary",
    "",
    paste0("- Generated at GMT: `", metrics$generated_at_gmt, "`"),
    paste0("- Input CSV: `", metrics$input_csv, "`"),
    paste0("- Markdown output: `", metrics$output_summary, "`"),
    paste0("- Missing values CSV: `", metrics$output_missing_values_csv, "`"),
    paste0("- Numeric summaries CSV: `", metrics$output_numeric_summaries_csv, "`"),
    "",
    "## Headline metrics",
    "",
    "| Metric | Value |",
    "|--------|-------|",
    paste0("| Row count | ", format_scalar(metrics$row_count), " |"),
    paste0("| Column count | ", format_scalar(metrics$column_count), " |"),
    paste0("| Unique EA_CODE count | ", format_scalar(metrics$unique_ea_code_count), " |"),
    paste0("| Total NA count | ", format_scalar(metrics$total_na_count), " |")
  )

  non_zero_na_counts <- missing_values_table[missing_values_table$missing_value_count > 0, , drop = FALSE]

  lines <- c(lines, "", "## Columns with missing values", "")
  if (nrow(non_zero_na_counts) == 0) {
    lines <- c(lines, "No columns contain missing values in the baseline CSV.")
  } else {
    lines <- c(lines, "| Column | Missing values |", "|--------|----------------|")
    lines <- c(
      lines,
      vapply(
        seq_len(nrow(non_zero_na_counts)),
        function(index) paste0("| `", non_zero_na_counts$column_name[[index]], "` | ", format_scalar(non_zero_na_counts$missing_value_count[[index]]), " |"),
        character(1)
      )
    )
  }

  source_coverage_table <- missing_values_table[missing_values_table$column_name %in% source_columns, c("column_name", "non_missing_value_count"), drop = FALSE]
  lines <- c(lines, "", "## Source coverage", "")
  if (nrow(source_coverage_table) == 0) {
    lines <- c(lines, "No source coverage columns were present in the baseline CSV.")
  } else {
    lines <- c(lines, "| Source column | Non-missing rows |", "|---------------|------------------|")
    lines <- c(
      lines,
      vapply(
        seq_len(nrow(source_coverage_table)),
        function(index) paste0("| `", source_coverage_table$column_name[[index]], "` | ", format_scalar(source_coverage_table$non_missing_value_count[[index]]), " |"),
        character(1)
      )
    )
  }

  lines <- c(lines, "", "## Key numeric summaries", "")
  if (nrow(numeric_summary_table) == 0) {
    lines <- c(lines, "No key numeric columns were present in the baseline CSV.")
  } else {
    lines <- c(
      lines,
      "| Column | Non-missing | Min | Median | Mean | Max | Sum |",
      "|--------|-------------|-----|--------|------|-----|-----|"
    )
    lines <- c(
      lines,
      vapply(
        seq_len(nrow(numeric_summary_table)),
        function(index) {
          paste0(
            "| `", numeric_summary_table$column_name[[index]], "` | ",
            format_scalar(numeric_summary_table$non_na_count[[index]]), " | ",
            format_scalar(numeric_summary_table$min[[index]]), " | ",
            format_scalar(numeric_summary_table$median[[index]]), " | ",
            format_scalar(numeric_summary_table$mean[[index]]), " | ",
            format_scalar(numeric_summary_table$max[[index]]), " | ",
            format_scalar(numeric_summary_table$sum[[index]]), " |"
          )
        },
        character(1)
      )
    )
  }

  paste(lines, collapse = "\n")
}

# Resolve input and output paths, then load the frozen benchmark CSV.
input_csv <- get_arg_value("--input", file.path("data", "Output_Data", "summarized_survey_data.csv"))
output_summary <- get_arg_value("--summary", file.path("data", "qa", "baseline_metrics.md"))
output_missing_values_csv <- get_arg_value("--missing-values-csv", file.path("data", "qa", "baseline_missing_values.csv"))
output_numeric_summaries_csv <- get_arg_value("--numeric-summaries-csv", file.path("data", "qa", "baseline_numeric_summaries.csv"))

if (!file.exists(input_csv)) {
  stop("Baseline CSV does not exist: ", input_csv)
}

baseline_data <- read.csv(input_csv, stringsAsFactors = FALSE, check.names = FALSE)

# Only summarize source-coverage fields that actually exist in the baseline file.
source_columns <- intersect(
  c(
    "dhs_hh_count",
    "mphc_hh_count",
    "ict_hh_count",
    "ihs_hh_count",
    "naca_hh_count",
    "zomba_hh_count",
    "malemia_hh_count"
  ),
  names(baseline_data)
)

# Keep the numeric summary focused on fields that matter most for benchmark review.
key_numeric_columns <- intersect(
  c(
    "mphc_total_pop",
    "observed_hh_count",
    "mphc_bcount",
    "hh_size",
    "mphc_median_hh_size",
    "mphc_mean_hh_size",
    "dhs_median_hh_size",
    "dhs_mean_hh_size"
  ),
  names(baseline_data)
)

# Capture completeness metrics for the full file plus source-specific coverage.
missing_values_table <- build_missing_values_table(baseline_data)
numeric_summary_table <- build_numeric_summary_table(baseline_data, key_numeric_columns)

# Store the headline metrics used in the human-readable outputs.
metrics <- list(
  generated_at_gmt = format(Sys.time(), tz = "GMT", usetz = TRUE),
  input_csv = normalizePath(input_csv, winslash = "/", mustWork = FALSE),
  output_summary = normalizePath(output_summary, winslash = "/", mustWork = FALSE),
  output_missing_values_csv = normalizePath(output_missing_values_csv, winslash = "/", mustWork = FALSE),
  output_numeric_summaries_csv = normalizePath(output_numeric_summaries_csv, winslash = "/", mustWork = FALSE),
  row_count = nrow(baseline_data),
  column_count = ncol(baseline_data),
  columns = as.list(names(baseline_data)),
  unique_ea_code_count = if ("EA_CODE" %in% names(baseline_data)) {
    length(unique(stats::na.omit(baseline_data$EA_CODE)))
  } else {
    NULL
  },
  total_na_count = sum(is.na(baseline_data))
)

# Ensure all output directories exist before writing the Markdown and CSV artifacts.
output_paths <- c(output_summary, output_missing_values_csv, output_numeric_summaries_csv)
output_dirs <- unique(dirname(output_paths))
for (output_dir in output_dirs) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

utils::write.csv(missing_values_table, file = output_missing_values_csv, row.names = FALSE)
utils::write.csv(numeric_summary_table, file = output_numeric_summaries_csv, row.names = FALSE)
writeLines(build_markdown_summary(metrics, missing_values_table, numeric_summary_table, source_columns), con = output_summary, useBytes = TRUE)

message(
  "Baseline metrics written. Rows=", metrics$row_count,
  ", Columns=", metrics$column_count,
  ", Total NAs=", metrics$total_na_count,
  ". Summary: ", output_summary,
  ". Missing values CSV: ", output_missing_values_csv,
  ". Numeric summaries CSV: ", output_numeric_summaries_csv
)