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

# Escape text for the lightweight JSON serializer below.
json_escape <- function(text) {
  text <- gsub("\\\\", "\\\\\\\\", text)
  text <- gsub('"', '\\"', text)
  text <- gsub("\n", "\\n", text)
  text <- gsub("\r", "\\r", text)
  text <- gsub("\t", "\\t", text)
  text
}

is_named_list <- function(value) {
  is.list(value) && !is.null(names(value)) && all(nzchar(names(value)))
}

# Serialize a nested R object to JSON without introducing an additional dependency.
to_json <- function(value, indent = 0) {
  indent_prefix <- paste(rep("  ", indent), collapse = "")
  next_indent_prefix <- paste(rep("  ", indent + 1), collapse = "")

  if (is.null(value)) {
    return("null")
  }

  if (is.list(value)) {
    if (length(value) == 0) {
      return(if (is_named_list(value)) "{}" else "[]")
    }

    if (is_named_list(value)) {
      json_entries <- vapply(
        names(value),
        function(name) {
          paste0(
            next_indent_prefix,
            '"',
            json_escape(name),
            '": ',
            to_json(value[[name]], indent + 1)
          )
        },
        character(1)
      )

      return(paste0("{\n", paste(json_entries, collapse = ",\n"), "\n", indent_prefix, "}"))
    }

    json_values <- vapply(
      value,
      function(entry) paste0(next_indent_prefix, to_json(entry, indent + 1)),
      character(1)
    )

    return(paste0("[\n", paste(json_values, collapse = ",\n"), "\n", indent_prefix, "]"))
  }

  if (is.character(value)) {
    if (length(value) == 1) {
      return(paste0('"', json_escape(value), '"'))
    }
    return(to_json(as.list(value), indent))
  }

  if (is.logical(value)) {
    if (length(value) == 1) {
      if (is.na(value)) {
        return("null")
      }
      return(tolower(as.character(value)))
    }
    return(to_json(as.list(value), indent))
  }

  if (is.numeric(value) || is.integer(value)) {
    if (length(value) == 1) {
      if (is.na(value) || !is.finite(value)) {
        return("null")
      }
      return(format(value, scientific = FALSE, trim = TRUE))
    }
    return(to_json(as.list(value), indent))
  }

  paste0('"', json_escape(as.character(value)), '"')
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

# Render the metrics object into a reviewer-friendly Markdown report.
build_markdown_summary <- function(metrics) {
  lines <- c(
    "# Baseline Metrics Summary",
    "",
    paste0("- Generated at UTC: `", metrics$generated_at_utc, "`"),
    paste0("- Input CSV: `", metrics$input_csv, "`"),
    paste0("- JSON output: `", metrics$output_json, "`"),
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

  na_counts <- unlist(metrics$per_column_na_counts, use.names = TRUE)
  na_counts <- sort(na_counts, decreasing = TRUE)
  non_zero_na_counts <- na_counts[na_counts > 0]

  lines <- c(lines, "", "## Columns with missing values", "")
  if (length(non_zero_na_counts) == 0) {
    lines <- c(lines, "No columns contain missing values in the baseline CSV.")
  } else {
    lines <- c(lines, "| Column | Missing values |", "|--------|----------------|")
    lines <- c(
      lines,
      vapply(
        names(non_zero_na_counts),
        function(column_name) paste0("| `", column_name, "` | ", format_scalar(non_zero_na_counts[[column_name]]), " |"),
        character(1)
      )
    )
  }

  source_counts <- unlist(metrics$source_column_non_na_counts, use.names = TRUE)
  lines <- c(lines, "", "## Source coverage", "")
  if (length(source_counts) == 0) {
    lines <- c(lines, "No source coverage columns were present in the baseline CSV.")
  } else {
    lines <- c(lines, "| Source column | Non-missing rows |", "|---------------|------------------|")
    lines <- c(
      lines,
      vapply(
        names(source_counts),
        function(column_name) paste0("| `", column_name, "` | ", format_scalar(source_counts[[column_name]]), " |"),
        character(1)
      )
    )
  }

  numeric_summaries <- metrics$key_numeric_summaries
  lines <- c(lines, "", "## Key numeric summaries", "")
  if (length(numeric_summaries) == 0) {
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
        names(numeric_summaries),
        function(column_name) {
          summary_values <- numeric_summaries[[column_name]]
          paste0(
            "| `", column_name, "` | ",
            format_scalar(summary_values$non_na_count), " | ",
            format_scalar(summary_values$min), " | ",
            format_scalar(summary_values$median), " | ",
            format_scalar(summary_values$mean), " | ",
            format_scalar(summary_values$max), " | ",
            format_scalar(summary_values$sum), " |"
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
output_json <- get_arg_value("--output", file.path("qa", "baseline_metrics.json"))
output_summary <- get_arg_value("--summary", file.path("qa", "baseline_metrics.md"))

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
per_column_na_counts <- as.list(unname(colSums(is.na(baseline_data))))
names(per_column_na_counts) <- names(baseline_data)

source_column_non_na_counts <- as.list(vapply(
  source_columns,
  function(column_name) sum(!is.na(baseline_data[[column_name]])),
  numeric(1)
))
names(source_column_non_na_counts) <- source_columns

key_numeric_summaries <- lapply(key_numeric_columns, function(column_name) {
  build_numeric_summary(baseline_data[[column_name]])
})
names(key_numeric_summaries) <- key_numeric_columns

# Store the machine-readable metric payload that later QA steps can consume.
metrics <- list(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  input_csv = normalizePath(input_csv, winslash = "/", mustWork = FALSE),
  output_json = normalizePath(output_json, winslash = "/", mustWork = FALSE),
  output_summary = normalizePath(output_summary, winslash = "/", mustWork = FALSE),
  row_count = nrow(baseline_data),
  column_count = ncol(baseline_data),
  columns = as.list(names(baseline_data)),
  unique_ea_code_count = if ("EA_CODE" %in% names(baseline_data)) {
    length(unique(stats::na.omit(baseline_data$EA_CODE)))
  } else {
    NULL
  },
  total_na_count = sum(is.na(baseline_data)),
  per_column_na_counts = per_column_na_counts,
  source_column_non_na_counts = source_column_non_na_counts,
  key_numeric_summaries = key_numeric_summaries
)

# Ensure both output directories exist before writing the JSON and Markdown artifacts.
output_dir <- dirname(output_json)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

writeLines(to_json(metrics), con = output_json, useBytes = TRUE)

summary_dir <- dirname(output_summary)
if (!dir.exists(summary_dir)) {
  dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
}

writeLines(build_markdown_summary(metrics), con = output_summary, useBytes = TRUE)

message(
  "Baseline metrics written. Rows=", metrics$row_count,
  ", Columns=", metrics$column_count,
  ", Total NAs=", metrics$total_na_count,
  ". JSON: ", output_json,
  ". Summary: ", output_summary
)