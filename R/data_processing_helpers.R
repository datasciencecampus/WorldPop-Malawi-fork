#' Return a default when a scalar-like value is missing
#'
#' Treats NULL, length-0 values, and scalar NA values as missing and returns
#' the supplied default instead.
#'
#' @param value Candidate value.
#' @param default_value Fallback value to return when value is missing.
#'
#' @return The original value or the supplied default.
value_or_default <- function(value, default_value) {
  if (is.null(value) || length(value) == 0) {
    return(default_value)
  }

  if (is.atomic(value) && length(value) == 1 && is.na(value)) {
    return(default_value)
  }

  value
}

#' Check whether a path is already absolute
#'
#' Supports Windows drive-letter paths, UNC paths, and Unix-style absolute
#' paths.
#'
#' @param path_value Path string to test.
#'
#' @return TRUE when the path is absolute, otherwise FALSE.
is_absolute_path <- function(path_value) {
  grepl("^([A-Za-z]:[/\\\\]|[/\\\\]{2}|/)", path_value)
}

#' Resolve a config path relative to the project root
#'
#' Dot-prefixed relative paths are preserved as project-relative children so
#' they do not get rooted twice.
#'
#' @param path_value Path value from config.
#' @param project_root Absolute project root used for resolution.
#'
#' @return A normalized absolute path, or the original value when it should not
#'   be resolved.
resolve_against_root <- function(path_value, project_root) {
  if (!is.character(path_value) || length(path_value) != 1 || is.na(path_value) || !nzchar(path_value)) {
    return(path_value)
  }

  if (is_absolute_path(path_value)) {
    return(normalizePath(path_value, winslash = "/", mustWork = FALSE))
  }

  relative_path <- path_value
  if (startsWith(relative_path, "./") || startsWith(relative_path, ".\\")) {
    relative_path <- substring(relative_path, 3)
  }

  normalizePath(file.path(project_root, relative_path), winslash = "/", mustWork = FALSE)
}

#' Decide whether a config value should be treated as a path
#'
#' Uses the config key suffix and simple path-shape heuristics to identify
#' values that should be resolved against the project root.
#'
#' @param key_name Config key name.
#' @param value Config value.
#'
#' @return TRUE when the value should be path-resolved, otherwise FALSE.
should_resolve_config_value <- function(key_name, value) {
  if (!is.character(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
    return(FALSE)
  }

  normalized_key <- value_or_default(key_name, "")

  grepl("(path|file|dir|csv|gpkg|geojson|shp|template)$", normalized_key, ignore.case = TRUE) ||
    grepl("[/\\\\]", value)
}

#' Recursively merge two config lists
#'
#' Scalar values from the overlay replace values in the base list, while nested
#' lists are merged recursively.
#'
#' @param base Base list.
#' @param overlay Overlay list whose values win on conflicts.
#'
#' @return A merged list.
deep_merge_lists <- function(base, overlay) {
  if (is.null(base)) {
    return(overlay)
  }

  if (is.null(overlay)) {
    return(base)
  }

  result <- base
  for (name in names(overlay)) {
    if (is.list(result[[name]]) && is.list(overlay[[name]])) {
      result[[name]] <- deep_merge_lists(result[[name]], overlay[[name]])
    } else {
      result[[name]] <- overlay[[name]]
    }
  }

  result
}

#' Recursively resolve path-like values inside a config tree
#'
#' Walks a nested list and converts path-like scalar values to absolute paths
#' rooted at the project directory.
#'
#' @param node Current config node.
#' @param project_root Absolute project root.
#' @param key_name Name of the current node.
#'
#' @return A config node with path-like values resolved.
resolve_config_paths <- function(node, project_root, key_name = "") {
  if (is.list(node)) {
    result <- node
    for (name in names(node)) {
      result[[name]] <- resolve_config_paths(node[[name]], project_root, name)
    }
    return(result)
  }

  if (should_resolve_config_value(key_name, node)) {
    return(resolve_against_root(node, project_root))
  }

  node
}

#' Load and normalize the data processing config
#'
#' Reads the YAML config, applies the selected profile, resolves path-like
#' values against the project root, and records the normalized config path.
#'
#' @param config_path Path to the YAML config file.
#' @param timepoint_override Optional timepoint override applied after reading
#'   the config file.
#'
#' @return A normalized config list ready for pipeline execution.
read_data_processing_config <- function(config_path = file.path("config", "data_processing.yml"), timepoint_override = NULL) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read the data processing config.", call. = FALSE)
  }

  resolved_config_path <- if (is_absolute_path(config_path)) {
    config_path
  } else {
    file.path(getwd(), config_path)
  }

  if (!file.exists(resolved_config_path)) {
    stop("Data processing config does not exist: ", resolved_config_path, call. = FALSE)
  }

  resolved_config_path <- normalizePath(resolved_config_path, winslash = "/", mustWork = TRUE)
  project_root <- normalizePath(file.path(dirname(resolved_config_path), ".."), winslash = "/", mustWork = TRUE)

  config <- yaml::read_yaml(resolved_config_path)
  if (!is.list(config)) {
    stop("Data processing config must deserialize to a named list.", call. = FALSE)
  }

  if (is.null(config$run)) {
    config$run <- list()
  }

  if (is.null(config$paths)) {
    config$paths <- list()
  }

  selected_timepoint <- as.character(value_or_default(timepoint_override, value_or_default(config$run$timepoint, "")))
  if (!nzchar(selected_timepoint)) {
    stop("Data processing config must define run.timepoint or provide a timepoint override.", call. = FALSE)
  }

  config$run$timepoint <- selected_timepoint

  if (!is.null(config$profiles)) {
    profile <- config$profiles[[selected_timepoint]]
    if (!is.null(profile)) {
      config$paths <- deep_merge_lists(config$paths, profile)
    }
  }

  config <- resolve_config_paths(config, project_root)
  config$config_path <- resolved_config_path
  config$project_root <- project_root
  config
}

pipeline_log_env <- new.env(parent = emptyenv())
pipeline_log_env$state <- NULL

#' Test whether a file connection is still open
#'
#' @param connection An R connection object.
#'
#' @return TRUE when the connection is open, otherwise FALSE.
is_connection_open <- function(connection) {
  tryCatch(isOpen(connection), error = function(...) FALSE)
}

#' Build the timestamped pipeline log file path
#'
#' Expands the configured log template and replaces `<timestamp>` using a GMT
#' timestamp suitable for reproducible log names.
#'
#' @param config Normalized pipeline config.
#' @param timestamp Timestamp to encode in the log filename.
#'
#' @return Normalized absolute log file path.
build_log_file_path <- function(config, timestamp = Sys.time()) {
  logging_config <- value_or_default(config$logging, list())
  log_template <- value_or_default(logging_config$log_file_template, "./logs/log_<timestamp>.log")
  timestamp_label <- format(timestamp, "%Y%m%d_%H%M%S", tz = "GMT")
  resolved_template <- resolve_against_root(log_template, value_or_default(config$project_root, getwd()))
  normalizePath(sub("<timestamp>", timestamp_label, resolved_template, fixed = TRUE), winslash = "/", mustWork = FALSE)
}

#' Write a pipeline event to the log and optionally stdout
#'
#' @param message_text Message body to emit.
#' @param level Severity label such as INFO or ERROR.
#' @param state Optional explicit log state. Defaults to the active pipeline log
#'   state.
#'
#' @return The formatted log line, invisibly.
log_pipeline_event <- function(message_text, level = "INFO", state = NULL) {
  active_state <- value_or_default(state, pipeline_log_env$state)
  timestamp_label <- format(Sys.time(), "%Y-%m-%d %H:%M:%S GMT", tz = "GMT")
  log_line <- sprintf("[%s] [%s] %s", timestamp_label, level, message_text)

  if (!is.null(active_state$connection) && is_connection_open(active_state$connection)) {
    writeLines(log_line, con = active_state$connection, useBytes = TRUE)
    flush(active_state$connection)
  }

  if (is.null(active_state) || isTRUE(active_state$mirror_to_stdout)) {
    message(log_line)
  }

  invisible(log_line)
}

#' Initialize the pipeline log connection
#'
#' Creates the destination directory when needed, opens a writable log file, and
#' stores the active log state in the helper environment.
#'
#' @param config Normalized pipeline config.
#'
#' @return A log state list containing the file path, connection, and stdout
#'   mirror flag.
init_pipeline_log <- function(config) {
  log_file_path <- build_log_file_path(config)
  log_dir <- dirname(log_file_path)
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }

  state <- list(
    path = log_file_path,
    connection = file(log_file_path, open = "wt"),
    mirror_to_stdout = isTRUE(config$logging$mirror_to_stdout)
  )

  pipeline_log_env$state <- state
  log_pipeline_event(paste("Initialized pipeline log at", log_file_path), state = state)
  state
}

#' Close the active pipeline log connection
#'
#' @param state Optional explicit log state. Defaults to the active pipeline log
#'   state.
#'
#' @return NULL, invisibly.
close_pipeline_log <- function(state = NULL) {
  active_state <- value_or_default(state, pipeline_log_env$state)

  if (!is.null(active_state$connection) && is_connection_open(active_state$connection)) {
    close(active_state$connection)
  }

  pipeline_log_env$state <- NULL
  invisible(NULL)
}

#' Run the baseline QA capture script with config-derived paths
#'
#' Executes the existing baseline metrics capture script in a controlled
#' environment so the wrapper can generate QA artifacts without relying on an
#' external `Rscript` call.
#'
#' @param config Normalized pipeline config.
#' @param log_state Optional active pipeline log state.
#'
#' @return A list of QA artifact paths, invisibly.
run_baseline_metrics_capture <- function(config, log_state = NULL) {
  qa_config <- value_or_default(config$qa, list())
  output_config <- value_or_default(config$outputs, list())
  capture_script_path <- file.path(value_or_default(config$project_root, getwd()), "qa", "capture_baseline_metrics.R")

  if (!file.exists(capture_script_path)) {
    stop("Baseline metrics capture script does not exist: ", capture_script_path, call. = FALSE)
  }

  qa_input_csv <- value_or_default(qa_config$baseline_summarized_csv, output_config$summarized_csv)
  qa_summary <- qa_config$baseline_summary_markdown
  qa_missing <- qa_config$baseline_missing_values_csv
  qa_numeric <- qa_config$baseline_numeric_summaries_csv

  qa_args <- c(
    "--input", qa_input_csv,
    "--summary", qa_summary,
    "--missing-values-csv", qa_missing,
    "--numeric-summaries-csv", qa_numeric
  )

  log_pipeline_event(paste("Running baseline QA capture via", capture_script_path), state = log_state)

  qa_env <- new.env(parent = globalenv())
  qa_env$commandArgs <- function(trailingOnly = FALSE) {
    if (isTRUE(trailingOnly)) {
      return(qa_args)
    }

    c("Rscript", capture_script_path, qa_args)
  }

  sys.source(capture_script_path, envir = qa_env)

  invisible(list(
    input_csv = qa_input_csv,
    summary = qa_summary,
    missing_values_csv = qa_missing,
    numeric_summaries_csv = qa_numeric
  ))
}