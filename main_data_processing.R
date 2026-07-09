#' Detect the directory of the current script
#'
#' Falls back to the supplied default when the script file cannot be inferred
#' from the call stack.
#'
#' @param default_dir Directory to use when the script location is unknown.
#'
#' @return Normalized directory path for the wrapper script.
detect_script_directory <- function(default_dir = getwd()) {
  frame_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) {
        return(NA_character_)
      }

      as.character(frame$ofile)
    },
    character(1)
  )

  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files) == 0) {
    return(normalizePath(default_dir, winslash = "/", mustWork = FALSE))
  }

  dirname(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = FALSE))
}

project_root <- detect_script_directory()
data_processing_helpers <- new.env(parent = globalenv())
sys.source(file.path(project_root, "R", "data_processing_helpers.R"), envir = data_processing_helpers)

data_processing_pipeline <- new.env(parent = globalenv())
sys.source(file.path(project_root, "00_Data_Processing2.R"), envir = data_processing_pipeline)
data_processing_pipeline$log_pipeline_event <- data_processing_helpers$log_pipeline_event

#' Read a simple CLI flag value
#'
#' @param args Character vector of trailing command-line arguments.
#' @param flag Flag name such as `--config`.
#' @param default_value Value to use when the flag is absent.
#'
#' @return The argument value following the flag, or the default.
get_cli_arg_value <- function(args, flag, default_value = NULL) {
  flag_index <- match(flag, args)
  if (is.na(flag_index) || flag_index == length(args)) {
    return(default_value)
  }

  args[[flag_index + 1]]
}

#' Parse a boolean CLI argument
#'
#' Accepts common true and false spellings used in wrapper invocations.
#'
#' @param value Raw CLI argument value.
#'
#' @return TRUE, FALSE, or NULL when the value is blank.
parse_cli_boolean <- function(value) {
  if (is.null(value) || !nzchar(value)) {
    return(NULL)
  }

  normalized_value <- tolower(trimws(as.character(value)))
  if (normalized_value %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }

  if (normalized_value %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }

  stop("--run-qa must be one of true/false, yes/no, or 1/0.", call. = FALSE)
}

#' Run the config-driven data processing wrapper
#'
#' Loads the pipeline config, initializes logging, runs the main processing
#' entrypoint, and optionally triggers the baseline QA capture step.
#'
#' @param config_path Path to the wrapper config file.
#' @param timepoint_override Optional timepoint override.
#' @param run_qa Optional boolean override for the config's QA flag.
#'
#' @return The pipeline result list, invisibly.
run_data_processing_main <- function(config_path = file.path("config", "data_processing.yml"), timepoint_override = NULL, run_qa = NULL) {
  resolved_config_path <- if (data_processing_helpers$is_absolute_path(config_path)) {
    config_path
  } else {
    file.path(project_root, config_path)
  }

  config <- data_processing_helpers$read_data_processing_config(resolved_config_path, timepoint_override = timepoint_override)
  if (!is.null(run_qa)) {
    config$run$run_qa <- isTRUE(run_qa)
  }

  log_state <- data_processing_helpers$init_pipeline_log(config)
  on.exit(data_processing_helpers$close_pipeline_log(log_state), add = TRUE)

  data_processing_helpers$log_pipeline_event(paste("Loaded data processing config from", config$config_path), state = log_state)
  data_processing_helpers$log_pipeline_event(paste("Selected timepoint", config$run$timepoint), state = log_state)

  result <- tryCatch(
    {
      pipeline_result <- data_processing_pipeline$run_data_processing_pipeline(
        config,
        run_qa = isTRUE(config$run$run_qa),
        log_state = log_state
      )

      if (isTRUE(config$run$run_qa)) {
        data_processing_helpers$run_baseline_metrics_capture(config, log_state = log_state)
      }

      data_processing_helpers$log_pipeline_event("Data processing wrapper completed successfully.", state = log_state)
      pipeline_result
    },
    error = function(error) {
      data_processing_helpers$log_pipeline_event(
        paste("Data processing wrapper failed:", conditionMessage(error)),
        level = "ERROR",
        state = log_state
      )
      stop(conditionMessage(error), call. = FALSE)
    }
  )

  invisible(result)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- get_cli_arg_value(args, "--config", file.path("config", "data_processing.yml"))
  timepoint_override <- get_cli_arg_value(args, "--timepoint", NULL)
  run_qa <- parse_cli_boolean(get_cli_arg_value(args, "--run-qa", NULL))

  run_data_processing_main(
    config_path = config_path,
    timepoint_override = timepoint_override,
    run_qa = run_qa
  )
}