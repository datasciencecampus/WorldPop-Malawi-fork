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

#' Ensure a data frame contains the requested columns
#'
#' Adds missing columns with a shared fill value and leaves existing columns
#' unchanged.
#'
#' @param data_frame Input data frame.
#' @param column_names Character vector of required columns.
#' @param fill_value Value assigned to any missing columns.
#'
#' @return The data frame with all requested columns present.
ensure_columns <- function(data_frame, column_names, fill_value = NA_real_) {
  result <- data_frame
  for (column_name in column_names) {
    if (!column_name %in% names(result)) {
      result[[column_name]] <- fill_value
    }
  }

  result
}

#' Convert tabular coordinates into an sf point object
#'
#' Drops rows with missing coordinates, converts the remaining records to points,
#' and assigns the supplied source CRS.
#'
#' @param data_frame Input data frame.
#' @param longitude_col Longitude column name.
#' @param latitude_col Latitude column name.
#' @param crs Source coordinate reference system.
#'
#' @return An sf object containing the non-missing point rows.
create_point_sf <- function(data_frame, longitude_col, latitude_col, crs = 4326) {
  point_rows <- !is.na(data_frame[[longitude_col]]) & !is.na(data_frame[[latitude_col]])
  sf_data <- sf::st_as_sf(data_frame[point_rows, , drop = FALSE], coords = c(longitude_col, latitude_col))
  sf::st_crs(sf_data) <- crs
  sf_data
}

#' Transform point data into the reference CRS
#'
#' @param sf_data Source sf object.
#' @param reference_sf Reference sf object whose CRS should be matched.
#'
#' @return The transformed sf object.
transform_to_reference_crs <- function(sf_data, reference_sf) {
  sf::st_transform(sf_data, crs = sf::st_crs(reference_sf))
}

#' Assign the nearest reference identifier to each point
#'
#' Uses nearest-neighbour lookup to copy an identifier column from the reference
#' geometry into the point data.
#'
#' @param sf_data Point sf object.
#' @param reference_sf Reference sf object.
#' @param reference_id_col Identifier column in the reference object.
#' @param output_col Output column name added to the point data.
#'
#' @return The sf object with the nearest identifier column attached.
assign_nearest_reference_id <- function(sf_data, reference_sf, reference_id_col = "EA_CODE", output_col = reference_id_col) {
  if (nrow(sf_data) == 0) {
    sf_data[[output_col]] <- reference_sf[[reference_id_col]][0]
    return(sf_data)
  }

  nearest_indices <- sf::st_nearest_feature(sf_data, reference_sf)
  sf_data[[output_col]] <- reference_sf[[reference_id_col]][nearest_indices]
  sf_data
}

#' Keep only points whose nearest reference geometry is within a distance limit
#'
#' Computes the nearest-neighbour distance, stores the distance and pass/fail
#' flag, and returns only the rows within the requested threshold.
#'
#' @param sf_data Point sf object.
#' @param reference_sf Reference sf object.
#' @param max_distance_m Maximum nearest-neighbour distance to retain.
#' @param distance_col Column name used to store the computed distance.
#' @param flag_col Column name used to store the threshold flag.
#'
#' @return Filtered sf object with distance metadata columns attached.
filter_by_nearest_distance <- function(sf_data, reference_sf, max_distance_m, distance_col = "nearest_dist_m", flag_col = "within_threshold") {
  if (nrow(sf_data) == 0) {
    sf_data[[distance_col]] <- numeric(0)
    sf_data[[flag_col]] <- logical(0)
    return(sf_data)
  }

  nearest <- nngeo::st_nn(sf_data, reference_sf, k = 1, returnDist = TRUE)
  distances <- vapply(nearest$dist, function(value) value[[1]], numeric(1))

  sf_data[[distance_col]] <- distances
  sf_data[[flag_col]] <- distances < max_distance_m
  sf_data[sf_data[[flag_col]], , drop = FALSE]
}

#' Build the fixed MPHC age-group order used by the direct output
#'
#' @return Character vector of age-group column names in stable output order.
get_mphc_age_group_columns <- function() {
  c(
    "age_group_01_04",
    "age_group_01_less",
    "age_group_05_09",
    "age_group_10_14",
    "age_group_15_19",
    "age_group_20_24",
    "age_group_25_29",
    "age_group_30_34",
    "age_group_35_39",
    "age_group_40_44",
    "age_group_45_49",
    "age_group_50_54",
    "age_group_55_59",
    "age_group_60_64",
    "age_group_65_69",
    "age_group_70_74",
    "age_group_75_79",
    "age_group_80plus"
  )
}

#' Summarize MPHC age counts per EA
#'
#' Bins ages into the fixed age-group schema used by the pipeline and widens the
#' counts into one row per EA.
#'
#' @param data_frame Input MPHC data frame.
#' @param ea_col EA identifier column.
#' @param age_col Age column.
#'
#' @return Tibble containing one row per EA with age-group count columns.
build_mphc_age_summary <- function(data_frame, ea_col = "EA_CODE", age_col = "p05") {
  age_groups <- dplyr::case_when(
    data_frame[[age_col]] < 1 ~ "age_group_01_less",
    data_frame[[age_col]] >= 1 & data_frame[[age_col]] <= 4 ~ "age_group_01_04",
    data_frame[[age_col]] >= 5 & data_frame[[age_col]] <= 9 ~ "age_group_05_09",
    data_frame[[age_col]] >= 10 & data_frame[[age_col]] <= 14 ~ "age_group_10_14",
    data_frame[[age_col]] >= 15 & data_frame[[age_col]] <= 19 ~ "age_group_15_19",
    data_frame[[age_col]] >= 20 & data_frame[[age_col]] <= 24 ~ "age_group_20_24",
    data_frame[[age_col]] >= 25 & data_frame[[age_col]] <= 29 ~ "age_group_25_29",
    data_frame[[age_col]] >= 30 & data_frame[[age_col]] <= 34 ~ "age_group_30_34",
    data_frame[[age_col]] >= 35 & data_frame[[age_col]] <= 39 ~ "age_group_35_39",
    data_frame[[age_col]] >= 40 & data_frame[[age_col]] <= 44 ~ "age_group_40_44",
    data_frame[[age_col]] >= 45 & data_frame[[age_col]] <= 49 ~ "age_group_45_49",
    data_frame[[age_col]] >= 50 & data_frame[[age_col]] <= 54 ~ "age_group_50_54",
    data_frame[[age_col]] >= 55 & data_frame[[age_col]] <= 59 ~ "age_group_55_59",
    data_frame[[age_col]] >= 60 & data_frame[[age_col]] <= 64 ~ "age_group_60_64",
    data_frame[[age_col]] >= 65 & data_frame[[age_col]] <= 69 ~ "age_group_65_69",
    data_frame[[age_col]] >= 70 & data_frame[[age_col]] <= 74 ~ "age_group_70_74",
    data_frame[[age_col]] >= 75 & data_frame[[age_col]] <= 79 ~ "age_group_75_79",
    data_frame[[age_col]] >= 80 ~ "age_group_80plus",
    TRUE ~ NA_character_
  )

  age_group_columns <- get_mphc_age_group_columns()
  working_data <- data.frame(
    group_key = data_frame[[ea_col]],
    age_group = age_groups,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  working_data <- working_data[!is.na(working_data$group_key) & !is.na(working_data$age_group), , drop = FALSE]

  if (nrow(working_data) == 0) {
    age_summary <- data.frame(group_key = character(0), stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    age_counts <- stats::xtabs(~ group_key + age_group, data = working_data)
    age_summary <- data.frame(
      group_key = rownames(age_counts),
      as.data.frame.matrix(age_counts, stringsAsFactors = FALSE),
      row.names = NULL,
      check.names = FALSE
    )
  }

  age_summary <- ensure_columns(age_summary, age_group_columns, fill_value = 0)
  age_summary <- age_summary[, c("group_key", age_group_columns), drop = FALSE]
  names(age_summary)[[1]] <- ea_col
  tibble::as_tibble(age_summary)
}

#' Summarize MPHC population, household, and sex counts per EA
#'
#' @param data_frame Input MPHC data frame.
#' @param ea_col EA identifier column.
#' @param household_id_col Household identifier column.
#' @param sex_col Sex code column.
#' @param person_col Person-count column.
#' @param total_pop_col Output column name for total population.
#' @param household_count_col Output column name for household counts.
#' @param male_count_col Output column name for male counts.
#' @param female_count_col Output column name for female counts.
#'
#' @return Tibble containing one row per EA with population summary columns.
build_mphc_population_summary <- function(
  data_frame,
  ea_col = "EA_CODE",
  household_id_col = "hhnumber",
  sex_col = "p03",
  person_col = "no_persons",
  total_pop_col = "mphc_total_pop",
  household_count_col = "mphc_hh_count",
  male_count_col = "male_count",
  female_count_col = "female_count"
) {
  working_data <- data.frame(
    group_key = data_frame[[ea_col]],
    household_id = data_frame[[household_id_col]],
    person_count = data_frame[[person_col]],
    sex_value = data_frame[[sex_col]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  working_data <- working_data[!is.na(working_data$group_key), , drop = FALSE]

  if (nrow(working_data) == 0) {
    population_summary <- data.frame(
      group_key = character(0),
      total_population = numeric(0),
      household_count = numeric(0),
      male_count = numeric(0),
      female_count = numeric(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    grouped_data <- split(working_data, working_data$group_key, drop = TRUE)
    population_rows <- lapply(names(grouped_data), function(group_name) {
      group_data <- grouped_data[[group_name]]
      data.frame(
        group_key = group_name,
        total_population = sum(group_data$person_count, na.rm = TRUE),
        household_count = length(unique(group_data$household_id)),
        male_count = sum(group_data$sex_value == 1, na.rm = TRUE),
        female_count = sum(group_data$sex_value == 2, na.rm = TRUE),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    population_summary <- do.call(rbind, population_rows)
    rownames(population_summary) <- NULL
  }

  names(population_summary) <- c(ea_col, total_pop_col, household_count_col, male_count_col, female_count_col)
  tibble::as_tibble(population_summary)
}

#' Summarize household sizes at the unique-household level
#'
#' @param data_frame Input data frame containing one or more rows per household.
#' @param unique_household_id_col Unique household identifier column.
#' @param ea_col EA identifier column.
#' @param person_col Person-count column.
#' @param output_col Output column name for per-household size.
#'
#' @return Tibble containing one row per household and EA with household sizes.
build_household_size_records <- function(
  data_frame,
  unique_household_id_col,
  ea_col,
  person_col = "no_persons",
  output_col = "household_size"
) {
  working_data <- data.frame(
    unique_household_id = data_frame[[unique_household_id_col]],
    group_key = data_frame[[ea_col]],
    person_count = data_frame[[person_col]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  working_data <- working_data[!is.na(working_data$unique_household_id) & !is.na(working_data$group_key), , drop = FALSE]

  if (nrow(working_data) == 0) {
    household_size_records <- data.frame(
      unique_household_id = character(0),
      group_key = character(0),
      household_size = numeric(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    household_size_records <- stats::aggregate(
      person_count ~ unique_household_id + group_key,
      data = working_data,
      FUN = function(values) sum(values, na.rm = TRUE)
    )
    names(household_size_records)[[3]] <- "household_size"
  }

  names(household_size_records) <- c(unique_household_id_col, ea_col, output_col)
  tibble::as_tibble(household_size_records)
}

#' Merge household-size records from multiple partitions
#'
#' Binds multiple per-household summary tables together and re-aggregates them so
#' a household that appears in more than one partition is combined safely.
#'
#' @param household_size_records_list List of per-household summary tables.
#' @param unique_household_id_col Unique household identifier column.
#' @param ea_col EA identifier column.
#' @param household_size_col Household-size column in each input table.
#' @param output_col Output household-size column name.
#'
#' @return Tibble containing one row per household and EA.
merge_household_size_records <- function(
  household_size_records_list,
  unique_household_id_col = "unique_hh_id",
  ea_col = "EA_CODE",
  household_size_col = "household_size",
  output_col = household_size_col
) {
  valid_records <- Filter(function(records) !is.null(records) && nrow(records) > 0, household_size_records_list)

  if (length(valid_records) == 0) {
    empty_records <- data.frame(character(0), character(0), numeric(0), stringsAsFactors = FALSE)
    names(empty_records) <- c(unique_household_id_col, ea_col, output_col)
    return(tibble::as_tibble(empty_records))
  }

  standardized_records <- lapply(valid_records, function(records) {
    names(records)[names(records) == unique_household_id_col] <- "unique_household_id"
    names(records)[names(records) == ea_col] <- "group_key"
    names(records)[names(records) == household_size_col] <- "household_size"
    records
  })

  merged_input <- do.call(rbind, standardized_records)
  merged_records <- stats::aggregate(
    household_size ~ unique_household_id + group_key,
    data = merged_input,
    FUN = function(values) sum(values, na.rm = TRUE)
  )

  names(merged_records) <- c(unique_household_id_col, ea_col, output_col)
  tibble::as_tibble(merged_records)
}

#' Summarize household-size records to EA-level medians and means
#'
#' @param household_size_records Per-household summary table.
#' @param ea_col EA identifier column.
#' @param household_size_col Household-size column.
#' @param median_col Output median household-size column.
#' @param mean_col Output mean household-size column.
#'
#' @return Tibble containing one row per EA with median and mean household sizes.
summarise_household_size_by_ea <- function(
  household_size_records,
  ea_col = "EA_CODE",
  household_size_col = "household_size",
  median_col = "mphc_median_hh_size",
  mean_col = "mphc_mean_hh_size"
) {
  working_data <- data.frame(
    group_key = household_size_records[[ea_col]],
    household_size = household_size_records[[household_size_col]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  working_data <- working_data[!is.na(working_data$group_key), , drop = FALSE]

  if (nrow(working_data) == 0) {
    household_size_summary <- data.frame(
      group_key = character(0),
      household_size_median = numeric(0),
      household_size_mean = numeric(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    grouped_data <- split(working_data$household_size, working_data$group_key, drop = TRUE)
    household_rows <- lapply(names(grouped_data), function(group_name) {
      values <- grouped_data[[group_name]]
      data.frame(
        group_key = group_name,
        household_size_median = stats::median(values, na.rm = TRUE),
        household_size_mean = base::mean(values, na.rm = TRUE),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    household_size_summary <- do.call(rbind, household_rows)
    rownames(household_size_summary) <- NULL
  }

  names(household_size_summary) <- c(ea_col, median_col, mean_col)
  tibble::as_tibble(household_size_summary)
}

#' Split source records into missing, low-accuracy, and high-accuracy partitions
#'
#' @param data_frame Input source data frame.
#' @param longitude_col Longitude column name.
#' @param latitude_col Latitude column name.
#' @param accuracy_col Accuracy column name.
#' @param threshold_m Accuracy threshold in meters.
#'
#' @return Named list containing `no_gps`, `over_threshold`, and
#'   `under_threshold` partitions.
split_by_gps_accuracy <- function(data_frame, longitude_col, latitude_col, accuracy_col, threshold_m) {
  missing_coordinates <- is.na(data_frame[[longitude_col]]) | is.na(data_frame[[latitude_col]])
  with_coordinates <- data_frame[!missing_coordinates, , drop = FALSE]

  list(
    no_gps = data_frame[missing_coordinates, , drop = FALSE],
    over_threshold = with_coordinates[with_coordinates[[accuracy_col]] > threshold_m, , drop = FALSE],
    under_threshold = with_coordinates[with_coordinates[[accuracy_col]] < threshold_m, , drop = FALSE]
  )
}

#' Summarize household counts by a grouping column
#'
#' @param data_frame Input source data frame.
#' @param group_col Grouping column name.
#' @param output_col Output count column name.
#' @param count_col Count column to sum.
#'
#' @return Tibble containing one row per group with the requested count column.
summarise_household_counts <- function(data_frame, group_col, output_col, count_col = "hh_count") {
  working_data <- data.frame(
    group_key = data_frame[[group_col]],
    count_value = data_frame[[count_col]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  working_data <- working_data[!is.na(working_data$group_key), , drop = FALSE]

  if (nrow(working_data) == 0) {
    household_counts <- data.frame(group_key = character(0), count_value = numeric(0), stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    household_counts <- stats::aggregate(
      count_value ~ group_key,
      data = working_data,
      FUN = function(values) sum(values, na.rm = TRUE)
    )
  }

  names(household_counts) <- c(group_col, output_col)
  tibble::as_tibble(household_counts)
}

#' Combine multiple EA-level summary tables that share a grouping column
#'
#' @param summary_list List of EA-level summary tables.
#' @param group_col Shared grouping column name.
#'
#' @return Tibble containing one row per group with numeric columns summed.
combine_group_summaries <- function(summary_list, group_col) {
  valid_summaries <- Filter(function(summary_data) !is.null(summary_data) && nrow(summary_data) > 0, summary_list)

  if (length(valid_summaries) == 0) {
    empty_summary <- data.frame(character(0), stringsAsFactors = FALSE)
    names(empty_summary) <- group_col
    return(tibble::as_tibble(empty_summary))
  }

  standardized_summaries <- lapply(valid_summaries, function(summary_data) {
    names(summary_data)[names(summary_data) == group_col] <- "group_key"
    summary_data
  })

  combined_input <- do.call(rbind, standardized_summaries)
  numeric_columns <- setdiff(names(combined_input), "group_key")

  combined_summary <- stats::aggregate(
    combined_input[, numeric_columns, drop = FALSE],
    by = list(group_key = combined_input$group_key),
    FUN = function(values) sum(values, na.rm = TRUE)
  )
  combined_summary <- combined_summary[order(combined_summary$group_key), , drop = FALSE]

  names(combined_summary)[[1]] <- group_col
  tibble::as_tibble(combined_summary)
}

#' Summarize source partitions that use original and spatial EA identifiers
#'
#' Applies the shared no-GPS, over-threshold, and under-threshold count logic
#' used by ICT, IHS6, and Naca after spatial identifiers have been assigned.
#'
#' @param no_gps_data Records without coordinates.
#' @param over_threshold_data Records whose accuracy exceeds the threshold.
#' @param under_threshold_data Records within the spatial threshold.
#' @param no_gps_group_col Grouping column for no-GPS records.
#' @param over_threshold_group_col Grouping column for over-threshold records.
#' @param under_threshold_group_col Grouping column for under-threshold records.
#' @param output_col Output household-count column name.
#' @param output_group_col Final grouping column name.
#' @param count_col Count column to sum.
#'
#' @return Tibble containing the combined EA-level source summary.
summarise_source_accuracy_partitions <- function(
  no_gps_data,
  over_threshold_data,
  under_threshold_data,
  no_gps_group_col,
  over_threshold_group_col,
  under_threshold_group_col,
  output_col,
  output_group_col,
  count_col = "hh_count"
) {
  partition_summaries <- list(
    summarise_household_counts(no_gps_data, no_gps_group_col, output_col, count_col = count_col),
    summarise_household_counts(over_threshold_data, over_threshold_group_col, output_col, count_col = count_col),
    summarise_household_counts(under_threshold_data, under_threshold_group_col, output_col, count_col = count_col)
  )

  partition_summaries <- lapply(partition_summaries, function(summary_data) {
    names(summary_data)[[1]] <- output_group_col
    summary_data
  })

  combine_group_summaries(partition_summaries, output_group_col)
}

#' Build the stable final output column order
#'
#' @return Character vector describing the direct output schema order.
get_final_output_column_order <- function() {
  c(
    "EA_CODE",
    "mphc_total_pop",
    "mphc_median_hh_size",
    "mphc_mean_hh_size",
    "dhs_median_hh_size",
    "dhs_mean_hh_size",
    "observed_hh_count",
    "dhs_hh_count",
    "mphc_hh_count",
    "ict_hh_count",
    "ihs_hh_count",
    "naca_hh_count",
    "zomba_hh_count",
    "malemia_hh_count",
    "female_count",
    "male_count",
    get_mphc_age_group_columns()
  )
}

#' Calculate observed household counts using the pipeline priority order
#'
#' Applies the current source precedence: malemia, DHS listing, IHS6, Naca,
#' ICT, then Zomba.
#'
#' @param data_frame EA-level combined output table.
#'
#' @return The data frame with an `observed_hh_count` column attached.
calculate_observed_household_count <- function(data_frame) {
  required_columns <- c(
    "malemia_hh_count",
    "dhs_hh_count",
    "ihs_hh_count",
    "naca_hh_count",
    "ict_hh_count",
    "zomba_hh_count"
  )

  result <- ensure_columns(data_frame, required_columns, fill_value = NA_real_)

  result$observed_hh_count <- dplyr::coalesce(
    result$malemia_hh_count,
    result$dhs_hh_count,
    result$ihs_hh_count,
    result$naca_hh_count,
    result$ict_hh_count,
    result$zomba_hh_count
  )

  result
}

#' Reorder the combined output into the direct-output schema
#'
#' Adds any missing expected columns and selects the stable final column order
#' used by downstream compatibility checks.
#'
#' @param data_frame Combined EA-level output table.
#'
#' @return Data frame restricted to the stable direct-output schema order.
shape_final_output_columns <- function(data_frame) {
  final_output_columns <- get_final_output_column_order()
  shaped_data <- ensure_columns(data_frame, final_output_columns, fill_value = NA_real_)
  shaped_data[, final_output_columns, drop = FALSE]
}

#' Combine EA-level source outputs into the final direct output table
#'
#' Joins the source summaries, applies optional-source placeholders when a
#' source table is absent, computes observed household counts, and returns the
#' stable output schema.
#'
#' @param mphc_data MPHC summary table keyed by EA_CODE.
#' @param ict_data ICT summary table keyed by EA_Number.
#' @param ihs_data IHS6 summary table keyed by EA_CODE.
#' @param naca_data Naca summary table keyed by EA_Number.
#' @param dhs_hh_size DHS household-size summary keyed by EA_CODE.
#' @param dhs_hh_count DHS household-count summary keyed by EA_CODE.
#' @param zomba_data Optional Zomba summary keyed by EA_CODE.
#' @param malemia_data Optional Malemia summary keyed by EA_CODE.
#'
#' @return Combined data frame in direct-output column order.
combine_pipeline_outputs <- function(
  mphc_data,
  ict_data,
  ihs_data,
  naca_data,
  dhs_hh_size,
  dhs_hh_count,
  zomba_data = NULL,
  malemia_data = NULL
) {
  combined_data <- dplyr::left_join(mphc_data, ict_data, by = c("EA_CODE" = "EA_Number"))
  combined_data <- dplyr::left_join(combined_data, ihs_data, by = "EA_CODE")
  combined_data <- dplyr::left_join(combined_data, naca_data, by = c("EA_CODE" = "EA_Number"))
  combined_data <- dplyr::left_join(combined_data, dhs_hh_size, by = "EA_CODE")
  combined_data <- dplyr::left_join(combined_data, dhs_hh_count, by = "EA_CODE")

  if (!is.null(zomba_data)) {
    combined_data <- dplyr::left_join(combined_data, zomba_data, by = "EA_CODE")
  } else {
    combined_data <- ensure_columns(combined_data, "zomba_hh_count", fill_value = NA_real_)
  }

  if (!is.null(malemia_data)) {
    combined_data <- dplyr::left_join(combined_data, malemia_data, by = "EA_CODE")
  } else {
    combined_data <- ensure_columns(combined_data, "malemia_hh_count", fill_value = NA_real_)
  }

  combined_data <- calculate_observed_household_count(combined_data)
  shape_final_output_columns(combined_data)
}