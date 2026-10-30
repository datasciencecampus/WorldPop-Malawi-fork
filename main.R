# Wrapper script to act as a single entry point into the pipeline
# This will contain all the steps to produce a model.

library(logger)

# helper functions
source("utils.R")

# ---- initialse logger ----
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_dir <- "logs" # file.path(config$paths$drive_path, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_dir <- normalizePath(log_dir, mustWork = FALSE)
run_log_file <- file.path(log_dir, paste0("pipeline_run_", run_id, ".log"))

logger::log_appender(logger::appender_tee(run_log_file))
logger::log_threshold(logger::INFO)

# ---- model options ----
# These option will go in the config later
#
# Run the data processing part of the pipeline
run_data_processing <- FALSE

# Run only the Growth model
run_growth_model <- TRUE

# ---- pipeline ----
#
# NOTE: I have separated these because we know the data processing is
# deterministic, I can feasibly see us having to run the growth models multiple
# times to confirm if they are deterministic or not.

# withCallingHandlers is used to dump any errors that fail the pipeline into
# the pipeline logs
withCallingHandlers(
  {
    if (run_data_processing) {
      log_success("Running '01_Data_Processing...'")
      source("01_Data_Processing.R")

      log_success("Running '02_Covariates_Extraction.R'...")
      source("02_Covariates_Extraction.R")

      log_success("Running '03_Rasterization.R'...")
      source("03_Rasterization.R")

      log_success("Running '04_Covs_Stack_Prediction_Grid_2018.R'...")
      source("04_Covs_Stack_Prediction_Grid_2018.R")

      log_success("Running '04_Covs_Stack_Prediction_Grid_2024.R' ...")
      source("04_Covs_Stack_Prediction_Grid_2024.R")

      log_success("Running '04_Covs_Stack_Prediction_Grid_2026.R' ...")
      source("04_Covs_Stack_Prediction_Grid_2026.R")
    } else {
      log_warn("Data Processing stages have been skipped.")
    }

    if (run_growth_model) {
      log_success("Running '08_Growth_Factor_Model_Full_Posterior.R'...")
      # source("08_Growth_Factor_Model_Full_Posterior.R")

      log_success("Running '08_Growth_Factor_Model_Latent_Posterior.R'...")
      # source("08_Growth_Factor_Model_Latent_Posterior.R")

      render_quarto_report("growth_factor_results_report.qmd")
    } else {
      log_warn("Growth Model has been skipped.")
    }
  },
  error = function(error) {
    logger::log_error("Pipeline failed: {conditionMessage(error)}")
    if (!is.null(conditionCall(error))) {
      logger::log_error("Failing call: {paste(deparse(conditionCall(error)), collapse = ' ')}")
    }
  }
)

# TODO: integrate config within the individual scripts, front load all data
# reading

# TODO: The logging messages will have to be checked again for relevance, a
# problem I can see happening when we integrate the config into the individual
# files (logging messages currently reference file names directly).

# TODO: Still need to use the config in all the scripts. Need to implement
# a date system for storing processed data/outputs.
#  - add timer functions?
#  - add git commit hash/branch to logs?

# TODO: When running data processing steps + modeling we get an error
# saying 'run_growth_model' doesn't exist, this is defined at the top of the
# pipeline. I suspect in 01_data_processing.R where *ALL* variables apart
# from a named few are deleted includes global variables.
