# Wrapper script to act as a single entry point into the pipeline
# This will contain all the steps to produce a model.

library(tictoc)
library(logger)

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

if (run_data_processing) {
  source("01_Data_Processing.R")
  source("02_Covariates_Extraction.R")
  source("03_Rasterization.R")
  source("04_Covs_Stack_Prediction_Grid_2018.R")
  source("04_Covs_Stack_Prediction_Grid_2024.R")
  source("04_Covs_Stack_Prediction_Grid_2026.R")
} else {
  log_info("Data Processing stages have been skipped.")
}

if (run_growth_model) {
  log_info("Running '08_Growth_Factor_Model_Full_Posterior.R'...")
  source("08_Growth_Factor_Model_Full_Posterior.R")

  log_info("Running '08_Growth_Factor_Model_Latent_Posterior.R'...")
  source("08_Growth_Factor_Model_Latent_Posterior.R")
} else {
  log_info("Growth Model has been skipped.")
}
