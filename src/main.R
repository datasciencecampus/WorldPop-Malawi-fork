# ---
# file will pull in all parts of the modelling pipeline
source("src/data_processing/data_processing_2.R")
source("src/quality_assurance/output_summary.R")
source("utils.R")

# ----
# Load in vars from config needed in main
config <- load_config()

# ---- Set up per-run log file --------------------------------------------------
run_id       <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_dir      <- file.path(config$paths$drive_path, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_dir      <- normalizePath(log_dir, mustWork = FALSE)
run_log_file <- file.path(log_dir, paste0("pipeline_run_", run_id, ".log"))

library(logger)
logger::log_appender(logger::appender_tee(run_log_file))
logger::log_threshold(logger::INFO)

# ----
# section 1
# previous data processing steps go here, raster mosaicking etc.

# call in data processing 2
data_processing_2_function()

# QA for data processing2
qa_output_dir <- normalizePath(
    file.path(config$paths$drive_path, "quality_assurance"),
    mustWork = FALSE
)
dir.create(qa_output_dir, recursive = TRUE, showWarnings = FALSE)
qa_output_dir <- normalizePath(qa_output_dir, mustWork = FALSE)

# Run QA for every preprocessing QA block in config and create one combined summary.
qa_block_names <- names(config)[grepl("_qa$", names(config))]
all_preprocessing_qa_summary <- data.frame()

for (qa_block_name in qa_block_names) {
    qa_cfg <- config[[qa_block_name]]

    required_fields <- c(
        "report_prefix",
        "baseline_summarized_csv",
        "baseline_hh_size_gpkg"
    )
    missing_fields <- setdiff(required_fields, names(qa_cfg))

    if (length(missing_fields) > 0) {
        stop(
            paste0(
                "Missing required fields in config block '",
                qa_block_name,
                "': ",
                paste(missing_fields, collapse = ", ")
            )
        )
    }

    current_summarized_csv_filepath <- if (!is.null(qa_cfg$current_summarized_csv_output_key)) {
        output_key <- qa_cfg$current_summarized_csv_output_key
        if (is.null(config$outputs[[output_key]])) {
            stop(paste0("Output key not found in config$outputs: ", output_key))
        }
        config$outputs[[output_key]]
    } else {
        qa_cfg$current_summarized_csv
    }

    current_hh_size_gpkg_filepath <- if (!is.null(qa_cfg$current_hh_size_gpkg_output_key)) {
        output_key <- qa_cfg$current_hh_size_gpkg_output_key
        if (is.null(config$outputs[[output_key]])) {
            stop(paste0("Output key not found in config$outputs: ", output_key))
        }
        config$outputs[[output_key]]
    } else {
        qa_cfg$current_hh_size_gpkg
    }

    if (is.null(current_summarized_csv_filepath) || is.null(current_hh_size_gpkg_filepath)) {
        stop(
            paste0(
                "QA block '",
                qa_block_name,
                "' must define either current path fields or *_output_key fields."
            )
        )
    }

    qa_result <- run_parity_qa(
        output_csv_path = current_summarized_csv_filepath,
        output_gpkg_path = current_hh_size_gpkg_filepath,
        baseline_csv_path = qa_cfg$baseline_summarized_csv,
        baseline_gpkg_path = qa_cfg$baseline_hh_size_gpkg,
        qa_output_dir = qa_output_dir,
        report_prefix = qa_cfg$report_prefix
    )

    qa_summary_row <- as.data.frame(qa_result$summary)
    qa_summary_row$qa_block <- qa_block_name
    qa_summary_row$report_prefix <- qa_cfg$report_prefix

    all_preprocessing_qa_summary <- rbind(all_preprocessing_qa_summary, qa_summary_row)
}

write.csv(
    all_preprocessing_qa_summary,
    file.path(qa_output_dir, "all_preprocessing_parity_summary.csv"),
    row.names = FALSE
)

print(all_preprocessing_qa_summary)

# ---- Write structured run log ------------------------------------------------
output_files_to_log <- unlist(lapply(
    qa_block_names,
    function(qa_block_name) {
        qa_cfg    <- config[[qa_block_name]]
        out_key_csv  <- qa_cfg$current_summarized_csv_output_key
        out_key_gpkg <- qa_cfg$current_hh_size_gpkg_output_key
        c(
            if (!is.null(out_key_csv))  config$outputs[[out_key_csv]]  else qa_cfg$current_summarized_csv,
            if (!is.null(out_key_gpkg)) config$outputs[[out_key_gpkg]] else qa_cfg$current_hh_size_gpkg
        )
    }
))

write_run_log(
    run_id       = run_id,
    log_file_path = run_log_file,
    config       = config,
    output_files = output_files_to_log,
    qa_summary   = all_preprocessing_qa_summary,
    log_dir      = log_dir
)

logger::log_info("Run log written: {run_log_file}")

# ---- Flush logger buffer before rendering report ----
# The logger package buffers output, so we need to close/reset the appender
# to ensure all messages are written to the log file before the report reads it
logger::log_appender(logger::appender_file(run_log_file))

# ---- Render HTML pipeline report -----------------------------------------------
tryCatch({
    # Configure Pandoc (checks system PATH, then Quarto install, then common locations)
    setup_pandoc()
    
    # Locate the QA CSV files that were just written
    qa_summary_csv_path <- file.path(qa_output_dir, "all_preprocessing_parity_summary.csv")
    
    # Build column and duplicate report paths based on report_prefix from config
    report_prefix <- config$data_processing2_qa$report_prefix  # use first QA block for now
    qa_col_report_path <- file.path(qa_output_dir, paste0(report_prefix, "_parity_column_report.csv"))
    qa_dup_report_path <- file.path(qa_output_dir, paste0(report_prefix, "_duplicate_ea_code_report.csv"))
    
    report_output_path <- normalizePath(
        file.path(qa_output_dir, "pipeline_report.html"),
        mustWork = FALSE
    )
    
    logger::log_info("Rendering HTML pipeline report...")
    rmarkdown::render(
        "src/quality_assurance/pipeline_report.Rmd",
        output_file = report_output_path,
        params = list(
            run_timestamp        = run_id,
            timepoint            = config$run$timepoint,
            log_file             = run_log_file,
            qa_summary_csv       = qa_summary_csv_path,
            qa_column_report_csv = qa_col_report_path,
            qa_duplicate_csv     = qa_dup_report_path,
            config_path               = normalizePath(file.path("src", "config.yaml"), mustWork = FALSE),
            transformation_stats_csv  = normalizePath(
                file.path(qa_output_dir, "data_processing2_transformation_stats.csv"),
                mustWork = FALSE
            )
        ),
        quiet = TRUE
    )
    logger::log_info("HTML report written: {report_output_path}")
    browseURL(report_output_path)
}, error = function(e) {
    logger::log_warn("Could not render HTML report: {e$message}")
})

# user inputs prompt questions
## does checks look good?
##
# ----

    ## Further pipeline