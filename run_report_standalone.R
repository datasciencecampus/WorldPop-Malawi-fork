# ----
# Standalone report renderer for testing report modifications
# without running the full data_processing2 pipeline
# ----

source("load_required_libraries.R")
source("utils.R")

# Load config
config <- load_config()

# ---- Find most recent log file ----
log_dir <- file.path(config$paths$drive_path, "logs")
log_files <- list.files(log_dir, pattern = "^pipeline_run_.*\\.log$", full.names = TRUE)

if (length(log_files) == 0) {
    cat("WARNING: No previous pipeline log files found in", log_dir, "\n")
    cat("Creating placeholder log for report testing...\n")
    
    # Create placeholder log file
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
    run_log_file <- file.path(log_dir, paste0("pipeline_run_", run_id, ".log"))
    
    # Write placeholder content
    writeLines(c(
        "[INFO] Pipeline run started",
        "[INFO] System and Environment Information:",
        "[INFO]   R version: 4.5.1",
        "[INFO]   Operating System: Windows",
        "[INFO]   Git user: Unknown",
        "[INFO] Data processing started",
        "[INFO] dp9.5 - calculating survey-only EA coverage",
        "[INFO] dp9.5 - Survey-only EA coverage: 0 out of 0 EAs (0%)",
        "[INFO] Data processing completed"
    ), run_log_file)
    
    cat("Placeholder log created:", run_log_file, "\n")
} else {
    # Use most recent log file (sorted by modification time)
    run_log_file <- log_files[which.max(file.info(log_files)$mtime)]
    run_id <- gsub("pipeline_run_|.log", "", basename(run_log_file))
    cat("Using most recent log file:", run_log_file, "\n")
}

# ---- Get system info ----
sys_info <- get_system_info()

# ---- Locate QA outputs ----
qa_output_dir <- normalizePath(
    file.path(config$paths$drive_path, "quality_assurance"),
    mustWork = FALSE
)

qa_summary_csv_path <- file.path(qa_output_dir, "all_preprocessing_parity_summary.csv")
report_prefix <- config$data_processing2_qa$report_prefix
qa_col_report_path <- file.path(qa_output_dir, paste0(report_prefix, "_parity_column_report.csv"))
qa_dup_report_path <- file.path(qa_output_dir, paste0(report_prefix, "_duplicate_ea_code_report.csv"))
transformation_stats_csv <- file.path(qa_output_dir, "data_processing2_transformation_stats.csv")

# Check which files exist and provide feedback
cat("\nQA output files status:\n")
cat("  Summary CSV:", ifelse(file.exists(qa_summary_csv_path), "✓", "✗"), qa_summary_csv_path, "\n")
cat("  Column report:", ifelse(file.exists(qa_col_report_path), "✓", "✗"), qa_col_report_path, "\n")
cat("  Duplicate report:", ifelse(file.exists(qa_dup_report_path), "✓", "✗"), qa_dup_report_path, "\n")
cat("  Transform stats:", ifelse(file.exists(transformation_stats_csv), "✓", "✗"), transformation_stats_csv, "\n")

# ---- Render report ----
tryCatch({
    # Configure Pandoc
    setup_pandoc()
    
    report_output_path <- normalizePath(
        file.path(qa_output_dir, "pipeline_report.html"),
        mustWork = FALSE
    )
    
    cat("\nRendering HTML pipeline report...\n")
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
            config_path          = normalizePath(file.path("src", "config.yaml"), mustWork = FALSE),
            transformation_stats_csv = transformation_stats_csv,
            sys_info             = sys_info
        ),
        quiet = FALSE
    )
    cat("✓ HTML report written:", report_output_path, "\n")
    browseURL(report_output_path)
}, error = function(e) {
    cat("✗ Error rendering report:", e$message, "\n")
})
