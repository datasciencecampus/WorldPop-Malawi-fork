#' Render a Quarto report as HTML
#'
#' Logs rendering progress and stops with an error if Quarto returns a nonzero
#' exit status. The pipeline's error handler logs the failure when called there.
#'
#' @param report_name Character string giving the path to the Quarto report,
#'   relative to the working directory or as an absolute path.
#' @details Requires the Quarto CLI on PATH. If Quarto cannot find Rscript,
#'   set QUARTO_R before calling this function. This environment variable tells
#'   Quarto which directory contains the R executables; it is a folder path,
#'   not the path to Rscript.exe itself. Quarto inherits it from the R session.
#'
#'   Use Sys.setenv(QUARTO_R = file.path(R.home(), "bin")) to select the bin
#'   directory of the running R installation. On Windows, R.home("bin") can
#'   resolve to bin/x64 instead, which may not contain Rscript.exe. This setting
#'   lasts for the current R session and does not change the system PATH.
#' @return Invisibly returns the result of the success logging call.
#' @examples
#' \dontrun{
#' Sys.setenv(QUARTO_R = file.path(R.home(), "bin"))
#' render_quarto_report("growth_factor_results_report.qmd")
#' }
render_quarto_report <- function(report_name) {
  logger::log_info("Rendering '{report_name}'...")
  if (system2("quarto", c("render", shQuote(report_name), "--to", "html")) != 0L) {
    stop("Failed to render report: ", report_name)
  }
  logger::log_success("Report rendered from '{report_name}'")
}

#' Copy an HTML report with a timestamped filename
#'
#' @param report_path Path to the rendered, self-contained HTML report.
#' @param run_id Pipeline timestamp, shared with the log filename.
#' @param report_name Name used before the timestamp in the HTML filename.
#' @param reports_dir Parent directory for archived reports.
#' @details Render with Quarto's embed-resources: true option so images and
#'   styles are embedded in the HTML. Copies are saved directly in reports_dir
#'   as report_name_run_id.html. Existing files are not overwritten, and the
#'   source report is unchanged.
#' @return Invisibly returns the path to the archived HTML report.
copy_report <- function(report_path, run_id,
                        report_name = tools::file_path_sans_ext(basename(report_path)),
                        reports_dir = "reports") {
  if (!file.exists(report_path)) {
    stop("Report not found: ", report_path)
  }

  archived_report <- file.path(reports_dir, paste0(report_name, "_", run_id, ".html"))
  if (file.exists(archived_report)) {
    stop("Report archive already exists: ", archived_report)
  }
  if (!dir.exists(reports_dir) &&
      !dir.create(reports_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create reports directory: ", reports_dir)
  }

  if (!file.copy(report_path, archived_report, overwrite = FALSE)) {
    stop("Failed to copy report to: ", archived_report)
  }

  logger::log_success("Report archived: {archived_report}")
  invisible(archived_report)
}
