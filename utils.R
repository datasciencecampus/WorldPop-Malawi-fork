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
