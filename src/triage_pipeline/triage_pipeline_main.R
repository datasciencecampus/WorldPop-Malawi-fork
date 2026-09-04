# Triage pipeline main script
# ---
source("utils.R")
source("src/triage_pipeline/triangulation.R")

# ----
# Load in vars from config
config <- load_config()

# Triangulation
triangulation_data <- load_triangulation_data(
  config$triage_pipeline$ratio_change_clean_csv,
  config$triage_pipeline$hh_estimate_clean_csv
)

triangulation <- calculate_and_write_triangulation(
  triangulation_data,
  config$triage_pipeline$triangulation_file
)

triangulation_summary <- triangulation |>
  dplyr::summarise(
    total_eas = dplyr::n(),
    ratio_change_eas = dplyr::n_distinct(triangulation_data$ratio_change_data$EA_CODE),
    worldpop_eas = dplyr::n_distinct(triangulation_data$growth_factor_data$EA_CODE),
    pct_within_ci = mean(within_ci, na.rm = TRUE) * 100,
    mean_pct_diff = mean(pct_diff, na.rm = TRUE),
    median_pct_diff = median(pct_diff, na.rm = TRUE)
  )

write.csv(
  triangulation_summary,
  config$triage_pipeline$triangulation_summary_file,
  row.names = FALSE
)

calculate_and_write_distribution_summary(
  triangulation,
  config$triage_pipeline$distribution_summary_file
)

write_distribution_plot(triangulation,
                        config$triage_pipeline$distribution_plot_file)

# HTML triage report
#---
# Setting the project root to the main repo dir 
# (where "WP-Malawi-fork.Rproj" is located)
project_root <- normalizePath(getwd(), mustWork = TRUE)
while (!file.exists(file.path(project_root, "WP-Malawi-fork.Rproj"))) {
  parent_dir <- dirname(project_root)
  if (parent_dir == project_root) {
    stop("Could not find the project root.")
  }
  project_root <- parent_dir
}
setwd(project_root)
setup_pandoc()

# Resolve paths for those used for html report
triage_output_dir <- resolve_config_path(config$triage_pipeline$output_dir, project_root)
triangulation_file <- resolve_config_path(config$triage_pipeline$triangulation_file, project_root)
triangulation_summary_file <- resolve_config_path(config$triage_pipeline$triangulation_summary_file, project_root)
distribution_summary_file <- resolve_config_path(config$triage_pipeline$distribution_summary_file, project_root)
distribution_plot_file <- resolve_config_path(config$triage_pipeline$distribution_plot_file, project_root)
report_file <- resolve_config_path(config$triage_pipeline$report_file, project_root)

rmarkdown::render(
  normalizePath(config$triage_pipeline$report_template, mustWork = TRUE),
  output_file = report_file,
  params = list(
    triangulation_csv = triangulation_file,
    triangulation_summary_csv = triangulation_summary_file,
    distribution_summary_csv = distribution_summary_file,
    distribution_plot_file = basename(
      config$triage_pipeline$distribution_plot_file
    )
  ),
  knit_root_dir = triage_output_dir
)
