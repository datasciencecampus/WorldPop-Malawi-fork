#' Load and validate datasets for triangulation analysis
#'
#' Loads ratio change and growth factor datasets, validates required columns,
#' and checks for duplicate keys in the join column. Ensures data integrity
#' before triangulation calculations.
#'
#' @param ratio_change_csv (character) Path to CSV file containing ratio change data.
#' @param growth_factor_csv (character) Path to CSV file containing growth factor data
#'   and confidence interval bounds.
#' @param join_key (character) Column name to join on. Defaults to "EA_CODE".
#' @param rc_col (character) Column name in ratio_change_csv containing ratio values.
#'   Defaults to "census_ratio_tmpl".
#' @param gf_cols (character vector) Column names in growth_factor_csv for predicted
#'   count, lower CI, and upper CI. Defaults to c("predicted_hh_count_2024",
#'   "hh_lower_2024", "hh_upper_2024").
#'
#' @return (list) List containing:
#'   - ratio_change_data: data frame from ratio_change_csv
#'   - growth_factor_data: data frame from growth_factor_csv
#'   - join_key: the join column name
#'   - rc_col: the ratio change column name
#'   - gf_cols: the growth factor column names
#'
#' @details
#' Stops with an error if:
#'   - Required columns are missing from either dataset
#'   - Duplicate values exist in the join_key column
load_triangulation_data <- function(
  ratio_change_csv,
  growth_factor_csv,
  join_key = "EA_CODE",
  rc_col = "census_ratio_tmpl",
  gf_cols = c(
    "predicted_hh_count_2024",
    "hh_lower_2024",
    "hh_upper_2024"
  )
) {
  ratio_change_data <- read.csv(ratio_change_csv, stringsAsFactors = FALSE)
  growth_factor_data <- read.csv(growth_factor_csv, stringsAsFactors = FALSE)

  stopifnot(join_key %in% names(ratio_change_data))
  stopifnot(rc_col %in% names(ratio_change_data))
  stopifnot(all(c(join_key, gf_cols) %in% names(growth_factor_data)))

  rc_dups <- duplicated(ratio_change_data[[join_key]])
  gf_dups <- duplicated(growth_factor_data[[join_key]])

  if (any(rc_dups)) {
    stop("ratio_change_data has duplicate EA_CODEs. Data must be cleaned before triangulation.")
  }

  if (any(gf_dups)) {
    stop("growth_factor_data has duplicate EA_CODEs. Data must be cleaned before triangulation.")
  }

  list(
    ratio_change_data = ratio_change_data,
    growth_factor_data = growth_factor_data,
    join_key = join_key,
    rc_col = rc_col,
    gf_cols = gf_cols
  )
}

#' Calculate triangulation agreement metrics and write results
#'
#' Performs left join between ratio change and growth factor data, calculates
#' absolute and percentage differences, determines agreement direction, and checks
#' whether ratio change values fall within confidence intervals. Writes results to CSV.
#'
#'  TODO: Use an existing file with all EA codes and get the right columns from each dataset
#'  with the total number of 2018 EAs
#'
#' @param data (list) Output from load_triangulation_data() containing ratio_change_data,
#'   growth_factor_data, and column specifications.
#' @param output_csv (character) Path where triangulation results should be written.
#'
#' @return (data.frame) Triangulation data frame with calculated metrics:
#'   - abs_diff: absolute difference between ratio change and predicted count
#'   - pct_diff: percentage difference
#'   - direction: whether ratio_change is higher, gf is higher, or they agree
#'   - within_ci: logical indicating if ratio change falls within confidence interval
calculate_and_write_triangulation <- function(data, output_csv) {
  triangulation <- dplyr::left_join(
    data$ratio_change_data,
    data$growth_factor_data,
    by = data$join_key
  )

  triangulation$abs_diff <- triangulation[[data$rc_col]] -
    triangulation[[data$gf_cols[1]]]
  triangulation$pct_diff <- triangulation$abs_diff /
    triangulation[[data$gf_cols[1]]] * 100
  triangulation$direction <- ifelse(
    triangulation$abs_diff > 0,
    "ratio_change_higher",
    ifelse(triangulation$abs_diff < 0, "gf_higher", "agree")
  )
  triangulation$within_ci <- triangulation[[data$rc_col]] >=
    triangulation[[data$gf_cols[2]]] &
    triangulation[[data$rc_col]] <= triangulation[[data$gf_cols[3]]]

  write.csv(triangulation, output_csv, row.names = FALSE)
  triangulation
}

#' Calculate summary statistics for triangulation percentage differences
#'
#' Computes distribution summary statistics (min, quartiles, median, mean, max,
#' and standard deviation) for percentage differences and writes to CSV.
#'
#' @param triangulation (data.frame) Triangulation results from calculate_and_write_triangulation()
#'   containing pct_diff column.
#' @param output_csv (character) Path where summary statistics should be written.
#'
#' @return (data.frame) Summary statistics table with columns:
#'   - statistic: descriptive name (e.g., "Minimum", "Median")
#'   - pct_diff: corresponding summary statistic value
calculate_and_write_distribution_summary <- function(
  triangulation,
  output_csv
) {
  distribution_summary <- data.frame(
    statistic = c(
      "Minimum (largest negative percentage difference)",
      "Lower quartile",
      "Median",
      "Mean",
      "Upper quartile",
      "Maximum (largest positive percentage difference)",
      "Standard deviation"
    ),
    pct_diff = c(
      min(triangulation$pct_diff, na.rm = TRUE),
      unname(stats::quantile(triangulation$pct_diff, 0.25, na.rm = TRUE)),
      median(triangulation$pct_diff, na.rm = TRUE),
      mean(triangulation$pct_diff, na.rm = TRUE),
      unname(stats::quantile(triangulation$pct_diff, 0.75, na.rm = TRUE)),
      max(triangulation$pct_diff, na.rm = TRUE),
      stats::sd(triangulation$pct_diff, na.rm = TRUE)
    )
  )

  write.csv(distribution_summary, output_csv, row.names = FALSE)
  distribution_summary
}

#' Create and save histogram of triangulation percentage differences
#'
#' Generates a histogram visualization of percentage differences between ratio change
#' and growth factor predictions, with reference line at zero. Filters out non-finite
#' values and saves plot to file.
#'
#' @param triangulation (data.frame) Triangulation results from calculate_and_write_triangulation()
#'   containing pct_diff column.
#' @param output_file (character) Path where histogram plot should be saved (typically .png).
#'
#' @return (invisible) Invisibly returns the result of ggplot2::ggsave().
#'
#' @details
#' Plot dimensions: 8 inches wide by 5 inches tall. Uses 30 bins with blue fill
#' and red dashed line at zero for reference.
write_distribution_plot <- function(triangulation, output_file) {
  plot_data <- triangulation[
    is.finite(triangulation$pct_diff),
    ,
    drop = FALSE
  ]

  distribution_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = !!rlang::sym("pct_diff"))
  ) +
    ggplot2::geom_histogram(bins = 30, fill = "#2980b9", colour = "white") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#c0392b") +
    ggplot2::labs(
      x = "Percentage difference",
      y = "Number of EAs",
      title = "Distribution of percentage differences"
    ) +
    ggplot2::theme_minimal()

  ggplot2::ggsave(
    output_file,
    distribution_plot,
    width = 8,
    height = 5,
    units = "in"
  )
}
