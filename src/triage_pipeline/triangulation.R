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
  ),
  urban_rural_col = "urban_rural"
) {
  ratio_change_data <- read.csv(ratio_change_csv, stringsAsFactors = FALSE)
  growth_factor_data <- read.csv(growth_factor_csv, stringsAsFactors = FALSE)

  stopifnot(join_key %in% names(ratio_change_data))
  stopifnot(rc_col %in% names(ratio_change_data))
  stopifnot(all(c(join_key, gf_cols) %in% names(growth_factor_data)))

  # Check if urban_rural column exists in ratio_change_data
  if (!is.null(urban_rural_col) && !(urban_rural_col %in% names(ratio_change_data))) {
    warning(
      "Column '", urban_rural_col, "' not found in ratio_change_data. ",
      "Urban/rural breakdown will not be available."
    )
  }

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
#'   - score: alignment score from 0-100 based on pct_diff and within_ci
#'
#' @param pct_weight (numeric) Points deducted per 1% difference (from config).
#' @param ci_bonus (numeric) Bonus points if within confidence interval (from config).
#' @param threshold_very_small (numeric) Household threshold for very_small category (from config).
#' @param threshold_near_300_lower (numeric) Lower household threshold for near_300_threshold (from config).
#' @param threshold_near_300_upper (numeric) Upper household threshold for near_300_threshold (from config).
calculate_and_write_triangulation <- function(data, output_csv, pct_weight, ci_bonus, threshold_very_small = 50, threshold_near_300_lower = 250, threshold_near_300_upper = 350) {
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

  # Calculate alignment score
  triangulation$score <- calculate_alignment_score(
    triangulation$pct_diff,
    triangulation$within_ci,
    pct_weight,
    ci_bonus
  )
  
  # Classify into operational categories
  triangulation <- classify_operational_category(
    triangulation,
    hh_col = data$gf_cols[1],
    threshold_very_small = threshold_very_small,
    threshold_near_300_lower = threshold_near_300_lower,
    threshold_near_300_upper = threshold_near_300_upper
  )

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

#' Calculate EA alignment score based on triangulation agreement
#'
#' Scores each EA from 0-100 based on how well ratio_change and growth factor
#' estimates agree. Higher scores indicate better agreement.
#'
#' @param pct_diff (numeric vector) Percentage differences from triangulation.
#' @param within_ci (logical vector) Whether ratio_change is within growth factor's CI.
#' @param pct_weight (numeric) Points deducted per 1% difference. Default 2.
#' @param ci_bonus (numeric) Bonus points if within_ci. Default 15.
#'
#' @return (numeric vector) Alignment scores clamped to 0-100.
#'
#' @details
#' Formula: (100 - (abs(pct_diff) * pct_weight)) + (within_ci ? ci_bonus : 0)
#' Clamped to [0, 100].
#'
#' @examples
#' calculate_alignment_score(c(2.5, -15.0, 0.5), c(TRUE, FALSE, TRUE), 2, 15)
#'
#' @export
calculate_alignment_score <- function(
  pct_diff,
  within_ci,
  pct_weight,
  ci_bonus
) {
  score <- (100 - (abs(pct_diff) * pct_weight)) + (within_ci * ci_bonus)
  pmin(100, pmax(0, score))  # Clamp to [0, 100]
}

#' Calculate summary statistics for alignment scores
#'
#' Counts EAs in each confidence band (High ≥80, Medium 50-79, Low <50)
#' and writes summary to CSV.
#'
#' @param triangulation (data.frame) Triangulation results from calculate_and_write_triangulation()
#'   containing score column.
#' @param output_csv (character) Path where score summary should be written.
#'
#' @return (data.frame) Summary table with columns:
#'   - confidence_level: "High confidence", "Medium confidence", or "Low confidence"
#'   - count: Number of EAs in that band
#'   - percentage: Percentage of total EAs
#'
#' @export
calculate_and_write_score_summary <- function(
  triangulation,
  output_csv
) {
  total_eas <- nrow(triangulation)
  
  high_conf <- sum(triangulation$score >= 80, na.rm = TRUE)
  med_conf <- sum(triangulation$score >= 50 & triangulation$score < 80, na.rm = TRUE)
  low_conf <- sum(triangulation$score < 50, na.rm = TRUE)
  
  score_summary <- data.frame(
    confidence_level = c(
      "High confidence (score ≥80)",
      "Medium confidence (score 50-79)",
      "Low confidence (score <50)"
    ),
    count = c(high_conf, med_conf, low_conf),
    percentage = c(
      high_conf / total_eas * 100,
      med_conf / total_eas * 100,
      low_conf / total_eas * 100
    )
  )
  
  write.csv(score_summary, output_csv, row.names = FALSE)
  score_summary
}

#' Classify EAs into operational categories based on household count
#'
#' Assigns each EA to a category based on WorldPop household estimates using
#' thresholds from config. Categories reflect operational needs for EA splitting:
#' - <50: < threshold_very_small households (no action needed)
#' - 50-249: threshold_very_small to threshold_near_300_lower households
#' - 250-350: threshold_near_300_lower to threshold_near_300_upper households
#'   (target range for NSO; may need splitting if >upper bound)
#' - >350: > threshold_near_300_upper households (will definitely need splitting)
#'
#' @param triangulation (data.frame) Triangulation results containing household counts.
#' @param hh_col (character) Name of the column containing household counts.
#'   Default: "predicted_hh_count_2024".
#' @param threshold_very_small (numeric) Upper bound for very_small category (default 50).
#' @param threshold_near_300_lower (numeric) Lower bound for near_300_threshold category (default 250).
#' @param threshold_near_300_upper (numeric) Upper bound for near_300_threshold category (default 350).
#'
#' @return (data.frame) Input data frame with added column `operational_category`.
#'
#' @export
classify_operational_category <- function(
  triangulation,
  hh_col = "predicted_hh_count_2024",
  threshold_very_small = 50,
  threshold_near_300_lower = 250,
  threshold_near_300_upper = 350
) {
  if (!(hh_col %in% names(triangulation))) {
    stop("Column '", hh_col, "' not found in triangulation data.")
  }
  
  hh_count <- triangulation[[hh_col]]
  
  triangulation$operational_category <- ifelse(
    hh_count < threshold_very_small,
    "<50",
    ifelse(
      hh_count > threshold_near_300_upper,
      ">350",
      ifelse(
        hh_count >= threshold_near_300_lower & hh_count <= threshold_near_300_upper,
        "250-350",
        "50-249"
      )
    )
  )
  
  triangulation
}

#' Generate flagged for review and ready for processing lists
#'
#' Splits triangulation results into two output files based on alignment score:
#' - flagged_for_review: All EAs with score <80 (requires manual investigation)
#' - ready_for_processing: High-confidence EAs with score >=80 (can proceed to QGIS)
#'
#' Includes only essential columns: EA_CODE, alignment score, operational category,
#' household estimates from both methods, and agreement metrics.
#'
#' @param triangulation (data.frame) Triangulation results with score and operational_category columns.
#' @param flagged_csv (character) Path to write flagged EAs (sorted by score, worst first).
#' @param ready_csv (character) Path to write ready-for-processing EAs (sorted by operational category).
#'
#' @return (list) List with two data frames: `flagged` and `ready`.
#'
#' @export
generate_flagged_ready_lists <- function(
  triangulation,
  flagged_csv,
  ready_csv
) {
  # Ensure operational_category exists
  if (!"operational_category" %in% names(triangulation)) {
    stop("Column 'operational_category' not found. Run classify_operational_category() first.")
  }
  
  # Define columns to keep - only essential ones for NSO review
  cols_to_keep <- c(
    "EA_CODE",
    "census_ratio_tmpl",
    "predicted_hh_count_2024",
    "hh_lower_2024",
    "hh_upper_2024",
    "pct_diff",
    "direction",
    "within_ci",
    "score",
    "operational_category"
  )
  
  # Filter to only columns that exist in the data
  cols_to_keep <- cols_to_keep[cols_to_keep %in% names(triangulation)]
  
  # Split by score threshold
  flagged <- triangulation[triangulation$score < 80, cols_to_keep]
  ready <- triangulation[triangulation$score >= 80, cols_to_keep]
  
  # Sort flagged by score (worst first)
  flagged <- flagged[order(flagged$score), ]
  
  # Sort ready by operational category
  ready <- ready[order(ready$operational_category), ]
  
  # Write to CSV
  write.csv(flagged, flagged_csv, row.names = FALSE)
  write.csv(ready, ready_csv, row.names = FALSE)
  
  invisible(list(flagged = flagged, ready = ready))
}

