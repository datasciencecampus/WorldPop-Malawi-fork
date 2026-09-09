# This script contains a wrapper function for the data  processing pipeline
# it is called upon in main.R
#
# TODO: This script needs a better name!
library(sf)
library(nngeo)
library(haven)
library(tidyverse)
library(logger)

# functions from within this package
source("utils.R")
source("src/data_processing/data_processing_helpers.R")

#' This function will process all steps in the data_processing_2 pipeline
#'
#'
data_processing_2_function <- function() {

log_info("Begining data Processing,")

  # setup
  ## this section includes loading config, variables and any datasets
  config <- load_config()
  if (is.null(config)) {
    stop("Config could not be loaded.")
  }
  print("Config loaded")
  
  log_info("dp1 - Loading config, reading data..")
  #Specify Drive Path
  data_dirs <- config$paths

  drive_path <- data_dirs$drive_path
  input_path <- file.path(drive_path, data_dirs$mnso_data_dir)
  output_path <- file.path(drive_path, data_dirs$output_dir)
  shapefile_path <- file.path(drive_path, data_dirs$shapefile_dir)

  data_sources <- config$sources
  data_thresholds <- config$thresholds

  #Load datasets
  mphc_2018 <- read_dta(file.path(input_path, data_sources$mphc$data_file))
  ICT_data <- read_dta(file.path(input_path, data_sources$ict$data_file))
  IHS6_data <- read_dta(file.path(input_path, data_sources$ihs6$data_file))
  Naca_data <- read_dta(file.path(input_path, data_sources$naca$data_file))
  dhs_listing <- read_dta(file.path(input_path, data_sources$dhs_listing$data_file))
  dhs_data <- read_dta(file.path(input_path, data_sources$dhs_survey$data_file))
  segmented_csv_path <- file.path(input_path, data_sources$dhs_listing$segmented_csv)
  zomba_csv_dir <- file.path(input_path, data_sources$zomba$csv_dir)
  zomba_output_file <- file.path(output_path, data_sources$zomba$output_file)
  malemia_data <- read.csv(file.path(input_path, data_sources$malemia$data_file))
  ea <- sf::st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp"))
  log_info("dp1 - .. config and datasets loaded successfully")

  # data processing 2
  ## this section calls in functions from the  helper script to clean
  ## and process the data

  # Compute total EAs in country from shapefile
  n_total_eas <- length(unique(ea$EA_CODE))

  log_info("dp2 - processing census data")
  mphc_rbind    <- process_census_data(mphc_2018, ea, output_path)
  
  # Deduplicate raw census data to household level for report metrics
  # (mphc_rbind is EA-aggregated and no longer has hhnumber)
  mphc_hh <- mphc_2018 %>%
    mutate(EA_CODE = make_ea_code(district, ta, ea)) %>%
    distinct(EA_CODE, !!as.name(data_sources$mphc$household_id_col), .keep_all = TRUE)
  
  mphc_ea_covered <- length(unique(mphc_hh$EA_CODE))
  
  mphc_dp_stats <- tibble::tibble(
      source           = "mphc",
      n_total          = nrow(mphc_2018),
      n_no_gps         = sum(is.na(mphc_2018[[data_sources$mphc$longitude_col]]) |
                             is.na(mphc_2018[[data_sources$mphc$latitude_col]])),
      n_accurate_gps   = NA_integer_,
      n_inaccurate_gps = NA_integer_,
      n_ea_changed     = NA_integer_,
      n_ea_covered     = mphc_ea_covered,
      n_total_eas      = n_total_eas
  )
  
  log_info("dp3 - processing ICT data")
  ict_result <- process_gps_household_data(
    survey_data = ICT_data,
    ea_shapefile = ea,
    source_config = data_sources$ict,
    output_count_col = "ict_hh_count",
    gps_accuracy_threshold_m = data_thresholds$gps_accuracy_threshold_m
  )
  ICT_rbind <- ict_result$data
  ict_ea_col <- data_sources$ict$source_ea_col
  ict_stats <- ict_result$stats %>%
    mutate(n_ea_covered = length(unique(ICT_rbind[[ict_ea_col]])), n_total_eas = n_total_eas)

  output_df <- mphc_rbind %>%
    left_join(ICT_rbind, by = c("EA_CODE" = ict_ea_col))

  log_info("dp4 - processing IHS6 data")
  IHS_result <- process_gps_household_data(
    survey_data = IHS6_data,
    ea_shapefile = ea,
    source_config = data_sources$ihs6,
    output_count_col = "ihs_hh_count",
    gps_accuracy_threshold_m = data_thresholds$gps_accuracy_threshold_m
  )
  IHS_rbind <- IHS_result$data
  ihs_ea_col <- data_sources$ihs6$source_ea_col
  IHS_stats <- IHS_result$stats %>%
    mutate(n_ea_covered = length(unique(IHS_rbind[[ihs_ea_col]])), n_total_eas = n_total_eas)

  output_df <- output_df %>%
    left_join(IHS_rbind, by = "EA_CODE")

  log_info("dp5 - processing NACA data")
  Naca_result <- process_gps_household_data(
    survey_data = Naca_data,
    ea_shapefile = ea,
    source_config = data_sources$naca,
    output_count_col = "naca_hh_count",
    gps_accuracy_threshold_m = data_thresholds$gps_accuracy_threshold_m
  )
  Naca_rbind <- Naca_result$data
  naca_ea_col <- data_sources$naca$source_ea_col
  Naca_stats <- Naca_result$stats %>%
    mutate(n_ea_covered = length(unique(Naca_rbind[[naca_ea_col]])), n_total_eas = n_total_eas)

  output_df <- output_df %>%
    left_join(Naca_rbind, by = c("EA_CODE" = naca_ea_col))

  log_info("dp6 - processing DHS listing data")
  dhs_listing_result <- process_dhs_listing_data(
    dhs_listing_data = dhs_listing,
    segmented_csv_path = segmented_csv_path,
    ea_shapefile = ea,
    source_config = data_sources$dhs_listing,
    dhs_max_distance_m = data_thresholds$dhs_max_distance_m
  )
  dhs_hh_count <- dhs_listing_result$data
  dhs_stats <- dhs_listing_result$stats %>%
    mutate(n_ea_covered = length(unique(dhs_hh_count$EA_CODE)), n_total_eas = n_total_eas)

  output_df <- output_df %>%
    left_join(dhs_hh_count, by = "EA_CODE")

  # ── Write transformation stats ──────────────────────────────────────────────────
  log_info("dp6.5 - writing transformation stats")
  transformation_stats <- dplyr::bind_rows(
      mphc_dp_stats %>% mutate(source = "mphc"),
      ict_stats %>% mutate(source = "ict"),
      IHS_stats %>% mutate(source = "ihs6"),
      Naca_stats %>% mutate(source = "naca"),
      dhs_stats %>% mutate(source = "dhs_listing")
  )
  qa_stats_dir <- file.path(drive_path, "quality_assurance")
  dir.create(qa_stats_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(transformation_stats,
      file.path(qa_stats_dir, "data_processing2_transformation_stats.csv"),
      row.names = FALSE)

  log_info("dp7 - processing DHS survey data")
  dhs_hh_size <- process_dhs_survey_data(
    dhs_survey_data = dhs_data,
    ea_shapefile = ea,
    source_config = data_sources$dhs_survey,
    dhs_max_distance_m = data_thresholds$dhs_max_distance_m
  )

  output_df <- output_df %>%
    left_join(dhs_hh_size, by = "EA_CODE")

  log_info("dp8 - processing Zomba data")
  zomba_tibble <- process_zomba_data(
    zomba_csv_dir = zomba_csv_dir,
    zomba_output_file = zomba_output_file,
    ea_shapefile = ea,
    source_config = data_sources$zomba
  )

  output_df <- output_df %>%
    left_join(zomba_tibble, by = "EA_CODE")

  log_info("dp9 - processing Malemia data")
  malemia_tibble <- process_malemia_data(
    malemia_data = malemia_data,
    ea_shapefile = ea,
    source_config = data_sources$malemia
  )

  output_df <- output_df %>%
    left_join(malemia_tibble, by = "EA_CODE")

  log_info("dp10 - deriving observed household counts")
  output_df <- output_df %>%
    mutate(
      observed_hh_count = case_when(
        !is.na(malemia_hh_count) ~ malemia_hh_count,
        !is.na(dhs_hh_count) ~ dhs_hh_count,
        !is.na(ihs_hh_count) ~ ihs_hh_count,
        !is.na(naca_hh_count) ~ naca_hh_count,
        !is.na(ict_hh_count) ~ ict_hh_count,
        !is.na(zomba_hh_count) ~ zomba_hh_count,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      EA_CODE,
      mphc_total_pop,
      mphc_median_hh_size,
      mphc_mean_hh_size,
      dhs_median_hh_size,
      dhs_mean_hh_size,
      observed_hh_count,
      dhs_hh_count,
      mphc_hh_count,
      ict_hh_count,
      ihs_hh_count,
      naca_hh_count,
      zomba_hh_count,
      malemia_hh_count,
      female_count,
      male_count,
      starts_with("age_")
    )

  log_info("dp11 - writing summarized survey csv")
  write.csv(
    output_df,
    config$outputs$data_processing2_summarized_csv,
    row.names = FALSE
  )

  log_info("dp12 - creating household size geopackage")
  hh_size <- output_df %>%
    mutate(EA_CODE = as.character(EA_CODE)) %>%
    dplyr::select(EA_CODE, mphc_total_pop, mphc_median_hh_size, mphc_mean_hh_size)

  hh_ea <- full_join(ea, hh_size, by = "EA_CODE") %>%
    dplyr::select(EA_CODE, mphc_total_pop, mphc_median_hh_size, mphc_mean_hh_size)

  st_write(
    hh_ea,
    config$outputs$data_processing2_hh_size_gpkg,
    driver = "GPKG",
    delete_layer = TRUE
  )

  invisible(NULL)
}