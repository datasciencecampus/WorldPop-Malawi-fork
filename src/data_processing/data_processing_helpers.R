# file contains helper functions used during the data processing steps
library(sf)
library(tidyverse)
library(logger)


#' Applies cleaning steps to the census data
#'
#' @param mphc_data (tibble) census data to clean, this usually will be mphc_2018
#'
#' @return list containing the cleaned no-GPS census data and the EA summary.
#'
clean_census_data <- function(mphc_data) {
  # Mutate and add a  variable called no_persons = 1 (individual record)
  mphc_data <- mphc_data %>%
    mutate(no_persons = 1) # Individual observation

  # Filter records without GPS coordinates
  log_info("filtering records with no GPS")
  mphc_data_no_gps <- mphc_data %>%
    filter(is.na(hh_longitude) | is.na(hh_latitude))

  # Add additional digits to EA and TA code
  log_info("Pad digits of identifier codes")
  mphc_data_no_gps <- mphc_data_no_gps %>%
    mutate(
      new_ta = str_pad(ta, width = 2, pad = 0),
      new_ea = str_pad(ea, width = 3, pad = 0)
    )

  # NOTE: currently errors with this
  # log_info(paste0("Unique Values for 'new_ta' column:",unique(mphc_data_no_gps$new_ta)))
  # log_info(paste0("Unique Values for 'new_ea' column:",unique(mphc_data_no_gps$new_ea)))

  # Create EA_CODE by concatenating district, new_ta and new_ea code
  mphc_data_no_gps <- mphc_data_no_gps %>%
    mutate(
      EA_CODE = make_ea_code(district, new_ta, new_ea),
      new_ta_ea = str_c(new_ta, new_ea),
      unique_hh_id = str_c(EA_CODE, hhnumber)
    )

  ## Summarise no gps data at EA

  # total population
  mphc_pop_no_gps <- mphc_data_no_gps %>%
    group_by(EA_CODE) %>%
    summarise(
      mphc_total_pop = sum(no_persons, na.rm = T),
      mphc_hh_count = n_distinct(hhnumber), # Distinct count of household
      male_count = sum(p03 == 1, na.rm = TRUE),
      female_count = sum(p03 == 2, na.rm = TRUE)
    )

  age_summary <- summarise_age(mphc_data_no_gps)

  # Join age summary to population data
  mphc_pop_no_gps <- mphc_pop_no_gps %>%
    left_join(age_summary, by = "EA_CODE")

  return(list(
    no_gps_data = mphc_data_no_gps,
    no_gps_summary = mphc_pop_no_gps
  ))
}


#' Summarise EA counts by age column for the given dataset.
#'
#' @param df (tibble) DataFrame containing data to be summarised by age.
#' @param age_col (character) name of the column containing age. Defaults to
#'      "p05".
#' @param ea_col (character, Optional) name contianing EA identifying code.
#'      Defaults to "EA_CODE".
#'
#' @return age_summary_df (tibble, Optional) Dataframe containing a count of
#'      EA's by age.
summarise_age <- function(df, age_col = "p05", ea_col = "EA_CODE") {
  # Create a bin for each age category
  age_summary_df <- df %>%
    mutate(age_group = case_when(
      .data[[age_col]] < 1 ~ "age_group_01_less", # less than 1
      .data[[age_col]] >= 1 & .data[[age_col]] <= 4 ~ "age_group_01_04",
      .data[[age_col]] >= 5 & .data[[age_col]] <= 9 ~ "age_group_05_09",
      .data[[age_col]] >= 10 & .data[[age_col]] <= 14 ~ "age_group_10_14",
      .data[[age_col]] >= 15 & .data[[age_col]] <= 19 ~ "age_group_15_19",
      .data[[age_col]] >= 20 & .data[[age_col]] <= 24 ~ "age_group_20_24",
      .data[[age_col]] >= 25 & .data[[age_col]] <= 29 ~ "age_group_25_29",
      .data[[age_col]] >= 30 & .data[[age_col]] <= 34 ~ "age_group_30_34",
      .data[[age_col]] >= 35 & .data[[age_col]] <= 39 ~ "age_group_35_39",
      .data[[age_col]] >= 40 & .data[[age_col]] <= 44 ~ "age_group_40_44",
      .data[[age_col]] >= 45 & .data[[age_col]] <= 49 ~ "age_group_45_49",
      .data[[age_col]] >= 50 & .data[[age_col]] <= 54 ~ "age_group_50_54",
      .data[[age_col]] >= 55 & .data[[age_col]] <= 59 ~ "age_group_55_59",
      .data[[age_col]] >= 60 & .data[[age_col]] <= 64 ~ "age_group_60_64",
      .data[[age_col]] >= 65 & .data[[age_col]] <= 69 ~ "age_group_65_69",
      .data[[age_col]] >= 70 & .data[[age_col]] <= 74 ~ "age_group_70_74",
      .data[[age_col]] >= 75 & .data[[age_col]] <= 79 ~ "age_group_75_79",
      .data[[age_col]] >= 80 ~ "age_group_80plus",
      TRUE ~ NA_character_
    )) %>%
    # summarise counts per EA_CODE × age_group
    group_by(.data[[ea_col]], age_group) %>%
    summarise(count = n(), .groups = "drop") %>%
    rename(EA_CODE = all_of(ea_col)) %>%
    arrange(EA_CODE, age_group) %>%
    # Pivot to wide columns
    pivot_wider(
      names_from = age_group,
      values_from = count,
      values_fill = 0
    ) %>%
    arrange(across(all_of(ea_col)))


  return(age_summary_df)
}

#' Checks that the mphc geopakage exists and creates one if not.
#'
#' 'gpkg_file_path' will be replaced by a value from the config
#'
#' @param mphc_df (tibble) dataframe containing census data. Only used when no
#'      gpkg existing
#' @param ea_shapefile (??) shapefile containing the current EA's
#' @param mphc_sf_filepath (filepath-like) filepath where mphc gpkg expected to be
#'      found.
check_mphc_gpkg_exists <- function(mphc_df, ea_shapefile, mphc_sf_filepath) {
  if (!file.exists(mphc_sf_filepath)) {
    log_info("The geopackage version of mphc_2018 is not available, creating dataframe and saving to disk.")

    # Convert remaining mphc_2018 data to shapefiles

    # Convert to sf object
    log_info("Converting to shapefile object..")
    mphc_2018_sf <- mphc_df %>%
      drop_na(hh_longitude, hh_latitude) %>%
      st_as_sf(coords = c("hh_longitude", "hh_latitude"))

    # set the spatial reference
    log_info("Setting spatial reference..")
    st_crs(mphc_2018_sf) <- 4326

    # Fix corrupt geometries
    log_info("Fixing corrupt geometries..")
    ea_shapefile <- st_make_valid(ea_shapefile)

    # Turn off invalid geometries
    sf::sf_use_s2(FALSE)

    # transform
    log_info("applying transfomation..")
    mphc_2018_sf <- st_transform(mphc_2018_sf, crs = st_crs(ea_shapefile))

    # EA Nearest Neighbor Assignment
    log_info("Assigning Ea's nearest neighbours..")
    nearest_indices <- st_nearest_feature(mphc_2018_sf, ea_shapefile)

    # Extract the EA_CODE  of the nearest polygons
    log_info("extracting EA_CODE of nearest polygons..")
    nearest_ids <- ea_shapefile$EA_CODE[nearest_indices]

    # Add the EA_CODE to data
    log_info("Add EA_CODE to data..")
    mphc_2018_sf$EA_CODE <- nearest_ids

    # Write to file
    log_info("Writing gpkg file...")
    st_write(mphc_2018_sf,
      dsn = mphc_sf_filepath,
      driver = "GPKG",
      delete_layer = TRUE
    )

    log_info(paste0("Geopackage successfully saved to: ", mphc_sf_filepath))
  }
}


#' Builds the census summary using both non-spatial and spatial records.
#'
#' @param mphc_data (tibble) census data to process.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param output_path (character) directory where the cached census geopackage lives.
#'
#' @return mphc_rbind (tibble) census summary at EA level with household size fields.
process_census_data <- function(mphc_data, ea_shapefile, output_path) {
  log_info("dp2.1 - cleaning census data without GPS")
  no_gps_results <- clean_census_data(mphc_data)

  mphc_sf_filepath <- file.path(output_path, "mphc_2018_sf_ea.gpkg")
  check_mphc_gpkg_exists(mphc_data, ea_shapefile, mphc_sf_filepath)

  log_info("dp2.2 - loading and summarising spatial census data")
  mphc_2018_sf <- st_read(mphc_sf_filepath)

  mphc_2018_df <- mphc_2018_sf %>%
    as_tibble() %>%
    mutate(
      no_persons = 1,
      unique_hh_id = str_c(EA_CODE, hhnumber)
    )

  mphc_2018_pop_spatial <- mphc_2018_df %>%
    group_by(EA_CODE) %>%
    summarise(
      mphc_total_pop = sum(no_persons, na.rm = T),
      mphc_hh_count = n_distinct(hhnumber),
      male_count = sum(p03 == 1, na.rm = TRUE),
      female_count = sum(p03 == 2, na.rm = TRUE)
    ) %>%
    left_join(summarise_age(mphc_2018_df), by = "EA_CODE")

  log_info("dp2.3 - combining census summaries")
  mphc_rbind <- bind_rows(mphc_2018_pop_spatial, no_gps_results$no_gps_summary) %>%
    group_by(EA_CODE) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
    mutate(
      tally_age = ifelse(
        rowSums(select(., starts_with("age_group_")), na.rm = TRUE) == mphc_total_pop,
        "true",
        "false"
      )
    )

  log_info("dp2.4 - calculating census household sizes")
  mphc_hh_size1 <- no_gps_results$no_gps_data %>%
    group_by(unique_hh_id, EA_CODE) %>%
    summarise(mphc_hh_size1 = sum(no_persons, na.rm = T), .groups = "drop")

  mphc_hh_size2 <- mphc_2018_df %>%
    group_by(unique_hh_id, EA_CODE) %>%
    summarise(mphc_hh_size2 = sum(no_persons, na.rm = T), .groups = "drop")

  mphc_hh_size <- full_join(mphc_hh_size2, mphc_hh_size1, by = c("unique_hh_id", "EA_CODE")) %>%
    mutate(hh_size_total = rowSums(across(c(mphc_hh_size1, mphc_hh_size2)), na.rm = TRUE)) %>%
    drop_na(EA_CODE) %>%
    group_by(EA_CODE) %>%
    summarise(
      mphc_median_hh_size = median(hh_size_total, na.rm = T),
      mphc_mean_hh_size = mean(hh_size_total, na.rm = T),
      .groups = "drop"
    )

  mphc_rbind <- mphc_rbind %>%
    inner_join(mphc_hh_size, by = "EA_CODE")

  return(mphc_rbind)
}


#' Generic processor for GPS-based household listing data at EA level.
#'
#' @param survey_data (tibble) source listing data.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) source-specific config containing EA, accuracy,
#'      longitude and latitude column names.
#' @param output_count_col (character) name of the output count column.
#' @param gps_accuracy_threshold_m (numeric) threshold in meters used to split
#'      original EA assignment versus spatial reassignment.
#'
#' @return survey_rbind (tibble) EA-level household counts.
process_gps_household_data <- function(
  survey_data,
  ea_shapefile,
  source_config,
  output_count_col,
  gps_accuracy_threshold_m
) {
  source_ea_col <- source_config$source_ea_col
  accuracy_col <- source_config$accuracy_col
  longitude_col <- source_config$longitude_col
  latitude_col <- source_config$latitude_col
  reassigned_ea_col <- paste0(source_ea_col, "_spatial")

  survey_data <- survey_data %>%
    mutate(hh_count = 1)

  no_gps <- survey_data %>%
    filter(is.na(.data[[longitude_col]]) | is.na(.data[[latitude_col]])) %>%
    group_by(.data[[source_ea_col]]) %>%
    summarise(hh_count = sum(hh_count, na.rm = T), .groups = "drop") %>%
    rename(!!output_count_col := hh_count)

  survey_sf <- survey_data %>%
    drop_na(all_of(c(longitude_col, latitude_col))) %>%
    st_as_sf(coords = c(longitude_col, latitude_col))

  st_crs(survey_sf) <- 4326
  survey_sf <- st_transform(survey_sf, crs = st_crs(ea_shapefile))

  nearest_indices <- st_nearest_feature(survey_sf, ea_shapefile)
  survey_sf[[reassigned_ea_col]] <- ea_shapefile$EA_CODE[nearest_indices]

  survey_sf <- survey_sf %>% as_tibble()

  greater <- survey_sf %>%
    filter(.data[[accuracy_col]] > gps_accuracy_threshold_m) %>%
    group_by(.data[[source_ea_col]]) %>%
    summarise(hh_count = sum(hh_count, na.rm = T), .groups = "drop") %>%
    rename(!!output_count_col := hh_count)

  less <- survey_sf %>%
    filter(.data[[accuracy_col]] < gps_accuracy_threshold_m) %>%
    group_by(.data[[reassigned_ea_col]]) %>%
    summarise(hh_count = sum(hh_count, na.rm = T), .groups = "drop") %>%
    rename(!!source_ea_col := all_of(reassigned_ea_col)) %>%
    rename(!!output_count_col := hh_count)

  survey_rbind <- bind_rows(no_gps, greater, less) %>%
    group_by(.data[[source_ea_col]]) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # ── EA change stats for accurate GPS rows ───────────────────────────────────────
  accurate_rows <- survey_sf %>%
    filter(.data[[accuracy_col]] < gps_accuracy_threshold_m)
  n_accurate <- nrow(accurate_rows)
  n_ea_changed <- sum(
    as.character(accurate_rows[[source_ea_col]]) !=
      as.character(accurate_rows[[reassigned_ea_col]]),
    na.rm = TRUE
  )

  return(list(
    data = survey_rbind,
    stats = tibble::tibble(
      n_total          = nrow(survey_data),
      n_no_gps         = nrow(survey_data) - nrow(survey_sf),
      n_accurate_gps   = n_accurate,
      n_inaccurate_gps = nrow(survey_sf) - n_accurate,
      n_ea_changed     = n_ea_changed
    )
  ))
}


#' Process DHS listing data to EA-level household counts.
#'
#' @param dhs_listing_data (tibble) DHS listing data.
#' @param segmented_csv_path (character) path to segmented DHS CSV.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for DHS listing source.
#' @param dhs_max_distance_m (numeric) maximum centroid distance allowed.
#'
#' @return dhs_hh_count (tibble) EA-level household counts from DHS listing.
process_dhs_listing_data <- function(
  dhs_listing_data,
  segmented_csv_path,
  ea_shapefile,
  source_config,
  dhs_max_distance_m
) {
  cluster_col <- source_config$cluster_id_col
  longitude_col <- source_config$longitude_col
  latitude_col <- source_config$latitude_col

  dhs_file <- read.csv(segmented_csv_path)
  non_seg_cluster <- dhs_file %>%
    filter(grepl("^no\\b", Cluster.Segmented, ignore.case = TRUE))

  n_total_clusters <- n_distinct(dhs_listing_data[[cluster_col]])

  dhs_listing_data <- dhs_listing_data %>%
    mutate(hh_count = 1) %>%
    filter(.data[[cluster_col]] %in% unique(non_seg_cluster$DHScluster))

  n_nonseg_clusters <- n_distinct(dhs_listing_data[[cluster_col]])

  dhs_hh_summary <- dhs_listing_data %>%
    group_by(.data[[cluster_col]]) %>%
    summarise(dhs_hh_count = sum(hh_count, na.rm = T), .groups = "drop")

  dhs_centroids <- dhs_listing_data %>%
    group_by(.data[[cluster_col]]) %>%
    summarise(
      lon = mean(.data[[longitude_col]], na.rm = TRUE),
      lat = mean(.data[[latitude_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(dhs_hh_summary, by = cluster_col)

  dhs_centroids_sf <- dhs_centroids %>%
    drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"))

  st_crs(dhs_centroids_sf) <- 4326
  dhs_centroids_sf <- st_transform(dhs_centroids_sf, crs = st_crs(ea_shapefile))

  nearest <- nngeo::st_nn(dhs_centroids_sf, ea_shapefile, k = 1, returnDist = TRUE)
  distances <- sapply(nearest$dist, function(x) x[1])

  dhs_centroids_sf <- dhs_centroids_sf %>%
    mutate(nearest_dist_m = distances) %>%
    filter(nearest_dist_m < dhs_max_distance_m)

  n_within_dist <- nrow(dhs_centroids_sf)

  nearest_indices <- st_nearest_feature(dhs_centroids_sf, ea_shapefile)
  dhs_centroids_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

  dhs_hh_count <- dhs_centroids_sf %>%
    as_tibble() %>%
    group_by(EA_CODE) %>%
    summarise(dhs_hh_count = sum(dhs_hh_count, na.rm = T), .groups = "drop")

  return(list(
    data = dhs_hh_count,
    stats = tibble::tibble(
      n_total_clusters  = n_total_clusters,
      n_nonseg_clusters = n_nonseg_clusters,
      n_seg_excluded    = n_total_clusters - n_nonseg_clusters,
      n_within_dist     = n_within_dist,
      n_dist_excluded   = n_nonseg_clusters - n_within_dist
    )
  ))
}


#' Process DHS survey data to EA-level household size summaries.
#'
#' @param dhs_survey_data (tibble) DHS survey person-level data.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for DHS survey source.
#' @param dhs_max_distance_m (numeric) maximum centroid distance allowed.
#'
#' @return dhs_hh_size (tibble) EA-level median/mean household size from DHS survey.
process_dhs_survey_data <- function(
  dhs_survey_data,
  ea_shapefile,
  source_config,
  dhs_max_distance_m
) {
  cluster_col <- source_config$cluster_id_col
  longitude_col <- source_config$longitude_col
  latitude_col <- source_config$latitude_col
  household_col <- source_config$household_id_col

  dhs_size <- dhs_survey_data %>%
    mutate(
      unique_id = str_c(
        .data[[cluster_col]],
        REG_NAME,
        DIST_NAME,
        TA_NAME,
        EA_NUMBER,
        .data[[household_col]]
      ),
      no_persons = 1
    ) %>%
    group_by(unique_id) %>%
    mutate(hh_size = sum(no_persons, na.rm = T)) %>%
    ungroup() %>%
    group_by(.data[[cluster_col]]) %>%
    summarise(
      median_hh_size = median(hh_size, na.rm = T),
      mean_hh_size = mean(hh_size, na.rm = T),
      .groups = "drop"
    )

  dhs_centroids <- dhs_survey_data %>%
    group_by(.data[[cluster_col]]) %>%
    summarise(
      lon = mean(.data[[longitude_col]], na.rm = TRUE),
      lat = mean(.data[[latitude_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(dhs_size, by = cluster_col)

  dhs_centroids_sf <- dhs_centroids %>%
    drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"))

  st_crs(dhs_centroids_sf) <- 4326
  dhs_centroids_sf <- st_transform(dhs_centroids_sf, crs = st_crs(ea_shapefile))

  nearest <- nngeo::st_nn(dhs_centroids_sf, ea_shapefile, k = 1, returnDist = TRUE)
  distances <- sapply(nearest$dist, function(x) x[1])

  dhs_centroids_sf <- dhs_centroids_sf %>%
    mutate(nearest_dist_m = distances) %>%
    filter(nearest_dist_m < dhs_max_distance_m)

  nearest_indices <- st_nearest_feature(dhs_centroids_sf, ea_shapefile)
  dhs_centroids_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

  dhs_hh_size <- dhs_centroids_sf %>%
    as_tibble() %>%
    group_by(EA_CODE) %>%
    summarise(
      dhs_median_hh_size = median(median_hh_size, na.rm = T),
      dhs_mean_hh_size = mean(mean_hh_size, na.rm = T),
      .groups = "drop"
    )

  return(dhs_hh_size)
}


#' Process Zomba district CSV listings to EA-level counts.
#'
#' @param zomba_csv_dir (character) folder containing Zomba CSV files.
#' @param zomba_output_file (character) path for bound intermediate CSV.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for Zomba source.
#'
#' @return zomba_tibble (tibble) EA-level Zomba household counts.
process_zomba_data <- function(
  zomba_csv_dir,
  zomba_output_file,
  ea_shapefile,
  source_config
) {
  longitude_col <- source_config$longitude_col
  latitude_col <- source_config$latitude_col
  household_size_col <- source_config$household_size_col

  rbind_zomba_csvs(
    csv_dir = zomba_csv_dir,
    output_file = zomba_output_file
  )

  zomba_data <- read.csv(zomba_output_file)

  zomba_data <- zomba_data %>%
    mutate(hh_count = 1)

  zomba_sf <- zomba_data %>%
    drop_na(all_of(c(longitude_col, latitude_col))) %>%
    st_as_sf(coords = c(longitude_col, latitude_col))

  st_crs(zomba_sf) <- 4326
  zomba_sf <- st_transform(zomba_sf, crs = st_crs(ea_shapefile))

  nearest_indices <- st_nearest_feature(zomba_sf, ea_shapefile)
  zomba_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

  zomba_tibble <- zomba_sf %>%
    as_tibble() %>%
    group_by(EA_CODE) %>%
    summarise(
      zomba_hh_count = sum(hh_count, na.rm = T),
      zomba_pop = sum(.data[[household_size_col]], na.rm = T),
      .groups = "drop"
    )

  return(zomba_tibble)
}


#' Process Malemia listing data to EA-level counts.
#'
#' @param malemia_data (tibble) Malemia source data.
#' @param ea_shapefile (sf) EA boundaries used for nearest-neighbour assignment.
#' @param source_config (list) config block for Malemia source.
#'
#' @return malemia_tibble (tibble) EA-level Malemia household counts.
process_malemia_data <- function(
  malemia_data,
  ea_shapefile,
  source_config
) {
  longitude_col <- source_config$longitude_col
  latitude_col <- source_config$latitude_col

  malemia_data <- malemia_data %>%
    mutate(hh_count = 1)

  malemia_sf <- malemia_data %>%
    drop_na(all_of(c(longitude_col, latitude_col))) %>%
    st_as_sf(coords = c(longitude_col, latitude_col))

  st_crs(malemia_sf) <- 4326
  malemia_sf <- st_transform(malemia_sf, crs = st_crs(ea_shapefile))

  nearest_indices <- st_nearest_feature(malemia_sf, ea_shapefile)
  malemia_sf$EA_CODE <- ea_shapefile$EA_CODE[nearest_indices]

  malemia_tibble <- malemia_sf %>%
    as_tibble() %>%
    group_by(EA_CODE) %>%
    summarise(malemia_hh_count = sum(hh_count, na.rm = T), .groups = "drop")

  return(malemia_tibble)
}
