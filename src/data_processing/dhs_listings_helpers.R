# file contains helper functions used during the processing of dhs listings data
library(sf)
library(tidyverse)
library(logger)


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
        data  = dhs_hh_count,
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