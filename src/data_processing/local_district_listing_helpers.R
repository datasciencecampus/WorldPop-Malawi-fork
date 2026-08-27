# file contains helper functions used during the processing of local district
# listings data
library(sf)
library(tidyverse)
library(logger)


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