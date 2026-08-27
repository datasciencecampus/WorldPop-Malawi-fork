# file contains functions used during the processing of the census data
library(sf)
library(tidyverse)
library(logger)


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


#' Applies cleaning steps to the census data
#'
#' @param mphc_data (tibble) census data to clean, this usually will be mphc_2018
#'
#' @return list containing the cleaned no-GPS census data and the EA summary.
#'
clean_census_data <- function(mphc_data){
  #Mutate and add a  variable called no_persons = 1 (individual record)
  mphc_data <- mphc_data %>%
  mutate(no_persons = 1)  # Individual observation

  # Filter records without GPS coordinates
  log_info("filtering records with no GPS")
  mphc_data_no_gps <- mphc_data %>%
    filter(is.na(hh_longitude) | is.na(hh_latitude))

  # Add additional digits to EA and TA code
  log_info("Pad digits of identifier codes")
  mphc_data_no_gps <- mphc_data_no_gps %>%
  mutate(new_ta = str_pad(ta, width = 2, pad = 0),
          new_ea = str_pad(ea, width = 3, pad = 0))

  # NOTE: currently errors with this 
  # log_info(paste0("Unique Values for 'new_ta' column:",unique(mphc_data_no_gps$new_ta)))
  # log_info(paste0("Unique Values for 'new_ea' column:",unique(mphc_data_no_gps$new_ea)))

  #Create EA_CODE by concatenating district, new_ta and new_ea code
  mphc_data_no_gps <- mphc_data_no_gps %>%
  mutate(EA_CODE = make_ea_code(district, new_ta, new_ea),
          new_ta_ea = str_c(new_ta, new_ea),
          unique_hh_id = str_c(EA_CODE, hhnumber))

  ## Summarise no gps data at EA 

  #total population
  mphc_pop_no_gps <- mphc_data_no_gps %>%  
  group_by(EA_CODE) %>%  
  summarise(mphc_total_pop = sum(no_persons, na.rm = T),
              mphc_hh_count = n_distinct(hhnumber),   #Distinct count of household
              male_count   = sum(p03 == 1, na.rm = TRUE),
              female_count = sum(p03 == 2, na.rm = TRUE))

  age_summary <- summarise_age(mphc_data_no_gps)

  #Join age summary to population data
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
    
    #Create a bin for each age category
    age_summary_df <- df %>%  
    mutate(age_group = case_when(
        .data[[age_col]] < 1  ~ "age_group_01_less",      #less than 1
        .data[[age_col]] >= 1 & .data[[age_col]] <= 4   ~ "age_group_01_04",
        .data[[age_col]] >= 5 & .data[[age_col]] <= 9   ~ "age_group_05_09",
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
        .data[[age_col]] >= 80             ~ "age_group_80plus",
        TRUE ~ NA_character_
    )) %>%  
    # summarise counts per EA_CODE × age_group
    group_by(.data[[ea_col]], age_group) %>%  
    summarise(count = n(), .groups = "drop") %>%  
    rename(EA_CODE = all_of(ea_col)) %>%
    arrange(EA_CODE, age_group)%>%  
    #Pivot to wide columns
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
check_mphc_gpkg_exists <- function(mphc_df, ea_shapefile, mphc_sf_filepath){

    if (!file.exists(mphc_sf_filepath)) {
        log_info("The geopackage version of mphc_2018 is not available, creating dataframe and saving to disk.")
        
        # Convert remaining mphc_2018 data to shapefiles
        
        #Convert to sf object
        log_info("Converting to shapefile object..")
        mphc_2018_sf <- mphc_df %>%
        drop_na(hh_longitude, hh_latitude) %>%
        st_as_sf(coords = c("hh_longitude", "hh_latitude"))
        
        #set the spatial reference
        log_info("Setting spatial reference..")
        st_crs(mphc_2018_sf) <- 4326

        #Fix corrupt geometries
        log_info("Fixing corrupt geometries..")
        ea_shapefile <- st_make_valid(ea_shapefile)

        #Turn off invalid geometries
        sf::sf_use_s2(FALSE)

        #transform
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

        #Write to file
        log_info("Writing gpkg file...")
        st_write(mphc_2018_sf ,
        dsn = mphc_sf_filepath,
        driver = "GPKG",
        delete_layer = TRUE
        )
        
        log_info(paste0("Geopackage successfully saved to: ", mphc_sf_filepath))
    }
}
