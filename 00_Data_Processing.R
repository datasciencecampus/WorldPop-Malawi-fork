# Data exploration workflow for dataset received

#load packages
library(sf)
library(haven)
library(tidyverse)
#library(foreign)

options(scipen = 999) # turn off scientific notation for all variables

#Specify Drive Path
drive_path <- "./data"
input_path <- file.path(drive_path, "MNSO-Data")
output_path <- file.path(drive_path, "Output_Data")
shapefile_path <- file.path(drive_path, "Shapefiles")

#Load datasets
mphc_2018 <- read_dta(file.path(input_path, "mphc2018Data_AllRegions.dta"))
mphc_structures_2018 <- read_dta(file.path(input_path, "mphc2018Data_structures.dta"))
ICT_data <- read_dta(file.path(input_path, "ICT Listing WorldPop.dta"))
IHS6_data <- read_dta(file.path(input_path, "IHS6 Listing WorldPop.dta"))
Naca_data <- read_dta(file.path(input_path, "Naca Listing WorldPop.dta"))
ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use.shp")) # replaces "2018_MPHC_EAs_Final_for_Use_Corrected.shp"
dhs_data <- read_dta(file.path(input_path, "MDHS_2024_NoDZLK_anonymized.dta"))
dhs_listing <- read_dta(file.path(input_path, "FINAL MDHS LISTING DATA_Annon.dta"))

mphc_structures_2018_filepath <- file.path(
  output_path, "mphc_structures_points.gpkg")

if (!file.exists(mphc_structures_2018_filepath)) {
  #Structures
  #Convert to sf object
  mphc_structures_sf <- mphc_structures_2018 |>
    drop_na(st_longitude, st_latitude) |>
    st_as_sf(coords = c("st_longitude", "st_latitude"))

  #set the spatial reference
  st_crs(mphc_structures_sf ) <- 4326

  # Write to GPKG file
  st_write(mphc_structures_sf ,
           dsn = file.path(output_path, "mphc_structures_points.gpkg"),
           driver = "GPKG",
           delete_layer = TRUE)
}
mphc_structures_2018 <- st_read(file.path(output_path, "mphc_structures_points.gpkg"))

# Write each data as a shapefile  -----------------------------------------

# #Convert to sf object
# mphc_2018_sf <- mphc_2018 |> 
#   drop_na(hh_longitude, hh_latitude) |> 
#   st_as_sf(coords = c("hh_longitude", "hh_latitude"))
# 
# #set the spatial reference
# st_crs(mphc_2018_sf) <- 4326
# 
# # Write to GPKG file
# st_write(mphc_2018_sf, 
#          dsn = file.path(output_path, "mphc_2018_points.gpkg"), 
#          driver = "GPKG", 
#          delete_layer = TRUE)  # overwrite if layer already exists
# 

# 
# #ICT
# ICT_sf <- ICT_data |> 
#   drop_na(GPS__Longitude, GPS__Latitude) |> 
#   st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"))
# 
# #set the spatial reference
# st_crs(ICT_sf) <- 4326
# 
# # Write to GPKG file
# st_write(ICT_sf, 
#          dsn = file.path(output_path, "ICT_points.gpkg"), 
#          driver = "GPKG", 
#          delete_layer = TRUE)
# 
# #IHS6
# IHS6_sf <- IHS6_data |> 
#   drop_na(GPS__Longitude, GPS__Latitude) |> 
#   st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"))
# 
# #set the spatial reference
# st_crs(IHS6_sf) <- 4326
# 
# # Write to GPKG file
# st_write(IHS6_sf, 
#          dsn = file.path(output_path, "IHS6_points.gpkg"), 
#          driver = "GPKG", 
#          delete_layer = TRUE)
# 
# #Naca
# Naca_sf <- Naca_data |> 
#   drop_na(longitude, latitude) |> 
#   st_as_sf(coords = c("longitude", "latitude"))
# 
# #set the spatial reference
# st_crs(Naca_sf) <- 4326
# 
# # Write to GPKG file
# st_write(Naca_sf, 
#          dsn = file.path(output_path, "Naca_points.gpkg"), 
#          driver = "GPKG", 
#          delete_layer = TRUE)

# #DHS Listing
 #DHS_listing_sf <- dhs_listing |> 
   #drop_na(llongitude, llatitude) |> 
  # st_as_sf(coords = c("llongitude", "llatitude"))
# 
# #set the spatial reference
 #st_crs( DHS_listing_sf) <- 4326
# 
# # Write to GPKG file
# st_write( DHS_listing_sf, 
        #  dsn = file.path(output_path, "DHS_listing_points.gpkg"), 
        #  driver = "GPKG", 
        #  delete_layer = TRUE)
 
 # #DHS Data
 #DHS_sf <- dhs_data |> 
  # drop_na(GHLONGITUDE, GHLATITUDE) |> 
   #st_as_sf(coords = c("GHLONGITUDE", "GHLATITUDE"))
 # 
 # #set the spatial reference
 #st_crs(DHS_sf) <- 4326
 # 
 # # Write to GPKG file
 #st_write(DHS_sf, 
          # dsn = file.path(output_path, "DHS_data_points.gpkg"), 
           #driver = "GPKG", 
          # delete_layer = TRUE)

#Add additonal digits to EA number

dhs_listing1 <- dhs_listing |> 
  mutate(new_EA_NUMBER = str_pad(EA_NUMBER, width = 3, pad = 0))

#remove A, B, C with columns

# To update your original data frame column:
dhs_listing1$new_EA_NUMBER_CLEAN <- gsub("^(\\d{3}).*", "\\1", dhs_listing1$new_EA_NUMBER)

#get unique EA number from EA
ea_number <- ea %>% 
  distinct(EA_NUMBER)

# join to dhs_listing
dhs_listing2 <- dhs_listing1 %>% 
  left_join(ea_number, by = c("new_EA_NUMBER_CLEAN" = "EA_NUMBER"), keep = T)

#

###############################################################################
###############################################################################
########### Explore Each Dataset ############################################

# MPHC 2018 ---------------------------------------------------------------

#Add additonal digits to EA and TA code

mphc_2018 <- mphc_2018 |> 
  mutate(new_ta = str_pad(ta, width = 2, pad = 0),
         new_ea = str_pad(ea, width = 3, pad = 0))

unique(mphc_2018$new_ta)
unique(mphc_2018$new_ea)

#Create EA_CODE by concatenating district, new_ta and new_ea code
mphc_2018 <- mphc_2018 |> 
  mutate(EA_CODE = str_c(district, new_ta, new_ea),
         new_ta_ea = str_c(new_ta, new_ea),
         hh_count = 1)  # Individual observation

# Summarise data at EA ------------------------------------------------

#total population
mphc_pop_per_ea <- mphc_2018 |> 
  group_by(EA_CODE) |> 
  summarise(mphc_total_pop = sum(hh_count, na.rm = T),
            mphc_hh_count = n_distinct(hhnumber),   #Distinct count of household
            hh_size = mphc_total_pop/mphc_hh_count, 
            male_count   = sum(p03 == 1, na.rm = TRUE),
            female_count = sum(p03 == 2, na.rm = TRUE))
  

#Create a bin for each age category
age_summary <- mphc_2018 |> 
  mutate(age_group = case_when(
    p05 < 1  ~ "age_group_01_less",      #less than 1
    p05 >= 1 & p05 <= 4   ~ "age_group_01_04",
    p05 >= 5 & p05 <= 9   ~ "age_group_05_09",
    p05 >= 10 & p05 <= 14 ~ "age_group_10_14",
    p05 >= 15 & p05 <= 19 ~ "age_group_15_19",
    p05 >= 20 & p05 <= 24 ~ "age_group_20_24",
    p05 >= 25 & p05 <= 29 ~ "age_group_25_29",
    p05 >= 30 & p05 <= 34 ~ "age_group_30_34",
    p05 >= 35 & p05 <= 39 ~ "age_group_35_39",
    p05 >= 40 & p05 <= 44 ~ "age_group_40_44",
    p05 >= 45 & p05 <= 49 ~ "age_group_45_49",
    p05 >= 50 & p05 <= 54 ~ "age_group_50_54",
    p05 >= 55 & p05 <= 59 ~ "age_group_55_59",
    p05 >= 60 & p05 <= 64 ~ "age_group_60_64",
    p05 >= 65 & p05 <= 69 ~ "age_group_65_69",
    p05 >= 70 & p05 <= 74 ~ "age_group_70_74",
    p05 >= 75 & p05 <= 79 ~ "age_group_75_79",
    p05 >= 80             ~ "age_group_80plus",
    TRUE ~ NA_character_
  )) |> 

# summarise counts per EA_CODE × age_group
  group_by(EA_CODE, new_ta_ea, age_group) |> 
  summarise(count = n(), .groups = "drop") |> 
  arrange(EA_CODE, age_group)|> 
  
  #Pivot to wide columns
  pivot_wider(
    names_from = age_group,
    values_from = count,
    values_fill = 0
  ) |> 
  arrange(EA_CODE)

#Join it to pop data
mphc_pop_per_ea <- mphc_pop_per_ea |> 
  left_join(age_summary, by = "EA_CODE")

#Check if age columns sum to total_pop
#mphc_pop_per_ea <- mphc_pop_per_ea |> 
 # mutate(
  #  tally_age = ifelse(
    #  rowSums(select(., starts_with("age_group_")), na.rm = TRUE) == mphc_total_pop,
    #  "true", "false"
   # )
#  )

#####################################################################
#####################################################################

# Structure Data Processing -----------------------------------------------
#Fix corrupt geometries
st_make_valid(ea)

#Turn off invalid geometries
sf::sf_use_s2(FALSE)

#transform
mphc_structures_2018 <- st_transform(mphc_structures_2018, crs = st_crs(ea))

# EA Nearest Neighbor Assignment 
nearest_indices <- st_nearest_feature(mphc_structures_2018, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
mphc_structures_2018$EA_CODE <- nearest_ids

#count buildings
mphc_structures_2018 <- mphc_structures_2018 |> 
  as_tibble() |> 
  mutate(b_count = 1)

#check the use of the structure
sort(unique(mphc_structures_2018$s3_structure_use))

#NB : Filter in future
#Filter only accomdation block or hostel
#res <- mphc_structures_2018 |> 
 # filter(s3_structure_use == 6 | s3_structure_use == 11)

#Summarize total no of buildings in each EA
ea_buildings <- mphc_structures_2018 |> 
  group_by(EA_CODE) |> 
  summarise(mphc_bcount = sum(b_count))

#Join to mphc census data
mphc_pop_per_ea <- mphc_pop_per_ea |> 
  left_join(ea_buildings, by = "EA_CODE")

################################################################
################################################################

# ICT Data Processing -----------------------------------------------------

ict_pop_per_ea <- ICT_data |> 
  mutate(hh_count = 1) |> 
  group_by(EA_Number) |> 
  summarise(ict_hh_count = sum(hh_count, na.rm = T)) |> 
  ungroup()

##################################################################
#################################################################

# IHS6 Data Processing ----------------------------------------------------
ihs_pop_per_ea <- IHS6_data |> 
  mutate(hh_count = 1) |> 
  group_by(EA_CODE) |> 
  summarise(ihs_hh_count = sum(hh_count, na.rm = T)) |> 
  ungroup()

###################################################################
###################################################################
# Naca Data Processing ----------------------------------------------------
naca_pop_per_ea <- Naca_data |> 
  mutate(hh_count = 1) |> 
  group_by(EA_Number) |> 
  summarise(naca_hh_count = sum(hh_count, na.rm = T)) |> 
  ungroup()

# Combine Data ------------------------------------------------------------

combined_data <- mphc_pop_per_ea |> 
  left_join(ict_pop_per_ea, by = c("EA_CODE" ="EA_Number")) |> 
  left_join(ihs_pop_per_ea, by = "EA_CODE") |> 
  left_join(naca_pop_per_ea, by = c("EA_CODE" ="EA_Number"))

#create observed hh_count based on priority conditions
combined_data <- combined_data |> 
  mutate(
    observed_hh_count = case_when(
      # if ihs_hh_count is available, use it (highest priority)
      !is.na(ihs_hh_count) ~ ihs_hh_count,
      # else if naca_hh_count is available, use it (second priority)
      !is.na(naca_hh_count) ~ naca_hh_count,
      # else if ict_hh_count is available, use it (last priority)
      !is.na(ict_hh_count) ~ ict_hh_count,
      # else put NA
      TRUE ~ NA_real_
    )
  )

#Arrange data
combined_data <- combined_data |> 
  select(EA_CODE, mphc_bcount, mphc_total_pop,hh_size, observed_hh_count,
         mphc_hh_count, ict_hh_count, ihs_hh_count,
         naca_hh_count, female_count, male_count, starts_with("age_"))
  
#Write to file

write.csv(combined_data, file.path(output_path, "summarized_survey_data.csv"), row.names = F)


#join combined data to EA shapefile and export
#Join to ea data
combined_data <- combined_data |> 
  mutate(EA_CODE = as.character(EA_CODE))  #convert EA code to integer

#select hh size
hh_size <- combined_data |> 
  dplyr::select(EA_CODE, mphc_total_pop, hh_size)

hh_ea <-full_join(ea, hh_size, by = "EA_CODE")

#select important variables
hh_ea <- hh_ea |> 
  select(EA_CODE, mphc_total_pop, hh_size)

#write to file
st_write(hh_ea, file.path(output_path, "hh_size_data.shp"))




