# Summarizing data at the EA level using EA-CODE and Spatial location of the points

# load packages
library(readxl)
library(tmap)
library(sf)
library(nngeo)
library(haven)
library(tidyverse)


options(scipen = 999) # turn off scientific notation for all variables

# Specify Drive Path
drive_path <- "//Internal_Path/"
input_path <- paste0(drive_path, "Data/Surveys/")
output_path <- paste0(drive_path, "/Output_Data/")
shapefile_path <- paste0(drive_path, "Data/Shapefiles/")

##################################################################################
##################################################################################
################## LOAD EA SHAPEFILE #############################################
# load data
ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use_Corrected.shp"))

# check names of variables in ea shapefile
names(ea)

# Check whether EA_CODE are duplicated (returns TRUE or FALSE)
any(duplicated(ea$EA_CODE))

# Count the total number of duplicate rows
sum(duplicated(ea$EA_CODE))

# Extract all rows where the EA_CODE appears more than once
ea_duplicates <- ea %>%
  filter(EA_CODE %in% EA_CODE[duplicated(EA_CODE)]) %>%
  arrange(EA_CODE) # Group them together by code for easy viewing

# View the duplicates interactively

tmap_mode("view")
tm_shape(ea_duplicates) +
  tm_polygons("EA_CODE", id = "DIST_NAME")


# Create a Pseudo Unique ID for all the EAs in the country
ea <- ea %>%
  mutate(
    cluster_id = paste0("EA", sprintf("%06d", row_number()))
  )

# check duplicates
any(duplicated(ea$cluster_id))

# Fix corrupt geometries
st_make_valid(ea)

# Turn of geometric plane
sf::sf_use_s2(FALSE)

#####################################################################################
####################################################################################
######### PROCESS 2018 CENSUS DATA #################################################

# Load datasets
mphc_2018 <- read_dta(paste0(input_path, "mphc2018Data_AllRegions.dta"))

# head
head(mphc_2018)

# names
names(mphc_2018)

# Mutate and add a  variable called no_persons = 1 (individual record)
mphc_2018 <- mphc_2018 %>%
  mutate(no_persons = 1) # Individual observation

# Filter records without GPS coordinates
mphc_2018_no_gps <- mphc_2018 %>%
  filter(is.na(hh_longitude) | is.na(hh_latitude))

########################################################################

# Here we are going to process record without gps first
# We are processing the data based on EA_CODE

# Add additional digits to EA and TA code
mphc_2018_no_gps <- mphc_2018_no_gps %>%
  mutate(
    new_ta = str_pad(ta, width = 2, pad = 0),
    new_ea = str_pad(ea, width = 3, pad = 0)
  )

# check new_ta and new_ea code
unique(mphc_2018_no_gps$new_ta)
unique(mphc_2018_no_gps$new_ea)

# Create EA_CODE by concatenating district, new_ta and new_ea code
# This will be useful to join the data to the EA shapefile
mphc_2018_no_gps <- mphc_2018_no_gps %>%
  mutate(EA_CODE = str_c(district, new_ta, new_ea))

# Summarise no gps data at EA to get total population, hh_count, male_count and female

# total population
mphc_summary_no_gps <- mphc_2018_no_gps %>%
  group_by(EA_CODE) %>%
  summarise(
    mphc_total_pop = sum(no_persons, na.rm = T),
    mphc_hh_count = n_distinct(hhnumber), # Distinct count of household
    male_count = sum(p03 == 1, na.rm = TRUE),
    female_count = sum(p03 == 2, na.rm = TRUE)
  ) %>%
  ungroup()

##############################################################################
#############################################################################

# Next we will process the records with gps based on spatial location

# Convert remaining mphc_2018 data to point shapefiles

# Convert to sf object
mphc_2018_sf <- mphc_2018 %>%
  drop_na(hh_longitude, hh_latitude) %>%
  st_as_sf(coords = c("hh_longitude", "hh_latitude"))

# set the spatial reference to wgs 1984
st_crs(mphc_2018_sf) <- 4326

# Project the points to the EA spatial reference
mphc_2018_sf <- st_transform(mphc_2018_sf, crs = st_crs(ea))

# Assign each point to it nearest EA it is located within
nearest_indices <- st_nearest_feature(mphc_2018_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
mphc_2018_sf$EA_CODE <- nearest_ids

# Write to file
st_write(mphc_2018_sf,
  dsn = file.path(output_path, "mphc_2018_sf.gpkg"),
  driver = "GPKG",
  delete_layer = TRUE
)

# load dataset
# mphc_2018_sf <- st_read(paste0(output_path, "mphc_2018_sf.gpkg"))

# convert to dataframe for easy manipulation
mphc_2018_df <- mphc_2018_sf %>%
  as_tibble()

# check the summary of gps accuracy
summary(mphc_2018_df$hh_gps_accuracy)

# Summarize data base on their spatial location
mphc_summary_with_gps <- mphc_2018_df %>%
  group_by(EA_CODE) %>%
  summarise(
    mphc_total_pop = sum(no_persons, na.rm = T),
    mphc_hh_count = n_distinct(hhnumber), # Distinct count of household
    male_count = sum(p03 == 1, na.rm = TRUE),
    female_count = sum(p03 == 2, na.rm = TRUE)
  )

#######################################################################
# We will rbind the summarized data

# rbind both dataset
mphc_rbind <- rbind(mphc_summary_with_gps, mphc_summary_no_gps)

# Summarize overall data
mphc_rbind <- mphc_rbind %>%
  group_by(EA_CODE) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))

# What is the total pop and hh count
sum(mphc_rbind$mphc_total_pop)
sum(mphc_rbind$mphc_hh_count)

# Check if Male and Female Count Add to the Total Population

sex_sum <- mphc_rbind %>%
  mutate(
    sex_group_sum = rowSums(
      select(., matches("^(male|female)_")),
      na.rm = TRUE
    ),
    sex_sum_matches_total = sex_group_sum == mphc_total_pop
  )

sex_sum %>%
  count(sex_sum_matches_total) # If True then sum of Male/Female matches total pop

# Rename hh_count
mphc_rbind <- mphc_rbind %>%
  rename(
    hh_count_2018 = mphc_hh_count,
    total_pop_2018 = mphc_total_pop,
    male_count_2018 = male_count,
    female_count_2018 = female_count
  )


# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea"
)))

# Clear console
cat("\014")

#####################################################################################
####################################################################################
######### PROCESS ICT DATA ################################################
# Load data
ICT_data <- read_dta(paste0(input_path, "ICT Listing WorldPop.dta"))

# Add a new column to data called hh_count
ICT_data <- ICT_data %>%
  mutate(hh_count = 1)

# summarize gps accuracy of ICT data record
summary(ICT_data$GPS__Accuracy)

# if gps accuracy is greater than 5m summarize data in original EA Code
# If record has no gps summarize data in original EA Code

# Filter records without GPS coordinates and those with gps accuracy above 5m
ict_no_gps <- ICT_data %>%
  filter(
    is.na(GPS__Longitude) |
      is.na(GPS__Latitude) |
      GPS__Accuracy > 5
  )

# summarize hh count
ict_no_gps <- ict_no_gps %>%
  group_by(EA_Number) %>%
  summarise(ict_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

#################################################################
# Next we will summarize records with gps based on spatial location
# And GPS accuracy is less than or equal to 5m

# Convert to an sf point object
ICT_sf <- ICT_data %>%
  filter(GPS__Accuracy <= 5) %>%
  drop_na(GPS__Longitude, GPS__Latitude) %>%
  st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"))

# #set the spatial reference
st_crs(ICT_sf) <- 4326

# transform/project to EA
ICT_sf <- st_transform(ICT_sf, crs = st_crs(ea))

# EA Nearest Neighbor Assignment
nearest_indices <- st_nearest_feature(ICT_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
ICT_sf$EA_CODE <- nearest_ids

# convert data to tibble
ict_with_gps <- ICT_sf %>%
  as_tibble()

# summarize hh count
ict_with_gps <- ict_with_gps %>%
  group_by(EA_Number) %>% # Use orginal EA_Number of data point
  summarise(ict_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

# Rbind both ICT data partitions
ICT_rbind <- rbind(ict_no_gps, ict_with_gps)

# Summarize overall data
ICT_rbind <- ICT_rbind %>%
  group_by(EA_Number) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))

# check whether data adds up to original
sum(ICT_rbind$ict_hh_count)
sum(ICT_data$hh_count)


# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea",
  "ICT_rbind"
)))

# Clear console
cat("\014")

#####################################################################################
####################################################################################
######### PROCESS IHS6 DATA ################################################

# Load data
IHS6_data <- read_dta(paste0(input_path, "IHS6 Listing WorldPop.dta"))

# Add a new column to data called hh_count
IHS6_data <- IHS6_data %>%
  mutate(hh_count = 1)

# summarize gps accuracy of data record
summary(IHS6_data$GPS__Accuracy)

# if gps accuracy is greater than 5m summarize data in original EA Code
# If record has no gps summarize data in original EA Code

# Filter records without GPS coordinates and those with gps accuracy above 5m
IHS_no_gps <- IHS6_data %>%
  filter(
    is.na(GPS__Longitude) |
      is.na(GPS__Latitude) |
      GPS__Accuracy > 5
  )

# summarize hh count
IHS_no_gps <- IHS_no_gps %>%
  group_by(EA_CODE) %>%
  summarise(ihs_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

#################################################################
# Next we will summarize records with gps based on spatial location
# And GPS accuracy is less than or equal to 5m

# Convert to sf object
IHS_sf <- IHS6_data %>%
  filter(GPS__Accuracy <= 5) %>%
  drop_na(GPS__Longitude, GPS__Latitude) %>%
  st_as_sf(coords = c("GPS__Longitude", "GPS__Latitude"))

# #set the spatial reference
st_crs(IHS_sf) <- 4326

# transform
IHS_sf <- st_transform(IHS_sf, crs = st_crs(ea))

# EA Nearest Neighbor Assignment
nearest_indices <- st_nearest_feature(IHS_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
IHS_sf$EA_CODE2 <- nearest_ids

# convert data to tibble
IHS_with_gps <- IHS_sf %>%
  as_tibble()

# summarize hh count
IHS_with_gps <- IHS_with_gps %>%
  group_by(EA_CODE) %>% # Use orginal EA_Number of data point
  summarise(ihs_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

# Rbind both data partitions
IHS_rbind <- rbind(IHS_no_gps, IHS_with_gps)

# Summarize overall data
IHS_rbind <- IHS_rbind %>%
  group_by(EA_CODE) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))

# check whether data adds up to original
sum(IHS_rbind$ihs_hh_count)
sum(IHS6_data$hh_count)

# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea",
  "ICT_rbind", "IHS_rbind"
)))

# Clear console
cat("\014")

#####################################################################################
####################################################################################
######### PROCESS NACA DATA ################################################

# Load dataset
Naca_data <- read_dta(paste0(input_path, "Naca Listing WorldPop.dta"))

# Add a new column to data called hh_count
Naca_data <- Naca_data %>%
  mutate(hh_count = 1)

# summarize gps accuracy of data record
summary(Naca_data$accuracy)

# if gps accuracy is greater than 5m summarize data in original EA Code
# If record has no gps summarize data in original EA Code

# Filter records without GPS coordinates and those with gps accuracy above 5m
Naca_no_gps <- Naca_data %>%
  filter(
    is.na(longitude) |
      is.na(latitude) |
      accuracy > 5
  )

# summarize hh count
Naca_no_gps <- Naca_no_gps %>%
  group_by(EA_Number) %>%
  summarise(naca_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

#################################################################
# Next we will summarize records with gps based on spatial location
# And GPS accuracy is less than or equal to 5m

# Convert to sf object
Naca_sf <- Naca_data %>%
  filter(accuracy <= 5) %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"))

# #set the spatial reference
st_crs(Naca_sf) <- 4326

# transform
Naca_sf <- st_transform(Naca_sf, crs = st_crs(ea))

# EA Nearest Neighbor Assignment
nearest_indices <- st_nearest_feature(Naca_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
Naca_sf$EA_CODE2 <- nearest_ids

# convert data to tibble
Naca_with_gps <- Naca_sf %>%
  as_tibble()

# summarize hh count
Naca_with_gps <- Naca_with_gps %>%
  group_by(EA_Number) %>% # Use orginal EA_Number of data point
  summarise(naca_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

# Rbind Naca data partitions
Naca_rbind <- rbind(Naca_no_gps, Naca_with_gps)

# Summarize overall data
Naca_rbind <- Naca_rbind %>%
  group_by(EA_Number) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))

# check whether data adds up to original
sum(Naca_rbind$naca_hh_count)
sum(Naca_data$hh_count)

# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea",
  "ICT_rbind", "IHS_rbind", "Naca_rbind"
)))

# Clear console
cat("\014")

#####################################################################################
####################################################################################
######### PROCESS DHS Listing DATA ################################################

# Load dhs data

dhs_listing <- read_dta(paste0(input_path, "FINAL MDHS LISTING DATA_Annon.dta"))
dhs_file <- read.csv(paste0(input_path, "DHS_Segmented_File.csv"))

# Not segmented clusters
unique(dhs_file$Cluster.Segmented)

# Get non-segmented cluster
non_seg_cluster <- dhs_file %>%
  filter(grepl("^no\\b", Cluster.Segmented, ignore.case = TRUE))

# Unique cluster id
unique(non_seg_cluster$DHScluster)

# Clusters in non_seg_cluster and not present in dhs listing
missing_clusters <- setdiff(unique(non_seg_cluster$DHScluster), unique(dhs_listing$QHCLUST))
missing_clusters

# check if there are duplicate in household head name
any(duplicated(dhs_listing$lname)) # If false it means individual record of hh

# Add a new column to data called hh_count
dhs_listing <- dhs_listing %>%
  mutate(hh_count = 1)

# Subset dhs_listing using the DHScluster IDs in non_seg_cluster
dhs_listing <- dhs_listing %>%
  filter(QHCLUST %in% unique(non_seg_cluster$DHScluster))

# Summarize total number of hhold per dhs cluster
dhs_hh_summary <- dhs_listing %>%
  group_by(QHCLUST) %>%
  summarise(dhs_hh_count = sum(hh_count, na.rm = T))

## Get the centroid of the cluster
dhs_centroids <- dhs_listing %>%
  group_by(QHCLUST) %>%
  summarise(
    llongitude = mean(llongitude, na.rm = TRUE),
    llatitude = mean(llatitude, na.rm = TRUE)
  ) %>%
  ungroup()

# Join dhs_hh_summary to dhs centroid
dhs_centroids <- dhs_centroids %>%
  left_join(dhs_hh_summary, by = "QHCLUST")

# Convert to sf object
dhs_centroids_sf <- dhs_centroids |>
  drop_na(llongitude, llatitude) |>
  st_as_sf(coords = c("llongitude", "llatitude"))

# #set the spatial reference
st_crs(dhs_centroids_sf) <- 4326

# Write to GPKG file
# st_write(dhs_centroids_sf,
# dsn = file.path(output_path, "dhs_centroids_sf.gpkg"),
# driver = "GPKG",
# delete_layer = TRUE)

# transform
dhs_centroids_sf <- st_transform(dhs_centroids_sf, crs = st_crs(ea))

# Calculate nearest neighbor distance from dhs_centroids to ea shapefile
nearest <- st_nn(dhs_centroids_sf, ea, k = 1, returnDist = TRUE)

# Extract distances (in meters for projected CRS)
distances <- sapply(nearest$dist, function(x) x[1])

# Add distance and within_5km columns to dhs_centroids_sf
dhs_centroids_sf <- dhs_centroids_sf %>%
  mutate(
    nearest_dist_m = distances,
    within_5km = ifelse(nearest_dist_m < 5000, 1, 2)
  )

# drop point more than 5km
dhs_centroids_sf <- dhs_centroids_sf %>%
  filter(within_5km == 1)

# EA Nearest Neighbor Assignment
nearest_indices <- st_nearest_feature(dhs_centroids_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
dhs_centroids_sf$EA_CODE <- nearest_ids

# convert data to tibble and summarize base on EA
dhs_hh_count <- dhs_centroids_sf %>%
  as_tibble() %>%
  group_by(EA_CODE) %>%
  summarise(dhs_hh_count = sum(dhs_hh_count, na.rm = T))


# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea",
  "ICT_rbind", "IHS_rbind", "Naca_rbind",
  "dhs_hh_count"
)))

# Clear console
cat("\014")

#####################################################################################
####################################################################################
######### PROCESS ZOMBA DISTRICT DATA ################################################

# Load all Excel files in Zomba folder
files <- list.files(
  path = paste0(input_path, "zomba_csv"),
  pattern = "\\.csv$",
  full.names = TRUE
)

# Check files
files

# Read and combine all files
zomba_data <- files %>%
  map_dfr(~ {
    read_csv(.x, show_col_types = FALSE) %>%
      select(
        -any_of(c("HOUSEHOLD NUMBER", "HOUSEHOLD.NUMBER", "registration_date")) # Remove variables
      )
  })

# Add a new column to data called hh_count
zomba_data <- zomba_data %>%
  mutate(hh_count = 1)

# Convert to sf object
zomba_sf <- zomba_data %>%
  drop_na(gps_longitude, gps_latitude) %>%
  st_as_sf(coords = c("gps_longitude", "gps_latitude"))

# #set the spatial reference
st_crs(zomba_sf) <- 4326

# transform
zomba_sf <- st_transform(zomba_sf, crs = st_crs(ea))

# EA Nearest Neighbor Assignment
nearest_indices <- st_nearest_feature(zomba_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
zomba_sf$EA_CODE <- nearest_ids

# Write to file
# st_write(zomba_sf ,
# dsn = file.path(output_path, "zomba_point.gpkg"),
# driver = "GPKG",
# delete_layer = TRUE)

# convert data to tibble
zomba_tibble <- zomba_sf %>%
  as_tibble()

# Summarize data
zomba_tibble <- zomba_tibble %>%
  group_by(EA_CODE) %>%
  summarise(zomba_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()

# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea",
  "ICT_rbind", "IHS_rbind", "Naca_rbind",
  "dhs_hh_count", "zomba_tibble"
)))

# Clear console
cat("\014")

#####################################################################################
####################################################################################
######### PROCESS MALEMA DISTRICT DATA ################################################

# Load data
malemia_data <- read.csv(paste0(input_path, "malemia_hh_without_IDs.csv"))

# Add a new column to data called hh_count
malemia_data <- malemia_data %>%
  mutate(hh_count = 1)

# Convert to sf object
malemia_sf <- malemia_data %>%
  drop_na(hh_longitude, hh_latitude) %>%
  st_as_sf(coords = c("hh_longitude", "hh_latitude"))

# #set the spatial reference
st_crs(malemia_sf) <- 4326

# transform
malemia_sf <- st_transform(malemia_sf, crs = st_crs(ea))

# EA Nearest Neighbor Assignment
nearest_indices <- st_nearest_feature(malemia_sf, ea)

# Extract the EA_CODE  of the nearest polygons
nearest_ids <- ea$EA_CODE[nearest_indices]

# Add the EA_CODE to data
malemia_sf$EA_CODE <- nearest_ids

# Write to file
# st_write(malemia_sf ,
# dsn = file.path(output_path, "malemia_point.gpkg"),
# driver = "GPKG",
# delete_layer = TRUE)

# convert data to tibble
malemia_tibble <- malemia_sf %>%
  as_tibble()

# Summarize data
malemia_tibble <- malemia_tibble %>%
  group_by(EA_CODE) %>%
  summarise(malemia_hh_count = sum(hh_count, na.rm = T)) %>%
  ungroup()


# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "mphc_rbind", "ea",
  "ICT_rbind", "IHS_rbind", "Naca_rbind",
  "dhs_hh_count", "zomba_tibble", "malemia_tibble"
)))

# Clear console
cat("\014")

################################################################################
###############################################################################
###############################################################################
############ SUMMARIZE 2026 DATA ##############################################
# Load datasets
data_2026 <- read_excel(paste0(input_path, "CensusMapping_data_04082026.xlsx"), sheet = 1) # or sheet = "Sheet name"

# Summarize data

summary_2026 <- data_2026 %>%
  mutate(hh = 1) %>%
  group_by(ea_code) %>%
  summarise(
    total_pop_2026 = sum(hh_size, na.rm = T),
    hh_count_2026 = sum(hh, na.rm = T),
    male_count_2026 = sum(hh_males, na.rm = TRUE),
    female_count_2026 = sum(hh_females, na.rm = TRUE)
  ) %>%
  ungroup()

sum(summary_2026$total_pop_2026)

# Check if Male and Female Count Add to the Total Population

sex_sum <- summary_2026 %>%
  mutate(
    sex_group_sum = rowSums(
      select(., matches("^(male|female)_")),
      na.rm = TRUE
    ),
    sex_sum_matches_total = sex_group_sum == total_pop_2026
  )

sex_sum %>%
  count(sex_sum_matches_total)

############################################################################
###########################################################################
###########################################################################
# After summarizing each data at the EA, we have to join them based on EA code

# Combine Data ------------------------------------------------------------

combined_data <- mphc_rbind %>%
  left_join(summary_2026, by = c("EA_CODE" = "ea_code")) %>%
  left_join(ICT_rbind, by = c("EA_CODE" = "EA_Number")) %>%
  left_join(IHS_rbind, by = "EA_CODE") %>%
  left_join(Naca_rbind, by = c("EA_CODE" = "EA_Number")) %>%
  left_join(dhs_hh_count, by = "EA_CODE") %>%
  left_join(zomba_tibble, by = "EA_CODE") %>%
  left_join(malemia_tibble, by = "EA_CODE")

# create hh_count for 2024 based on priority conditions
combined_data <- combined_data %>%
  mutate(
    hh_count_2024 = case_when(
      # if malemia_tibble is available, use it (highest priority)
      !is.na(malemia_hh_count) ~ malemia_hh_count,
      # if dhs_hh_count is available, use it (highest priority)
      !is.na(dhs_hh_count) ~ dhs_hh_count,
      # if ihs_hh_count is available, use it (second priority)
      !is.na(ihs_hh_count) ~ ihs_hh_count,
      # else if naca_hh_count is available, use it (third priority)
      !is.na(naca_hh_count) ~ naca_hh_count,
      # else if ict_hh_count is available, use it (4th priority)
      !is.na(ict_hh_count) ~ ict_hh_count,
      # else if zomba_hh_count is available, use it (last priority)
      !is.na(zomba_hh_count) ~ zomba_hh_count,
      # else put NA
      TRUE ~ NA_real_
    )
  )

# Arrange data in proper order
combined_data <- combined_data %>%
  select(
    EA_CODE, total_pop_2018, total_pop_2026, hh_count_2018, hh_count_2024,
    hh_count_2026, everything()
  )

# Write to file
write.csv(combined_data, paste0(output_path, "summarized_survey_data.csv"), row.names = F)

#################### END OF SCRIPT #########################################
###########################################################################
