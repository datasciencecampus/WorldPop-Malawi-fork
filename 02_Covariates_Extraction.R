# Extracting Covariates

# Packages
library(tidyverse)
library(sf)
library(tictoc)
library(terra)
library(exactextractr)

# Specify Drive Path
drive_path <- "//Internal_path/"
input_path <- paste0(drive_path, "Output_Data/")
shapefile_path <- paste0(drive_path, "Data/Shapefiles/")
covs_path_2018 <- paste0(drive_path, "Data/Covariates/Covariates_2018/")
bcount_path_2018 <- paste0(drive_path, "Data/Covariates/Buildings_2018/")
output_path <- paste0(drive_path, "Output_Data/")


# Load dataset ------------------------------------------------------------
ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use_Corrected.shp"))
pop_data <- read.csv(file.path(input_path, "summarized_survey_data.csv"))
r1 <- rast(file.path(bcount_path_2018, "MOS_MLW_buildings_count_BCB_gl_100m_v1_1.tif"))

# check names of variables in ea shapefile
names(ea)

# Create a Pseudo Unique ID for all the EAs in the country
ea <- ea %>%
  mutate(
    cluster_id = paste0("EA", sprintf("%06d", row_number()))
  )

# Fix corrupt geometries
st_make_valid(ea)

# Turn of geometric plane
sf::sf_use_s2(FALSE)

# Project EA to the raster spatial reference
ea <- st_transform(ea, crs = st_crs(r1))

#################################################################################################
#################################################################################################
############ Extract 2018 Data #################################################################
################################################################################################

# First we will extract building count for 2018 by doing a zonal statistics operation
# We will sum all the pixels located within a given EA to get total count of buildings

# Get a list of all building count rasters
bcount_rasters_list <- list.files(path = bcount_path_2018, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
bcount_rasters_list


# Stack all rasters
bcount_2018 <- rast(paste0(bcount_path_2018, c(bcount_rasters_list)))

# Extract rasters by summing all pixel values
tic()

bcount_2018_extract <- exactextractr::exact_extract(bcount_2018, ea, fun = "sum")

toc()

# Rename variables
bcount_2018_extract <- bcount_2018_extract %>%
  rename(
    google_v2_5 = sum.buildings_count_2018_glv2_5_t0_5_C_100m_v1,
    google_BCB = sum.buildings_count_BCB_gl_100m_v1_1,
    microsoft_BCB = sum.buildings_count_BCB_ms_100m_v1_1,
    PIB_total_area_google = sum.MOS_MLW_buildings_total_area_PIB_gl_100m_v1_1
  )

# Extract 2018 covariates -------------------------------------------------
# Next we will extract the geospatial covariates for 2018
# We will perform zonal statistics by taking the mean values

# 2018 - Rasters
# Get a list of rasters
raster_list <- list.files(path = covs_path_2018, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
raster_list

# Stack all covariates
raster_2018_covariates <- rast(paste0(covs_path_2018, c(raster_list)))

# Extract rasters using their mean values
tic()

raster_2018_extract <- exactextractr::exact_extract(raster_2018_covariates, ea, fun = "mean")

toc()

# Extract variable names
var_names <- names(raster_2018_extract)

# Change names
colnames(raster_2018_extract) <- c(paste0("x", 1:63))

# Extract names of raster
var_names2 <- names(raster_2018_extract)

# cbind names
var_names <- cbind(var_names, var_names2) %>%
  as_tibble()

# Export names
write.csv(var_names, paste0(output_path, "var_names_2018.csv"))

###############################################################################
############################################################################

# Next we will get the centroid of each EA

# Get Centroid of EA as Lat Long ------------------------------------------
# Extract the centroid of the polygon

centroid <- st_point_on_surface(ea) # centroid is inside

# Extract the latitude and longitude of the centroid
lat_long <- st_coordinates(centroid)

# Rename XY coord as lat long
lat_long <- lat_long %>%
  as_tibble() %>%
  rename(lat = Y, long = X)


# Cbind raster_extract to ea
ea_2018 <- ea %>%
  cbind(bcount_2018_extract, raster_2018_extract, lat_long) %>%
  as_tibble()

# convert pop data to character
pop_data <- pop_data %>%
  mutate(EA_CODE = as.character(EA_CODE))

# join pop_data to ea data
ea_2018 <- ea_2018 %>%
  left_join(pop_data, by = "EA_CODE")

# Arrange data in order
ea_2018 <- ea_2018 %>%
  select(-starts_with(c("x", "geometry")), starts_with("x"))

# Export to file
write.csv(ea_2018, paste0(output_path, "Malawi_2018_data.csv"), row.names = F)

################## END OF COVARIATES EXTRACTION FOR 2018 ######################
###############################################################################
###############################################################################

# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "ea", "pop_data"
)))

# Clear console
cat("\014")


#################################################################################################
#################################################################################################
############ Extract 2024 Data #################################################################
################################################################################################

# Specify data location
covs_path_2024 <- paste0(drive_path, "Data/Covariates/Covariates_2024/")
bcount_path_2024 <- paste0(drive_path, "Data/Covariates/Buildings_2024/")

# Extract Building Count --------------------------------------------------

# 2024
# Get a list of all building count rasters
bcount_rasters_list <- list.files(path = bcount_path_2024, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
bcount_rasters_list


# Stack all rasters
bcount_2024 <- rast(paste0(bcount_path_2024, c(bcount_rasters_list)))

# Extract rasters using their sum values
tic()

bcount_2024_extract <- exactextractr::exact_extract(bcount_2024, ea, fun = "sum")

toc()

# Rename variables
bcount_2024_extract <- bcount_2024_extract %>%
  rename(
    google_v2_5 = sum.buildings_count_2023_glv2_5_t0_5_C_100m_v1,
    google_BCB = sum.buildings_count_BCB_gl_100m_v1_1,
    microsoft_BCB = sum.buildings_count_BCB_ms_100m_v1_1,
    PIB_total_area_google = sum.MOS_MLW_buildings_total_area_PIB_gl_100m_v1_1
  )


#####################################################################################
######################################################################################
# Extract 2024 covariates -------------------------------------------------
# Next we will extract the geospatial covariates for 2024
# We will perform zonal statistics by taking the mean values

raster_list <- list.files(path = covs_path_2024, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
raster_list

# Stack all covariates
raster_2024_covariates <- rast(paste0(covs_path_2024, c(raster_list)))

# Extract rasters using their mean values
tic()

raster_2024_extract <- exactextractr::exact_extract(raster_2024_covariates, ea, fun = "mean")

toc()

# Extract variable names
var_names <- names(raster_2024_extract)

# Change names
colnames(raster_2024_extract) <- c(paste0("x", 1:63))

# Extract names of raster
var_names2 <- names(raster_2024_extract)

# cbind names
var_names <- cbind(var_names, var_names2) %>%
  as_tibble()

# Export names
write.csv(var_names, paste0(output_path, "var_names_2024.csv"))

###############################################################################
############################################################################

# Get Centroid of EA as Lat Long ------------------------------------------
# Extract the centroid of the polygon

centroid <- st_point_on_surface(ea) # centroid is inside

# Extract the latitude and longitude of the centroid
lat_long <- st_coordinates(centroid)

# Rename XY coord as lat long
lat_long <- lat_long %>%
  as_tibble() %>%
  rename(lat = Y, long = X)


# Cbind raster_extract to ea
ea_2024 <- ea %>%
  cbind(bcount_2024_extract, raster_2024_extract, lat_long) %>%
  as_tibble()

# convert pop data to character
pop_data <- pop_data %>%
  mutate(EA_CODE = as.character(EA_CODE))

# join pop_data to ea data
ea_2024 <- ea_2024 %>%
  left_join(pop_data, by = "EA_CODE")

# Arrange data in order
ea_2024 <- ea_2024 %>%
  select(-starts_with(c("x", "geometry")), starts_with("x"))

# Export to file
write.csv(ea_2024, paste0(output_path, "Malawi_2024_data.csv"), row.names = F)

################## END OF COVARIATES EXTRACTION FOR 2024 ######################
###############################################################################
###############################################################################

# Remove all object except the ones listed
rm(list = setdiff(ls(), c(
  "drive_path", "input_path", "output_path",
  "shapefile_path", "ea", "pop_data"
)))

# Clear console
cat("\014")

#################################################################################################
#################################################################################################
############ Extract 2026 Data #################################################################
################################################################################################

# Specify data location
covs_path_2026 <- paste0(drive_path, "Data/Covariates/Covariates_2026/")
bcount_path_2026 <- paste0(drive_path, "Data/Covariates/Buildings_2026/")

# 2026
bcount_rasters_list <- list.files(path = bcount_path_2026, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
bcount_rasters_list

# Stack all rasters
bcount_2026 <- rast(paste0(bcount_path_2026, c(bcount_rasters_list)))

# Extract rasters using their sum values
tic()

bcount_2026_extract <- exactextractr::exact_extract(bcount_2026, ea, fun = "sum")

toc()

# Rename variables
bcount_2026_extract <- bcount_2026_extract %>%
  rename(
    google_v2_5 = sum.mwi_buildings_count_2023_glv2_5_t0_5_C_100m_v1,
    google_BCB = sum.mwi_buildings_count_BCB_gl_100m_v1_1,
    microsoft_BCB = sum.mwi_buildings_count_BCB_ms_100m_v1_1,
    PIB_total_area_google_2023 = sum.mwi_buildings_total_area_PIB_gl_100m_v1_1
  )


# Extract 2026 covariates -------------------------------------------------

# 2026 - Rasters

raster_list <- list.files(path = covs_path_2026, pattern = ".tif$", all.files = TRUE, full.names = FALSE)
raster_list

# Stack all covariates
raster_2026_covariates <- rast(paste0(covs_path_2026, c(raster_list)))

# Extract rasters using their mean values
tic()

raster_2026_extract <- exactextractr::exact_extract(raster_2026_covariates, ea, fun = "mean")

toc()

# Extract variable names
var_names <- names(raster_2026_extract)

# Change names
colnames(raster_2026_extract) <- c(paste0("x", 1:63))

# Extract names of raster
var_names2 <- names(raster_2026_extract)

# cbind names
var_names <- cbind(var_names, var_names2) %>%
  as_tibble()

# Export names
write.csv(var_names, paste0(output_path, "var_names_2026.csv"))

###############################################################################
############################################################################

# Get Centroid of EA as Lat Long ------------------------------------------
# Extract the centroid of the polygon

centroid <- st_point_on_surface(ea) # centroid is inside

# Extract the latitude and longitude of the centroid
lat_long <- st_coordinates(centroid)

# Rename XY coord as lat long
lat_long <- lat_long %>%
  as_tibble() %>%
  rename(lat = Y, long = X)


# Cbind raster_extract to ea
ea_2026 <- ea %>%
  cbind(bcount_2026_extract, raster_2026_extract, lat_long) %>%
  as_tibble()

# convert pop data to character
pop_data <- pop_data %>%
  mutate(EA_CODE = as.character(EA_CODE))

# join pop_data to ea data
ea_2026 <- ea_2026 %>%
  left_join(pop_data, by = "EA_CODE")

# Arrange data in order
ea_2026 <- ea_2026 %>%
  select(-starts_with(c("x", "geometry")), starts_with("x"))

# Export to file
write.csv(ea_2026, paste0(output_path, "Malawi_2026_data.csv"), row.names = F)

###################################################################################################
############################### END OF SCRIPT ######################################################
####################################################################################################
####################################################################################################
