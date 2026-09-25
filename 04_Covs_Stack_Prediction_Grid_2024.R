
#Load packages

library(tidyverse)
library(terra)
library(tictoc)
library(feather)
library(sf)

#Specify Drive Path
drive_path <- "C:/Users/baizaa/Office for National Statistics/H_drive_backup/CDS-AI projects/MalawiWorlPop census/September 26 workshop/Workshop_Script/"
covs_path_2024 <- paste0(drive_path, "Data/Covariates/Covariates_2024/")
output_path <- paste0(drive_path, "Output_Data/")
input_path <- paste0(drive_path, "Output_Data/")
shapefile_path <-  paste0(drive_path, "Data/Shapefiles/")
bcount_path_2024 <- paste0(drive_path, "Data/Covariates/Buildings_2024/")

#load rasters
google_v2_5 <- rast(paste0(bcount_path_2024, "MOS_MLW_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))
rural_urban <- rast(file.path(input_path, "rural_urban_raster.tif"))
country <- rast(file.path(input_path, "country_raster.tif"))
ea <- rast(paste0(input_path, "ea_raster.tif"))
district <- rast(paste0(input_path, "district_raster.tif"))
region <- rast(paste0(input_path, "region_raster.tif"))
microsoft <- rast(paste0(bcount_path_2024, "MOS_MLW_buildings_count_BCB_ms_100m_v1_1.tif"))
google_BCB <- rast(paste0(bcount_path_2024, "MOS_MLW_buildings_count_BCB_gl_100m_v1_1.tif"))
PIB_total_area_google <- rast(paste0(bcount_path_2024, "MOS_MLW_buildings_total_area_PIB_gl_100m_v1_1.tif"))

#Put rasters into a list
bcount_list <- list(
  country_id = country,
  ea_id = ea,
  rural_urban_id = rural_urban,
  dist_id = district, 
  REG_CODE = region,
  microsoft = microsoft,
  google_BCB = google_BCB,
  PIB_total_area_google = PIB_total_area_google
)
#remove raster files from memory
rm(microsoft, ea, district, region, rural_urban,
   google_BCB, PIB_total_area_google, country); gc()

#Get bcount values for google_v2_5
google_v2_5 <- terra::values(google_v2_5, dataframe = TRUE)

#check names of dataframe
names(google_v2_5)

# Define batch size
batch_size <- 4

tic()

# Loop through covariates in batches
for (i in seq(1, length(bcount_list), batch_size)) {
  batch_covs <- bcount_list[i:min(i + batch_size - 1, length(bcount_list))]
  
  # Load batch of covariate rasters
  bcount_raster <- rast(batch_covs)
  
  # Get raster values
  covs_raster_values <- terra::values(bcount_raster, dataframe = TRUE)
  
  #Write only settled pixels to file
  covs_raster_values <- covs_raster_values %>%  
    cbind(google_v2_5) %>%  
    filter(!is.na(buildings_count_2023_glv2_5_t0_5_C_100m_v1)) %>%  
    dplyr::select(-buildings_count_2023_glv2_5_t0_5_C_100m_v1)
  
  # Write processed covariate values to a feather file
  feather_output_path <- paste0(output_path, "Processed_bcount_2024_", i, "_to_", min(i + batch_size - 1, length(bcount_list)), ".feather")
  feather::write_feather(covs_raster_values, feather_output_path)
  
  # Free up memory
  rm(bcount_raster, covs_raster_values); gc()
}

toc()

#remove bcount_list from memory
rm(bcount_list); gc()

#Read all files back to memory and cbind them
tic()

#specify pattern for file names
pattern = "Processed_bcount_2024_.*\\.feather$"


myfiles <-dir(output_path,pattern= pattern)
myfiles

stack_values <- myfiles %>%  
  map(function(x) read_feather(file.path(output_path, x))) %>%  
  reduce(cbind) 

toc()

############################################################################
########### PROCESS COVARIATES ###################################
# Stack Covariates Rasters -----------------------------------------------------------

#Load rasters and stack them in batches

process_rasters_list <- list.files(path = covs_path_2024, pattern = ".tif$", full.names = TRUE)
process_rasters_list

# Define batch size
batch_size <- 10

tic()

# Loop through covariates in batches
for (i in seq(1, length(process_rasters_list), batch_size)) {
  batch_covs <- process_rasters_list[i:min(i + batch_size - 1, length(process_rasters_list))]
  
  # Load batch of covariate rasters
  covs_raster <- rast(batch_covs)
  
  # Get raster values
  covs_raster_values <- terra::values(covs_raster, dataframe = TRUE)
  
  #Write only settled pixels to file
  covs_raster_values <- covs_raster_values %>%  
    cbind(google_v2_5) %>%  
    filter(!is.na(buildings_count_2023_glv2_5_t0_5_C_100m_v1)) %>%  
    dplyr::select(-buildings_count_2023_glv2_5_t0_5_C_100m_v1)
  
  # Write processed covariate values to a feather file
  feather_output_path <- paste0(output_path, "Processed_Covariates_2024_", i, "_to_", min(i + batch_size - 1, length(process_rasters_list)), ".feather")
  feather::write_feather(covs_raster_values, feather_output_path)
  
  # Free up memory
  rm(covs_raster, covs_raster_values); gc()
}

toc()

#Read all files back to memory and cbind them
tic()

#specify pattern for file names
pattern = "Processed_Covariates_2024_.*\\.feather$"

myfiles <-dir(output_path,pattern= pattern)
myfiles

raster_values <- myfiles %>%  
  map(function(x) read_feather(file.path(output_path, x))) %>%  
  reduce(cbind) 

toc()


# Rename variables ----------------------------------------------------

#load variable names
predictor_names <- read.csv(paste0(output_path, "var_names_2024.csv"))

# Remove only the first occurrence of "mean." from the var_names column
predictor_names$var_names <- sub("mean.", "", predictor_names$var_names)

# Create a named vector for renaming
rename_vector <- setNames(predictor_names$var_names2, predictor_names$var_names)

# Rename the columns in raster_values
names(raster_values) <- sapply(names(raster_values), function(name) {
  if (name %in% names(rename_vector)) {
    rename_vector[[name]]
  } else {
    name
  }
})

# Add Building Count and Coordinate ---------------------------------------

#Read raster and get xy values
google_v2_5 <- rast(paste0(bcount_path_2024, "MOS_MLW_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))

#Get values
bcount_values <- terra::values(google_v2_5, dataframe = TRUE)

# Get the xy coordinate of the centroid of each pixel as a dataframe
coord <- xyFromCell(google_v2_5, 1:ncell(google_v2_5))

#cbind coordinates to stack_values
stack_coord <- cbind(bcount_values, coord)

rm(bcount_values, coord); gc()

#filter out unsettled pixels
stack_coord <- stack_coord %>%  
  drop_na(buildings_count_2023_glv2_5_t0_5_C_100m_v1) %>% 
  rename(google_v2_5 = buildings_count_2023_glv2_5_t0_5_C_100m_v1)

#Cbind covs to other data
prediction_covs <- cbind(stack_values, stack_coord, raster_values)

#drop NA in country (These pixels are outside the study extent)
prediction_covs <- prediction_covs %>% 
  drop_na(country_id)

###########################################################################
###########################################################################
## Add Shapefile variables

#Read EA shapefiles and join to data
ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use_Corrected.shp"))

# create unique id for each ea 
#Create a Pseudo Unique ID for all the EAs in the country
ea1 <- ea %>% 
  mutate(cluster_id = paste0("EA", sprintf("%06d", row_number()))) %>% 
  as_tibble() %>% 
  rowid_to_column("ea_id") %>% 
  dplyr::select(EA_CODE, cluster_id, ea_id)

#create unique id for each district
district <- ea %>% 
  as_tibble() %>% 
  group_by(DIST_NAME) %>%
  mutate(dist_id = cur_group_id()) %>%
  ungroup() %>% 
  dplyr::select(dist_id, DIST_NAME) %>% 
  distinct()

#get REG CODE
region <- ea %>% 
  drop_na(REG_NAME) %>% 
  distinct(REG_CODE, REG_NAME)

#Create id for rural urban
rural_urban <- ea %>% 
  as_tibble() %>% 
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1)) %>% 
  dplyr::select(rural_urban_id, ADM_STATUS) %>% 
  distinct()

#Join names to prediction covariates stack
prediction_covs1 <- prediction_covs %>% 
  left_join(ea1, by = "ea_id") %>% 
  left_join(district, by = "dist_id") %>% 
  left_join(region, by = "REG_CODE") %>% 
  left_join(rural_urban, by = "rural_urban_id")


#Rename variables
prediction_covs1 <- prediction_covs1 %>%  
  rename(long = x, lat = y) %>%  
  dplyr::select(EA_CODE, cluster_id, DIST_NAME, ADM_STATUS, REG_NAME, everything())


#Export data to file
write_feather(prediction_covs1, paste0(output_path, "Malawi_covs_stack_2024.feather"))

################################# END SCRIPT ################################################
##########################################################################################