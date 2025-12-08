
library(tidyverse)
library(terra)
library(tictoc)
library(feather)
library(sf)

#Specify Drive Path
drive_path <- "//Working/MALAWI/Ortis/"
covs_path_2024 <- paste0(drive_path, "Input_Data/Mosaic_Covariates_2024/")
output_path <- paste0(drive_path, "Output_Data/")
shapefile_path <- paste0(drive_path, "Input_Data/Shapefiles/")
bcount_path_2024 <- paste0(drive_path, "Input_Data/Mosaic_Buildings_2024/")


#load rasters
r1 <- rast(file.path(bcount_path_2024, "MOS_MLW_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))
rural_urban <- rast(file.path(output_path, "rural_urban_raster.tif"))
country <- rast(file.path(output_path, "country_raster.tif"))
district <- rast(file.path(output_path, "district_raster.tif"))
hh_size <- rast(file.path(output_path, "hh_raster.tif"))
non_res <- rast(paste0(output_path, "non_res_raster.tif"))

#stack rasters
stack_rasters <- c(r1, rural_urban, country, district, hh_size, non_res)

#Get values
stack_values <- terra::values(stack_rasters, dataframe = T)

#head
head(stack_values)

stack_values <- stack_values |>  
  drop_na(buildings_count_2023_glv2_5_t0_5_C_100m_v1)

summary(stack_values$buildings_count_2023_glv2_5_t0_5_C_100m_v1)

rm(rural_urban, country, district, non_res); gc()

# Stack Covariates Rasters -----------------------------------------------------------

#Load rasters and stack them in batches

r1 <- terra::values(r1, dataframe = TRUE)

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
  covs_raster_values <- covs_raster_values |>  
    cbind(r1) |>  
    filter(!is.na(buildings_count_2023_glv2_5_t0_5_C_100m_v1)) |>  
    select(-buildings_count_2023_glv2_5_t0_5_C_100m_v1)
  
  # Write processed covariate values to a feather file
  feather_output_path <- paste0(output_path, "Processed_Covariates_", i, "_to_", min(i + batch_size - 1, length(process_rasters_list)), ".feather")
  feather::write_feather(covs_raster_values, feather_output_path)
  
  # Free up memory
  rm(covs_raster, covs_raster_values); gc()
}

toc()


#Read all files back to memory and cbind them
tic()

myfiles <-dir(output_path,pattern="*.feather")
myfiles

raster_values <- myfiles |>  
  map(function(x) read_feather(file.path(output_path, x))) |>  
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
r1 <- rast(file.path(bcount_path_2024, "MOS_MLW_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))

#Get values
bcount_values <- terra::values(r1, dataframe = TRUE)


# Get the xy coordinate of the centroid of each pixel as a dataframe
coord <- xyFromCell(r1, 1:ncell(r1))

#cbind coordinates to stack_values
stack_coord <- cbind(bcount_values, coord)

rm(bcount_values, coord); gc()

#filter out unsettled pixels
stack_coord <- stack_coord |>  
  filter(!is.na(buildings_count_2023_glv2_5_t0_5_C_100m_v1)) |>  
  select(-buildings_count_2023_glv2_5_t0_5_C_100m_v1)

#Cbind covs to other data
prediction_covs <- cbind(stack_values, stack_coord, raster_values)

#drop NA in country
prediction_covs <- prediction_covs |> 
  drop_na(country_id)

#Read EA shapefiles and join to data

ea <- st_read(file.path(shapefile_path, "EA_Shapefile.shp"))
  
#create unique id for each district
district <- ea |> 
  as_tibble() |> 
  group_by(DIST_NAME) |>
  mutate(dist_id = cur_group_id()) |>
  ungroup() |> 
  select(dist_id, DIST_NAME) |> 
  distinct()

#Create id for rural urban
rural_urban <- ea |> 
  as_tibble() |> 
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1)) |> 
  select(rural_urban_id, ADM_STATUS) |> 
  distinct()


#Join names to prediction covariates stack
prediction_covs1 <- prediction_covs |>  
  left_join(district, by = "dist_id") |> 
  left_join(rural_urban, by = "rural_urban_id")


#select important variables
prediction_covs1 <- prediction_covs1 |>  
  rename(long = x, lat = y,
         bcount = buildings_count_2023_glv2_5_t0_5_C_100m_v1) |>  
  select(country_id,DIST_NAME, dist_id, hh_size, ADM_STATUS, rural_urban_id,
         bcount, non_res_bcount, long, lat, starts_with("x"))  
 

#Export data to file
write_feather(prediction_covs1, paste0(output_path, "Malawi_covs_stack.feather"))

################################# END SCRIPT ################################################
##########################################################################################
