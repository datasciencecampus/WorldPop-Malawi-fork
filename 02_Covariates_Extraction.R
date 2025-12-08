
# Extracting Covariates at the EA-Level

#Packages
library(tidyverse)
library(sf)
library(tictoc)
library(terra)
library(exactextractr)

#Specify Drive Path
drive_path <- "//Working/MALAWI/Ortis/"
input_path <- paste0(drive_path, "Output_Data/")
shapefile_path <- paste0(drive_path, "Input_Data/Shapefiles/")
covs_path_2018 <- paste0(drive_path, "Input_Data/Mosaic_Covariates_2018/")
covs_path_2024 <- paste0(drive_path, "Input_Data/Mosaic_Covariates_2024/")
bcount_path_2018 <- paste0(drive_path, "Input_Data/Mosaic_Buildings_2018/")
bcount_path_2024 <- paste0(drive_path, "Input_Data/Mosaic_Buildings_2024/")
output_path <- paste0(drive_path, "Output_Data/")


# Load dataset ------------------------------------------------------------
ea <- st_read(file.path(shapefile_path, "EA_Shapefile.shp"))
pop_data <- read.csv(file.path (input_path, "summarized_survey_data.csv"))
r1 <- rast(file.path(bcount_path_2024, "MOS_MLW_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))

#Fix corrupt geometries
st_make_valid(ea)

#Turn off invalid geometries
sf::sf_use_s2(FALSE)

ea <- st_transform(ea, crs = st_crs(r1))

################################################################################
################################################################################

# Extract Building Count --------------------------------------------------

#2024
bcount_rasters_list <- list.files(path=bcount_path_2024, pattern= ".tif$", all.files=TRUE, full.names=FALSE)
bcount_rasters_list


#Stack all rasters
bcount_2024 <- rast(paste0(bcount_path_2024, c(bcount_rasters_list)))

#Extract rasters using their sum values
tic()

bcount_2024_extract <- exactextractr::exact_extract(bcount_2024, ea, fun = 'sum')

toc()

#Rename variables
bcount_2024_extract <- bcount_2024_extract |> 
  rename(google_v2_5 = sum.buildings_count_2023_glv2_5_t0_5_C_100m_v1,
         google_BCB = sum.buildings_count_BCB_gl_100m_v1_1,
         google_PIB = sum.buildings_count_PIB_gl_100m_v1_1,
         microsoft_BCB = sum.buildings_count_BCB_ms_100m_v1_1,
         microsoft_PIB = sum.buildings_count_PIB_ms_100m_v1_1)

#Extract non residential bcount
non_res <- rast(paste0(input_path, "non_res_raster.tif"))

non_res_extract <- exactextractr::exact_extract(non_res, ea, fun = 'sum')

#convert to tibille
non_res_extract <- non_res_extract |> 
  as_tibble() |> 
  rename(non_res_bcount = value)


#####################################################################################
######################################################################################
# Extract Continuous Rasters -------------------------------------------------------

#2024 - Rasters

#specify pattern for file names
#pattern = "DRC.*\\.tif$"

raster_list <-list.files(path=covs_path_2024, pattern= ".tif$", all.files=TRUE, full.names=FALSE)
raster_list

#Stack all covariates 
raster_2024_covariates <- rast(paste0(covs_path_2024, c(raster_list)))

#Extract rasters using their mean values
tic()

raster_2024_extract <- exactextractr::exact_extract(raster_2024_covariates, ea, fun = 'mean')

toc()

#Extract variable names
var_names <- names(raster_2024_extract)

#Change names
colnames(raster_2024_extract) <- c(paste0('x', 1:64))

#Extract names of raster
var_names2<- names(raster_2024_extract)

#cbind names
var_names <- cbind(var_names, var_names2) %>% 
  as_tibble()

#Export names
write.csv(var_names, paste0(output_path, "var_names_2024.csv"))

###############################################################################
############################################################################

# Get Centroid of EA as Lat Long ------------------------------------------
# Extract the centroid of the polygon

centroid <- st_point_on_surface(ea)   #centroid is inside

# Extract the latitude and longitude of the centroid
lat_long <- st_coordinates(centroid) 

#Rename XY coord as lat long
lat_long <- lat_long %>% 
  as_tibble() %>% 
  rename(lat = Y, long = X)


#Cbind raster_extract to ea
ea_2024 <- ea %>% 
  cbind(bcount_2024_extract, non_res_extract, raster_2024_extract, lat_long) |> 
  as_tibble()

#convert pop data to character
pop_data <- pop_data |> 
  mutate(EA_CODE = as.character(EA_CODE))

#join pop_data to ea data
ea_2024 <- ea_2024 |> 
  left_join(pop_data, by = "EA_CODE")

#Arrange data in order
ea_2024 <- ea_2024 %>%
  select(-starts_with(c("x", "geometry")), starts_with("x"))

#Export to file
write.csv(ea_2024, paste0(output_path, "Malawi_2024_data.csv"), row.names = F)

#################################################################################################
#################################################################################################
############ Extract 2018 Data #################################################################
################################################################################################

#2018
bcount_rasters_list <- list.files(path=bcount_path_2018, pattern= ".tif$", all.files=TRUE, full.names=FALSE)
bcount_rasters_list


#Stack all rasters
bcount_2018 <- rast(paste0(bcount_path_2018, c(bcount_rasters_list)))

#Extract rasters using their sum values
tic()

bcount_2018_extract <- exactextractr::exact_extract(bcount_2018, ea, fun = 'sum')

toc()

#Rename variables
bcount_2018_extract <- bcount_2018_extract |> 
rename(google_v2_5 = sum.buildings_count_2018_glv2_5_t0_5_C_100m_v1,
  google_BCB = sum.buildings_count_BCB_gl_100m_v1_1,
 microsoft_BCB = sum.buildings_count_BCB_ms_100m_v1_1)

#Extract non residential bcount
non_res <- rast(paste0(input_path, "non_res_raster.tif"))

non_res_extract <- exactextractr::exact_extract(non_res, ea, fun = 'sum')

#convert to tibille
non_res_extract <- non_res_extract |> 
  as_tibble() |> 
  rename(non_res_bcount = value)

# Extract 2018 covariates -------------------------------------------------

#2018 - Rasters

raster_list <-list.files(path=covs_path_2018, pattern= ".tif$", all.files=TRUE, full.names=FALSE)
raster_list

#Stack all covariates 
raster_2018_covariates <- rast(paste0(covs_path_2018, c(raster_list)))

#Extract rasters using their mean values
tic()

raster_2018_extract <- exactextractr::exact_extract(raster_2018_covariates, ea, fun = 'mean')

toc()

#Extract variable names
var_names <- names(raster_2018_extract)

#Change names
colnames(raster_2018_extract) <- c(paste0('x', 1:64))

#Extract names of raster
var_names2<- names(raster_2018_extract)

#cbind names
var_names <- cbind(var_names, var_names2) %>% 
  as_tibble()

#Export names
write.csv(var_names, paste0(output_path, "var_names_2018.csv"))

###############################################################################
############################################################################

# Get Centroid of EA as Lat Long ------------------------------------------
# Extract the centroid of the polygon

centroid <- st_point_on_surface(ea)   #centroid is inside

# Extract the latitude and longitude of the centroid
lat_long <- st_coordinates(centroid) 

#Rename XY coord as lat long
lat_long <- lat_long %>% 
  as_tibble() %>% 
  rename(lat = Y, long = X)


#Cbind raster_extract to ea
ea_2018 <- ea %>% 
  cbind(bcount_2018_extract, non_res_extract, raster_2018_extract, lat_long) |> 
  as_tibble()

#convert pop data to character
pop_data <- pop_data |> 
  mutate(EA_CODE = as.character(EA_CODE))

#join pop_data to ea data
ea_2018 <- ea_2018 |> 
  left_join(pop_data, by = "EA_CODE")

#Arrange data in order
ea_2018 <- ea_2018 %>%
  select(-starts_with(c("x", "geometry")), starts_with("x"))

#Export to file
write.csv(ea_2018, paste0(output_path, "Malawi_2018_data.csv"), row.names = F)

############################### END OF SCRIPT ######################################################
####################################################################################################
####################################################################################################
