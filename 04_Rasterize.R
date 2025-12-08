#Packages
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(tictoc)
library(raster)


#Specify Drive Path
drive_path <- "//Working/MALAWI/Ortis/"
output_path <- paste0(drive_path, "Output_Data/")
shapefile_path <- paste0(drive_path, "Input_Data/Shapefiles/")
bcount_path_2024 <- paste0(drive_path, "Input_Data/Mosaic_Buildings_2024/")

#Load datasets
ea <- st_read(file.path(shapefile_path, "EA_Shapefile.shp"))
bcount <- rast(file.path(bcount_path_2024, "MOS_MLW_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))
country <- st_read(file.path(shapefile_path, "Country_Shapefile.shp"))
hh_size <- read.csv(paste0(output_path, "summarized_survey_data.csv"))
mphc_structures_2018 <- st_read(paste0(output_path, "mphc_structures_points.gpkg"))
mphc_2018_sf <- st_read(paste0(output_path, "mphc_2018_sf_ea.gpkg"))

#create unique id for each district
ea <- ea |> 
  group_by(DIST_NAME) |>
  mutate(dist_id = cur_group_id()) |>
  ungroup() 

#Create id for rural urban
ea <- ea |> 
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1))

#select hh size
hh_size <- hh_size |> 
  dplyr::select(EA_CODE, mphc_total_pop, hh_size)

#Join to ea data
hh_size <- hh_size |> 
  mutate(EA_CODE = as.character(EA_CODE))  #convert EA code to interger
  
hh_ea <-full_join(ea, hh_size, by = "EA_CODE")

# Rasterize Country ------------------------------------------------------

country <- st_transform(country, crs = st_crs(bcount))

country_raster <- rasterize(country, bcount, field = "Country_ID")
plot(country_raster)

#stack rasters
stack_raster <- c(bcount, country_raster)

#Export raster
writeRaster(country_raster, paste0(output_path, "country_raster.tif"), 
            overwrite = T, names = "country_id" )

# Rasterize Rural Urban ------------------------------------------------------

rural_urban <- st_transform(ea, crs = st_crs(bcount))


rural_urban_raster <- rasterize(rural_urban, bcount, field = "rural_urban_id")
plot(rural_urban_raster)

#stack rasters
stack_raster <- c(bcount, rural_urban_raster)

#Export raster
writeRaster(rural_urban_raster, paste0(output_path, "rural_urban_raster.tif"), 
            overwrite = T, names = "rural_urban_id" )


# Rasterize District ------------------------------------------------------

district <- st_transform(ea, crs = st_crs(bcount))

district_raster<- rasterize(district, bcount, field = "dist_id")
plot(district_raster)

#stack rasters
stack_raster <- c(bcount, district_raster)

#Export raster
writeRaster(district_raster, paste0(output_path, "district_raster.tif"), 
            overwrite = T, names = "dist_id" )


# Rasterize hh Size ------------------------------------------------------

hh_ea <- st_transform(hh_ea, crs = st_crs(bcount))

hh_raster<- rasterize(hh_ea, bcount, field = "hh_size")
plot(hh_raster)

#stack rasters
stack_raster <- c(bcount, hh_raster)

#Export raster
writeRaster(hh_raster, paste0(output_path, "hh_raster.tif"), 
            overwrite = T, names = "hh_size" )


# Rasterize Commercial Buildings ------------------------------------------


#count buildings
mphc_structures_2018 <- mphc_structures_2018 |> 
  mutate(b_count = 1)

#check the use of the structure
sort(unique(mphc_structures_2018$s3_structure_use))

#Filter non accomdation block or hostel
non_res <- mphc_structures_2018 |> 
  filter(s3_structure_use != 6 | s3_structure_use != 11)

#Rasterize
non_res <- st_transform(non_res, crs = st_crs(bcount))

non_res_raster<- rasterize(non_res, bcount, field = "b_count", fun = "sum")
plot(non_res_raster)

#stack rasters
stack_raster <- c(bcount, non_res_raster)

#Export raster
writeRaster(non_res_raster, paste0(output_path, "non_res_raster.tif"), 
            overwrite = T, names = "non_res_bcount" )


# Rasterize 2018 hh gps point ------------------------------------------------------

#get distinct hh hold
mphc_2018_sf <- mphc_2018_sf %>%
  mutate(unique_hh = str_c(hhnumber, EA_CODE))

mphc_2018_sf_distinct <- mphc_2018_sf[!duplicated(mphc_2018_sf$unique_hh), ]

mphc_2018_sf_distinct <- mphc_2018_sf_distinct %>% 
  mutate(count_hh = 1)
  
mphc_2018_sf_distinct <- st_transform(mphc_2018_sf_distinct, crs = st_crs(bcount))

count_hh_raster<- rasterize(mphc_2018_sf_distinct, bcount, field = "count_hh", fun = "sum")
plot(count_hh_raster)

#stack rasters
stack_raster <- c(bcount, count_hh_raster)

#Export raster
writeRaster(count_hh_raster, paste0(output_path, "mphc_count_hh_raster.tif"), 
            overwrite = T, names = "mphc_hh_count" )

