# Packages
library(tidyverse)
library(sf)
library(terra)
library(exactextractr)
library(tictoc)
library(raster)


# Specify Drive Path
output_path <- "Output_Data/"
shapefile_path <- "Data/Shapefiles/"
bcount_path_2018 <- "Data/Covariates/Buildings_2018/"

# Load datasets
logger::log_info("Starting rasterization; loading EA boundaries, country boundary and building-count template.")
ea <- st_read(file.path(shapefile_path, "2018_MPHC_EAs_Final_for_Use_Corrected.shp"))
bcount <- rast(file.path(bcount_path_2018, "MOS_MLW_buildings_count_BCB_gl_100m_v1_1.tif"))
country <- st_read(file.path(shapefile_path, "Country_Shapefile.shp"))
logger::log_info("Loaded {nrow(ea)} EA boundary rows; creating administrative identifiers.")


# create unique id for each ea - this is what will be rasterized
ea <- ea %>%
  rowid_to_column("ea_id")

# create unique id for each district - it groups the eas per district and hen
# it will give ids to the districts
ea <- ea %>%
  group_by(DIST_NAME) %>%
  mutate(dist_id = cur_group_id()) %>%
  ungroup()

# Create id for rural urban - this is a character which needs to be a number to be rasterised
# Lake Malawi had NAso he's calling it rural
ea <- ea %>%
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1
  ))

############################################################################
# Rasterize Country ------------------------------------------------------

# Transform Raster - first the polygon needs to be projected
logger::log_info("Rasterizing the country boundary.")
country <- st_transform(country, crs = st_crs(bcount))

# Rasterize
country_raster <- terra::rasterize(country, bcount, field = "Country_ID")
plot(country_raster)

# stack rasters
stack_raster <- c(bcount, country_raster)

# Export raster
writeRaster(country_raster, paste0(output_path, "country_raster.tif"),
  overwrite = T, names = "country_id"
)
logger::log_info("Country raster written to {output_path}country_raster.tif.")

# Rasterize Rural Urban ------------------------------------------------------

# Transform
logger::log_info("Rasterizing rural/urban identifiers.")
rural_urban <- st_transform(ea, crs = st_crs(bcount))

# Rasterize
rural_urban_raster <- terra::rasterize(rural_urban, bcount, field = "rural_urban_id")
plot(rural_urban_raster)

# stack rasters
stack_raster <- c(bcount, rural_urban_raster)

# Export raster
writeRaster(rural_urban_raster, paste0(output_path, "rural_urban_raster.tif"),
  overwrite = T, names = "rural_urban_id"
)
logger::log_info("Rural/urban raster written to {output_path}rural_urban_raster.tif.")


# Rasterize District ------------------------------------------------------

logger::log_info("Rasterizing district identifiers.")
district <- st_transform(ea, crs = st_crs(bcount))

district_raster <- terra::rasterize(district, bcount, field = "dist_id")
plot(district_raster)

# stack rasters
stack_raster <- c(bcount, district_raster)

# Export raster
writeRaster(district_raster, paste0(output_path, "district_raster.tif"),
  overwrite = T, names = "dist_id"
)
logger::log_info("District raster written to {output_path}district_raster.tif.")


# Rasterize Region ------------------------------------------------------

logger::log_info("Rasterizing region identifiers.")
region <- ea %>%
  drop_na(REG_NAME)

region <- st_transform(region, crs = st_crs(bcount))

region_raster <- terra::rasterize(region, bcount, field = "REG_CODE")
plot(region_raster)

# stack rasters
stack_raster <- c(bcount, region_raster)

# Export raster
writeRaster(region_raster, paste0(output_path, "region_raster.tif"),
  overwrite = T, names = "REG_CODE"
)
logger::log_info("Region raster written to {output_path}region_raster.tif.")

# Rasterize EA ID ------------------------------------------------------

logger::log_info("Rasterizing EA identifiers.")
ea <- st_transform(ea, crs = st_crs(bcount))

ea_raster <- terra::rasterize(ea, bcount, field = "ea_id")
plot(ea_raster)

# stack rasters
stack_raster <- c(bcount, ea_raster)

# Export raster
writeRaster(ea_raster, paste0(output_path, "ea_raster.tif"),
  overwrite = T, names = "ea_id"
)
logger::log_info("EA raster written to {output_path}ea_raster.tif.")

logger::log_info("Rasterization complete; all five administrative rasters written successfully.")

############# END OF RASTERIZATION ############################################
###############################################################################
