library(terra)
library(sf)
library(tictoc)

source("utils.R")

# Specify data path
drive_path <- "./data/"
base_path <- file.path(drive_path) ## Base path where the folders are located
shp_path <- file.path(drive_path, "Shapefiles") ## Shapefile path
result_path <- file.path(drive_path, "Mosaic_Covariates_2024") # Result path
building_path <- file.path(drive_path, "Malawi_Covs", "2024_Buildings/")

#Load data
boundary_data_filename <- "Country_Shapefile_Buffer_10km.shp"
if(file.exists(file.path(shp_path, boundary_data_filename))) {
  boundary <- st_read(file.path(shp_path, boundary_data_filename))
} else {
  boundary <- generate_buffered_country_boundary(shp_path, boundary_data_filename)
}

r1 <- rast(file.path(building_path, "mwi_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))

#Reproject boundary to r1
boundary <- st_transform(boundary, crs = st_crs(r1))

# Define folder names
folders <- c("Malawi_Covs/2024_Covariates","Mozambique_Covs/2024_Covariates", 
             "Tanzania_Covs/2024_Covariates", "Zambia_Covs/2024_Covs")

# Read files in the result folder
files <- list.files(path = result_path, pattern = ".tif$", all.files = TRUE, full.names = FALSE)

# Remove the first 7 characters from filenames (for comparison only)
files_trim <- substr(files, 8, nchar(files))


# Initialize a list to store raster file names
raster_files <- list()

tic()

# Loop through each folder and read all raster files  (.tif files)
for (folder in folders) {
  folder_path <- file.path(base_path, folder)
  files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  raster_files[[folder]] <- files
}

# Extract unique raster names (except the first three alphabets)
unique_raster_names <- unique(sapply(basename(unlist(raster_files)), function(x) substr(x, 4, nchar(x))))

# ---- Exclude raster names already in files_trim ----
unique_raster_names <- setdiff(unique_raster_names, files_trim)

# Function to process each unique raster name
process_raster <- function(raster_name) {
  rasters <- list()
  
  # Collect rasters with same name from each folder
  for (folder in folders) {
    matching_files <- raster_files[[folder]][
      sapply(basename(raster_files[[folder]]), function(x) substr(x, 4, nchar(x)) == raster_name)
    ]
    rasters <- c(rasters, lapply(matching_files, rast))
  }
  
  # Get the CRS of the first raster
  ref_crs <- crs(rasters[[1]])
  
  # Reproject all rasters to the CRS of the first raster
  rasters_reprojected <- lapply(rasters, function(r) project(r, ref_crs))
  
  # Mosaic the rasters together
  mosaic_raster <- do.call(mosaic, c(rasters_reprojected, fun = "first"))
  
  # Crop the mosaicked raster using the shapefile boundary
  cropped_raster <- crop(mosaic_raster, boundary)
  
  # Mask the cropped raster to the boundary
  masked_raster <- mask(cropped_raster, boundary)
  
  # Save the mosaicked raster to a file with a name based on the original raster file name
  output_name <- paste0("MOS_MLW", raster_name)
  
  if (!file.exists(result_path)){
    dir.create(file.path(result_path))
  }
  
  writeRaster(masked_raster, file.path(result_path, output_name), overwrite = TRUE)
  
  message("Saved ", output_name)
  
  rm(list = ls(pattern = "raster"))
  gc()
}

# Loop through each unique raster name and process
for (raster_name in unique_raster_names) {
  process_raster(raster_name)
}

toc()





