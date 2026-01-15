library(terra)
library(sf)
library(tictoc)

# Specify data path
drive_path <- "./data/"
base_path <- paste0(drive_path, "Input_Data/") ## Base path where the folders are located
shp_path <- paste0(drive_path, "Input_Data/Shapefiles/") ## Shapefile path
result_path <- paste0(drive_path, "Input_Data/Mosaic_Covariates_2018/") # Result path
building_path <- paste0(drive_path, "Input_Data/Malawi_Covs/2024_Buildings/")

#Load data
boundary <- st_read(paste0(shp_path, "Country_Shapefile_Buffer_10km.shp"))
r1 <- rast(paste0(building_path, "mwi_buildings_count_2023_glv2_5_t0_5_C_100m_v1.tif"))

#Reproject boundary to r1
boundary <- st_transform(boundary, crs = st_crs(r1))

# Define folder names
folders <- c("Malawi_Covs/2018_Covariates","Mozambique_Covs/2018_Covariates", 
             "Tanzania_Covs/2018_Covariates", "Zambia_Covs/2018_Covs")

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
  writeRaster(masked_raster, paste0(result_path, output_name), overwrite = TRUE)
  
  message("Saved ", output_name)
  
  rm(list = ls(pattern = "raster"))
  gc()
}

# Loop through each unique raster name and process
for (raster_name in unique_raster_names) {
  process_raster(raster_name)
}

toc()




