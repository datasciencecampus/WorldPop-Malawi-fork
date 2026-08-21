# Mosaic building rasters from Malawi + neighboring countries for a given year

library(terra)
library(sf)
library(tictoc)

source("utils.R")


#' Mosaic building rasters from Malawi and neighboring countries (Tanzania,
#' Mozambique, Zambia), crop and mask to a buffered Malawi boundary, and write
#' the results to data/Mosaic_Buildings_{year}/.
#'
#' @param config (list) Pipeline config file.
#' @param year (numeric) Data year, e.g. 2018 or 2024.
#' @param boundary_data_filename (character) filename for boundary buffer file.
mosaic_buildings <- function(
  config,
  year,
  boundary_data_filename = "Country_Shapefile_Buffer_10km.shp"
) {
  drive_path <- config$paths$drive_path
  shp_path <- file.path(drive_path, config$paths$shapefile_dir)
  result_path <- file.path(drive_path, paste0("Mosaic_Buildings_", year))
  building_path <- file.path(drive_path, "Malawi_Covs", paste0(year, "_Buildings"))

  # Load boundary
  boundary_data_filename <- "Country_Shapefile_Buffer_10km.shp"
  if (file.exists(file.path(shp_path, boundary_data_filename))) {
    boundary <- st_read(file.path(shp_path, boundary_data_filename))
  } else {
    boundary <- generate_buffered_country_boundary(shape_path = shp_path, file_name = boundary_data_filename, buffer = 10E3)
  }

  # Use first .tif in the Malawi buildings folder as CRS reference
  ref_tif <- list.files(building_path, pattern = "\\.tif$", full.names = TRUE)[1]
  r1 <- rast(ref_tif)
  boundary <- st_transform(boundary, crs = st_crs(r1))

  # Build folder paths for Malawi + neighbors
  countries <- c("Malawi_Covs", "Tanzania_Covs", "Mozambique_Covs", "Zambia_Covs")
  folders <- file.path(countries, paste0(year, "_Buildings"))

  raster_files <- list()

  tic()
  # Loop through each folder and read all raster files  (.tif files)
  for (folder in folders) {
    folder_path <- file.path(drive_path, folder)
    files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
    raster_files[[folder]] <- files
  }

  # Extract unique raster names (strip 3-letter country prefix)
  unique_raster_names <- unique(sapply(basename(unlist(raster_files)), function(x) substr(x, 4, nchar(x))))

  # Loop through each unique raster name and process
  for (raster_name in unique_raster_names) {
    process_raster(raster_name)
  }

  toc()
}


#' Mosaic, crop, and mask a single building raster across country folders.
#'
#' Collects rasters sharing the same name (after stripping the 3-letter country
#' prefix) from each country folder, reprojects them to a common CRS, mosaics
#' them (first-priority), then crops and masks to the country boundary before
#' writing the result to disk.
#'
#' @param raster_name (character) Raster filename without the 3-letter country
#'   prefix, e.g. "_buildings_count_2018_glv2_5_t0_5_C_100m_v1.tif".
#'   Must match files present in `raster_files`.
#'
#' @note Relies on `folders`, `raster_files`, `boundary`, and `result_path`
#'   from the calling scope (`mosaic_buildings`).
process_raster <- function(raster_name) {
  rasters <- list()
  # Collect rasters with same name from each folder
  for (folder in folders) {
    matching_files <- raster_files[[folder]][sapply(basename(raster_files[[folder]]), function(x) substr(x, 4, nchar(x)) == raster_name)]
    rasters <- c(rasters, lapply(matching_files, rast))
  }
  # Get the CRS of the first raster (CMR)
  ref_crs <- crs(rasters[[1]])
  # Reproject all rasters to the CRS of the first raster
  rasters_reprojected <- lapply(rasters, function(r) project(r, ref_crs))
  # Mosaic the rasters together, prioritizing the first raster in case of overlap
  mosaic_raster <- do.call(mosaic, c(rasters_reprojected, fun = "first"))

  # Crop the mosaicked raster using the shapefile boundary
  cropped_raster <- crop(mosaic_raster, boundary)
  # Mask the cropped raster to the boundary
  masked_raster <- mask(cropped_raster, boundary)
  # Save the masked raster to a file with a name based on the original raster file name
  output_name <- paste0("MOS_MLW", raster_name)

  if (!file.exists(result_path)) {
    dir.create(result_path, recursive = TRUE)
  }

  writeRaster(masked_raster, file.path(result_path, output_name), overwrite = TRUE)
  message("Saved ", output_name)
  # Remove the rasters from memory
  rm(list = ls(pattern = "raster"))
  gc()
}
