load_library <- function(library_name) {
  #' Check if named library installed, if so load, if not install and load
  #'
  #' @param library_name (character). The name of the library to be installed/loaded.
  #'
  if (library_name %in% rownames(installed.packages()) == FALSE) {
    print(paste0("The required library '", library_name, "' is not installed, installing now."))
    install.packages(library_name, dependencies = TRUE)
    library(library_name, character.only = TRUE)
  } else {
    library(library_name, character.only = TRUE)
  }
}

load_libraries <- function(library_names) {
  #' Apply the load_library function across a vector of library names.
  #'
  #' @param library_names (vector). A vector of libraries to be loaded (or installed).
  #'
  sapply(library_names, load_library)
}

# Add any required packages to this list
required_libraries <- c(
  "sf", # package for handling spatial data
  "tidyverse", # tidyverse
  "haven", # package for import foreign statistical formats
  "terra", # package for spatial data analysis
  "tictoc", # package for timing R Scripts
  "exactextractr", # zonal statistics of polygons
  "gstat", # Spatial and Spatio-Temporal Geostatistical Modelling, Prediction and Simulation
  "spdep", # spatial dependence and weights
  "car", # Companion to Applied Regression
  "caret", # Classification And REgression Training
  "kableExtra", # for html tables
  "inlabru", # package for Bayesian spatial modelling 
  "feather" # reading and writing feather files
)

# Execute code to load (and install) libraries
load_libraries(required_libraries)
