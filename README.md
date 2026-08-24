# WorldPop-Malawi-fork (ons-compatability-updates branch)

## Project Overview

This project provides scripts and workflows for processing, analyzing, and modeling population and building data for Malawi, following the WorldPop methodology and adapted for ONS compatibility.

## Branch Information

This is the `ons-compatability-updates` branch, which includes updates for compatibility with ONS workflows and standards.

## Dependencies

This project requires R and the following R packages:

-   tidyverse
-   sf
-   tictoc
-   terra
-   exactextractr
-   haven
-   nngeo
-   INLA
-   gstat
-   spdep
-   car
-   caret
-   kableExtra
-   inlabru
-   feather
-   raster
-   glmnet

To install all required packages, run this in your R console:

``` r
install.packages(c("tidyverse", "sf", "tictoc", "terra", "exactextractr", "haven", "nngeo", "INLA", "gstat", "spdep", "car", "caret", "kableExtra", "inlabru", "feather", "raster", "glmnet"))
```

## Data Pre-processing

Before running the main scripts, data pre-processing steps are required. Please refer to the [Pre-processing Steps section of the project wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki/Pre-processing-Steps) for detailed instructions.

Run the following scripts in order for a typical workflow:

1.  `01_Raster_Mosaicking_Buildings_2018.R` - successfully run
2.  `01_Raster_Mosaicking_Buildings_2024.R` - successfully run
3.  `01_Raster_Mosaicking_Workflow_2018.R` - successfully run
4.  `01_Raster_Mosaicking_Workflow_2024.R` - successfully run
5.  `00_Data_Processing.R` - successfully run
6.  `00_Data_Processing2.R` - successfully run - caveat "missing files"
7.  `04_Rasterize.R` - not used in modelling and not run
8.  `02_Covariates_Extraction.R` - successfully run - with caveat modified to work without data created in `04_Rasterize.R`

## Modelling scripts

In progress - see [Model code review of the project wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki/Model-code-review)

The important script in this section is `03_HH_Model_Workflow_2024.R`

## Additional Resources

-   [Full Project Wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki)

## Ratio change method
- [Ratio change Wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki/Ratio-change-metadata)