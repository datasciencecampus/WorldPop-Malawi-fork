# Malawi_Project
# 📌 Malawi_Project Workflow

This repository contains a structured workflow for **geospatial data processing**, **raster preparation**, **covariate extraction** and **modelling workflow** for population modeling in Malawi. 
The scripts are organized in sequential stages to ensure a reproducible and streamlined processing pipeline.

---

## 📂 Repository Structure

| File | Description |
|------|-----------|
| `00_Data_Processing2.R` | Initial preprocessing of household survey or enumeration data. |
| `01_Raster_Mosaicking_Buildings_2018.R` | Mosaicking of 2018 Google footprint rasters. |
| `01_Raster_Mosaicking_Buildings_2024.R` | Mosaicking of 2024 updated building footprint data. |
| `01_Raster_Mosaicking_Workflow_2018.R` | Full workflow script for 2018 covariates raster mosaicking automation. |
| `01_Raster_Mosaicking_Workflow_2024.R` | Automated workflow for mosaicking 2024 covariates raster data. |
| `02_Covariates_Extraction.R` | Extraction of geospatial covariates for modelling at the EA. |
| `04_Rasterize.R` | Converts vector geospatial layers into raster format for analysis. |
| `04_Covs_Stack_Raster_cropping.R` | Creating the prediction grid. |
| `README.md` | Overview and usage instructions (this file). |

---

## 🎯 Objective

The main goal of this project is to **model household count** and **population** for census preparation in Malawi

---

## 🔄 Processing Flow

1. **Data Cleaning & Setup (`00_*.R`)**
2. **Raster Mosaicking (`01_*.R`)**
   - Process and harmonize building footprints
3. **Covariate Preparation (`02_*.R`)**
4. **Rasterization of Vector Inputs (`04_Rasterize.R`)**
5. **Covariate Raster Stacking & Cropping (`04_Covs_*.R`)**

---

## 🛠️ Requirements

Make sure the following R packages are installed:

```r
install.packages(c("tidyverse", "sf", "terra", "raster", "exactextractr"))
