# WorldPop-Malawi-fork (ons-compatability-updates branch)

## Project Overview

This project provides scripts and workflows for processing, analyzing, and modeling population and building data for Malawi, following the WorldPop methodology and adapted for ONS compatibility.

## Branch Information

This is the `development` branch, which includes updates to the WorldPop code.

### Running the Pipeline

To run the whole pipeline in an R Interactive terminal use:

```r
source("main.R")
```

Currently this will run the scripts provided by WorldPop and updated by ONS which were run in the workshop in Malawi in September 2026.

### Current Output

The pipeline produces household population estimates at multiple spatial resolutions and with different modeling approaches.

#### Data Processing Outputs (`Output_Data/`)

When `run_data_processing = TRUE`, the pipeline generates:

**CSV Data Files:**
- `summarized_survey_data.csv` — EA-level summary statistics with household counts aggregated from all survey sources (MPHC, ICT, IHS6, NACA, DHS, Zomba, Malemia)
- `Malawi_2018_data.csv`, `Malawi_2024_data.csv`, `Malawi_2026_data.csv` — EA-level covariate data extracted for each time period
- `var_names_2018.csv`, `var_names_2024.csv`, `var_names_2026.csv` — Variable name mappings for covariates

**Administrative Rasters (100m resolution, GeoTIFF):**
- `country_raster.tif` — Country boundary mask
- `ea_raster.tif` — Enumeration area identifiers
- `district_raster.tif` — District identifiers
- `region_raster.tif` — Region codes
- `rural_urban_raster.tif` — Rural/urban classification

**Covariate Stacks (Apache Feather format):**
- `Malawi_covs_stack_2018.feather`, `Malawi_covs_stack_2024.feather`, `Malawi_covs_stack_2026.feather` — Combined covariate data with grid coordinates and building counts for each year
- `Processed_Covariates_YYYY_N_to_M.feather` (multiple batch files) — Intermediate covariate processing files for memory efficiency

#### Growth Factor Model Outputs (`Output_Data/Predicted_Estimates/`)

When `run_growth_model = TRUE`, the pipeline generates 2026 household estimates:

**Full Posterior Model:**
- `Growth_Factor_Full_Posterior.csv` — EA-level growth factor predictions (100 posterior draws) with 2.5% and 97.5% credible intervals
- `HH_Estimates_2026.tif` — 2026 household count raster predictions at 100m grid resolution

**Latent Posterior Model:**
- `Growth_Factor_Latent_Posterior.csv` — EA-level latent posterior growth factor predictions with intervals
- `HH_Estimates_2026.gpkg` — Pixel-level household estimates with spatial geometry (GeoPackage)
- `Latent_HH_Estimates_2026.tif` — 2026 household count raster predictions at 100m grid resolution

#### Log Files

**Log files** (`logs/`):
- `pipeline_run_YYYYMMDD_HHMMSS.log` — Timestamped run log containing pipeline progress, model fitting details, and validation metrics

### Data Directory Structure

The pipeline requires input data organized as follows. Only create directories and files that your workflow requires.

```
(Repository Root)
├── Data/
│   ├── Surveys/                          # Input survey data (read-only)
│   │   ├── 0923_pop_modelling_data.dta   # 2018 MPHC census data
│   │   ├── ICT Listing WorldPop.dta      # ICT survey listing
│   │   ├── IHS6 Listing WorldPop.dta     # IHS6 survey listing
│   │   ├── Naca Listing WorldPop.dta     # NACA survey listing
│   │   ├── FINAL MDHS LISTING DATA_Annon.dta  # DHS listing
│   │   ├── MDHS_2024_NoDZLK_anonymized.dta   # 2024 DHS survey
│   │   ├── DHS_Segmented_File.csv        # DHS segmentation data
│   │   ├── malemia_hh_without_IDs.csv    # Malemia survey data
│   │   ├── CensusMapping_data_09092026.xlsx   # Census mapping reference
│   │   └── zomba_csv/                    # Folder containing zombie CSVs
│   │       └── *.csv (14 files)          # Individual village survey files
│   │
│   ├── Shapefiles/                       # Spatial reference data (read-only)
│   │   ├── 2018_MPHC_EAs_Final_for_Use_Corrected.shp (+ .dbf, .prj, .shx)
│   │   └── Country_Shapefile.shp (+ .dbf, .prj, .shx)
│   │
│   ├── Covariates/                       # Covariate raster data (read-only)
│   │   ├── Covariates_2018/              # 2018 covariate GeoTIFFs
│   │   ├── Covariates_2024/              # 2024 covariate GeoTIFFs
│   │   ├── Covariates_2026/              # 2026 covariate GeoTIFFs
│   │   ├── Buildings_2018/               # 2018 building count rasters
│   │   ├── Buildings_2024/               # 2024 building count rasters
│   │   └── Buildings_2026/               # 2026 building count rasters
│
├── Output_Data/                          # Pipeline outputs (read/write)
│   ├── summarized_survey_data.csv
│   ├── Malawi_2018_data.csv
│   ├── Malawi_2024_data.csv
│   ├── Malawi_2026_data.csv
│   ├── var_names_*.csv
│   ├── *_raster.tif
│   ├── Malawi_covs_stack_*.feather
│   ├── Processed_Covariates_*.feather
│   └── Predicted_Estimates/              # Growth model predictions
│       ├── Growth_Factor_Full_Posterior.csv
│       ├── Growth_Factor_Latent_Posterior.csv
│       ├── HH_Estimates_2026.tif
│       ├── Latent_HH_Estimates_2026.tif
│       └── HH_Estimates_2026.gpkg
│
└── logs/                                 # Pipeline execution logs (auto-created)
    └── pipeline_run_*.log
```

**Notes:**
- All file paths in scripts are relative to the repository root
- Input data directories (Surveys, Shapefiles, Covariates) must exist before running `run_data_processing = TRUE`
- The `Output_Data/` and `logs/` directories are created automatically by the pipeline
- The `Predicted_Estimates/` subdirectory is created when `run_growth_model = TRUE`

## Additional Resources

-   [Full Project Wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki)
