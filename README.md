# WorldPop-Malawi-fork (ons-compatability-updates branch)

## Project Overview

This project provides scripts and workflows for processing, analyzing, and modeling population and building data for Malawi, following the WorldPop methodology and adapted for ONS compatibility.

## Branch Information

This is the `ons-compatability-updates` branch, which includes updates for compatibility with ONS workflows and standards.

## Dependencies

To install all required packages, run this in your R console:

``` r
install.packages(c("tidyverse", "sf", "tictoc", "terra", "exactextractr", "haven", "nngeo", "INLA", "gstat", "spdep", "car", "caret", "kableExtra", "inlabru", "feather", "raster", "glmnet", "yaml", "modelsummary", "logger"))
```

## Refactor

The refactor work consolidates the data pre-processing pipeline into a unified orchestration workflow with integrated quality assurance reporting. This section describes how to run the refactored pipeline.

### Running the Pipeline

To run the complete data processing and QA pipeline, in an R Interactive terminal use:

```r
source("src/main.R")
```

Currently this consists of 12 stages each labelled `dp<stage number>` and will take roughly 40 minutes to run.

This will:
1. **Load configuration** from `src/config.yaml`
2. **Process all data sources** (MPHC census, ICT, IHS6, NACA, DHS, Zomba, Malemia surveys)
3. **Compute transformation statistics** (data filtering, EA reassignment, GPS accuracy splits)
4. **Run parity QA** comparing current outputs against baseline files
5. **Generate HTML report** showing source data profiles, transformations, QA results, and pipeline logs

### Current Output

The pipeline produces:

**CSV outputs** (`data/Output_Data/`):
- `summarized_survey_data.csv` — EA-level summary statistics with household counts from each source (MPHC, ICT, IHS6, NACA, DHS, Zomba, Malemia), plus gender and age distribution

**Geopackage outputs** (`data/Output_Data/`):
- `hh_size_data.gpkg` — Spatial household size summaries at EA level with geometry

**QA artifacts** (`data/quality_assurance/`):
- `all_preprocessing_parity_summary.csv` — Overall QA pass/fail status and mismatch counts
- `data_processing2_parity_column_report.csv` — Value mismatch details by column
- `data_processing2_duplicate_ea_code_report.csv` — Duplicate EA_CODE analysis (informational)
- `data_processing2_transformation_stats.csv` — Data filtering and EA reassignment metrics per source
- `pipeline_report.html` — Interactive HTML report summarizing all above

**Log files** (`data/logs/`):
- `pipeline_run_YYYYMMDD_HHMMSS.log` — Structured run log with config, pipeline messages, and output summaries

### Data Directory Structure

The pipeline expects input data organized as follows:

```
data/
├── MNSO-Data/                    # Raw input files (read-only)
│   ├── mphc2018Data_AllRegions.dta
│   ├── ICT Listing WorldPop.dta
│   ├── IHS6 Listing WorldPop.dta
│   ├── Naca Listing WorldPop.dta
│   ├── FINAL MDHS LISTING DATA_Annon.dta
│   ├── DHS_Segmented_File.csv
│   ├── MDHS_2024_NoDZLK_anonymized.dta
│   ├── malemia_hh_without_IDs.csv
│   └── zomba_csv/               # Folder of 14 CSVs from NSO
├── Shapefiles/
│   └── 2018_MPHC_EAs_Final_for_Use.shp (+ .dbf, .prj, .shx)
├── Output_Data/                  # Pipeline output (read/write)
│   ├── summarized_survey_data.csv
│   ├── hh_size_data.gpkg
│   ├── mphc_2018_sf_ea.gpkg     # Cached MPHC with spatial EA assignment
│   └── ...
├── quality_assurance/            # QA reports (created each run)
│   ├── pipeline_report.html
│   ├── all_preprocessing_parity_summary.csv
│   ├── data_processing2_*_report.csv
│   └── data_processing2_transformation_stats.csv
├── logs/                         # Pipeline logs (created each run)
│   ├── pipeline_run_*.log
│   └── ...
└── baseline/                     # Baseline files for QA comparison (optional)
    ├── summarized_survey_data.csv
    └── hh_size_data.gpkg
```

### Configuration

Edit `src/config.yaml` to control:
- **Timepoint:** 2024 or 2018
- **GPS accuracy threshold:** Default 5 m (higher values = more lenient)
- **DHS max distance:** Default 5000 m (max distance from cluster to assigned EA)
- **Source file names:** Adjust if filenames differ
- **QA baseline paths:** Point to reference outputs for comparison

## Original scripts: Data Pre-processing

Before running the main scripts, data pre-processing steps are required. Please refer to the [Pre-processing Steps section of the project wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki/Pre-processing-Steps) for detailed instructions.

Run the following scripts in order for a typical workflow:

1.  `01_Raster_Mosaicking_Buildings_2018.R` - successfully run
2.  `01_Raster_Mosaicking_Buildings_2024.R` - successfully run
3.  `01_Raster_Mosaicking_Workflow_2018.R` - successfully run
4.  `01_Raster_Mosaicking_Workflow_2024.R` - successfully run
5.  `00_Data_Processing.R` - successfully run
6.  `00_Data_Processing2.R` - successfully run
7.  `04_Rasterize.R` - mostly not used except for "# Rasterize Country" (line 47)
8.  `02_Covariates_Extraction.R` - successfully run - with caveat modified to work without data created in `04_Rasterize.R`

## Original scripts: Modelling scripts

In progress - see [Model code review of the project wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki/Model-code-review)

The important script in this section is `03_HH_Model_Workflow_2024.R`

## Additional Resources

-   [Full Project Wiki](https://github.com/datasciencecampus/WorldPop-Malawi-fork/wiki)
