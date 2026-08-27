# Changelog - Malawi Project ONS fork

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Added
- Added main.R file used to run the data processing steps.
- Added data_processing module which includes functions used to process data.
- Added quality_assurance module which produces reports on data quality.
- Added configuration file used to adjust settings in the pipeline.

### Improvements
- Refactored `00_Data_Processing2.R` into repeatable, functionalised code.
- Python script to retrieve covariate data.
- Method to create mphc shapefiles if they don't exist during the data processing.
- Helper function to load libraries used.
- Added .gitignore file for repo security.
- Improved error handling in data processing and mosaicking scripts.

### Documentation Update

