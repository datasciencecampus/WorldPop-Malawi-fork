
library(tidyverse)

#Load data files

malawi_urban_ea_bldgs <- read.csv("C:/Users/plummm/OneDrive - Office for National Statistics/Geospatial projects/Malawi_EA/ratio_change_method/malawi_urban_ea_bldgs.csv")

malawi_rural_ea_bldgs <- read.csv("C:/Users/plummm/OneDrive - Office for National Statistics/Geospatial projects/Malawi_EA/ratio_change_method/malawi_rural_ea_bldgs.csv")



#Function to calculate ratio change

ratio_change <- function(df) {
  
  df %>%
    mutate(
      bldgs_ratio = bldgs_2023/bldgs_2018,
      census_ratio = as.numeric(census_hh) * bldgs_ratio,
      model_abs_error = abs(HH_Model - as.numeric(survey_hh)),
      ratio_abs_error = abs(census_ratio - as.numeric(survey_hh))
    )
}

urban_ratio <- ratio_change(malawi_urban_ea_bldgs)
rural_ratio <- ratio_change(malawi_rural_ea_bldgs)


#Summarise performance


summarise_abs_errors <- function(df) {
  df %>%
    pivot_longer(
      cols = c(model_abs_error, ratio_abs_error),
      names_to = "Method",
      values_to = "abs_error"
    ) %>%
    filter(is.finite(abs_error)) %>%
    summarise(
      max_abs_error = max(abs_error, na.rm = TRUE),
      sd_abs_error  = sd(abs_error, na.rm = TRUE),
      mean_abs_error = mean(abs_error, na.rm = TRUE),
      .by = Method
    )
}

summary_abs_error_urban <- summarise_abs_errors(urban_ratio)
summary_abs_error_rural <- summarise_abs_errors(rural_ratio)




summarise_hh_sizes <- function(df, area_type = c("urban", "rural")) {
  
  area_type <- match.arg(area_type)
  threshold <- if (area_type == "urban") 300 else 200
  
  df %>%
    pivot_longer(
      cols = c(HH_Model, census_ratio, bldgs_ratio),
      names_to = "Metric",
      values_to = "Estimate"
    ) %>%
    filter(is.finite(Estimate)) %>%
    summarise(
      Max = max(Estimate, na.rm = TRUE),
      Min = min(Estimate, na.rm = TRUE),
      Sum_over_threshold = sum(Estimate > threshold, na.rm = TRUE),
      Sum_less_100hh = sum(Estimate < 100, na.rm = TRUE),
      Total = sum(Estimate > 0, na.rm = TRUE),
      Too_big_perc = (Sum_over_threshold/Total)*100,
      Not_too_big_count = Total-Sum_over_threshold,
      .by = Metric
    )
}

summarise_hh_sizes_urban <- summarise_hh_sizes(urban_ratio, area_type = "urban")
summarise_hh_sizes_rural <- summarise_hh_sizes(rural_ratio, area_type = "rural")


