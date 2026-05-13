
library(tidyverse)
library(ggplot2)

#Load data files
drive_path <- "./data/"
input_path <- paste0(drive_path, "Input_Data/")
output_path <- paste0(drive_path, "Output_Data/")


malawi_urban_ea_bldgs <- read.csv(paste0(input_path, "malawi_urban_ea_bldgs.csv"))
malawi_rural_ea_bldgs <- read.csv(paste0(input_path, "malawi_rural_ea_bldgs.csv"))



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


summarise_abs_errors <- function(df, area_type = c("urban", "rural"), output_path = ".") {
  
  area_type <- match.arg(area_type)
  
  summary_table <- df %>%
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
  
  
  file_name <- paste0("summary_abs_error_", area_type, ".csv")
  file_path <- paste0(output_path, file_name)
  print(file_path)
  write.csv(summary_table, file_path, row.names = FALSE)

}

summary_abs_error_urban <- summarise_abs_errors(urban_ratio, area_type = "urban", output_path = output_path)
summary_abs_error_rural <- summarise_abs_errors(rural_ratio, area_type = "rural", output_path = output_path)



summarise_hh_sizes <- function(df, area_type = c("urban", "rural")) {
  
  area_type <- match.arg(area_type)
  threshold <- if (area_type == "urban") 300 else 200
  
  summary_table <- df %>%
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
  
  file_name <- paste0("summarise_hh_sizes_", area_type, ".csv")
  file_path <- paste0(output_path, file_name)
  print(file_path)
  write.csv(summary_table, file_path, row.names = FALSE)
  
}

summarise_hh_sizes_urban <- summarise_hh_sizes(urban_ratio, area_type = "urban")
summarise_hh_sizes_rural <- summarise_hh_sizes(rural_ratio, area_type = "rural")




correlation_analysis <- function(df, area_type = c("urban", "rural"), save_plot = TRUE, output_dir = ".") {
  
  
  area_type <- match.arg(area_type)
  
  # Remove rows with NA in any relevant column
  
  df_clean <- df %>%
    mutate(
      survey_hh = as.numeric(survey_hh),
      HH_Model  = as.numeric(HH_Model),
      census_hh = as.numeric(census_hh)
    ) %>%
    filter(
      is.finite(survey_hh),
      is.finite(HH_Model),
      is.finite(census_hh)
    )
  
  
  # Fit models
  mod_model <- lm(HH_Model ~ survey_hh, data = df_clean)
  mod_census <- lm(census_hh ~ survey_hh, data = df_clean)
  
  
  # Extract equation + R²
  eq_model <- sprintf(
    "y = %.4f x + %.2f\nR² = %.4f",
    coef(mod_model)[2], coef(mod_model)[1],
    summary(mod_model)$r.squared
  )
  
  eq_census <- sprintf(
    "y = %.4f x + %.2f\nR² = %.4f",
    coef(mod_census)[2], coef(mod_census)[1],
    summary(mod_census)$r.squared
  )
  
  
  plot_title <- paste0(
    "Survey against WorldPop and Ratio Change method - ",
    tools::toTitleCase(area_type), " Enumeration Areas"
  )
  
  p <- ggplot(df_clean) +
    
    # Points
    geom_point(aes(x = survey_hh, y = HH_Model, colour = "WorldPop model"), alpha = 0.8) +
    geom_point(aes(x = survey_hh, y = census_hh, colour = "Census Ratio Change"), alpha = 0.8) +
    
    # Regression lines
    geom_smooth(aes(x = survey_hh, y = HH_Model, colour = "WorldPop model"),
                method = "lm", se = FALSE, linetype = "dotted") +
    
    geom_smooth(aes(x = survey_hh, y = census_hh, colour = "Census Ratio Change"),
                method = "lm", se = FALSE, linetype = "dotted") +
    
    # x = y line
    
    geom_abline(intercept = 0, slope = 1,
                colour = "black", linetype = "solid", linewidth = 1) +
    
    # Annotations (you may tweak positions)
    annotate("text",
             x = max(df_clean$survey_hh) * 0.9,
             y = max(df_clean$HH_Model) * 0.5,
             label = eq_model,
             colour = "black",
             size = 4) +
    
    annotate("text",
             x = max(df_clean$survey_hh) * 0.5,
             y = max(df_clean$census_hh) * 0.5,
             label = eq_census,
             colour = "black",
             size = 4) +
    
    # Labels
    labs(
      x = "Household Survey Estimate",
      y = "WorldPop and Ratio Change Estimates",
      colour = "",
      title = plot_title
    ) +
    
    # Colours to match your plot
    scale_colour_manual(values = c(
      "WorldPop model" = "#1f77b4",     # blue
      "Census Ratio Change" = "#ff7f0e"   # orange
    )) +
    
    # Theme similar to your image
    theme_minimal() +
    theme(
      legend.position = "right",
      panel.grid.major = element_line(colour = "grey80")
    )
  
  if (save_plot) {
    
    file_name <- paste0("scatterplot_", area_type, ".png")
    file_path <- file.path(output_path, file_name)
    
    ggsave(
      filename = file_path,
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
  
  return(p)
  
}

correlation_analysis(malawi_urban_ea_bldgs, area_type = "urban")
correlation_analysis(malawi_rural_ea_bldgs, area_type = "rural")

