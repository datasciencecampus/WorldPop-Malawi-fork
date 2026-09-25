###############################################################################
##############################################################################
##############################################################################
########### VISUALIZE ESTIMATES #############################################

library(sf)
library(tidyverse)
library(ggplot2)

set.seed(1234) #set seed for reproducibility

options(scipen = 999) # turn off scientific notation for all variables
#options(digits = 3)

#Specify Drive Path
drive_path <- "C:/Users/oy1r22/OneDrive - University of Southampton/Desktop/Malawi_Workshop/"
pop_path <- paste0(drive_path, "Output_Data/Predicted_Estimates/")

#Load data
latent <- read.csv(paste0(pop_path, "Growth_Factor_Latent_Posterior.csv"))
full <- read.csv(paste0(pop_path, "Growth_Factor_Full_Posterior.csv"))
top_down <- st_read(paste0(pop_path, "mwi_ea_hh_estimates_2026_gl_bf_constrained.shp"))
estimates_2024 <- read.csv(paste0(pop_path, "EA_HH_Estimates_2024.csv"))


# Mutate and Selected Needed Variables ------------------------------------

#Latent model
latent <- latent %>% 
  select(EA_CODE, ADM_STATUS, DIST_NAME, hh_count_2026, hh_lower_2026,
         predicted_hh_count_2026, hh_upper_2026) %>% 
  drop_na(hh_count_2026) %>% 
  rename(latent_hh_lower_2026 = hh_lower_2026, 
         latent_predicted = predicted_hh_count_2026, 
         latent_hh_upper_2026 = hh_upper_2026)
  
#Full model
full <- full %>% 
  drop_na(hh_count_2026)%>% 
  select(EA_CODE, hh_lower_2026,
         predicted_hh_count_2026, hh_upper_2026) %>% 
  rename(full_posterior_hh_lower_2026 = hh_lower_2026, 
         full_posterior_predicted = predicted_hh_count_2026, 
         full_posterior_hh_upper_2026 = hh_upper_2026)

#Top-down estimates
top_down <- top_down %>% 
  as_tibble() %>% 
  select(EA_CODE, hh_stmt) %>% 
  rename(top_down_estimates_2026 = hh_stmt) %>% 
  mutate(EA_CODE = as.integer(EA_CODE))

#Bottom-up model
estimates_2024 <- estimates_2024 %>% 
  select(EA_CODE, mean_estimate, lower, upper) %>% 
  rename(bottom_up_estimates = mean_estimate,
         bottom_up_upper = upper, 
         bottom_up_lower = lower)

#Join all da
predictions_2026 <- latent %>% 
  inner_join(full, by = "EA_CODE") %>% 
  inner_join(estimates_2024, by = "EA_CODE") %>% 
  inner_join(top_down, by = "EA_CODE")

############################################################################
###########################################################################
#----------------------------------------------------------
# Validate Model
#----------------------------------------------------------
#Long data format
pred_long <- predictions_2026 %>%
  transmute(
    EA_CODE,
    ADM_STATUS,
    DIST_NAME,
    hh_count_2026,
    
    # Latent posterior
    latent_predicted = latent_predicted,
    latent_lower     = latent_hh_lower_2026,
    latent_upper     = latent_hh_upper_2026,
    
    # Full posterior
    full_posterior_predicted = full_posterior_predicted,
    full_posterior_lower     = full_posterior_hh_lower_2026,
    full_posterior_upper     = full_posterior_hh_upper_2026,
    
    # Bottom-up
    bottom_up_predicted = bottom_up_estimates,
    bottom_up_lower     = bottom_up_lower,
    bottom_up_upper     = bottom_up_upper,
    
    # Top-down
    top_down_predicted = top_down_estimates_2026
  ) %>%
  pivot_longer(
    cols = -c(EA_CODE, ADM_STATUS, DIST_NAME, hh_count_2026),
    names_to = c("model", "statistic"),
    names_sep = "_(?=[^_]+$)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  )

#Validation of models
model_val <- pred_long %>% 
  group_by(model) %>% 
  mutate(
    residual = hh_count_2026 - predicted,
    eps = 1e-10,
    P = (hh_count_2026 + eps) / sum(hh_count_2026 + eps),
    Q = (predicted + eps) / sum(predicted + eps)
  ) %>%
  summarise(
    n           = n(),
    Bias        = mean(residual),
    Imprecision = sd(residual, na.rm = TRUE),
    MAE         = mean(abs(residual)),
    MSE         = mean(residual^2),
    RMSE        = sqrt(MSE),
    Corr        = cor(hh_count_2026, predicted, use = "complete.obs"),
    KL          = sum(P * log(P/Q))
  )


model_val 

############################################################################
################################################################################
#----------------------------------------------------------
# Distribution of Data 
#----------------------------------------------------------

#Standardise model names
plot_predictions <- pred_long %>%
  mutate(
    model = recode(
      model,
      latent = "Latent Posterior",
      full_posterior = "Full Posterior",
      bottom_up = "Bottom-Up",
      top_down = "Top-Down"
    ),
    #Change the model names to factor
    model = factor(
      model,
      levels = c(
        "Latent Posterior",
        "Full Posterior",
        "Bottom-Up",
        "Top-Down"
      )
    )
  )

#get distinct EA Code

survey_data <- predictions_2026 %>%
  distinct(EA_CODE, hh_count_2026) %>%
  transmute(
    EA_CODE,
    Variable = "Survey 2026",
    Value = hh_count_2026
  )


#Get models predictions
prediction_data <- plot_predictions %>%
  transmute(
    EA_CODE,
    Variable = as.character(model),
    Value = predicted)


#Rbind the observed data
plot_data <- bind_rows(
  survey_data,
  prediction_data
) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(
        "Survey 2026",
        "Latent Posterior",
        "Full Posterior",
        "Bottom-Up",
        "Top-Down"
      )
    )
  )


# ==========================================================
# Calculate Summary statistics
# ==========================================================

summary_stats <- plot_data %>%
  group_by(Variable) %>%
  summarise(
    Min    = round(min(Value, na.rm = TRUE), 2),
    Q1     = round(quantile(Value, 0.25, na.rm = TRUE), 2),
    Median = round(median(Value, na.rm = TRUE), 2),
    Mean   = round(mean(Value, na.rm = TRUE), 2),
    Q3     = round(quantile(Value, 0.75, na.rm = TRUE), 2),
    Max    = round(max(Value, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      "Min = ", Min,
      "\nQ1 = ", Q1,
      "\nMed = ", Median,
      "\nMean = ", Mean,
      "\nQ3 = ", Q3,
      "\nMax = ", Max
    )
  )

summary_stats

##############################################################################
##############################################################################
# ==========================================================
# Boxplot
# ==========================================================

ggplot(plot_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(
    width = 0.6,
    alpha = 0.7
  ) +
  geom_text(
    data = summary_stats,
    aes(
      x = Variable,
      y = 1500,
      label = label
    ),
    hjust = 1.2,
    vjust = 1,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  labs(
    title = "Ground-Truth and Model-Based 2026 Household Counts",
    x = "",
    y = "Household Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      face = "bold",
      angle = 20,
      hjust = 1
    )
  )


# ==========================================================
# Density plot
# ==========================================================

ggplot(plot_data, aes(x = Value, fill = Variable)) +
  geom_density(
    alpha = 0.35,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Survey and Model-Based 2026 Household Counts",
    x = "Household Count",
    y = "Density",
    fill = "Data source"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

##################################################################
##################################################################

# Calculate Coverage Rate -------------------------------------------------

coverage_rate <- plot_predictions %>%
  group_by(model) %>%
  summarise(
    n = sum(
      !is.na(hh_count_2026) &
        !is.na(predicted)
    ),
    coverage = mean(
      hh_count_2026 >= lower &
        hh_count_2026 <= upper,
      na.rm = TRUE
    ),
    coverage_percent = round(
      coverage * 100,
      1
    ),
    mean_interval_width = mean(
      upper - lower,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

coverage_rate


## Find which EAs are covered or not
plot_predictions <- plot_predictions %>%
  mutate(
    covered = case_when(
      is.na(lower) | is.na(upper) ~ NA,
      hh_count_2026 >= lower &
        hh_count_2026 <= upper ~ TRUE,
      TRUE ~ FALSE
    )
  )

#Join coverage proportion to data
plot_predictions <- plot_predictions %>%
  left_join(
    coverage_rate %>%
      select(model, coverage_percent),
    by = "model"
  )

#########################################################################
##########################################################################
# Plot of Predictions Vs Validation ---------------------------------------

# Plot
ggplot(plot_predictions, aes(x = hh_count_2026, y = predicted)) +
  # 1:1 line
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#219ebc",
    linewidth = 1
  ) +
  # Uncertainty intervals
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    alpha = 0.6,
    width = 0.2,
    color = "darkblue",
    linewidth = 0.8
  ) +
  # Points
  geom_point(
    #aes(color = abs(hh_count_2026 - predicted_hh_count)),
    size = 2.5,
    alpha = 0.7
  ) +
  labs(
    title = "Survey-2026 vs Predictions",
    #subtitle = "Prediction intervals shown as vertical error bars",
    x = "Grouth-Truth",
    y = "Predictions"
  ) +
  #Theme
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  facet_wrap(~model)

################################################################################
################################################################################

# Proportion Covered ------------------------------------------------------
ggplot(
  plot_predictions,
  aes(
    x = hh_count_2026,
    y = predicted,
    color = covered
  )
) +
  # Prediction intervals
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.15,
    width = 0
  ) +
  # Points
  geom_point(
    size = 2.8,
    alpha = 0.8
  ) +
  # 1:1 line
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      "TRUE" = "#2A9D8F",
      "FALSE" = "#E63946"
    ),
    labels = c(
      "TRUE" = "Covered",
      "FALSE" = "Outside Interval"
    ),
    name = "Survey-2026 Coverage"
  ) +
  
  labs(
    title = "Scatter Plot of Observed Vs Predicted",
    x = "Survey-2026",
    y = "Predictions"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    strip.text = element_text(
      face = "bold",
      size = 14
    )
  ) +
  facet_wrap(~model)+
  # Add coverage percentage inside each panel
  geom_text(
    aes(
      x = Inf,
      y = Inf,
      label = paste0(
        "Coverage = ",
        coverage_percent,
        "%"
      )
    ),
    hjust = 1.1,
    vjust = 1.5,
    size = 5,
    fontface = "bold",
    color = "black",
    inherit.aes = TRUE
  )


################################################################
################################################################

# Rural Urban -------------------------------------------------------------

#Coverage
coverage_by_rural_urban <- plot_predictions %>%
  group_by(model, ADM_STATUS) %>%
  summarise(
    coverage = mean(covered),
    n = n()
  ) %>%
  arrange(coverage)

coverage_by_rural_urban


rural_urban <- plot_predictions %>% 
  select(ADM_STATUS, model, hh_count_2026, lower, predicted, upper)

validation_rural_urban <- rural_urban %>% 
  group_by(model, ADM_STATUS) %>% 
  mutate(
    residual = hh_count_2026 - predicted,
    eps = 1e-10,
    P = (hh_count_2026 + eps) / sum(hh_count_2026 + eps),
    Q = (predicted + eps) / sum(predicted + eps)
  ) %>%
  summarise(
    n           = n(),
    Bias        = mean(residual),
    Imprecision = sd(residual, na.rm = TRUE),
    MAE         = mean(abs(residual)),
    MSE         = mean(residual^2),
    RMSE        = sqrt(MSE),
    Corr        = cor(hh_count_2026, predicted, use = "complete.obs"),
    KL          = sum(P * log(P/Q))
  )


validation_rural_urban

#Put results into a long format
rural_urban_long <- validation_rural_urban %>%
  select(model, ADM_STATUS, Bias, MAE, RMSE, Corr) %>%
  pivot_longer(
    cols = c(Bias, MAE, RMSE, Corr),
    names_to = "Metric",
    values_to = "Value"
  )

#############################################################################
############################################################################

# Facet Dot Plot ----------------------------------------------------------
# Rural Urban validation
ggplot(
  rural_urban_long,
  aes(
    x = model,
    y = Value,
    color = ADM_STATUS
  )
) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 3
  ) +
  # Reference line
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  facet_grid(
    Metric ~ .,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "lightgrey"
    ),
    axis.text.y = element_text(),
    panel.spacing = unit(1, "lines"),
    strip.placement = "outside",
    legend.position = "right",
    panel.grid.major = element_line(
      color = "grey90"
    ),
    panel.grid.minor = element_line(
      color = "grey95"
    )
  ) +
  
  labs(
    title = "Model Validation by Rural–Urban Status",
    x = "",
    y = "",
    color = "Rural-Urban"
  )

#####################################################################
####################################################################

# Predicted Vs Observed Rural Urban ---------------------------------------

#Plot of rural vs urban
ggplot(rural_urban,
       aes(x = hh_count_2026,
           y = predicted,
           color = ADM_STATUS)) +
  
  # Prediction intervals
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    alpha = 0.6,
    width = 0.5,
    color = "darkblue",
    linewidth = 0.8
  ) +
  # Points
  geom_point(
    size = 2.8,
    alpha = 0.8
  ) +
  
  # 1:1 line
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) +
  
  scale_color_manual(
    values = c(
      "Urban" = "#2A9D8F",
      "Rural" = "#E63946"
    ),
    labels = c(
      "Rural" = "Rural",
      "Urban" = "Urban"
    ),
    name = "Strata"
  ) +
  
  #coord_equal() +
  
  labs(
    title = "Survey-2026 Vs Predictions",
    x = "Survey-2026",
    y = "Predictions"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )+
  facet_wrap( ~model)

#########################################################################

# Prediction Interval -----------------------------------------------------

# Get a vector of 5 unique random EA codes
set.seed(42) # Optional: ensures you get the same 5 EAs every time you run it
sampled_eas <- plot_predictions %>% 
  pull(EA_CODE) %>% 
  unique() %>% 
  sample(size = 5)

# Filter the main dataset for only those 5 EAs
plot_ea <- plot_predictions %>% 
  filter(EA_CODE %in% sampled_eas) 

ggplot(
  plot_ea,
  aes(
    x = predicted,
    y = fct_reorder(as.character(EA_CODE), predicted)
  )
) +
  # Prediction intervals
  geom_errorbar(
    aes(
      xmin = lower,
      xmax = upper,
      color = model
    ),
    width = 0,
    linewidth = 1.2,
    alpha = 0.7,
    position = position_dodge(width = 0.35)
  ) +
  
  # Prediction estimates
  geom_point(
    aes(
      color = model
    ),
    size = 3.5,
    position = position_dodge(width = 0.35)
  ) +
  
  # Labels and axes
  labs(
    title = "2026 Household Count Predictions by EA",
    x = "Predicted household count",
    y = "EA CODE",
    color = "Prediction type"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18
    ),
    axis.text.y = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


##################END OF SCRIPT #############################################
############################################################################
############################################################################
