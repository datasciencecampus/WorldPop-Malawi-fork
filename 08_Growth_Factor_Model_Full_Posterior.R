# This script implement the growth factor approach 
# The estimates are based off the full posterior distribution

#Load packages
library(INLA)
library(sf)
library(spdep)
library(car)
library(caret)
library(tictoc)
library(terra)
library(kableExtra)
library(inlabru)
library(feather)
library(tidyverse)

set.seed(1234) #set seed for reproducibility

options(scipen = 999) # turn off scientific notation for all variables
#options(digits = 3)

#Specify Drive Path
drive_path <- "C:/Users/oy1r22/OneDrive - University of Southampton/Desktop/Malawi_Workshop/"
input_path <- paste0(drive_path, "Output_Data/")
output_path <- paste0(drive_path, "Output_Data/Predicted_Estimates/")
shapefile_path <- paste0(drive_path, "Data/Shapefiles/")
pop_path <- paste0(drive_path, "Output_Data/Predicted_Estimates/")

#####################################################################################
#####################################################################################
#####################################################################################
##### ESTIMATING GROWTH FACTOR BETWEEN 2018 and 2024 ################################

# Load summarized Population data
pop_data <-  read.csv(paste0(input_path, "Malawi_2018_data.csv")) 

#names
names(pop_data)

#create unique id for each district
pop_data <- pop_data %>% 
  group_by(DIST_NAME) %>%
  mutate(dist_id = cur_group_id()) %>%
  ungroup() 

#Create id for rural urban
pop_data <- pop_data %>% 
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1))

# Create a nested ids
pop_data <- pop_data %>%
  group_by(rural_urban_id, dist_id, REG_CODE) %>%
  mutate(nested_id = cur_group_id()) %>%
  ungroup()

#check summary of household count for 2018
summary(pop_data$hh_count_2018)  # There are 45 EAs without hh count for 2018

#Filter those EAs and check them
eas_na <- pop_data %>% 
  filter(is.na(hh_count_2018))

#For the purpose of this work we will replace the hh count with 1 in those EAs
#pop_data <- pop_data %>% 
  #replace_na(list(hh_count_2018 = 1))

#We will remove EAs without hh count 2018
pop_data <- pop_data %>% 
  drop_na(hh_count_2018)

#Calculate the ratio between 2024 to 2018 hh count
pop_data <- pop_data %>% 
  mutate(ratio = (hh_count_2024/hh_count_2018))

#Find the annual multiplicative growth factor
# What constant yearly multiplication factor would produce the observed 6 years increase?

pop_data <- pop_data %>% 
  mutate(growth_factor = ratio^0.16)  #2024 - 2018 = 6years ie 1/6

#check summary stats 
summary(pop_data$growth_factor)

###########################################################################
############################################################################
# Visualize the distribution of data and clean the data

#filter growth factor which is NA
EA_data <- pop_data %>% 
  drop_na(growth_factor) %>%   # drop NA 
  filter(!is.infinite(growth_factor))  #drop Infinity values

#check summary stats 
summary(EA_data$growth_factor)   #Summary of growth factor
summary(EA_data$hh_count_2024)   #Summary of 2024 hh count

#Boxplot of growth_factor distribution
ggplot(data = EA_data, aes(y=growth_factor))+
  geom_boxplot(color="blue", alpha=0.2)


# Density plot of growth_factor
ggplot(data = EA_data, aes(x = growth_factor)) +
  geom_density(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "household growth_factor",
    y = "Density",
    title = "Density Plot of household growth_factor"
  ) +
  theme_minimal()

#plot HH Count 2024
ggplot(data = EA_data, aes(x = hh_count_2024)) +
  geom_histogram(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "Household Count",
    y = "Density",
    title = "Density Plot of Household Count"
  ) +
  theme_minimal()

# Remove HH count below 10
EA_data <- EA_data %>% 
  filter(hh_count_2024 > 10)

#plot HH Count
ggplot(data = EA_data, aes(x = hh_count_2024)) +
  geom_histogram(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "Household Count",
    y = "Density",
    title = "Density Plot of Household Count"
  ) +
  theme_minimal()


#Boxplot of HH density
ggplot(data = EA_data, aes(y=growth_factor))+
  geom_boxplot(color="blue", alpha=0.2)


#plot Density
ggplot(data = EA_data, aes(x = growth_factor)) +
  geom_histogram(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "household growth_factor",
    y = "Density",
    title = "HH Density"
  ) +
  theme_minimal()

#check summary stats 
summary(EA_data$growth_factor)

###############################################################################
###############################################################################

# Get Random Effect Indexing ----------------------------------------------

#Assign unique values to each row
EA_data <- EA_data %>% 
  tibble::rowid_to_column("id")


#get distinct count of rural urban
rural_urban_group <- EA_data %>% 
  distinct(rural_urban_id) %>% 
  nrow()

# #get distinct count of district
dist_groups <- EA_data %>% 
  distinct(dist_id) %>% 
  nrow()

# #get distinct count of EA
ea_groups <- EA_data %>% 
  distinct(id) %>% 
  nrow()

#Get distinct count of nesting
nested_group <- EA_data %>% 
  distinct(nested_id) %>% 
  nrow()

#Specify the number of samples to draw

n.samples <- 100


#########################################################################
#########################################################################
################ GROWTH FACTOR MODELLING ################################
# Fit Models --------------------------------------------------------------

#Model1 -  Fixed Effect + Urban_Rural_Random_Effect

#------------------------------------------------------------------------------
# Fit log-normal Model
#------------------------------------------------------------------------------

formula1 <- growth_factor ~  google_v2_5 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))

mod1_count <- bru(
  formula1,
  data = EA_data,
  family = "lognormal",
  options = list(
    control.compute = list(
      waic = TRUE,
      cpo  = TRUE,
      dic  = TRUE
    ),
    control.inla = list(
      int.strategy = "eb"
    ),
    verbose = FALSE,
    num.threads = "1"
  )
)

summary(mod1_count)

#------------------------------------------------------------------------------
# Predict Growth Factor
#Generate Posterior Mean growth_factor Samples for Training Data (EA_data)
#------------------------------------------------------------------------------

# Get the precision (variance parameter) and convert to std deviation
sd <- sqrt(1/mod1_count$summary.hyperpar["Precision for the lognormal observations","mean"])

#Generate samples
mu_samples <- generate(
  mod1_count,
  newdata = EA_data,
  formula = ~ (Intercept + google_v2_5 +
                    Random_rural_urban_eval(rural_urban_id)),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for Training Data
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(EA_data),
        meanlog = mu_samples,
        sdlog = sd)))


# Summarize Growth Factor Predictions for Training Data

train_prediction_summary <- tibble(
  # Observed
  observed_growth_factor   = EA_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975)
)

# Calculate Coverage Proportions for Train Data
train_prediction_summary <- train_prediction_summary %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper
  )

# Train Data Metrics/ Validations

growth_factor_metrics1 <- train_prediction_summary %>%
  mutate(
    residual = observed_growth_factor - predicted_growth_factor
  ) %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(observed_growth_factor, predicted_growth_factor),
    Coverage     = mean(growth_factor_covered)
  )

growth_factor_metrics1 %>%
  kable(digits = 3)


#------------------------------------------------------------------------------
# Generate Posterior Mean growth_factor Samples for Full Data for 2024
#------------------------------------------------------------------------------

#Generate samples
mu_samples <- generate(
  mod1_count,
  newdata = pop_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for 2024
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(pop_data),
        meanlog = mu_samples,
        sdlog = sd)))

#------------------------------------------------------------------------------
# Calculate 2024 Estimates = HH_Count_2018 * predicted growth_factors
#Where Growth factor = (Growth factor Estimates)^2024-2018 = 
# = Growth factor estimates ^ 6
#------------------------------------------------------------------------------

hh_draws_2024 <- growth_factor_draws %>%
  mutate(
    across(
      everything(),
      ~ .x^6 * pop_data$hh_count_2018
    )
  )

#------------------------------------------------------------------------------
# Summarise Predictions for 2024
#------------------------------------------------------------------------------

 prediction_summary_2024 <- tibble(
  #Admin 
  EA_CODE = pop_data$EA_CODE,
  ADM_STATUS = pop_data$ADM_STATUS,
  REG_NAME = pop_data$REG_NAME,
  DIST_NAME = pop_data$DIST_NAME,
  hh_count_2018 = pop_data$hh_count_2018,
  hh_count_2024 = pop_data$hh_count_2024,
  hh_count_2026 = pop_data$hh_count_2026,


  # Observed
  observed_growth_factor   = pop_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975),
  
  # 2024 hh Estimates
  predicted_hh_count_2024 = rowMeans(hh_draws_2024),
  hh_lower_2024 = apply(hh_draws_2024,1,quantile,probs = 0.025),
  hh_upper_2024 = apply(hh_draws_2024,1,quantile,probs = 0.975)
)

#------------------------------------------------------------------------------
# Calculate Coverage Proportions
#------------------------------------------------------------------------------

prediction_summary_2024 <- prediction_summary_2024 %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper,
    
    #2024 HH Count
    hh_covered_2024 = hh_count_2024 >= hh_lower_2024 & hh_count_2024 <= hh_upper_2024
  )

#------------------------------------------------------------------------------
# Totals
#------------------------------------------------------------------------------

#2024 Overall total
sum(prediction_summary_2024$predicted_hh_count_2024)

#Check observed Vs Predicted 
prediction_summary_2024 %>%
  drop_na(hh_count_2024) %>%  
  summarise(
    observed_total  = sum(hh_count_2024, na.rm = TRUE),
    predicted_total = sum(predicted_hh_count_2024, na.rm = TRUE)
  ) 

#------------------------------------------------------------------------------
# Validate 2024 Predictions Against Observed HH Count
#------------------------------------------------------------------------------
predictions_2024 <- prediction_summary_2024 %>% 
  drop_na(hh_count_2024) %>% 
  filter(hh_count_2024 > 17) %>% 
  mutate(residual = hh_count_2024 - predicted_hh_count_2024)
      

val1_2024 <- predictions_2024 %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(hh_count_2024, predicted_hh_count_2024),
    Coverage     = mean(hh_covered_2024)
  )

val1_2024 %>%
  kable(digits = 3)


##############################################################################
#############################################################################
#############################################################################
###############################################################################
#Model 2 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect

#------------------------------------------------------------------------------
# Fit lognormal Model
#------------------------------------------------------------------------------

formula2 <- growth_factor ~  google_v2_5 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))


mod2_count <- bru(
  formula2,
  data = EA_data,
  family = "lognormal",
  options = list(
    control.compute = list(
      waic = TRUE,
      cpo  = TRUE,
      dic  = TRUE
    ),
    control.inla = list(
      int.strategy = "eb"
    ),
    verbose = FALSE,
    num.threads = "1"
  )
)

summary(mod2_count)

#------------------------------------------------------------------------------
# Predict Growth Factor
#Generate Posterior Mean growth_factor Samples for Training Data (EA_data)
#------------------------------------------------------------------------------

# Get the precision (variance parameter) and convert to std deviation
sd <- sqrt(1/mod2_count$summary.hyperpar["Precision for the lognormal observations","mean"])

#Generate samples
mu_samples <- generate(
  mod2_count,
  newdata = EA_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id)),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for Training Data
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(EA_data),
        meanlog = mu_samples,
        sdlog = sd)))


# Summarize Growth Factor Predictions for Training Data

train_prediction_summary <- tibble(
  # Observed
  observed_growth_factor   = EA_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975)
)

# Calculate Coverage Proportions for Train Data
train_prediction_summary <- train_prediction_summary %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper
  )

# Train Data Metrics/ Validations

growth_factor_metrics2 <- train_prediction_summary %>%
  mutate(
    residual = observed_growth_factor - predicted_growth_factor
  ) %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(observed_growth_factor, predicted_growth_factor),
    Coverage     = mean(growth_factor_covered)
  )

growth_factor_metrics2 %>%
  kable(digits = 3)


#------------------------------------------------------------------------------
# Generate Posterior Mean growth_factor Samples for Full Data for 2024
#------------------------------------------------------------------------------

#Generate samples
mu_samples <- generate(
  mod2_count,
  newdata = pop_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id)),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for 2024
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(pop_data),
        meanlog = mu_samples,
        sdlog = sd)))

#------------------------------------------------------------------------------
# Calculate 2024 Estimates = HH_Count_2018 * predicted growth_factors
#Where Growth factor = (Growth factor Estimates)^2024-2018 = 
# = Growth factor estimates ^ 6
#------------------------------------------------------------------------------

hh_draws_2024 <- growth_factor_draws %>%
  mutate(
    across(
      everything(),
      ~ .x^6 * pop_data$hh_count_2018
    )
  )

#------------------------------------------------------------------------------
# Summarise Predictions for 2024
#------------------------------------------------------------------------------

prediction_summary_2024 <- tibble(
  #Admin 
  EA_CODE = pop_data$EA_CODE,
  ADM_STATUS = pop_data$ADM_STATUS,
  REG_NAME = pop_data$REG_NAME,
  DIST_NAME = pop_data$DIST_NAME,
  hh_count_2018 = pop_data$hh_count_2018,
  hh_count_2024 = pop_data$hh_count_2024,
  hh_count_2026 = pop_data$hh_count_2026,
  
  
  # Observed
  observed_growth_factor   = pop_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975),
  
  # 2024 hh Estimates
  predicted_hh_count_2024 = rowMeans(hh_draws_2024),
  hh_lower_2024 = apply(hh_draws_2024,1,quantile,probs = 0.025),
  hh_upper_2024 = apply(hh_draws_2024,1,quantile,probs = 0.975)
)

#------------------------------------------------------------------------------
# Calculate Coverage Proportions
#------------------------------------------------------------------------------

prediction_summary_2024 <- prediction_summary_2024 %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper,
    
    #2024 HH Count
    hh_covered_2024 = hh_count_2024 >= hh_lower_2024 & hh_count_2024 <= hh_upper_2024
  )

#------------------------------------------------------------------------------
# Totals
#------------------------------------------------------------------------------

#2024 Overall total
sum(prediction_summary_2024$predicted_hh_count_2024)

#Check observed Vs Predicted 
prediction_summary_2024 %>%
  drop_na(hh_count_2024) %>%  
  summarise(
    observed_total  = sum(hh_count_2024, na.rm = TRUE),
    predicted_total = sum(predicted_hh_count_2024, na.rm = TRUE)
  ) 

#------------------------------------------------------------------------------
# Validate 2024 Predictions Against Observed HH Count
#------------------------------------------------------------------------------
predictions_2024 <- prediction_summary_2024 %>% 
  drop_na(hh_count_2024) %>% 
  filter(hh_count_2024 > 17) %>% 
  mutate(residual = hh_count_2024 - predicted_hh_count_2024)


val2_2024 <- predictions_2024 %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(hh_count_2024, predicted_hh_count_2024),
    Coverage     = mean(hh_covered_2024)
  )

val2_2024 %>%
  kable(digits = 3)

#############################################################################
#############################################################################
###############################################################################
#Model 3 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect + EA Random_Effect

#------------------------------------------------------------------------------
# Fit lognormal Model
#------------------------------------------------------------------------------
formula3 <- growth_factor ~  google_v2_5 +
  Random_rural_urban(rural_urban_id, model = "iid",  mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
  Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups))


mod3_count <- bru(
  formula3,
  data = EA_data,
  family = "lognormal",
  options = list(
    control.compute = list(
      waic = TRUE,
      cpo  = TRUE,
      dic  = TRUE
    ),
    control.inla = list(
      int.strategy = "eb"
    ),
    verbose = FALSE,
    num.threads = "1"
  )
)

summary(mod3_count)


#------------------------------------------------------------------------------
# Predict Growth Factor
#Generate Posterior Mean growth_factor Samples for Training Data (EA_data)
#------------------------------------------------------------------------------

# Get the precision (variance parameter) and convert to std deviation
sd <- sqrt(1/mod3_count$summary.hyperpar["Precision for the lognormal observations","mean"])

#Generate samples
mu_samples <- generate(
  mod3_count,
  newdata = EA_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id) +
                 Random_EA_eval(id)),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for Training Data
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(EA_data),
        meanlog = mu_samples,
        sdlog = sd)))


# Summarize Growth Factor Predictions for Training Data

train_prediction_summary <- tibble(
  # Observed
  observed_growth_factor   = EA_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975)
)

# Calculate Coverage Proportions for Train Data
train_prediction_summary <- train_prediction_summary %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper
  )

# Train Data Metrics/ Validations

growth_factor_metrics3 <- train_prediction_summary %>%
  mutate(
    residual = observed_growth_factor - predicted_growth_factor
  ) %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(observed_growth_factor, predicted_growth_factor),
    Coverage     = mean(growth_factor_covered)
  )

growth_factor_metrics3 %>%
  kable(digits = 3)


#------------------------------------------------------------------------------
# Generate Posterior Mean growth_factor Samples for Full Data for 2024
#------------------------------------------------------------------------------

#Generate samples
mu_samples <- generate(
  mod3_count,
  newdata = pop_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id)),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Extract EA Precision Parameter (EA Random Effect) and convert to sd
iid.sd <- sqrt(1 / mod3_count$summary.hyperpar["Precision for Random_EA",1])
Random_EA_eval <- matrix(rnorm(nrow(pop_data)*n.samples, 0, iid.sd),
                         nrow = nrow(pop_data),
                         ncol = n.samples)


# Add Parameters values to mu_samples
#Add random effect to data
mu_samples <- mu_samples + Random_EA_eval

# Simulate Posterior Predictive growth_factor for 2024
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(pop_data),
        meanlog = mu_samples,
        sdlog = sd)))

#------------------------------------------------------------------------------
# Calculate 2024 Estimates = HH_Count_2018 * predicted growth_factors
#Where Growth factor = (Growth factor Estimates)^2024-2018 = 
# = Growth factor estimates ^ 6
#------------------------------------------------------------------------------

hh_draws_2024 <- growth_factor_draws %>%
  mutate(
    across(
      everything(),
      ~ .x^6 * pop_data$hh_count_2018
    )
  )

#------------------------------------------------------------------------------
# Summarise Predictions for 2024
#------------------------------------------------------------------------------

prediction_summary_2024 <- tibble(
  #Admin 
  EA_CODE = pop_data$EA_CODE,
  ADM_STATUS = pop_data$ADM_STATUS,
  REG_NAME = pop_data$REG_NAME,
  DIST_NAME = pop_data$DIST_NAME,
  hh_count_2018 = pop_data$hh_count_2018,
  hh_count_2024 = pop_data$hh_count_2024,
  hh_count_2026 = pop_data$hh_count_2026,
  
  
  # Observed
  observed_growth_factor   = pop_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975),
  
  # 2024 hh Estimates
  predicted_hh_count_2024 = rowMeans(hh_draws_2024),
  hh_lower_2024 = apply(hh_draws_2024,1,quantile,probs = 0.025),
  hh_upper_2024 = apply(hh_draws_2024,1,quantile,probs = 0.975)
)

#------------------------------------------------------------------------------
# Calculate Coverage Proportions
#------------------------------------------------------------------------------

prediction_summary_2024 <- prediction_summary_2024 %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper,
    
    #2024 HH Count
    hh_covered_2024 = hh_count_2024 >= hh_lower_2024 & hh_count_2024 <= hh_upper_2024
  )

#------------------------------------------------------------------------------
# Totals
#------------------------------------------------------------------------------

#2024 Overall total
sum(prediction_summary_2024$predicted_hh_count_2024)

#Check observed Vs Predicted 
prediction_summary_2024 %>%
  drop_na(hh_count_2024) %>%  
  summarise(
    observed_total  = sum(hh_count_2024, na.rm = TRUE),
    predicted_total = sum(predicted_hh_count_2024, na.rm = TRUE)
  ) 

#------------------------------------------------------------------------------
# Validate 2024 Predictions Against Observed HH Count
#------------------------------------------------------------------------------
predictions_2024 <- prediction_summary_2024 %>% 
  drop_na(hh_count_2024) %>% 
  filter(hh_count_2024 > 17) %>% 
  mutate(residual = hh_count_2024 - predicted_hh_count_2024)


val3_2024 <- predictions_2024 %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(hh_count_2024, predicted_hh_count_2024),
    Coverage     = mean(hh_covered_2024)
  )

val3_2024 %>%
  kable(digits = 3)

#############################################################################
##############################################################################
#############################################################################
# Geostatistical Model - INLA SPDE ----------------------------------------

#-Define the coordinates of centroids
coords <- cbind(EA_data$long, EA_data$lat) 

#measure distance between coordinates
summary(dist(coords)) #summarizes the Euclidean distance between points in the spatial domain


#build non-convex hull mesh
non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
mesh <- fm_mesh_2d_inla(boundary = non_convex_bdry, max.edge=c(0.1, 1), 
                        offset = c(0.05, 1),
                        cutoff = 0.003)

plot(mesh)
plot(mesh, add=T)
points(coords, col="red", pch="*")

#Count of mesh nodes
mesh$n

#Build the SPDE
spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

#------------------------------------------------------------------------------
# Fit lognormal Model
#------------------------------------------------------------------------------

formula4 <- growth_factor ~ google_v2_5 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
  Random_Spat(main = coords, model = spde)

mod4_count <- bru(
  formula4,
  data = EA_data,
  family = "lognormal",
  options = list(
    control.compute = list(
      waic = TRUE,
      cpo  = TRUE,
      dic  = TRUE
    ),
    control.inla = list(
      int.strategy = "eb"
    ),
    verbose = FALSE,
    num.threads = "1"
  )
)

summary(mod4_count)

#------------------------------------------------------------------------------
# Predict Growth Factor
#Generate Posterior Mean growth_factor Samples for Training Data (EA_data)
#------------------------------------------------------------------------------

# Get the precision (variance parameter) and convert to std deviation
sd <- sqrt(1/mod4_count$summary.hyperpar["Precision for the lognormal observations","mean"])

#Generate samples
mu_samples <- generate(
  mod4_count,
  newdata = EA_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id) +
                 Random_Spat_eval(cbind(long, lat))),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for Training Data
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(EA_data),
        meanlog = mu_samples,
        sdlog = sd)))


# Summarize Growth Factor Predictions for Training Data

train_prediction_summary <- tibble(
  # Observed
  observed_growth_factor   = EA_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975)
)

# Calculate Coverage Proportions for Train Data
train_prediction_summary <- train_prediction_summary %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper
  )

# Train Data Metrics/ Validations

growth_factor_metrics4 <- train_prediction_summary %>%
  mutate(
    residual = observed_growth_factor - predicted_growth_factor
  ) %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(observed_growth_factor, predicted_growth_factor),
    Coverage     = mean(growth_factor_covered)
  )

growth_factor_metrics4 %>%
  kable(digits = 3)


#------------------------------------------------------------------------------
# Generate Posterior Mean growth_factor Samples for Full Data for 2024
#------------------------------------------------------------------------------

#Generate samples
mu_samples <- generate(
  mod4_count,
  newdata = pop_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id) +
                 Random_Spat_eval(cbind(long, lat))),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for 2024
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(pop_data),
        meanlog = mu_samples,
        sdlog = sd)))

#------------------------------------------------------------------------------
# Calculate 2024 Estimates = HH_Count_2018 * predicted growth_factors
#Where Growth factor = (Growth factor Estimates)^2024-2018 = 
# = Growth factor estimates ^ 6
#------------------------------------------------------------------------------

hh_draws_2024 <- growth_factor_draws %>%
  mutate(
    across(
      everything(),
      ~ .x^6 * pop_data$hh_count_2018
    )
  )

#------------------------------------------------------------------------------
# Summarise Predictions for 2024
#------------------------------------------------------------------------------

prediction_summary_2024 <- tibble(
  #Admin 
  EA_CODE = pop_data$EA_CODE,
  cluster_id = pop_data$cluster_id,
  ADM_STATUS = pop_data$ADM_STATUS,
  REG_NAME = pop_data$REG_NAME,
  DIST_NAME = pop_data$DIST_NAME,
  hh_count_2018 = pop_data$hh_count_2018,
  hh_count_2024 = pop_data$hh_count_2024,
  hh_count_2026 = pop_data$hh_count_2026,
  
  
  # Observed
  observed_growth_factor   = pop_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975),
  
  # 2024 hh Estimates
  predicted_hh_count_2024 = rowMeans(hh_draws_2024),
  hh_lower_2024 = apply(hh_draws_2024,1,quantile,probs = 0.025),
  hh_upper_2024 = apply(hh_draws_2024,1,quantile,probs = 0.975)
)

#------------------------------------------------------------------------------
# Calculate Coverage Proportions
#------------------------------------------------------------------------------

prediction_summary_2024 <- prediction_summary_2024 %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper,
    
    #2024 HH Count
    hh_covered_2024 = hh_count_2024 >= hh_lower_2024 & hh_count_2024 <= hh_upper_2024
  )

#------------------------------------------------------------------------------
# Totals
#------------------------------------------------------------------------------

#2024 Overall total
sum(prediction_summary_2024$predicted_hh_count_2024)

#Check observed Vs Predicted 
prediction_summary_2024 %>%
  drop_na(hh_count_2024) %>%  
  summarise(
    observed_total  = sum(hh_count_2024, na.rm = TRUE),
    predicted_total = sum(predicted_hh_count_2024, na.rm = TRUE)
  ) 

#------------------------------------------------------------------------------
# Validate 2024 Predictions Against Observed HH Count
#------------------------------------------------------------------------------
predictions_2024 <- prediction_summary_2024 %>% 
  drop_na(hh_count_2024) %>% 
  filter(hh_count_2024 > 17) %>% 
  mutate(residual = hh_count_2024 - predicted_hh_count_2024)


val4_2024 <- predictions_2024 %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(hh_count_2024, predicted_hh_count_2024),
    Coverage     = mean(hh_covered_2024)
  )

val4_2024 %>%
  kable(digits = 3)


# Model Checks ------------------------------------------------------------

#DIC
t(c(mod1_count=mod1_count$dic$dic, mod2_count=mod2_count$dic$dic,
    mod3_count=mod3_count$dic$dic, mod4_count=mod4_count$dic$dic))

#compare models
#Growth factor metrics
growth_factor_metrics <- rbind(growth_factor_metrics1, growth_factor_metrics2,
                               growth_factor_metrics3, growth_factor_metrics4)
growth_factor_metrics %>%  kable()

# 2024 Validation Metrics

metrics_2024 <- rbind(val1_2024, val2_2024, val3_2024, val4_2024)
metrics_2024 %>%  kable()

#Selected predicted hh count for 2024 to be used to train 2024 and 2026 estimate
predicted_hh_count_2024 <- prediction_summary_2024 %>% 
  select(cluster_id, predicted_hh_count_2024)

################## END OF GROWTH FACTOR 2018 to 2024 #########################
###############################################################################
###############################################################################

#Remove all object except the ones listed
rm(list = setdiff(ls(), c("drive_path", "input_path", "output_path", 
                          "shapefile_path", "pop_output", "predicted_hh_count_2024"))) 

# Clear console
cat("\014")


#################################################################################################
#################################################################################################
############ GROWTH FACTOR 2024 to 2026 MODEL #################################################################
################################################################################################

# Load 2024 Data
pop_data <-  read.csv(paste0(input_path, "Malawi_2024_data.csv")) 

#Join 2024 Predictions to Data
pop_data <- pop_data %>% 
  inner_join(predicted_hh_count_2024, by = "cluster_id")

#names
names(pop_data)

#create unique id for each district
pop_data <- pop_data %>% 
  group_by(DIST_NAME) %>%
  mutate(dist_id = cur_group_id()) %>%
  ungroup() 

#Create id for rural urban
pop_data <- pop_data %>% 
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1))

# Create a nested ids
pop_data <- pop_data %>%
  group_by(rural_urban_id, dist_id, REG_CODE) %>%
  mutate(nested_id = cur_group_id()) %>%
  ungroup()

#check summary of household count for 2026
summary(pop_data$hh_count_2026)  

#Calculate the ratio between Predicted 2024 HH Count to 2026 hh count
pop_data <- pop_data %>% 
  mutate(ratio = (hh_count_2026/predicted_hh_count_2024))

#Find the annual multiplicative growth factor
# What constant yearly multiplication factor would produce the observed 2 years increase?

pop_data <- pop_data %>% 
  mutate(growth_factor = ratio^0.5)  #2026 - 2024 = 2years ie 1/2

#check summary stats 
summary(pop_data$growth_factor)

###########################################################################
############################################################################
# Visualize the distribution of data and clean the data

#filter growth factor which is NA
EA_data <- pop_data %>% 
  drop_na(growth_factor) %>%   # drop NA 
  filter(!is.infinite(growth_factor))  #drop Infinity values

#check summary stats 
summary(EA_data$growth_factor)   #Summary of growth factor
summary(EA_data$hh_count_2026)   #Summary of 2026 hh count

#Boxplot of growth_factor distribution
ggplot(data = EA_data, aes(y=growth_factor))+
  geom_boxplot(color="blue", alpha=0.2)


# Density plot of growth_factor
ggplot(data = EA_data, aes(x = growth_factor)) +
  geom_density(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "household growth_factor",
    y = "Density",
    title = "Density Plot of household growth_factor"
  ) +
  theme_minimal()

#plot HH Count 2026
ggplot(data = EA_data, aes(x = hh_count_2026)) +
  geom_histogram(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "Household Count",
    y = "Density",
    title = "Density Plot of Household Count"
  ) +
  theme_minimal()

###############################################################################
###############################################################################

# Get Random Effect Indexing ----------------------------------------------

#Assign unique values to each row
EA_data <- EA_data %>% 
  tibble::rowid_to_column("id")

#get distinct count of rural urban
rural_urban_group <- EA_data %>% 
  distinct(rural_urban_id) %>% 
  nrow()

# #get distinct count of district
dist_groups <- EA_data %>% 
  distinct(dist_id) %>% 
  nrow()

# #get distinct count of EA
ea_groups <- EA_data %>% 
  distinct(id) %>% 
  nrow()

#Get distinct count of nesting
nested_group <- EA_data %>% 
  distinct(nested_id) %>% 
  nrow()

#Specify the number of samples to draw

n.samples <- 100

#############################################################################
##############################################################################
#############################################################################
# Geostatistical Model - INLA SPDE ----------------------------------------

#-Define the coordinates of centroids
coords <- cbind(EA_data$long, EA_data$lat) 

#measure distance between coordinates
summary(dist(coords)) #summarizes the Euclidean distance between points in the spatial domain


#build non-convex hull mesh
non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
mesh <- fm_mesh_2d_inla(boundary = non_convex_bdry, max.edge=c(0.1, 1), 
                        offset = c(0.05, 1),
                        cutoff = 0.003)

plot(mesh)
plot(mesh, add=T)
points(coords, col="red", pch="*")

#Count of mesh nodes
mesh$n

#Build the SPDE
spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

#------------------------------------------------------------------------------
# Fit lognormal Model
#------------------------------------------------------------------------------

formula5 <- growth_factor ~ google_v2_5 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
  Random_Spat(main = coords, model = spde)

mod5_count <- bru(
  formula5,
  data = EA_data,
  family = "lognormal",
  options = list(
    control.compute = list(
      waic = TRUE,
      cpo  = TRUE,
      dic  = TRUE
    ),
    control.inla = list(
      int.strategy = "eb"
    ),
    verbose = FALSE,
    num.threads = "1"
  )
)

summary(mod5_count)

#------------------------------------------------------------------------------
# Predict Growth Factor
#Generate Posterior Mean growth_factor Samples for Training Data (EA_data)
#------------------------------------------------------------------------------

# Get the precision (variance parameter) and convert to std deviation
sd <- sqrt(1/mod5_count$summary.hyperpar["Precision for the lognormal observations","mean"])

#Generate samples
mu_samples <- generate(
  mod5_count,
  newdata = EA_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id) +
                 Random_Spat_eval(cbind(long, lat))),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for Training Data
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(EA_data),
        meanlog = mu_samples,
        sdlog = sd)))


# Summarize Growth Factor Predictions for Training Data

train_prediction_summary <- tibble(
  # Observed
  observed_growth_factor   = EA_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975)
)

# Calculate Coverage Proportions for Train Data
train_prediction_summary <- train_prediction_summary %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper
  )

# Train Data Metrics/ Validations

growth_factor_metrics5 <- train_prediction_summary %>%
  mutate(
    residual = observed_growth_factor - predicted_growth_factor
  ) %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(observed_growth_factor, predicted_growth_factor),
    Coverage     = mean(growth_factor_covered)
  )

growth_factor_metrics5 %>%
  kable(digits = 3)


#------------------------------------------------------------------------------
# Generate Posterior Mean growth_factor Samples for Full Data for 2026
#------------------------------------------------------------------------------

#Generate samples
mu_samples <- generate(
  mod5_count,
  newdata = pop_data,
  formula = ~ (Intercept + google_v2_5 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id) +
                 Random_Spat_eval(cbind(long, lat))),
  n.samples = n.samples,
  seed = 2,
  num.threads = "1"
)

# Simulate Posterior Predictive growth_factor for 2026
growth_factor_draws <- map_dfc(
  1:n.samples,
  ~ tibble(
    !!paste0("draw_", .x) :=
      rlnorm(
        n = nrow(pop_data),
        meanlog = mu_samples,
        sdlog = sd)))

#------------------------------------------------------------------------------
# Calculate 2026 Estimates = Predicted HH Count 2024 * predicted growth_factors
#Where Growth factor = (Growth factor Estimates)^2026-2024 = 
# = Growth factor estimates ^ 2
#------------------------------------------------------------------------------

hh_draws_2026 <- growth_factor_draws %>%
  mutate(
    across(
      everything(),
      ~ .x^2 * pop_data$predicted_hh_count_2024
    )
  )

#------------------------------------------------------------------------------
# Summarise Predictions for 2026
#------------------------------------------------------------------------------

prediction_summary_2026 <- tibble(
  #Admin 
  EA_CODE = pop_data$EA_CODE,
  cluster_id = pop_data$cluster_id,
  ADM_STATUS = pop_data$ADM_STATUS,
  REG_NAME = pop_data$REG_NAME,
  DIST_NAME = pop_data$DIST_NAME,
  hh_count_2018 = pop_data$hh_count_2018,
  hh_count_2024 = pop_data$hh_count_2024,
  hh_count_2026 = pop_data$hh_count_2026,
  predicted_hh_count_2024 = pop_data$predicted_hh_count_2024,
  
  # Observed
  observed_growth_factor   = pop_data$growth_factor,
  # growth_factor predictions
  predicted_growth_factor = rowMeans(growth_factor_draws),
  growth_factor_lower = apply(growth_factor_draws, 1,quantile,probs = 0.025),
  growth_factor_upper = apply(growth_factor_draws, 1,quantile,probs = 0.975),
  
  # 2026 hh Estimates
  predicted_hh_count_2026 = rowMeans(hh_draws_2026),
  hh_lower_2026 = apply(hh_draws_2026,1,quantile,probs = 0.025),
  hh_upper_2026 = apply(hh_draws_2026,1,quantile,probs = 0.975)
)

#------------------------------------------------------------------------------
# Calculate Coverage Proportions
#------------------------------------------------------------------------------

prediction_summary_2026 <- prediction_summary_2026 %>%
  mutate(
    #growth_factor
    growth_factor_covered =observed_growth_factor >= growth_factor_lower & observed_growth_factor <= growth_factor_upper,
    
    #2026 HH Count
    hh_covered_2026 = hh_count_2026 >= hh_lower_2026 & hh_count_2026 <= hh_upper_2026
  )

#------------------------------------------------------------------------------
# Totals
#------------------------------------------------------------------------------

#2026 Overall total
sum(prediction_summary_2026$predicted_hh_count_2026)

#Check observed Vs Predicted 
prediction_summary_2026 %>%
  drop_na(hh_count_2026) %>%  
  summarise(
    observed_total  = sum(hh_count_2026, na.rm = TRUE),
    predicted_total = sum(predicted_hh_count_2026, na.rm = TRUE)
  ) 

#------------------------------------------------------------------------------
# Validate 2026 Predictions Against Observed HH Count
#------------------------------------------------------------------------------
predictions_2026 <- prediction_summary_2026 %>% 
  drop_na(hh_count_2026) %>% 
  mutate(residual = hh_count_2026 - predicted_hh_count_2026)


val_2026 <- predictions_2026 %>%
  summarise(
    Bias         = mean(residual),
    Imprecision  = sd(residual),
    MAE          = mean(abs(residual)),
    MSE          = mean(residual^2),
    RMSE         = sqrt(MSE),
    Corr         = cor(hh_count_2026, predicted_hh_count_2026),
    Coverage     = mean(hh_covered_2026)
  )

val_2026 %>%
  kable(digits = 3)

# Write Results to file
write.csv(predictions_2026, paste0(pop_path, "Growth_Factor_Full_Posterior.csv"), row.names = F)

###############################################################################
##############################################################################
##############################################################################
########### VISUALIZE 2026 ESTIMATES ########################################

#------------------------------------------------------------------------------
# Get Estimates
#------------------------------------------------------------------------------
# Calculate the posterior mean, median, and 95% CI for the 2026 total
summary_2026 <- hh_draws_2026 %>%
  as_tibble() %>%
  
  #Sum up all households (rows) for each individual draw column
  summarise(across(starts_with("draw_"), \ (x) sum(x, na.rm = TRUE))) %>%
  
  #Pivot the sums into a single long column called 'total_hh'
  pivot_longer(
    cols = everything(), 
    names_to = "draw", 
    values_to = "total_hh"
  ) %>%
  
  #Calculate your summary statistics across all the draw totals
  summarise(
    mean           = mean(total_hh),
    lower_quantile = quantile(total_hh, 0.025),
    upper_quantile = quantile(total_hh, 0.975),
    median         = quantile(total_hh, 0.500)
  )
summary_2026 %>%
  kable(digits = 3)

#Calculate uncertainty
predictions_2026 <- predictions_2026 %>% 
  mutate(uncertainty = (hh_upper_2026 - hh_lower_2026)/predicted_hh_count_2026,
         range = hh_upper_2026 - hh_lower_2026)

#mean Uncertainty
mean(predictions_2026$uncertainty)
#----------------------------------------------------------
# Prepare data
#----------------------------------------------------------

plot_data <- predictions_2026 %>% 
  select(hh_count_2026, predicted_hh_count_2026) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(Variable,
                      hh_count_2026 = "Survey 2026 Data",
                      predicted_hh_count = "Predictions")
  )

#----------------------------------------------------------
# Create summary statistics
#----------------------------------------------------------

summary_stats <- plot_data %>%
  group_by(Variable) %>%
  summarise(
    Min    = round(min(Value, na.rm = TRUE), 1),
    Q1     = round(quantile(Value, 0.25, na.rm = TRUE), 1),
    Median = round(median(Value, na.rm = TRUE), 1),
    Mean   = round(mean(Value, na.rm = TRUE), 1),
    Q3     = round(quantile(Value, 0.75, na.rm = TRUE), 1),
    Max    = round(max(Value, na.rm = TRUE), 1)
  ) %>%
  mutate(
    label = paste0(
      "Min = ", Min,
      "\nQ1 = ", Q1,
      "\nMedian = ", Median,
      "\nMean = ", Mean,
      "\nQ3 = ", Q3,
      "\nMax = ", Max
    )
  )

#----------------------------------------------------------
# Boxplot
#----------------------------------------------------------

ggplot(plot_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(width = 0.6, alpha = 0.7) +
  
  # Add summary statistics text
  geom_text(
    data = summary_stats,
    aes(
      x = Variable,
      y = 750,
      label = label
    ),
    #vjust = 1.1,#1.1,
    hjust = 1.1,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Comparison - Survey 2026 Data HH Count and Predicted EA Estimates",
    x = "",
    y = "Household Count"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )

#----------------------------------------------------------
# Density Plot
#----------------------------------------------------------
ggplot(plot_data, aes(x = Value, fill = Variable)) +
  geom_density(
    position = "identity",
    #bins = 30,
    alpha = 0.4   # transparency
  ) +
  labs(
    title = "Overlayed Histogram: Predictions vs Survey 2026 Data",
    x = "Household Count",
    y = "Frequency"
  ) +
  theme_minimal()

# Compute observed coverage
ea_plot <- predictions_2026 %>%
  mutate(
    covered = hh_count_2026 >= hh_lower_2026 & hh_count_2026 <= hh_upper_2026)

#Overall coverage rate
coverage_rate <- mean(ea_plot$covered)
coverage_rate
coverage_percent <- round(coverage_rate * 100, 3)

##########################################################################
# Plot of Predictions Vs Validation ---------------------------------------

# Plot
ggplot(ea_plot, aes(x = hh_count_2026, y = predicted_hh_count_2026)) +
  
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
    aes(ymin = hh_lower_2026, ymax = hh_upper_2026),
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
  
  theme_minimal(base_size = 15) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

################################################################################
################################################################################

# Proportion Covered ------------------------------------------------------

ggplot(ea_plot,
       aes(x = hh_count_2026,
           y = predicted_hh_count_2026,
           color = covered)) +
  
  # Prediction intervals
  geom_errorbar(
    aes(ymin = hh_lower_2026, ymax = hh_upper_2026),
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
  
  #coord_equal() +
  
  labs(
    title = "Survey-2026 Interval Coverage at EA Level",
    subtitle = paste0(
      "Coverage Probability = ",
      coverage_percent,
      "%"
    ),
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
  )

################################################################
################################################################

# Rural Urban -------------------------------------------------------------

#Coverage
coverage_by_rural_urban <- ea_plot %>%
  group_by(ADM_STATUS) %>%
  summarise(
    coverage = mean(covered),
    n = n()
  ) %>%
  arrange(coverage)

coverage_by_rural_urban


rural_urban <- predictions_2026 %>% 
  select(ADM_STATUS, hh_count_2026, hh_lower_2026, predicted_hh_count_2026, hh_upper_2026)

validation_rural_urban <- rural_urban %>% 
  group_by(ADM_STATUS) %>% 
  mutate(
    residual = hh_count_2026 - predicted_hh_count_2026,
    eps = 1e-10,
    P = (hh_count_2026 + eps) / sum(hh_count_2026 + eps),
    Q = (predicted_hh_count_2026 + eps) / sum(predicted_hh_count_2026 + eps)
  ) %>%
  summarise(
    n           = n(),
    Bias        = mean(residual),
    Imprecision = sd(residual, na.rm = TRUE),
    MAE         = mean(abs(residual)),
    MSE         = mean(residual^2),
    RMSE        = sqrt(MSE),
    Corr        = cor(hh_count_2026, predicted_hh_count_2026, use = "complete.obs"),
    KL          = sum(P * log(P/Q))
  )


validation_rural_urban

#Plot of rural vs urban
ggplot(rural_urban,
       aes(x = hh_count_2026,
           y = predicted_hh_count_2026,
           color = ADM_STATUS)) +
  
  # Prediction intervals
  geom_errorbar(
    aes(ymin = hh_lower_2026, ymax = hh_upper_2026),
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
    title = "Survey-2024 Vs Predictions",
    x = "Survey-2024",
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
  )


# Box plot for Rural Vs Urban ---------------------------------------------

plot_data_rural_urban <- rural_urban %>% 
  select(hh_count_2026, predicted_hh_count_2026, ADM_STATUS) %>% 
  pivot_longer(
    cols = c(hh_count_2026, predicted_hh_count_2026),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = recode(Variable,
                      hh_count_2026 = "Survey 2026 Data",
                      predicted_hh_count = "Predictions")
  )

#----------------------------------------------------------
# Create summary statistics
#----------------------------------------------------------

summary_stats <- plot_data_rural_urban %>%
  group_by(ADM_STATUS, Variable) %>%
  summarise(
    Min    = round(min(Value, na.rm = TRUE), 1),
    Q1     = round(quantile(Value, 0.25, na.rm = TRUE), 1),
    Median = round(median(Value, na.rm = TRUE), 1),
    Mean   = round(mean(Value, na.rm = TRUE), 1),
    Q3     = round(quantile(Value, 0.75, na.rm = TRUE), 1),
    Max    = round(max(Value, na.rm = TRUE), 1)
  ) %>%
  mutate(
    label = paste0(
      "Min = ", Min,
      "\nQ1 = ", Q1,
      "\nMedian = ", Median,
      "\nMean = ", Mean,
      "\nQ3 = ", Q3,
      "\nMax = ", Max
    )
  ) %>% 
  ungroup()

#----------------------------------------------------------
# Boxplot
#----------------------------------------------------------

ggplot(plot_data_rural_urban, aes(x = Variable, y = Value, fill = ADM_STATUS)) +
  geom_boxplot(width = 0.6, alpha = 0.7) +
  
  # Add summary statistics text
  geom_text(
    data = summary_stats,
    aes(
      x = Variable,
      y = 750,
      label = label
    ),
    #vjust = 1.1,#1.1,
    hjust = 1.1,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  
  labs(
    title = "Survey 2026 Vs Prediction (Rural Vs Urban)",
    x = "",
    y = "Household Count"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )+
  facet_wrap(~ADM_STATUS)

#################################################################################
#################################################################################
#################################################################################
########## DISAGREGATE 2026 PREDICTIONS TO GRIDCELL #############################

#load covariates
pred_covs <-  read_feather(paste0(input_path, "Malawi_covs_stack_2024.feather"))
r1 <- rast(paste0(input_path, "country_raster.tif"))

#Select needed variables
pred_covs <- pred_covs %>% 
  select(-starts_with("x"))

# Join 2026 Predictions to grid cells
pred_covs_2026 <- pred_covs %>% 
  inner_join(prediction_summary_2026, by = "cluster_id")

#Calculate building weight
pred_covs_2026 <- pred_covs_2026 %>% 
  group_by(EA_CODE.x) %>% 
  mutate(weight = google_v2_5/(sum(google_v2_5)))

#Multiply weight by predicted_hh_count_2016 to disaggregate to pixel level
pred_covs_2026 <- pred_covs_2026 %>% 
  mutate(grid_hh_estimates_2026 = predicted_hh_count_2026 * weight)

#Check total again
sum(pred_covs_2026$grid_hh_estimates_2026)


#Sum each pixel population totals to see if it matches ea totals

test <- pred_covs_2026 %>% 
  group_by(cluster_id) %>% 
  summarise(total_grid_estimates = sum(grid_hh_estimates_2026)) %>% 
  ungroup() %>% 
  inner_join(prediction_summary_2026, by = "cluster_id") %>% 
  select(total_grid_estimates, predicted_hh_count_2026)

# test if estimates match ea totals
all(round(test$total_grid_estimates) == round(test$predicted_hh_count_2026))  #If TRUE then it matches

##################################################################################
############### EXPORT RASTER TO FILE ##################################
###################################################################################

# #Convert to sf object
pixel_predictions  <- st_as_sf(pred_covs_2026 , coords = c("long", "lat"))
st_crs(pixel_predictions) <- 4326

#write to file
#st_write(pixel_predictions, paste0(output_path, "HH_Estimates_2026.gpkg"), append = T)

#Rasterize predictions and export to file
pred_raster  <- rasterize(pixel_predictions, r1, field = "grid_hh_estimates_2026")
plot(pred_raster)

#export
writeRaster(pred_raster,  
            paste0(output_path, "Full_Posterior_HH_Estimates_2026.tif"), 
            overwrite=TRUE, names="HH_Count")

##################END OF SCRIPT #############################################
############################################################################
############################################################################
