library(corrplot) # Correlation analysis
library(INLA) # INLA for Bayesian Analysis
library(spdep)
library(sf) # Vector data manipulation
library(car)
library(caret)
library(tictoc)
library(terra)
library(kableExtra)
library(inlabru)
library(feather)
library(tidyverse)


options(scipen = 999)

# Specify Drive Path
drive_path <- "C:/Users/oy1r22/OneDrive - University of Southampton/Desktop/Malawi_Workshop/"
input_path <- paste0(drive_path, "Output_Data/")
output_path <- paste0(drive_path, "Output_Data/")
shapefile_path <- paste0(drive_path, "Data/Shapefiles/")
pop_output <- paste0(drive_path, "Output_Data/Predicted_Estimates/")

# Load summarized Population data
pop_data <- read.csv(paste0(input_path, "Malawi_2024_data.csv"))
shapefile <- st_read(paste0(shapefile_path, "2018_MPHC_EAs_Final_for_Use_Corrected.shp"))

# names
names(pop_data)

# create unique id for each district
pop_data <- pop_data %>%
  group_by(DIST_NAME) %>%
  mutate(dist_id = cur_group_id()) %>%
  ungroup()

# Create id for rural urban
pop_data <- pop_data %>%
  mutate(rural_urban_id = case_when(
    ADM_STATUS == "Rural" ~ 1,
    ADM_STATUS == "Urban" ~ 2,
    ADM_STATUS == "NA" ~ 1
  ))

# Create a nested ids
pop_data <- pop_data %>%
  group_by(rural_urban_id, dist_id, REG_CODE) %>%
  mutate(nested_id = cur_group_id()) %>%
  ungroup()

# We are going to use data for 2024 for the HH Count Estimation

# We want to check a summary of our household count 2024
summary(pop_data$hh_count_2024)

# Calculate hh count density
EA_data <- pop_data %>%
  mutate(hh_density = hh_count_2024 / google_v2_5)

# check summary stats
summary(EA_data$hh_density)

# filter hh_density which is NA
EA_data <- EA_data %>%
  drop_na(hh_density) %>%
  filter(!is.infinite(hh_density))

# check summary stats again
summary(EA_data$hh_density)
summary(EA_data$hh_count_2024)
summary(EA_data$google_v2_5)

##############################################################################
#############################################################################
#############################################################################
# We have to do some data visualization and remove some outliers
# Which might impact our estimate

# Boxplot of household density distribution
ggplot(data = EA_data, aes(y = hh_density)) +
  geom_boxplot(color = "blue", alpha = 0.2)


# Density plot of household density
ggplot(data = EA_data, aes(x = hh_density)) +
  geom_density(
    fill = "blue",
    alpha = 0.4,
    color = "blue"
  ) +
  labs(
    x = "Household Density",
    y = "Density",
    title = "Density Plot of Household Density"
  ) +
  theme_minimal()

# plot HH Count
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

# plot HH Count
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


# Boxplot of HH density
ggplot(data = EA_data, aes(y = hh_density)) +
  geom_boxplot(color = "blue", alpha = 0.2)


# plot Density
ggplot(data = EA_data, aes(x = hh_density)) +
  geom_histogram(
    fill = "blue",
    alpha = 0.4,
    color = "blue"
  ) +
  labs(
    x = "Household Density",
    y = "Density",
    title = "HH Density"
  ) +
  theme_minimal()

# check summary stats
summary(EA_data$hh_density)

# Density below 30
EA_data <- EA_data %>%
  filter(hh_density < 30)

# check summary stats
summary(EA_data$hh_density)

# Density plot of household density
ggplot(data = EA_data, aes(x = hh_density)) +
  geom_density(
    fill = "blue",
    alpha = 0.4,
    color = "blue"
  ) +
  labs(
    x = "Household Density",
    y = "Density",
    title = "Density Plot of Household Density"
  ) +
  theme_minimal()

# Check building count
summary(EA_data$google_v2_5)

# Boxplot of Building Count
ggplot(data = EA_data, aes(y = google_v2_5)) +
  geom_boxplot(color = "blue", alpha = 0.2)

# Density plot of Building Count
ggplot(data = EA_data, aes(x = google_v2_5)) +
  geom_density(
    fill = "blue",
    alpha = 0.4,
    color = "blue"
  ) +
  labs(
    x = "Count Building",
    y = "Density",
    title = "Building Count"
  ) +
  theme_minimal()

# There are some outliers in building count. Removing bcount above 2000
EA_data <- EA_data %>%
  filter(google_v2_5 < 2000)

# plot the distribution again and check
# Density plot of Building Count
ggplot(data = EA_data, aes(x = google_v2_5)) +
  geom_density(
    fill = "blue",
    alpha = 0.4,
    color = "blue"
  ) +
  labs(
    x = "Count Building",
    y = "Density",
    title = "Building Count"
  ) +
  theme_minimal()

###########################################################################
###########################################################################
############ COVARIATE SELECTION #########################################
# We want to do covariate selection ---------------------------------------

# Covs selection
covs <- EA_data %>%
  select(starts_with("x")) %>%
  select(where(~ !any(is.na(.)))) # Remove covariates with NAs

# Compute Correlation Matrix
cor_matrix <- cor(covs)
cor_matrix

# Visualize the correlation matrix

corrplot(cor_matrix, method = "circle")

# Calcute mean and standard deviation of covariates
cov_stats <- data.frame(
  Covariate = colnames(covs),
  Mean = apply(covs, 2, mean, na.rm = TRUE),
  Std_Dev = apply(covs, 2, sd, na.rm = TRUE)
)

# Scaling function to scale covariates
stdize <- function(x) {
  stdz <- (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
  return(stdz)
}

# apply scaling function
covs <- apply(covs, 2, stdize) %>% # z-score
  as_tibble()

# Select response variable and cbind covs
covs_selection <- EA_data %>%
  select(hh_density) %>%
  cbind(covs)

# Stepwise Covariate Selection --------------------------------------------
# Stepwise covariates selection

# fit a glm model to select covariates
full_model <- glm(hh_density ~ ., data = covs_selection)

# Model summary
summary(full_model)

# stepwise selection
step_model1 <- MASS::stepAIC(full_model, direction = "both")
summary(step_model1)
vif(step_model1)


# Function to iteratively drop variables with high VIF
drop_high_vif <- function(model, threshold = 5) {
  # Calculate VIFs
  vif_values <- vif(model)

  # Loop until all VIFs are below the threshold
  while (any(vif_values > threshold)) {
    # Find the variable with the highest VIF
    max_vif_var <- names(which.max(vif_values))

    # Update the formula to exclude the variable with the highest VIF
    formula <- as.formula(paste(". ~ . -", max_vif_var))
    model <- update(model, formula)

    # Recalcuye VIFs
    vif_values <- vif(model)
  }

  return(model)
}


# Apply the function to drop high VIF variables
step1_updated <- drop_high_vif(step_model1)
summary(step1_updated)
vif(step1_updated)

# Extract selected variables
selected_vars <- step1_updated$coefficients %>%
  names() # Get the selected variables

# Create model formula
formula_string <- paste("hh_density ~", paste(selected_vars, collapse = " + "))
final_formula <- as.formula(formula_string)

# Print final model formula
print(final_formula)

# function to drop non-significant variables
# Start with full model
current_formula <- as.formula("hh_density ~  x13 + x35 + x37 + x38 + x40 + x42 +
    x44 + x49 + x50 + x54 + x55 + x56 + x57 + x58 + x59 + x61 +
    x62 + x63")

# Loop to drop non-significant variables
repeat {
  model <- glm(current_formula, data = covs_selection, family = gaussian)
  model_summary <- summary(model)

  # Extract p-values (skip intercept)
  p_vals <- coef(model_summary)[-1, "Pr(>|t|)"]

  # Identify variable with highest p-value
  max_pval <- max(p_vals, na.rm = TRUE)
  worst_var <- names(p_vals)[which.max(p_vals)]

  # Stop if all p-values < 0.05
  if (max_pval < 0.05) break

  # Drop the variable with the highest p-value
  message("Dropping variable: ", worst_var, " (p = ", signif(max_pval, 4), ")")
  rhs <- attr(terms(current_formula), "term.labels")
  new_rhs <- setdiff(rhs, worst_var)
  current_formula <- as.formula(paste("hh_density ~", paste(new_rhs, collapse = " + ")))
}

# Final model
final_model <- model
summary(final_model)
vif(final_model)


# Extract the formula from final model
final_model <- formula(final_model)
final_model

######### OPTIONAL ######################################
# You can also use lasso regression to rank the covariate importance
# important covariates

# Fit a model using the LASSO with the caret package

# drop NAs in covariates for LASSO fitting
covs_selection1 <- covs_selection %>%
  drop_na()

# Lasso Regression
fit1_lasso <- train(
  hh_density ~ x13 + x37 + x42 + x44 + x49 + x50 + x55 + x56 +
    x57 + x61 + x63,
  data = covs_selection1,
  method = "glmnet",
  metric = "RMSE", # Choose from RMSE, RSquared, AIC, BIC, ...others?
  tuneGrid = expand.grid(
    .alpha = 1, # optimize a ridge regression
    .lambda = seq(0, 5, length.out = 101)
  )
)

fit1_lasso

# Select important variables

varImp(fit1_lasso)

# Rank variables

plot(varImp(fit1_lasso))

# Selected covariates for final modelling
# Selecting covariates with importance above 10%
# x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56

# select important variables from pop_data and cbind scaled covariates for model fitting
EA_data <- EA_data %>%
  select(-starts_with("x")) %>%
  cbind(covs)

# Assign unique values to each row
EA_data <- EA_data %>%
  tibble::rowid_to_column("id")

# get distinct count of rural urban
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

# Get distinct count of nesting index
nested_group <- EA_data %>%
  distinct(nested_id) %>%
  nrow()


# Defining our priors
# The default prior distribution for a fixed effect parameter is a normal distribution
# with mean 0 and precision(tau) = 0.001. Because tau = 1/sigma, this translate as sigma = 31.62
# Hence the default prior is of the form
# Bi ~ N (0, 31.6^2)

# We want to assign our own priors
# define priors
# hyper.prec = list(theta = list(prior="pc.prec", param=c(0.01,0.01)))
# control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000)

#########################################################################
#########################################################################
################ HH COUNT MODELLING USING A GAMMA DISTRIBUTION ##########
# Fit Models --------------------------------------------------------------

# Model1 -  Intercept Only Model

formula1 <- hh_density ~  Intercept(1)

# fit model using a gamma distribution
mod1_gamma <- bru(formula1,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod1_gamma)

# Plot intercept parameter
plot(mod1_gamma, "Intercept")


## Predict function gives summarized hh_density estimates
mu <- predict(mod1_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)
mu

# Using the generate function which gives the posteriors of the latent mean
mu_samples <- generate(mod1_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# mu_samples is a matrix: rows = 500 samples
mu_summary <- data.frame(
  mean            = mean(mu_samples),
  sd              = sd(mu_samples),
  q0.025          = quantile(mu_samples, 0.025),
  q0.5            = quantile(mu_samples, 0.5),
  q0.975          = quantile(mu_samples, 0.975),
  median          = median(mu_samples),
  # Standard error of the mean (Monte Carlo error)
  mean.mc_std_err = sd(mu_samples) / sqrt(length(mu_samples)),
  # Standard error of the SD (Monte Carlo error)
  sd.mc_std_err   = sd(mu_samples) / sqrt(2 * (length(mu_samples) - 1))
)

# Rbind both calculations using Predict and generate to compare
rbind(mu, mu_summary)

# If you want to visualize the posterior, you need to convert it to a dataframe and plot it
# Convert posteriors to a dataframe
mu_samples <- mu_samples %>%
  t() %>%
  as_tibble() %>%
  rename(predicted_density = V1)

# Plot the predicted density
ggplot(mu_samples, aes(x = predicted_density)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Predicted HH Density",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()


# Estimate the HH Count = Predicted Density * Bcount
hh_estimates1 <- EA_data %>%
  cbind(mu_summary) %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates1$predicted_hh_count) # Predicted Total HH
sum(hh_estimates1$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics1 <- hh_estimates1 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics1 %>%
  kable()

# hh count metrics
hh_metrics1 <- hh_estimates1 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics1 %>%
  kable()

###############################################################################
###############################################################################

# Model 2 - Intercept + Covariates ----------------------------------------

formula2 <- hh_density ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56

# formula2b <- hh_density ~ x13(main = x13, model="linear", mean.linear=0, prec.linear=0.001) +
# x63(main = x63, model="linear", mean.linear=5, prec.linear=0.001) +
# x50(main = x50, model="linear", mean.linear=10, prec.linear=50) +
# x44(main = x44, model="linear", mean.linear=4, prec.linear=3) +
# x49(main = x49, model="linear", mean.linear=6, prec.linear=8) +
# x57 (main = x57, model="linear", mean.linear=10, prec.linear=0.001)
# x55 (main = x55, model="linear", mean.linear=10, prec.linear=0.001)
# x56 (main = x56, model="linear", mean.linear=10, prec.linear=0.001)

# fit model using a gamma distribution
mod2_gamma <- bru(formula2,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod2_gamma)

# Plot intercept parameter
plot(mod2_gamma, "Intercept")

# Plot fixed effect parameters
plot(mod2_gamma, "x13")
plot(mod2_gamma, "x63")
plot(mod2_gamma, "x50")


## Predict the mean hh_density
mu <- predict(mod2_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# Using the generate function to generate posteriors
mu_samples <- generate(mod2_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# mu_samples: rows = 3896 locations, columns = 500 samples
mu_summary <- data.frame(
  mean            = apply(mu_samples, 1, mean),
  sd              = apply(mu_samples, 1, sd),
  q0.025          = apply(mu_samples, 1, quantile, probs = 0.025),
  q0.5            = apply(mu_samples, 1, quantile, probs = 0.5),
  q0.975          = apply(mu_samples, 1, quantile, probs = 0.975),
  median          = apply(mu_samples, 1, median),
  # MC error uses the number of SAMPLES (500), which is ncol
  mean.mc_std_err = apply(mu_samples, 1, sd) / sqrt(ncol(mu_samples))
)

# View the result
head(mu_summary)

# Let plot the predicted density (posteriors)

# Plot the predicted density
ggplot(mu_summary, aes(x = mean)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(
    title = "Predicted HH Density",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# OR you can plot from mu which would give same results
ggplot(mu, aes(x = mean)) +
  geom_density(fill = "#590d22", alpha = 0.5) +
  labs(
    title = "Predicted HH Density",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# We want to plot the credible intervals and the observed vs predicted

# plot predicted vs observed density
ggplot(mu) +
  geom_pointrange(aes(x = hh_density, y = mean, ymin = q0.025, ymax = q0.975),
    fill = "grey50", color = "firebrick", shape = 21
  ) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1) +
  theme_minimal() +
  labs(
    title = "Posterior Mean and 95% Credible Intervals",
    x = "Observed HH density", y = "Predicted HH Density"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Estimate the HH Count = Predicted Density * Bcount
hh_estimates2 <- mu %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates2$predicted_hh_count) # Predicted Total HH
sum(hh_estimates2$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics2 <- hh_estimates2 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics2 %>%
  kable()

# hh count metrics
hh_metrics2 <- hh_estimates2 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics2 %>%
  kable()

###############################################################################
###############################################################################

# Model 3 - Intercept + Covariates + + rural_urban_Random_Effect ------------

formula3 <- hh_density ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))

# fit model using a gamma distribution
mod3_gamma <- bru(formula3,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod3_gamma)

# Plot intercept parameter
plot(mod3_gamma, "Intercept")

# Plot fixed effect parameters
plot(mod3_gamma, "x13")
plot(mod3_gamma, "x63")
plot(mod3_gamma, "x50")

# Plot Random effect parameter

# To see the summary (mean, sd, etc.) for that specific index
mod3_gamma$summary.random$Random_rural_urban

# To plot the posterior density of the first stratum manually
plot(mod3_gamma$marginals.random$Random_rural_urban[[1]],
  type = "l",
  main = "Posterior for Random_rural_urban Index 1"
)


## Predict the mean hh_density
mu <- predict(mod3_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# Let plot the predicted density (posteriors)

# Plot the predicted density
ggplot(mu, aes(x = mean)) +
  geom_density(fill = "#590d22", alpha = 0.5) +
  labs(
    title = "Predicted HH Density",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# We want to plot the credible intervals and the observed vs predicted

# plot predicted vs observed density
ggplot(mu) +
  geom_pointrange(aes(x = hh_density, y = mean, ymin = q0.025, ymax = q0.975),
    fill = "grey50", color = "firebrick", shape = 21
  ) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1) +
  theme_minimal() +
  labs(
    title = "Posterior Mean and 95% Credible Intervals",
    x = "Observed HH density", y = "Predicted HH Density"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Estimate the HH Count = Predicted Density * Bcount
hh_estimates3 <- mu %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates3$predicted_hh_count) # Predicted Total HH
sum(hh_estimates3$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics3 <- hh_estimates3 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics3 %>%
  kable()

# hh count metrics
hh_metrics3 <- hh_estimates3 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics3 %>%
  kable()


###############################################################################
###############################################################################

# Model 4 - Intercept + Covariates + rural_urban_id_Random_Effect + Dist_Random_Effect

formula4 <- hh_density ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group)) +
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))


# fit model using a gamma distribution
mod4_gamma <- bru(formula4,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod4_gamma)


## Predict the mean hh_density
mu <- predict(mod4_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id) +
    Random_dist_eval(dist_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# Plot the predicted density
ggplot(mu, aes(x = mean)) +
  geom_density(fill = "#590d22", alpha = 0.5) +
  labs(
    title = "Predicted HHDensity",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# plot predicted vs observed density
ggplot(mu) +
  geom_pointrange(aes(x = hh_density, y = mean, ymin = q0.025, ymax = q0.975),
    fill = "grey50", color = "firebrick", shape = 21
  ) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1) +
  theme_minimal() +
  labs(
    title = "Posterior Mean and 95% Credible Intervals",
    x = "Observed population density", y = "Predicted HHDensity"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Estimate the HH Count = Predicted Density * Bcount
hh_estimates4 <- mu %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates4$predicted_hh_count) # Predicted Total HH
sum(hh_estimates4$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics4 <- hh_estimates4 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics4 %>%
  kable()

# hh count metrics
hh_metrics4 <- hh_estimates4 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics4 %>%
  kable()


###############################################################################
###############################################################################

# Model 5 - Intercept + Covariates + Nested_Effect

formula5 <- hh_density ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
  Random_nested(nested_id, model = "iid", mapper = bru_mapper_index(n = nested_group))


# fit model using a gamma distribution
mod5_gamma <- bru(formula5,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod5_gamma)

mod5_gamma$summary.random$Random_nested

## Predict the mean hh_density
mu <- predict(mod5_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_nested_eval(nested_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# Plot the predicted density
ggplot(mu, aes(x = mean)) +
  geom_density(fill = "#590d22", alpha = 0.5) +
  labs(
    title = "Predicted HHDensity",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# plot predicted vs observed density
ggplot(mu) +
  geom_pointrange(aes(x = hh_density, y = mean, ymin = q0.025, ymax = q0.975),
    fill = "grey50", color = "firebrick", shape = 21
  ) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1) +
  theme_minimal() +
  labs(
    title = "Posterior Mean and 95% Credible Intervals",
    x = "Observed population density", y = "Predicted HHDensity"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Estimate the HH Count = Predicted Density * Bcount
hh_estimates5 <- mu %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates5$predicted_hh_count) # Predicted Total HH
sum(hh_estimates5$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics5 <- hh_estimates5 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics5 %>%
  kable()

# hh count metrics
hh_metrics5 <- hh_estimates5 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics5 %>%
  kable()

###############################################################################
###############################################################################

# Model 6 - Intercept + Covariates + Urban_Rural_Random_Effect + Dist_Random_Effect + EA Random_Effect

formula6 <- hh_density ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group)) +
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups)) +
  Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups))


# fit model using a gamma distribution
mod6_gamma <- bru(formula6,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod6_gamma)

mod6_gamma$summary.random$Random_EA

## Predict the mean hh_density
mu <- predict(mod6_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id) +
    Random_EA_eval(id) +
    Random_dist_eval(dist_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# Plot the predicted density
ggplot(mu, aes(x = mean)) +
  geom_density(fill = "#590d22", alpha = 0.5) +
  labs(
    title = "Predicted HH Density",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# plot predicted vs observed density
ggplot(mu) +
  geom_pointrange(aes(x = hh_density, y = mean, ymin = q0.025, ymax = q0.975),
    fill = "grey50", color = "firebrick", shape = 21
  ) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1) +
  theme_minimal() +
  labs(
    title = "Posterior Mean and 95% Credible Intervals",
    x = "Observed population density", y = "Predicted HH Density"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Estimate the HH Count = Predicted Density * Bcount
hh_estimates6 <- mu %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates6$predicted_hh_count) # Predicted Total HH
sum(hh_estimates6$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics6 <- hh_estimates6 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics6 %>%
  kable()

# hh count metrics
hh_metrics6 <- hh_estimates6 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics6 %>%
  kable()


# Compare all models ------------------------------------------------------------

# DIC
t(c(
  mod1_gamma = mod1_gamma$dic$dic, mod2_gamma = mod2_gamma$dic$dic,
  mod3_gamma = mod3_gamma$dic$dic, mod4_gamma = mod4_gamma$dic$dic,
  mod5_gamma = mod5_gamma$dic$dic, mod6_gamma = mod6_gamma$dic$dic
))


# compare models
hh <- rbind(
  hh_metrics1, hh_metrics2, hh_metrics3, hh_metrics4,
  hh_metrics5, hh_metrics6
)
hh %>% kable()

dens <- rbind(
  density_metrics1, density_metrics2, density_metrics3,
  density_metrics4, density_metrics5, density_metrics6
)
dens %>% kable()


###################################################################################
##################################################################################
################ BAYESIAN SPATIAL MODELS #########################################

## Check for Spatial Autocorrelation  -------------------------------------------------------

#-Define the coordinates of centroids
coords <- cbind(EA_data$long, EA_data$lat)

# Find the 5 nearest neighbours
knn <- spdep::knearneigh(coords, k = 5)
nb <- spdep::knn2nb(knn)

# Convert nb to spatial weight
weights <- spdep::nb2listw(nb, style = "W")

# Calculate the residuals in model 6
hh_estimates6 <- hh_estimates6 %>%
  mutate(residual = observed_hh_count - predicted_hh_count)

# Calculate Moran's I for the residuals for the best model
moran_test <- spdep::moran.test(hh_estimates6$residual, weights)
moran_test

# Results indicate that the model has a Statistically significant positive spatial autocorrelation
# Hence the need to include a spatial aspect to capture this autocorrelation

#-Define the coordinates of centroids
coords <- cbind(EA_data$long, EA_data$lat)

# measure distance between coordinates
summary(dist(coords)) # summarizes the Euclidean distance between points in the spatial domain


# build non-convex hull mesh
non_convex_bdry <- fmesher::fm_nonconvex_hull(coords, -0.03, -0.05, resolution = c(100, 100))
mesh <- fm_mesh_2d_inla(
  boundary = non_convex_bdry, max.edge = c(0.1, 1), # 0.1, 1
  offset = c(0.05, 1),
  cutoff = 0.003
)

plot(mesh)
plot(mesh, add = T)
points(coords, col = "red", pch = "*")

# Count of mesh nodes
mesh$n

# Build the SPDE
spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

# specify the spatial model
formula7 <- hh_density ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group)) +
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups)) +
  Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups)) +
  Random_Spat(main = coords, model = spde) # Spatial Effect


# fit model using a gamma distribution
mod7_gamma <- bru(formula7,
  data = EA_data,
  family = "gamma",
  options = list(
    # control.fixed = control.fixed,
    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
    control.inla = list(int.strategy = "eb"),
    verbose = FALSE,
    num.threads = "1"
  )
)

# Summary
summary(mod7_gamma)


## Predict the mean hh_density
mu <- predict(mod7_gamma,
  newdata = EA_data,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id) +
    Random_dist_eval(dist_id) +
    Random_EA_eval(id) +
    Random_Spat_eval(cbind(long, lat))),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# Plot the predicted density
ggplot(mu, aes(x = mean)) +
  geom_density(fill = "#590d22", alpha = 0.5) +
  labs(
    title = "Predicted HHDensity",
    x = "Predicted Value", y = "Density"
  ) +
  theme_minimal()

# plot predicted vs observed density
ggplot(mu) +
  geom_pointrange(aes(x = hh_density, y = mean, ymin = q0.025, ymax = q0.975),
    fill = "grey50", color = "firebrick", shape = 21
  ) +
  geom_abline(slope = 1, intercept = 0, color = "orange", size = 1) +
  theme_minimal() +
  labs(
    title = "Posterior Mean and 95% Credible Intervals",
    x = "Observed population density", y = "Predicted HHDensity"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Estimate the HH Count = Predicted Density * Bcount
hh_estimates7 <- mu %>%
  select(hh_density, mean, google_v2_5, hh_count_2024) %>%
  rename(
    observed_density = hh_density,
    predicted_density = mean,
    observed_hh_count = hh_count_2024
  ) %>%
  mutate(predicted_hh_count = predicted_density * google_v2_5)


# sum hh count
sum(hh_estimates7$predicted_hh_count) # Predicted Total HH
sum(hh_estimates7$observed_hh_count, na.rm = T) # Observed Total HH


# Compute model performance metrics

# Density metrics

density_metrics7 <- hh_estimates7 %>%
  mutate(residual = observed_density - predicted_density) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density)
  )

density_metrics7 %>%
  kable()

# hh count metrics
hh_metrics7 <- hh_estimates7 %>%
  mutate(residual = observed_hh_count - predicted_hh_count) %>%
  summarise(
    Bias = mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_hh_count, predicted_hh_count)
  )

hh_metrics7 %>%
  kable()


# Compare all models ------------------------------------------------------------

# DIC
t(c(
  mod1_gamma = mod1_gamma$dic$dic, mod2_gamma = mod2_gamma$dic$dic,
  mod3_gamma = mod3_gamma$dic$dic, mod4_gamma = mod4_gamma$dic$dic,
  mod5_gamma = mod5_gamma$dic$dic, mod6_gamma = mod6_gamma$dic$dic,
  mod7_gamma = mod7_gamma$dic$dic
))


# compare models
hh <- rbind(
  hh_metrics1, hh_metrics2, hh_metrics3, hh_metrics4,
  hh_metrics5, hh_metrics6, hh_metrics7
)
hh %>% kable()

dens <- rbind(
  density_metrics1, density_metrics2, density_metrics3,
  density_metrics4, density_metrics5, density_metrics6,
  density_metrics7
)
dens %>% kable()

# Model 7 which include a spatial effect component is the best performing model

###############################################################################
################################################################################
################################################################################
############## CROSS VALIDATION ###############################################

#------------------------------------------------------------------------------
# Random  K-Fold Function
#------------------------------------------------------------------------------

# function to calculate k-fold
kfold_cv <- function(data, k, n.samples, seed = 123) {
  set.seed(seed)
  n <- nrow(data)
  fold_size <- n %/% k
  folds <- sample(rep(1:k, each = fold_size, length.out = n))

  # Create separate dataframes for train and test metrics
  train_metrics <- data.frame()
  test_metrics <- data.frame()

  # Place holder for train metrics calculation ----------------------------

  # density metrics
  dens_train_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_train_pearson_values <- numeric(k) # Placeholder for corr
  dens_train_mae_values <- numeric(k) # Placeholder for MAE
  dens_train_bias_values <- numeric(k) # Placeholder for bias

  # HH metrics
  hh_train_rmse_values <- numeric(k) # Placeholder for RMSE
  hh_train_pearson_values <- numeric(k) # Placeholder for corr
  hh_train_mae_values <- numeric(k) # Placeholder for MAE
  hh_train_bias_values <- numeric(k) # Placeholder for bias


  # Place holder for test metrics calculation -------------------------

  # Density metrics
  dens_test_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_test_pearson_values <- numeric(k) # Placeholder for corr
  dens_test_mae_values <- numeric(k) # Placeholder for MAE
  dens_test_bias_values <- numeric(k) # Placeholder for bias

  # HH metrics
  hh_test_rmse_values <- numeric(k) # Placeholder for RMSE
  hh_test_pearson_values <- numeric(k) # Placeholder for corr
  hh_test_mae_values <- numeric(k) # Placeholder for MAE
  hh_test_bias_values <- numeric(k) # Placeholder for bias

  # For loop for implementation ---------------------------------------------
  for (i in 1:k) {
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)

    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]

    print(paste("Processing fold", i, "out of", k))

    #-Define the coordinates of centroids
    coords <- cbind(train_data$long, train_data$lat)

    # measure distance between coordinates
    summary(dist(coords)) # summarizes the Euclidean distance between points in the spatial domain


    # build non-convex hull mesh
    non_convex_bdry <- fmesher::fm_nonconvex_hull(coords, -0.03, -0.05, resolution = c(100, 100))
    mesh <- fm_mesh_2d_inla(
      boundary = non_convex_bdry, max.edge = c(0.1, 1),
      offset = c(0.05, 1),
      cutoff = 0.003
    )

    # Build the SPDE
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

    # Model formula
    xval_formula <- hh_density ~  x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
      Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group)) +
      Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups)) +
      Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups)) +
      Random_Spat(main = coords, model = spde)

    # fit model using a gamma distribution
    mod_xval <- bru(xval_formula,
      data = train_data,
      family = "gamma",
      options = list(
        control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
        control.inla = list(int.strategy = "eb"),
        verbose = FALSE
      )
    )

    # summary(mod2)

    # Make Predictions for train data
    train_predictions <- predict(mod_xval,
      newdata = train_data,
      formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
        Random_rural_urban_eval(rural_urban_id) +
        Random_dist_eval(dist_id) +
        Random_EA_eval(id) +
        Random_Spat_eval(cbind(long, lat))),
      n.samples = n.samples,
      seed = 2,
      num.threads = "1"
    )

    # Estimate the HH Count = Predicted Density * Bcount
    train_predictions <- train_predictions %>%
      select(hh_density, mean, google_v2_5, hh_count_2024) %>%
      rename(
        observed_density = hh_density,
        predicted_density = mean,
        observed_hh_count = hh_count_2024
      ) %>%
      mutate(predicted_hh_count = predicted_density * google_v2_5)

    # Train data metrics
    # Density
    dens_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_density - train_predictions$predicted_density)^2))
    dens_train_pearson_values[i] <- cor(train_predictions$observed_density, train_predictions$predicted_density)
    dens_train_mae_values[i] <- mean(abs(train_predictions$observed_density - train_predictions$predicted_density))
    dens_train_bias_values[i] <- mean(train_predictions$observed_density - train_predictions$predicted_density)

    # HH Count
    hh_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_hh_count - train_predictions$predicted_hh_count)^2))
    hh_train_pearson_values[i] <- cor(train_predictions$observed_hh_count, train_predictions$predicted_hh_count)
    hh_train_mae_values[i] <- mean(abs(train_predictions$observed_hh_count - train_predictions$predicted_hh_count))
    hh_train_bias_values[i] <- mean(train_predictions$observed_hh_count - train_predictions$predicted_hh_count)

    # Make Predictions for Test Data ------------------------------------------

    # Make Predictions for test data
    test_predictions <- predict(mod_xval,
      newdata = test_data,
      formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
        Random_rural_urban_eval(rural_urban_id) +
        Random_dist_eval(dist_id) +
        Random_Spat_eval(cbind(long, lat))),
      n.samples = n.samples,
      seed = 2,
      num.threads = "1"
    )

    # Estimate the HH Count = Predicted Density * Bcount
    test_predictions <- test_predictions %>%
      select(hh_density, mean, google_v2_5, hh_count_2024) %>%
      rename(
        observed_density = hh_density,
        predicted_density = mean,
        observed_hh_count = hh_count_2024
      ) %>%
      mutate(predicted_hh_count = predicted_density * google_v2_5)

    # Test data metrics
    # Density
    dens_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_density - test_predictions$predicted_density)^2))
    dens_test_pearson_values[i] <- cor(test_predictions$observed_density, test_predictions$predicted_density)
    dens_test_mae_values[i] <- mean(abs(test_predictions$observed_density - test_predictions$predicted_density))
    dens_test_bias_values[i] <- mean(test_predictions$observed_density - test_predictions$predicted_density)

    # HH Count
    hh_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_hh_count - test_predictions$predicted_hh_count)^2))
    hh_test_pearson_values[i] <- cor(test_predictions$observed_hh_count, test_predictions$predicted_hh_count)
    hh_test_mae_values[i] <- mean(abs(test_predictions$observed_hh_count - test_predictions$predicted_hh_count))
    hh_test_bias_values[i] <- mean(test_predictions$observed_hh_count - test_predictions$predicted_hh_count)

    # Train metrics
    train_metrics <- data.frame(
      Model = "Random K-Fold",
      dens_train_rmse = mean(dens_train_rmse_values),
      dens_train_corr = mean(dens_train_pearson_values),
      dens_train_mae = mean(dens_train_mae_values),
      dens_train_bias = mean(dens_train_bias_values),
      hh_train_rmse = mean(hh_train_rmse_values),
      hh_train_corr = mean(hh_train_pearson_values),
      hh_train_mae = mean(hh_train_mae_values),
      hh_train_bias = mean(hh_train_bias_values)
    )

    # Test metrics
    test_metrics <- data.frame(
      Model = "Random K-Fold",
      dens_test_rmse = mean(dens_test_rmse_values),
      dens_test_corr = mean(dens_test_pearson_values),
      dens_test_mae = mean(dens_test_mae_values),
      dens_test_bias = mean(dens_test_bias_values),
      hh_test_rmse = mean(hh_test_rmse_values),
      hh_test_corr = mean(hh_test_pearson_values),
      hh_test_mae = mean(hh_test_mae_values),
      hh_test_bias = mean(hh_test_bias_values)
    )
  }

  # Return separate lists for density and hh metrics
  list(train_metrics = train_metrics, test_metrics = test_metrics)
}

# Apply function
result1 <- kfold_cv(
  data = EA_data,
  k = 10,
  n.samples = 500
)

# Train data results
result1$train_metrics %>%
  kable()

# Test data results
result1$test_metrics %>%
  kable()


###################################################################################
###################################################################################
############### SPATIAL CROSS VALIDATION #################################################

#------------------------------------------------------------------------------
# Spatial Leave-Group-Out Cross Validation
#------------------------------------------------------------------------------

# function to calculate k-fold
lgocv <- function(data, k, n.samples, seed = 123) {
  set.seed(seed)

  n <- nrow(data)
  fold_size <- n %/% k
  folds <- sample(rep(1:k, each = fold_size, length.out = n))

  # Create separate dataframes for train and test metrics
  train_metrics <- data.frame()
  test_metrics <- data.frame()

  # Place holder for train metrics calculation ----------------------------

  # density metrics
  dens_train_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_train_pearson_values <- numeric(k) # Placeholder for corr
  dens_train_mae_values <- numeric(k) # Placeholder for MAE
  dens_train_bias_values <- numeric(k) # Placeholder for bias

  # HH metrics
  hh_train_rmse_values <- numeric(k) # Placeholder for RMSE
  hh_train_pearson_values <- numeric(k) # Placeholder for corr
  hh_train_mae_values <- numeric(k) # Placeholder for MAE
  hh_train_bias_values <- numeric(k) # Placeholder for bias


  # Place holder for test metrics calculation -------------------------

  # Density metrics
  dens_test_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_test_pearson_values <- numeric(k) # Placeholder for corr
  dens_test_mae_values <- numeric(k) # Placeholder for MAE
  dens_test_bias_values <- numeric(k) # Placeholder for bias

  # HH metrics
  hh_test_rmse_values <- numeric(k) # Placeholder for RMSE
  hh_test_pearson_values <- numeric(k) # Placeholder for corr
  hh_test_mae_values <- numeric(k) # Placeholder for MAE
  hh_test_bias_values <- numeric(k) # Placeholder for bias

  # For loop for implementation ---------------------------------------------
  for (i in 1:k) {
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)

    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]

    # tag train dataset
    train_data <- train_data %>%
      mutate(observed_data = hh_density, dataset = "train")

    # tag test dataset
    test_data <- test_data %>%
      mutate(observed_data = NA, dataset = "test")

    # combine data
    xval_data <- rbind(train_data, test_data)

    #-Define the coordinates of centroids
    coords <- cbind(xval_data$long, xval_data$lat)

    # measure distance between coordinates
    summary(dist(coords)) # summarizes the Euclidean distance between points in the spatial domain


    # build non-convex hull mesh
    non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
    mesh <- fm_mesh_2d_inla(
      boundary = non_convex_bdry, max.edge = c(0.1, 1),
      offset = c(0.05, 1),
      cutoff = 0.003
    )

    # Build the SPDE
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

    # Model formula
    xval_formula <- observed_data ~  x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
      Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group)) +
      Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups)) +
      Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups)) +
      Random_Spat(main = coords, model = spde)

    # fit model using a gamma distribution
    mod_cv <- bru(xval_formula,
      data = xval_data,
      family = "gamma",
      options = list(
        control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
        control.inla = list(int.strategy = "eb"),
        verbose = FALSE
      )
    )

    # summary(mod2)

    # LGOCV
    cv_results <- inla.group.cv(mod_cv, num.level.sets = 3)

    # Mean predictions
    cv_results <- data.frame(mean = cv_results$mean)

    # cbind to data
    predictions <- cbind(xval_data, cv_results) %>%
      as_tibble()

    # Estimate the HH Count = Predicted Density * Bcount
    train_predictions <- predictions %>%
      filter(dataset == "train") %>%
      mutate(
        predicted_density = exp(mean),
        predicted_hh_count = predicted_density * google_v2_5
      ) %>%
      rename(observed_hh_count = hh_count_2024, observed_density = hh_density)

    # Train data metrics
    # Density
    dens_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_density - train_predictions$predicted_density)^2))
    dens_train_pearson_values[i] <- cor(train_predictions$observed_density, train_predictions$predicted_density)
    dens_train_mae_values[i] <- mean(abs(train_predictions$observed_density - train_predictions$predicted_density))
    dens_train_bias_values[i] <- mean(train_predictions$observed_density - train_predictions$predicted_density)

    # HH Count
    hh_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_hh_count - train_predictions$predicted_hh_count)^2))
    hh_train_pearson_values[i] <- cor(train_predictions$observed_hh_count, train_predictions$predicted_hh_count)
    hh_train_mae_values[i] <- mean(abs(train_predictions$observed_hh_count - train_predictions$predicted_hh_count))
    hh_train_bias_values[i] <- mean(train_predictions$observed_hh_count - train_predictions$predicted_hh_count)

    # Make Predictions for Test Data ------------------------------------------

    # Estimate the HH Count = Predicted Density * Bcount
    test_predictions <- predictions %>%
      filter(dataset == "test") %>%
      mutate(
        predicted_density = exp(mean),
        predicted_hh_count = predicted_density * google_v2_5
      ) %>%
      rename(observed_hh_count = hh_count_2024, observed_density = hh_density)


    # Test data metrics
    # Density
    dens_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_density - test_predictions$predicted_density)^2))
    dens_test_pearson_values[i] <- cor(test_predictions$observed_density, test_predictions$predicted_density)
    dens_test_mae_values[i] <- mean(abs(test_predictions$observed_density - test_predictions$predicted_density))
    dens_test_bias_values[i] <- mean(test_predictions$observed_density - test_predictions$predicted_density)

    # HH Count
    hh_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_hh_count - test_predictions$predicted_hh_count)^2))
    hh_test_pearson_values[i] <- cor(test_predictions$observed_hh_count, test_predictions$predicted_hh_count)
    hh_test_mae_values[i] <- mean(abs(test_predictions$observed_hh_count - test_predictions$predicted_hh_count))
    hh_test_bias_values[i] <- mean(test_predictions$observed_hh_count - test_predictions$predicted_hh_count)

    # Train metrics
    train_metrics <- data.frame(
      Model = "Spatial K-Fold",
      dens_train_rmse = mean(dens_train_rmse_values),
      dens_train_corr = mean(dens_train_pearson_values),
      dens_train_mae = mean(dens_train_mae_values),
      dens_train_bias = mean(dens_train_bias_values),
      hh_train_rmse = mean(hh_train_rmse_values),
      hh_train_corr = mean(hh_train_pearson_values),
      hh_train_mae = mean(hh_train_mae_values),
      hh_train_bias = mean(hh_train_bias_values)
    )

    # Test metrics
    test_metrics <- data.frame(
      Model = "Spatial K-Fold",
      dens_test_rmse = mean(dens_test_rmse_values),
      dens_test_corr = mean(dens_test_pearson_values),
      dens_test_mae = mean(dens_test_mae_values),
      dens_test_bias = mean(dens_test_bias_values),
      hh_test_rmse = mean(hh_test_rmse_values),
      hh_test_corr = mean(hh_test_pearson_values),
      hh_test_mae = mean(hh_test_mae_values),
      hh_test_bias = mean(hh_test_bias_values)
    )
  }

  # Return separate lists for density and hh metrics
  list(train_metrics = train_metrics, test_metrics = test_metrics)
}

# Apply function
result2 <- lgocv(
  data = EA_data,
  k = 10,
  n.samples = 500
)

# Train data results
result2$train_metrics %>%
  kable()

# Test data results
result2$test_metrics %>%
  kable()


#########################################################################################################
#########################################################################################################
################## HH COUNT PREDICTIONS ##########################################################

# load covariates
pred_covs <- read_feather(paste0(input_path, "Malawi_covs_stack_2024.feather"))
r1 <- rast(paste0(input_path, "country_raster.tif"))


# Remove pixels with dist ids of 33 and NA
pred_covs <- pred_covs %>%
  drop_na(dist_id) %>%
  filter(dist_id != 33)

# Check for NAs in covariates ---------------------------------------------

# Function to count NAs in covariates starting with 'x'
count_nas <- function(data) {
  # Get the names of the covariates starting with 'x'
  covariates <- names(data)[grepl("^x", names(data))]

  # Loop through each covariate and count NAs
  for (covariate in covariates) {
    na_count <- sum(is.na(data[[covariate]]))
    cat("Number of NAs in", covariate, ":", na_count, "\n")
  }
}

# Call the function
count_nas(pred_covs)

#--Standardize covariates
vars <- pred_covs %>%
  select(starts_with("x")) %>%
  names()

# Scale covariates
pred_covs[, vars] <- apply(pred_covs[, vars], 2, stdize)

# check scaled covariates
head(pred_covs)

# Replace NAs with 0 to avoid numerical issues
pred_covs <- pred_covs %>%
  mutate_at(vars(starts_with("x")), ~ replace(., is.na(.), 0))

# Create nesting index
pred_covs <- pred_covs %>%
  group_by(rural_urban_id, dist_id, REG_CODE) %>%
  mutate(nested_id = cur_group_id()) %>%
  ungroup()

#######################################################################################
######################### MAKE GRIDCELL PREDICTIONS ##################################

# Model 2 - Intercept + Covariates ----------------------------------------

# Use the generate function to make predictions
mu <- generate(mod2_gamma,
  newdata = pred_covs,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# Convert to tibble
predicted_density <- mu %>%
  as_tibble()

# Add building count for each grid
predicted_density <- predicted_density %>%
  mutate(bcount = pred_covs$google_v2_5)

# Estimate Predicted Total HH
predicted_hh_count2 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>%
  select(-bcount)

# Total Predicted Total HH and Uncertainty

hh_model2 <- predicted_hh_count2 %>%
  apply(2, sum, na.rm = T) %>%
  as_tibble() %>%
  summarise(
    mean_count = round(mean(value)),
    upper_quantile = round(quantile(value, probs = 0.975)),
    lower_quantile = round(quantile(value, probs = 0.025))
  )

hh_model2 %>% kable()

# District Estimates ---------------------------------------------

district_names <- pred_covs %>%
  select(DIST_NAME)

# cbind district to data
district_estimates <- cbind(predicted_hh_count2, district_names) %>%
  as_tibble()

# Group by district and split data according to district
district_estimates <- district_estimates %>%
  group_by(DIST_NAME) %>%
  group_split()


# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(district_estimates)) {
  df <- district_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    district_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
print(AA)

# Convert to tibble and export as a csv
district_hh_count <- AA %>%
  as_tibble() %>%
  rename(
    DIST_NAME = district_names, Estimated_Count = mean,
    Lower_Count = "lower_quantile.2.5%",
    Median_Count = "median.50%",
    Upper_Count = "upper_quantile.97.5%"
  )

# Write to file

# EA Estimates --------------------------------------------------

ea_names <- pred_covs %>%
  select(EA_CODE, ea_id)

# cbind to data
ea_names <- cbind(predicted_hh_count2, ea_names) %>%
  as_tibble()

# Group by ea_id and split data according to ea
ea_estimates <- ea_names %>%
  group_by(EA_CODE, ea_id) %>%
  group_split()

# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(ea_estimates)) {
  df <- ea_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$EA_CODE)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    ea_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500),
    sd = sd(df)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
EA_estimates <- as_tibble(AA) %>%
  rename(
    EA_CODE = ea_names,
    mean_estimate = mean,
    median = `median.50%`,
    lower = `lower_quantile.2.5%`,
    upper = `upper_quantile.97.5%`,
    std = sd
  ) %>%
  mutate(
    across(-EA_CODE, ~ as.numeric(.))
  ) %>%
  mutate(
    cv = std / mean_estimate,
    CI = upper - lower
  )

# Write table to file

# Summarize pixel level predictions ---------------------------------------

# Summarize predictions
tic()

mean_count <- rowMeans(predicted_hh_count2, na.rm = T)
median_count <- apply(predicted_hh_count2, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_hh_count2, 1, sd)
lower_quantile <- apply(predicted_hh_count2, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm = T))
upper_quantile <- apply(predicted_hh_count2, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty <- (upper_quantile - lower_quantile) / mean_count
coe_var <- std_count / mean_count

toc()

# sum predictions
sum(median_count, na.rm = T)
sum(mean_count, na.rm = T)

# Cbind predictions to xy coord

pixel_predictions2 <- cbind(
  mean_count, median_count, std_count,
  lower_quantile, upper_quantile, uncertainty, coe_var
) %>%
  as_tibble() %>%
  mutate(
    long = pred_covs$long, lat = pred_covs$lat,
    bcount = pred_covs$google_v2_5,
    hh_density = mean_count / bcount
  )

summary(pixel_predictions2$mean_count)
summary(pixel_predictions2$hh_density)

###############################################################################
# Model 3 - Intercept + Covariates +  Rural_Urban_Random_Effect --------------------------------

# Use the generate function to make predictions
mu <- generate(mod3_gamma,
  newdata = pred_covs,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)


# Predicted Density
predicted_density <- mu %>%
  as_tibble()

# Add building count for each grid
predicted_density <- predicted_density %>%
  mutate(bcount = pred_covs$google_v2_5)

# Estimate Predicted Total HH
predicted_hh_count3 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>%
  select(-bcount)

# Total Predicted Total HH and Uncertainty

hh_model3 <- predicted_hh_count3 %>%
  apply(2, sum, na.rm = T) %>%
  as_tibble() %>%
  summarise(
    mean_count = round(mean(value)),
    upper_quantile = round(quantile(value, probs = 0.975)),
    lower_quantile = round(quantile(value, probs = 0.025))
  )

hh_model3 %>% kable()


# District Estimates ---------------------------------------------

district_names <- pred_covs %>%
  select(DIST_NAME)

# cbind district to data
district_estimates <- cbind(predicted_hh_count3, district_names) %>%
  as_tibble()

# Group by district and split data according to district
district_estimates <- district_estimates %>%
  group_by(DIST_NAME) %>%
  group_split()


# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(district_estimates)) {
  df <- district_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    district_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
district_hh_count <- AA %>%
  as_tibble() %>%
  rename(
    DIST_NAME = district_names, Estimated_Count = mean,
    Lower_Count = "lower_quantile.2.5%",
    Median_Count = "median.50%",
    Upper_Count = "upper_quantile.97.5%"
  )

# Write to file

# EA Estimates --------------------------------------------------

ea_names <- pred_covs %>%
  select(EA_CODE, ea_id)

# cbind to data
ea_names <- cbind(predicted_hh_count3, ea_names) %>%
  as_tibble()

# Group by ea_id and split data according to ea
ea_estimates <- ea_names %>%
  group_by(EA_CODE, ea_id) %>%
  group_split()

# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(ea_estimates)) {
  df <- ea_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$EA_CODE)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    ea_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500),
    sd = sd(df)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
EA_estimates <- as_tibble(AA) %>%
  rename(
    EA_CODE = ea_names,
    mean_estimate = mean,
    median = `median.50%`,
    lower = `lower_quantile.2.5%`,
    upper = `upper_quantile.97.5%`,
    std = sd
  ) %>%
  mutate(
    across(-EA_CODE, ~ as.numeric(.))
  ) %>%
  mutate(
    cv = std / mean_estimate,
    CI = upper - lower
  )

# Write table to file

# Summarize pixel level predictions ---------------------------------------

# Summarize predictions
tic()

mean_count <- rowMeans(predicted_hh_count3, na.rm = T)
median_count <- apply(predicted_hh_count3, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_hh_count3, 1, sd)
lower_quantile <- apply(predicted_hh_count3, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm = T))
upper_quantile <- apply(predicted_hh_count3, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty <- (upper_quantile - lower_quantile) / mean_count
coe_var <- std_count / mean_count

toc()

# sum predictions
sum(median_count, na.rm = T)
sum(mean_count, na.rm = T)

# Cbind predictions to xy coord

pixel_predictions3 <- cbind(
  mean_count, median_count, std_count,
  lower_quantile, upper_quantile, uncertainty, coe_var
) %>%
  as_tibble() %>%
  mutate(
    long = pred_covs$long, lat = pred_covs$lat,
    bcount = pred_covs$google_v2_5,
    hh_density = mean_count / bcount
  )

summary(pixel_predictions3$mean_count)
summary(pixel_predictions3$hh_density)

###############################################################################
###############################################################################

# # Model 4 - Intercept + Covariates + Rural_Urban_Random_Effect + Dist_Random_Effect

# Use the generate function to make predictions
mu <- generate(mod4_gamma,
  newdata = pred_covs,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id) +
    Random_dist_eval(dist_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# Predicted Density
predicted_density <- mu %>%
  as_tibble()

# Add building count for each grid
predicted_density <- predicted_density %>%
  mutate(bcount = pred_covs$google_v2_5)

# Estimate Predicted Total HH
predicted_hh_count4 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>%
  select(-bcount)

# Total Predicted Total HH and Uncertainty

hh_model4 <- predicted_hh_count4 %>%
  apply(2, sum, na.rm = T) %>%
  as_tibble() %>%
  summarise(
    mean_count = round(mean(value)),
    upper_quantile = round(quantile(value, probs = 0.975)),
    lower_quantile = round(quantile(value, probs = 0.025))
  )

hh_model4 %>% kable()


# District Estimates ---------------------------------------------

district_names <- pred_covs %>%
  select(DIST_NAME)

# cbind district to data
district_estimates <- cbind(predicted_hh_count4, district_names) %>%
  as_tibble()

# Group by district and split data according to district
district_estimates <- district_estimates %>%
  group_by(DIST_NAME) %>%
  group_split()


# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(district_estimates)) {
  df <- district_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    district_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
district_hh_count <- AA %>%
  as_tibble() %>%
  rename(
    DIST_NAME = district_names, Estimated_Count = mean,
    Lower_Count = "lower_quantile.2.5%",
    Median_Count = "median.50%",
    Upper_Count = "upper_quantile.97.5%"
  )

# Write to file

# EA Estimates --------------------------------------------------

ea_names <- pred_covs %>%
  select(EA_CODE, ea_id)

# cbind to data
ea_names <- cbind(predicted_hh_count4, ea_names) %>%
  as_tibble()

# Group by ea_id and split data according to ea
ea_estimates <- ea_names %>%
  group_by(EA_CODE, ea_id) %>%
  group_split()

# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(ea_estimates)) {
  df <- ea_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$EA_CODE)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    ea_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500),
    sd = sd(df)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
EA_estimates <- as_tibble(AA) %>%
  rename(
    EA_CODE = ea_names,
    mean_estimate = mean,
    median = `median.50%`,
    lower = `lower_quantile.2.5%`,
    upper = `upper_quantile.97.5%`,
    std = sd
  ) %>%
  mutate(
    across(-EA_CODE, ~ as.numeric(.))
  ) %>%
  mutate(
    cv = std / mean_estimate,
    CI = upper - lower
  )

# Write table to file

# Summarize pixel level predictions ---------------------------------------

# Summarize predictions
tic()

mean_count <- rowMeans(predicted_hh_count4, na.rm = T)
median_count <- apply(predicted_hh_count4, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_hh_count4, 1, sd)
lower_quantile <- apply(predicted_hh_count4, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm = T))
upper_quantile <- apply(predicted_hh_count4, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty <- (upper_quantile - lower_quantile) / mean_count
coe_var <- std_count / mean_count

toc()

# sum predictions
sum(median_count, na.rm = T)
sum(mean_count, na.rm = T)

# Cbind predictions to xy coord

pixel_predictions4 <- cbind(
  mean_count, median_count, std_count,
  lower_quantile, upper_quantile, uncertainty, coe_var
) %>%
  as_tibble() %>%
  mutate(
    long = pred_covs$long, lat = pred_covs$lat,
    bcount = pred_covs$google_v2_5,
    hh_density = mean_count / bcount
  )

summary(pixel_predictions4$mean_count)
summary(pixel_predictions4$hh_density)


###############################################################################
##############################################################################
# Model 5 - Intercept + Covariates + Nested_Effect

# Use the generate function to make predictions
mu <- generate(mod5_gamma,
  newdata = pred_covs,
  formula = ~ exp(Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_nested_eval(nested_id)),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# Predicted Density
predicted_density <- mu %>%
  as_tibble()

# Add building count for each grid
predicted_density <- predicted_density %>%
  mutate(bcount = pred_covs$google_v2_5)

# Estimate Predicted Total HH
predicted_hh_count5 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>%
  select(-bcount)

# Total Predicted Total HH and Uncertainty

hh_model5 <- predicted_hh_count5 %>%
  apply(2, sum, na.rm = T) %>%
  as_tibble() %>%
  summarise(
    mean_count = round(mean(value)),
    upper_quantile = round(quantile(value, probs = 0.975)),
    lower_quantile = round(quantile(value, probs = 0.025))
  )

hh_model5 %>% kable()

# District Estimates ---------------------------------------------

district_names <- pred_covs %>%
  select(DIST_NAME)

# cbind district to data
district_estimates <- cbind(predicted_hh_count5, district_names) %>%
  as_tibble()

# Group by district and split data according to district
district_estimates <- district_estimates %>%
  group_by(DIST_NAME) %>%
  group_split()


# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(district_estimates)) {
  df <- district_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    district_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
district_hh_count <- AA %>%
  as_tibble() %>%
  rename(
    DIST_NAME = district_names, Estimated_Count = mean,
    Lower_Count = "lower_quantile.2.5%",
    Median_Count = "median.50%",
    Upper_Count = "upper_quantile.97.5%"
  )

# EA Estimates --------------------------------------------------

ea_names <- pred_covs %>%
  select(EA_CODE, ea_id)

# cbind to data
ea_names <- cbind(predicted_hh_count5, ea_names) %>%
  as_tibble()

# Group by ea_id and split data according to ea
ea_estimates <- ea_names %>%
  group_by(EA_CODE, ea_id) %>%
  group_split()

# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(ea_estimates)) {
  df <- ea_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$EA_CODE)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    ea_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500),
    sd = sd(df)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
EA_estimates <- as_tibble(AA) %>%
  rename(
    EA_CODE = ea_names,
    mean_estimate = mean,
    median = `median.50%`,
    lower = `lower_quantile.2.5%`,
    upper = `upper_quantile.97.5%`,
    std = sd
  ) %>%
  mutate(
    across(-EA_CODE, ~ as.numeric(.))
  ) %>%
  mutate(
    cv = std / mean_estimate,
    CI = upper - lower
  )

# Write table to file

# Summarize pixel level predictions ---------------------------------------

# Summarize predictions
tic()

mean_count <- rowMeans(predicted_hh_count5, na.rm = T)
median_count <- apply(predicted_hh_count5, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_hh_count5, 1, sd)
lower_quantile <- apply(predicted_hh_count5, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm = T))
upper_quantile <- apply(predicted_hh_count5, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty <- (upper_quantile - lower_quantile) / mean_count
coe_var <- std_count / mean_count

toc()

# sum predictions
sum(median_count, na.rm = T)
sum(mean_count, na.rm = T)

# Cbind predictions to xy coord

pixel_predictions5 <- cbind(
  mean_count, median_count, std_count,
  lower_quantile, upper_quantile, uncertainty, coe_var
) %>%
  as_tibble() %>%
  mutate(
    long = pred_covs$long, lat = pred_covs$lat,
    bcount = pred_covs$google_v2_5,
    hh_density = mean_count / bcount
  )

summary(pixel_predictions5$mean_count)
summary(pixel_predictions5$hh_density)

###############################################################################
###############################################################################
# Model 6 Predictions  --------------------------------------------------------
# Model 6 - Intercept + Covariates + Urban_Rural_Random_Effect +
#           Dist_Random_Effect + EA Random_Effect

# Use the generate function to make predictions
mu <- generate(mod6_gamma,
  newdata = pred_covs, formula = ~ Intercept +
    x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id) +
    Random_dist_eval(dist_id),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# Get iid random effect for EA id
n.samples <- 500

iid.sd <- sqrt(1 / mod6_gamma$summary.hyperpar["Precision for Random_EA", 1])
Random_EA_eval <- matrix(rnorm(nrow(pred_covs) * n.samples, 0, iid.sd),
  nrow = nrow(pred_covs),
  ncol = n.samples
)

# Add random effect to data
mu <- mu + Random_EA_eval

# Predicted Density
predicted_density <- exp(mu) %>%
  as_tibble()

# Add building count for each grid
predicted_density <- predicted_density %>%
  mutate(bcount = pred_covs$google_v2_5)

# Estimate Predicted Total HH
predicted_hh_count6 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>%
  select(-bcount)

# Total Predicted Total HH and Uncertainty

hh_model6 <- predicted_hh_count6 %>%
  apply(2, sum, na.rm = T) %>%
  as_tibble() %>%
  summarise(
    mean_count = round(mean(value)),
    upper_quantile = round(quantile(value, probs = 0.975)),
    lower_quantile = round(quantile(value, probs = 0.025))
  )

hh_model6 %>% kable()

# District Estimates ---------------------------------------------

district_names <- pred_covs %>%
  select(DIST_NAME)

# cbind district to data
district_estimates <- cbind(predicted_hh_count6, district_names) %>%
  as_tibble()

# Group by district and split data according to district
district_estimates <- district_estimates %>%
  group_by(DIST_NAME) %>%
  group_split()


# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(district_estimates)) {
  df <- district_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    district_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
district_hh_count <- AA %>%
  as_tibble() %>%
  rename(
    DIST_NAME = district_names, Estimated_Count = mean,
    Lower_Count = "lower_quantile.2.5%",
    Median_Count = "median.50%",
    Upper_Count = "upper_quantile.97.5%"
  )

# EA Estimates --------------------------------------------------

ea_names <- pred_covs %>%
  select(EA_CODE, ea_id)

# cbind to data
ea_names <- cbind(predicted_hh_count6, ea_names) %>%
  as_tibble()

# Group by ea_id and split data according to ea
ea_estimates <- ea_names %>%
  group_by(EA_CODE, ea_id) %>%
  group_split()

# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(ea_estimates)) {
  df <- ea_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$EA_CODE)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    ea_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500),
    sd = sd(df)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
EA_estimates <- as_tibble(AA) %>%
  rename(
    EA_CODE = ea_names,
    mean_estimate = mean,
    median = `median.50%`,
    lower = `lower_quantile.2.5%`,
    upper = `upper_quantile.97.5%`,
    std = sd
  ) %>%
  mutate(
    across(-EA_CODE, ~ as.numeric(.))
  ) %>%
  mutate(
    cv = std / mean_estimate,
    CI = upper - lower
  )

# Write table to file

# Summarize predictions
tic()

mean_count <- rowMeans(predicted_hh_count6, na.rm = T)
median_count <- apply(predicted_hh_count6, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_hh_count6, 1, sd)
lower_quantile <- apply(predicted_hh_count6, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm = T))
upper_quantile <- apply(predicted_hh_count6, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty <- (upper_quantile - lower_quantile) / mean_count
coe_var <- std_count / mean_count

toc()

# sum predictions
sum(median_count, na.rm = T)
sum(mean_count, na.rm = T)

# Cbind predictions to xy coord

pixel_predictions6 <- cbind(
  mean_count, median_count, std_count,
  lower_quantile, upper_quantile, uncertainty, coe_var
) %>%
  as_tibble() %>%
  mutate(
    long = pred_covs$long, lat = pred_covs$lat,
    bcount = pred_covs$google_v2_5,
    hh_density = mean_count / bcount
  )

summary(pixel_predictions6$mean_count)
summary(pixel_predictions6$hh_density)

##############################################################################
###############################################################################
# # Model 7 - Intercept + Covariates + rural_urban_Random_Effect +
# Dist_Random_Effect+ Spatial Effect

# Use the generate function to make predictions
mu <- generate(mod7_gamma,
  newdata = pred_covs,
  formula = ~ (Intercept + x13 + x63 + x50 + x44 + x49 + x57 + x55 + x56 +
    Random_rural_urban_eval(rural_urban_id) +
    Random_dist_eval(dist_id) +
    Random_Spat_eval(cbind(long, lat))),
  n.samples = 500,
  seed = 2,
  num.threads = "1"
)

# Get iid random effect for EA id
n.samples <- 500

iid.sd <- sqrt(1 / mod7_gamma$summary.hyperpar["Precision for Random_EA", 1])
Random_EA_eval <- matrix(rnorm(nrow(pred_covs) * n.samples, 0, iid.sd),
  nrow = nrow(pred_covs),
  ncol = n.samples
)

# Add random effect to data
mu <- mu + Random_EA_eval

# Predicted Density
predicted_density <- exp(mu) %>%
  as_tibble()

# Add building count for each grid
predicted_density <- predicted_density %>%
  mutate(bcount = pred_covs$google_v2_5)

# Estimate Predicted Total HH
predicted_hh_count7 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>%
  select(-bcount)

# Total Predicted Total HH and Uncertainty

hh_model7 <- predicted_hh_count7 %>%
  apply(2, sum, na.rm = T) %>%
  as_tibble() %>%
  summarise(
    mean_count = round(mean(value)),
    upper_quantile = round(quantile(value, probs = 0.975)),
    lower_quantile = round(quantile(value, probs = 0.025))
  )

hh_model7 %>% kable()


# District Estimates ---------------------------------------------

district_names <- pred_covs %>%
  select(DIST_NAME)

# cbind district to data
district_estimates <- cbind(predicted_hh_count7, district_names) %>%
  as_tibble()

# Group by district and split data according to district
district_estimates <- district_estimates %>%
  group_by(DIST_NAME) %>%
  group_split()


# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(district_estimates)) {
  df <- district_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    district_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
district_hh_count <- AA %>%
  as_tibble() %>%
  rename(
    DIST_NAME = district_names, Estimated_Count = mean,
    Lower_Count = "lower_quantile.2.5%",
    Median_Count = "median.50%",
    Upper_Count = "upper_quantile.97.5%"
  )

# Write to file
write.csv(district_hh_count, paste0(pop_output, "District_HH_Estimates_2024.csv"), row.names = F)

# EA Estimates --------------------------------------------------

ea_names <- pred_covs %>%
  select(EA_CODE, ea_id)

# cbind to data
ea_names <- cbind(predicted_hh_count7, ea_names) %>%
  as_tibble()

# Group by ea_id and split data according to ea
ea_estimates <- ea_names %>%
  group_by(EA_CODE, ea_id) %>%
  group_split()

# for loop to get CI for each admin
OUT <- list()
for (dd in 1:length(ea_estimates)) {
  df <- ea_estimates[[dd]]

  # get the ID of the current area being processed
  typro <- unique(df$EA_CODE)
  print(typro)


  df <- df %>%
    select(starts_with("v")) %>%
    apply(2, sum, na.rm = T)

  OUT[[dd]] <- c(
    ea_names = typro, mean = mean(df),
    lower_quantile = quantile(df, 0.025),
    upper_quantile = quantile(df, 0.975),
    median = quantile(df, 0.500),
    sd = sd(df)
  )

  # print(OUT)
}

AA <- do.call(rbind, OUT)
AA

# Convert to tibble and export as a csv
EA_estimates <- as_tibble(AA) %>%
  rename(
    EA_CODE = ea_names,
    mean_estimate = mean,
    median = `median.50%`,
    lower = `lower_quantile.2.5%`,
    upper = `upper_quantile.97.5%`,
    std = sd
  ) %>%
  mutate(
    across(-EA_CODE, ~ as.numeric(.))
  ) %>%
  mutate(
    cv = std / mean_estimate,
    CI = upper - lower
  )

# Write table to file
write.csv(EA_estimates, paste0(pop_output, "EA_HH_Estimates_2024.csv"), row.names = F)

# Summarize pixel level predictions ---------------------------------------

# Summarize predictions
tic()

mean_count <- rowMeans(predicted_hh_count7, na.rm = T)
median_count <- apply(predicted_hh_count7, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_hh_count7, 1, sd)
lower_quantile <- apply(predicted_hh_count7, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm = T))
upper_quantile <- apply(predicted_hh_count7, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty <- (upper_quantile - lower_quantile) / mean_count
coe_var <- std_count / mean_count

toc()

# sum predictions
sum(median_count, na.rm = T)
sum(mean_count, na.rm = T)

# Cbind predictions to xy coord

pixel_predictions7 <- cbind(
  mean_count, median_count, std_count,
  lower_quantile, upper_quantile, uncertainty, coe_var
) %>%
  as_tibble() %>%
  mutate(
    long = pred_covs$long, lat = pred_covs$lat,
    bcount = pred_covs$google_v2_5,
    hh_density = mean_count / bcount
  )

summary(pixel_predictions7$mean_count)
summary(pixel_predictions7$hh_density)


####################################################################
# Compare predictions

pred <- rbind(hh_model2, hh_model3, hh_model4, hh_model5, hh_model6, hh_model7)
pred %>% kable()

hh <- rbind(
  hh_metrics1, hh_metrics2, hh_metrics3, hh_metrics4,
  hh_metrics5, hh_metrics6, hh_metrics7
)
hh %>% kable()

dens <- rbind(
  density_metrics1, density_metrics2, density_metrics3,
  density_metrics4, density_metrics5, density_metrics6,
  density_metrics7
)
dens %>% kable()

##################################################################################
############### EXPORT BEST MODEL RASTER TO FILE ##################################
###################################################################################

# Rasterize Predictions ---------------------------------------------------
# Base on the model we will rasterize model 7 as best model

# #Convert to sf object
pixel_predictions7 <- st_as_sf(pixel_predictions7, coords = c("long", "lat"))
st_crs(pixel_predictions7) <- 4326

# write to file
st_write(pixel_predictions7, paste0(pop_output, "Predicted_hh_2024.gpkg"), append = T)

# Rasterize mean and export to file
mean_raster <- rasterize(pixel_predictions7, r1, field = "mean_count")
plot(mean_raster)

# export
writeRaster(mean_raster,
  paste0(pop_output, "prediction_hh_mean_2024.tif"),
  overwrite = TRUE, names = "hh_count"
)

# Rasterize sd
std_raster <- rasterize(pixel_predictions7, r1, field = "std_count")

writeRaster(std_raster,
  paste0(pop_output, "prediction_hh_std_2024.tif"),
  overwrite = TRUE, names = "std"
)

# Lower
lower_raster <- rasterize(pixel_predictions7, r1, field = "lower_quantile")

writeRaster(lower_raster,
  paste0(pop_output, "prediction_hh_lower_2024.tif"),
  overwrite = TRUE, names = "lower"
)

# upper
upper_raster <- rasterize(pixel_predictions7, r1, field = "upper_quantile")

# Write to file
writeRaster(upper_raster,
  paste0(pop_output, "prediction_hh_upper_2024.tif"),
  overwrite = TRUE, names = "upper"
)


################## END OF SCRIPT #############################################
############################################################################
############################################################################
