library(INLA)
library(sf)
library(gstat)
library(spdep)
#library(olsrr)
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
drive_path <- "./data/"
input_path <- paste0(drive_path, "Output_Data/")
shapefile_path <- paste0(drive_path, "Shapefiles/")
output_path <- paste0(drive_path, "Output_Data/")
output_path1 <- paste0(drive_path, "Output_Data/Pop_Rasters/")

#Load data
pop_data <-  read.csv(paste0(input_path, "Malawi_2018_data.csv"))
shapefile <- st_read(paste0(shapefile_path, "2018_MPHC_EAs_Final_for_Use_Corrected.shp"))

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

#Calculate pop density
pop_data <- pop_data %>%  
  mutate(pop_density = mphc_total_pop/google_v2_5)

#filter pop_density which is NA
EA_pop <- pop_data %>%  
  drop_na(pop_density) %>%  
  filter(!is.infinite(pop_density))

#check summary stats 
summary(EA_pop$pop_density)

#Boxplot of density distribution
ggplot(data = EA_pop, aes(y=pop_density))+
  geom_boxplot(color="blue", alpha=0.2)


# Density plot of household density
ggplot(data = EA_pop, aes(x = pop_density)) +
  geom_density(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "Pop Density",
    y = "Density",
    title = "Density Plot of Pop Density"
  ) +
  theme_minimal()

# Remove pop density below 30
EA_pop <- EA_pop %>% 
  filter(pop_density < 30)

#plot Pop Density
ggplot(data = EA_pop, aes(x = pop_density)) +
  geom_histogram(
    fill = "blue", 
    alpha = 0.4, 
    color = "blue"
  ) +
  labs(
    x = "Pop Density",
    y = "Density",
    title = "Density Plot"
  ) +
  theme_minimal()


#Boxplot of density
ggplot(data = EA_pop, aes(y=pop_density))+
  geom_boxplot(color="blue", alpha=0.2)


#check summary stats 
summary(EA_pop$pop_density)


##########################################################################################################
# Covariate selection -----------------------------------------------------

#Covs selection
covs <- EA_pop  %>%  
  select(starts_with("x"))

# Calcute mean and standard deviation of covariates
cov_stats <- data.frame(Covariate = colnames(covs),
                        Mean = apply(covs, 2, mean, na.rm = TRUE),
                        Std_Dev = apply(covs, 2, sd, na.rm = TRUE))

#Scaling function to scale covariates
stdize <- function(x)
{ stdz <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
return(stdz) }

#apply scaling function
covs <- apply(covs, 2, stdize) %>%     #z-score
  as_tibble()

#Select response variable and cbind covs

covs_selection <- EA_pop %>%  
  select(pop_density) %>%  
  cbind(covs) 


#drop columns with NAs
covs_selection <- covs_selection %>% 
  select(where(~ !all(is.na(.))))

# Covariate Selection -----------------------------------------------------

#Stepwise covariates selection for pop data

#fit a glm model with poisson distribution
full_model <- glm(pop_density ~ ., data = covs_selection, family = gaussian)

#stepwise selection
step_model1 <- MASS::stepAIC(full_model, direction = "both")
summary(step_model1)
vif(step_model1)

# Function to iteratively drop variables with high VIF
drop_high_vif <- function(model, threshold = 5) {
  # Calcuye VIFs
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
selected_vars <- step1_updated$coefficients%>%  
  names()  # Get the selected variables

# Create model formula
formula_string <- paste("pop_density ~", paste(selected_vars, collapse = " + "))
final_formula <- as.formula(formula_string)

# Print final model formula
print(final_formula)

#function to drop non-significant variables
# Start with full model
current_formula <- as.formula("pop_density ~  x14 + x39 + x41 + x43 + x45 + x46 + 
    x48 + x49 + x50 + x51 + x52 + x54 + x56 + x57 + x58 + x59 + 
    x61 + x62 + x63 + x64")

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
  current_formula <- as.formula(paste("pop_density ~", paste(new_rhs, collapse = " + ")))
}

# Final model
final_model <- model
summary(final_model)
vif(final_model)


# Extract the formula from final model
final_model <- formula(final_model)
final_model 

#selected covariates are still many use LASSO regression to rank covariate importance and drop less
#important covariates

#Fit a model using the LASSO with the caret package

#drop NAs in covariates for LASSO fitting
covs_selection1 <- covs_selection %>%  
  drop_na() 

#Lasso Regression
fit1_lasso <- train(
  pop_density ~ x14 + x39 + x41 + x43 + x45 + x46 + x48 + x50 + 
    x51 + x52 + x54 + x56 + x57 + x58 + x59 + x61 + x62 + x63 + 
    x64,
  data = covs_selection1,
  method = "glmnet",
  metric = "RMSE",  # Choose from RMSE, RSquared, AIC, BIC, ...others?
  tuneGrid = expand.grid(
    .alpha = 1,  # optimize a ridge regression
    .lambda = seq(0, 5, length.out = 101)))

fit1_lasso

#Select important variables

varImp(fit1_lasso)

#Rank variables

plot(varImp(fit1_lasso))


#Selecting covariates with variable importance above 30
# x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48   



#select important variables from pop_data and cbind scaled covariates for model fitting
EA_pop <- EA_pop %>%  
  select(-starts_with("x")) %>%  
  cbind(covs) 

#
#Assign unique values to each row
EA_pop <- EA_pop %>%  
tibble::rowid_to_column("id")


#get distinct count of rural urban
rural_urban_group <- EA_pop %>%  
  distinct(rural_urban_id) %>%  
  nrow()

# #get distinct count of district
dist_groups <- EA_pop %>%  
  distinct(dist_id) %>%  
  nrow()

# #get distinct count of EA
ea_groups <- EA_pop %>%  
  distinct(id) %>%  
  nrow()

#define priors
#hyper.prec = list(theta = list(prior="pc.prec", param=c(0.01,0.01)))
#control.fixed = list(mean=0, prec=1/1000, mean.intercept=0, prec.intercept=1/1000) 

#########################################################################
#########################################################################
# Fit Models --------------------------------------------------------------

#Model1 -  Fixed Effect + Urban_Rural_Random_Effect

formula1 <- pop_density ~  x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))

#fit model using a gamma distribution
mod1_count <- bru(formula1,
                  data = EA_pop,
                  family = "gamma", 
                  options = list(
                    #control.fixed = control.fixed,
                    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                    control.inla = list(int.strategy = "eb"),
                    verbose = FALSE,
                    num.threads = "1"))


summary(mod1_count)

#Extract in-sample predictions
in_sample_predictions1 <- predict(mod1_count, newdata = EA_pop, formula = ~ Intercept + 
                                    x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 + 
                                    Random_rural_urban_eval(rural_urban_id),
                                  n.samples = 100, 
                                  seed = 2,
                                  num.threads = "1" )

#Back transform the mean 
in_sample_predictions1 <- in_sample_predictions1 %>%  
  mutate (predicted_density = exp(mean), 
          predicted_population = predicted_density * google_v2_5) %>%  
  rename(observed_population = mphc_total_pop, observed_density = pop_density)

#sum population
sum(in_sample_predictions1$predicted_population)   # Predicted population
sum(in_sample_predictions1$observed_population, na.rm = T)  # Observed population

#Compute model performance metrics

#Density metrics

density_metrics1 <- in_sample_predictions1 %>%  
  # drop_na(observed_density) %>%  
  mutate(residual = observed_density - predicted_density) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density))

density_metrics1 %>%  
  kable()

#Population Metrics
pop_metrics1 <- in_sample_predictions1 %>%  
  # filter(observed_population > 0) %>%  
  mutate(residual = observed_population - predicted_population) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_population, predicted_population))

pop_metrics1 %>%  
  kable()

#############################################################################
#############################################################################
###############################################################################
#Model 2 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect

formula2 <- pop_density ~  x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 + 
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))


#fit model using a gamma distribution
mod2_count <- bru(formula2,
                  data = EA_pop,
                  family = "gamma", 
                  options = list(
                    #control.fixed = control.fixed,
                    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                    control.inla = list(int.strategy = "eb"),
                    verbose = FALSE,
                    num.threads = "1"))

summary(mod2_count)

#Extract in-sample predictions
in_sample_predictions2 <- predict(mod2_count, newdata = EA_pop, formula = ~ Intercept + 
                                    x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
                                    Random_rural_urban_eval(rural_urban_id)+
                                    Random_dist_eval(dist_id),
                                  n.samples = 100, 
                                  seed = 2,
                                  num.threads = "1" )

#Back transform the mean 
in_sample_predictions2 <- in_sample_predictions2 %>%  
  mutate (predicted_density = exp(mean), 
          predicted_population = predicted_density * google_v2_5) %>%  
  rename(observed_population = mphc_total_pop, observed_density = pop_density)

#sum population
sum(in_sample_predictions2$predicted_population)   # Predicted population
sum(in_sample_predictions2$observed_population)  # Observed population

#Compute model performance metrics

#Density metrics

density_metrics2 <- in_sample_predictions2 %>%  
  #drop_na(observed_density) %>%  
  mutate(residual = observed_density - predicted_density) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density))

density_metrics2 %>%  
  kable()

#Population Metrics
pop_metrics2 <- in_sample_predictions2 %>%  
  #filter(observed_population > 0) %>%  
  mutate(residual = observed_population - predicted_population) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_population, predicted_population))

pop_metrics2 %>%  
  kable()

#############################################################################
#############################################################################
###############################################################################
#Model 3 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect + EA Random_Effect

formula3 <- pop_density ~  x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
  Random_rural_urban(rural_urban_id, model = "iid",  mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
  Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups))


#fit model using a gamma distribution
mod3_count <- bru(formula3,
                  data = EA_pop,
                  family = "gamma", 
                  options = list(
                    #control.fixed = control.fixed,
                    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                    control.inla = list(int.strategy = "eb"),
                    verbose = FALSE,
                    num.threads = "1"))
                  

summary(mod3_count)

#Extract in-sample predictions
in_sample_predictions3 <- predict(mod3_count, newdata = EA_pop, formula = ~ Intercept + 
                                    x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 + 
                                    Random_rural_urban_eval(rural_urban_id)+
                                    Random_EA_eval(id)+
                                    Random_dist_eval(dist_id),
                                  n.samples = 100, 
                                  seed = 2,
                                  num.threads = "1" )

#Back transform the mean 
in_sample_predictions3 <- in_sample_predictions3 %>%  
  mutate (predicted_density = exp(mean), 
          predicted_population = predicted_density * google_v2_5) %>%  
  rename(observed_population = mphc_total_pop, observed_density = pop_density)

#sum population
sum(in_sample_predictions3$predicted_population)   # Predicted population
sum(in_sample_predictions3$observed_population)  # Observed population

#Compute model performance metrics

#Density metrics

density_metrics3 <- in_sample_predictions3 %>%  
  #drop_na(observed_density) %>%  
  mutate(residual = observed_density - predicted_density) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density))

density_metrics3 %>%  
  kable()

#Pop metrics
pop_metrics3 <- in_sample_predictions3 %>%  
  #filter(observed_population > 0) %>%  
  mutate(residual = observed_population - predicted_population) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_population, predicted_population))

pop_metrics3 %>%  
  kable()


##############################################################################
#############################################################################
# Geostatistical Model - INLA SPDE ----------------------------------------

#-Define the coordinates of centroids
coords <- cbind(EA_pop$long, EA_pop$lat) 

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

#specify model 

formula4 <- pop_density ~  x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
  Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
  Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
  Random_EA(id, model = "iid",  mapper = bru_mapper_index(n = ea_groups))+
  Random_Spat(main = coords, model = spde)

#fit model using a gamma distribution
mod4_count <- bru(formula4,
                  data = EA_pop,
                  family = "gamma", 
                  options = list(
                    #control.fixed = control.fixed,
                    control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                    control.inla = list(int.strategy = "eb"),
                    verbose = FALSE,
                    num.threads = "1"))

summary(mod4_count)

#Extract in-sample predictions
in_sample_predictions4 <- predict(mod4_count, newdata = EA_pop, formula = ~ Intercept + 
                                    x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 + 
                                    Random_rural_urban_eval(rural_urban_id)+
                                    Random_EA_eval(id)+
                                    Random_dist_eval(dist_id)+
                                    Random_Spat_eval(cbind(long, lat)),
                                    n.samples = 100, 
                                    seed = 2,
                                    num.threads = "1" )

#Back transform the mean 
in_sample_predictions4 <- in_sample_predictions4 %>%  
  mutate (predicted_density = exp(mean), 
          predicted_population = predicted_density * google_v2_5) %>%  
  rename(observed_population = mphc_total_pop, observed_density = pop_density)

#sum population
sum(in_sample_predictions4$predicted_population)   # Predicted population
sum(in_sample_predictions4$observed_population)  # Observed population

#Compute model performance metrics

#Density metrics

density_metrics4 <- in_sample_predictions4 %>%  
  #drop_na(observed_density) %>%  
  mutate(residual = observed_density - predicted_density) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_density, predicted_density))

density_metrics4 %>%  
  kable()

#Total Population
pop_metrics4 <- in_sample_predictions4 %>%  
  # filter(observed_population > 0) %>%  
  mutate(residual = observed_population - predicted_population) %>%  
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(observed_population, predicted_population))

pop_metrics4 %>%  
  kable()


# Model Checks ------------------------------------------------------------

t(c(mod1_count=mod1_count$dic$dic, mod2_count=mod2_count$dic$dic,
    mod3_count=mod3_count$dic$dic, mod4_count=mod4_count$dic$dic))

#compare models
pop <- rbind(pop_metrics1, pop_metrics2, pop_metrics3, pop_metrics4)
pop%>%   kable()

dens <- rbind(density_metrics1, density_metrics2, density_metrics3, density_metrics4)
dens %>%   kable()


##############################################################################################
##############################################################################################
########## CROSS VALIDATION #############################################################

#Xval data
xval_data <- EA_pop  

# K-Fold Cross Validation for Model 4-------------------------------------------------

# function to calculate k-fold
kfold_cv <- function(data, k) {
  n <- nrow(data)
  fold_size <- n %/% k
  folds <- sample(rep(1:k, each = fold_size, length.out = n))
  
  # Create separate dataframes for train and test metrics
  train_metrics <- data.frame()
  test_metrics <- data.frame()
  
  # Place holder for train metrics calculation ----------------------------
  
  #density metrics
  dens_train_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_train_pearson_values <- numeric(k) # Placeholder for corr
  dens_train_mae_values <- numeric(k)  # Placeholder for MAE
  dens_train_bias_values <- numeric(k)  # Placeholder for bias
  
  #HH metrics
  pop_train_rmse_values <- numeric(k) # Placeholder for RMSE
  pop_train_pearson_values <- numeric(k) # Placeholder for corr
  pop_train_mae_values <- numeric(k)  # Placeholder for MAE
  pop_train_bias_values <- numeric(k)  # Placeholder for bias
  
  
  # Place holder for test metrics calculation -------------------------
  
  #Density metrics
  dens_test_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_test_pearson_values <- numeric(k) # Placeholder for corr
  dens_test_mae_values <- numeric(k)  # Placeholder for MAE
  dens_test_bias_values <- numeric(k)  # Placeholder for bias
  
  #HH metrics
  pop_test_rmse_values <- numeric(k) # Placeholder for RMSE
  pop_test_pearson_values <- numeric(k) # Placeholder for corr
  pop_test_mae_values <- numeric(k)  # Placeholder for MAE
  pop_test_bias_values <- numeric(k)  # Placeholder for bias
  
  # Initialize data frame to store test values and predictions outside the loop
  results_df <- data.frame(random_effect = numeric(), Total_Building_Count = numeric(), 
                           folds = numeric(), observed_density = numeric(), 
                           predicted_density = numeric(), observed_population = numeric(), 
                           predicted_population =numeric())
  
  train_results_df <- data.frame(random_effect = numeric(), Total_Building_Count = numeric(), 
                           folds = numeric(), observed_density = numeric(), 
                           predicted_density = numeric(), observed_population = numeric(), 
                           predicted_population =numeric())
  
  # For loop for implementation ---------------------------------------------
  for (i in 1:k) {
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    #create a new cluster id for the train data
    train_data <- train_data %>% 
      rowid_to_column("idx")
    
    # #get distinct count of idx
    train_id_groups <- train_data %>% 
      distinct(idx) %>% 
      nrow()
    
    #create a new idx in test data for evaluation
    test_data <- test_data %>% 
      mutate(idx = (train_id_groups+1) : (train_id_groups + nrow(test_data)))
    
    print(paste("Processing fold", i, "out of", k))
    
    #-Define the coordinates of centroids
    coords <- cbind(train_data$long, train_data$lat) 
    
    #measure distance between coordinates
    summary(dist(coords)) #summarizes the Euclidean distance between points in the spatial domain
    
    
    #build non-convex hull mesh
    non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
    mesh <- fm_mesh_2d_inla(boundary = non_convex_bdry, max.edge=c(0.1, 1), 
                            offset = c(0.05, 1),
                            cutoff = 0.003)
    
    #Build the SPDE
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
    
    #Model formula
    formula4 <- pop_density ~  x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
      Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
      Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
      Random_EA(idx, model = "iid", mapper = bru_mapper_index(n = train_id_groups))+
      Random_Spat(main = coords, model = spde)
    
    #fit model using a gamma distribution
    mod2 <- bru(formula4,
                data = train_data,
                family = "gamma", 
                options = list(
                  control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                  control.inla = list(int.strategy = "eb"),
                  verbose = FALSE))
    
    #summary(mod2)
    
    #Make Predictions for train data
    train_predictions <- predict(mod2, newdata = train_data, formula = ~ Intercept + 
                                   x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
                                   Random_rural_urban_eval(rural_urban_id)+
                                   Random_EA_eval(idx)+
                                   Random_dist_eval(dist_id)+
                                   Random_Spat_eval(cbind(long, lat)),
                                 n.samples = 100, 
                                 seed = 2)
    
    #Back transform the mean 
    train_predictions  <- train_predictions %>% 
      mutate (predicted_density = exp(mean), 
              predicted_population = predicted_density * google_v2_5) %>% 
      rename(observed_population = mphc_total_pop, observed_density = pop_density)
    
    
    #Train data metrics
    #Density
    dens_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_density - train_predictions$predicted_density)^2))
    dens_train_pearson_values[i] <- cor(train_predictions$observed_density, train_predictions$predicted_density)
    dens_train_mae_values[i] <- mean(abs(train_predictions$observed_density - train_predictions$predicted_density))
    dens_train_bias_values[i] <- mean(train_predictions$observed_density - train_predictions$predicted_density)
    
    #Population
    pop_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_population - train_predictions$predicted_population)^2))
    pop_train_pearson_values[i] <- cor(train_predictions$observed_population, train_predictions$predicted_population)
    pop_train_mae_values[i] <- mean(abs(train_predictions$observed_population - train_predictions$predicted_population))
    pop_train_bias_values[i] <- mean(train_predictions$observed_population - train_predictions$predicted_population)
    
    # Make Predictions for Test Data ------------------------------------------
    
    #Make Predictions for test data
    test_predictions <- predict(mod2, newdata = test_data, formula = ~ Intercept + 
                                  x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
                                  Random_rural_urban_eval(rural_urban_id)+
                                  Random_EA_eval(idx)+
                                  Random_dist_eval(dist_id)+
                                  Random_Spat_eval(cbind(long, lat)),
                                n.samples = 100, 
                                seed = 2)
    
    #Back transform the mean 
    test_predictions  <- test_predictions %>% 
      mutate (predicted_density = exp(mean), 
              predicted_population = predicted_density * google_v2_5) %>% 
      rename(observed_population = mphc_total_pop, observed_density = pop_density)
    
    #Test data metrics
    #Density
    dens_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_density - test_predictions$predicted_density)^2))
    dens_test_pearson_values[i] <- cor(test_predictions$observed_density, test_predictions$predicted_density)
    dens_test_mae_values[i] <- mean(abs(test_predictions$observed_density - test_predictions$predicted_density))
    dens_test_bias_values[i] <- mean(test_predictions$observed_density - test_predictions$predicted_density)
    
    #Population
    pop_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_population - test_predictions$predicted_population)^2))
    pop_test_pearson_values[i] <- cor(test_predictions$observed_population, test_predictions$predicted_population)
    pop_test_mae_values[i] <- mean(abs(test_predictions$observed_population - test_predictions$predicted_population))
    pop_test_bias_values[i] <- mean(test_predictions$observed_population - test_predictions$predicted_population)
    
    # Append observed and predictions to results_df with fold information
    fold_results <- data.frame(random_effect = test_predictions$id, 
                               Total_Building_Count = test_predictions$google_v2_5,
                               folds = i, observed_density = test_predictions$observed_density, 
                               predicted_density = test_predictions$predicted_density, 
                               observed_population= test_predictions$observed_population, 
                               predicted_population = test_predictions$predicted_population)
    
    results_df <- rbind(results_df, fold_results)
    
    
    # Train metrics
    train_metrics <- data.frame(Model = "Model 4",
                                dens_train_rmse = mean(dens_train_rmse_values),
                                dens_train_corr = mean(dens_train_pearson_values),
                                dens_train_mae = mean(dens_train_mae_values),
                                dens_train_bias = mean(dens_train_bias_values),
                                pop_train_rmse = mean(pop_train_rmse_values),
                                pop_train_corr = mean(pop_train_pearson_values),
                                pop_train_mae = mean(pop_train_mae_values),
                                pop_train_bias = mean(pop_train_bias_values)
                                
    )
    
    # Test metrics
    test_metrics <- data.frame(Model = "Model 4",
                               dens_test_rmse = mean(dens_test_rmse_values),
                               dens_test_corr = mean(dens_test_pearson_values),
                               dens_test_mae = mean(dens_test_mae_values),
                               dens_test_bias = mean(dens_test_bias_values),
                               pop_test_rmse = mean(pop_test_rmse_values),
                               pop_test_corr = mean(pop_test_pearson_values),
                               pop_test_mae = mean(pop_test_mae_values),
                               pop_test_bias = mean(pop_test_bias_values)
    )
  }
  
  # Return separate lists for density and population metrics
  list(train_metrics = train_metrics, test_metrics = test_metrics, 
       results_df = results_df, train_results_df = train_results_df)
  
  
}

# Apply function
result1 <- kfold_cv(data = xval_data, k = 10)

#Train data results
result1$train_metrics %>% 
  kable()

#Test data results
result1$test_metrics %>% 
  kable()

#Get results as a dataframe
result_df <- result1$results_df %>% 
  as_tibble()

#write to file
#write.csv(paste0(output_path1, "random_kfold_prediction.csv"), row.names = T)

############################################################################################
############################################################################################
# INLA Group CV Model 2-------------------------------------------------

# function to calculate k-fold
lgocv <- function(data, k) {
  n <- nrow(data)
  fold_size <- n %/% k
  folds <- sample(rep(1:k, each = fold_size, length.out = n))
  
  # Create separate dataframes for train and test metrics
  train_metrics <- data.frame()
  test_metrics <- data.frame()
  
  # Place holder for train metrics calculation ----------------------------
  
  #density metrics
  dens_train_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_train_pearson_values <- numeric(k) # Placeholder for corr
  dens_train_mae_values <- numeric(k)  # Placeholder for MAE
  dens_train_bias_values <- numeric(k)  # Placeholder for bias
  
  #HH metrics
  pop_train_rmse_values <- numeric(k) # Placeholder for RMSE
  pop_train_pearson_values <- numeric(k) # Placeholder for corr
  pop_train_mae_values <- numeric(k)  # Placeholder for MAE
  pop_train_bias_values <- numeric(k)  # Placeholder for bias
  
  
  # Place holder for test metrics calculation -------------------------
  
  #Density metrics
  dens_test_rmse_values <- numeric(k) # Placeholder for RMSE
  dens_test_pearson_values <- numeric(k) # Placeholder for corr
  dens_test_mae_values <- numeric(k)  # Placeholder for MAE
  dens_test_bias_values <- numeric(k)  # Placeholder for bias
  
  #HH metrics
  pop_test_rmse_values <- numeric(k) # Placeholder for RMSE
  pop_test_pearson_values <- numeric(k) # Placeholder for corr
  pop_test_mae_values <- numeric(k)  # Placeholder for MAE
  pop_test_bias_values <- numeric(k)  # Placeholder for bias
  
  # Initialize data frame to store test values and predictions outside the loop
  results_df <- data.frame(random_effect = numeric(), Total_Building_Count = numeric(), 
                           folds = numeric(), observed_density = numeric(), 
                           predicted_density = numeric(), observed_population = numeric(), 
                           predicted_population =numeric())
  
  # For loop for implementation ---------------------------------------------
  for (i in 1:k) {
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    #tag train dataset
    train_data <- train_data  %>% 
      mutate(observed_data = pop_density, dataset = "train")
    
    #tag test dataset
    test_data <- test_data %>% 
      mutate(observed_data = NA, dataset = "test")
    
    #combine data
    xval_data <- rbind(train_data, test_data)
    
    #-Define the coordinates of centroids
    coords <- cbind(xval_data$long, xval_data$lat) 
    
    #measure distance between coordinates
    summary(dist(coords)) #summarizes the Euclidean distance between points in the spatial domain
    
    
    #build non-convex hull mesh
    non_convex_bdry <- inla.nonconvex.hull(coords, -0.03, -0.05, resolution = c(100, 100))
    mesh <- fm_mesh_2d_inla(boundary = non_convex_bdry, max.edge=c(0.1, 1), 
                            offset = c(0.05, 1),
                            cutoff = 0.003)
    
    #Build the SPDE
    spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)
    
    #print fold
    print(paste("Processing fold", i, "out of", k))
    
    formula4 <- observed_data ~ x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
      Random_rural_urban(rural_urban_id, model = "iid", mapper = bru_mapper_index(n = rural_urban_group))+
      Random_dist(dist_id, model = "iid", mapper = bru_mapper_index(n = dist_groups))+
      Random_EA(id, model = "iid", mapper = bru_mapper_index(n = ea_groups))+
      Random_Spat(main = coords, model = spde)
    
    #fit model using a gamma distribution
    mod2 <- bru(formula4,
                data = xval_data,
                family = "gamma", 
                options = list(
                  control.compute = list(waic = TRUE, cpo = TRUE, dic = TRUE),
                  control.inla = list(int.strategy = "eb"),
                  verbose = FALSE))
    
    #summary(mod2)
    
    #LGOCV
    results <- inla.group.cv(mod2, num.level.sets = 3)
    
    #Mean predictions
    cv <- data.frame(mean = results$mean)
    
    predictions <- cbind(xval_data, cv) %>% 
      as_tibble()
    
    
    #Back transform the mean 
    train_predictions  <- predictions %>% 
      filter(dataset == "train") %>% 
      mutate (predicted_density = exp(mean), 
              predicted_population = predicted_density * google_v2_5) %>% 
      rename(observed_population = mphc_total_pop, observed_density = pop_density)
    
    
    #Train data metrics
    #Density
    dens_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_density - train_predictions$predicted_density)^2))
    dens_train_pearson_values[i] <- cor(train_predictions$observed_density, train_predictions$predicted_density)
    dens_train_mae_values[i] <- mean(abs(train_predictions$observed_density - train_predictions$predicted_density))
    dens_train_bias_values[i] <- mean(train_predictions$observed_density - train_predictions$predicted_density)
    
    #Population
    pop_train_rmse_values[i] <- sqrt(mean((train_predictions$observed_population - train_predictions$predicted_population)^2))
    pop_train_pearson_values[i] <- cor(train_predictions$observed_population, train_predictions$predicted_population)
    pop_train_mae_values[i] <- mean(abs(train_predictions$observed_population- train_predictions$predicted_population))
    pop_train_bias_values[i] <- mean(train_predictions$observed_population- train_predictions$predicted_population)
    
    #Test Predictions
    
    #Back transform the mean 
    test_predictions  <- predictions %>% 
      filter(dataset == "test") %>% 
      mutate (predicted_density = exp(mean), 
              predicted_population = predicted_density * google_v2_5) %>% 
      rename(observed_population = mphc_total_pop, observed_density = pop_density)
    
    #Test data metrics
    #Density
    dens_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_density - test_predictions$predicted_density)^2))
    dens_test_pearson_values[i] <- cor(test_predictions$observed_density, test_predictions$predicted_density)
    dens_test_mae_values[i] <- mean(abs(test_predictions$observed_density - test_predictions$predicted_density))
    dens_test_bias_values[i] <- mean(test_predictions$observed_density - test_predictions$predicted_density)
    
    #Population
    pop_test_rmse_values[i] <- sqrt(mean((test_predictions$observed_population - test_predictions$predicted_population)^2))
    pop_test_pearson_values[i] <- cor(test_predictions$observed_population, test_predictions$predicted_population)
    pop_test_mae_values[i] <- mean(abs(test_predictions$observed_population - test_predictions$predicted_population))
    pop_test_bias_values[i] <- mean(test_predictions$observed_population - test_predictions$predicted_population)
    
    # Append observed and predictions to results_df with fold information
    fold_results <- data.frame(random_effect = test_predictions$id, 
                               Total_Building_Count = test_predictions$google_v2_5,
                               folds = i, observed_density = test_predictions$observed_density, 
                               predicted_density = test_predictions$predicted_density, 
                               observed_population = test_predictions$observed_population, 
                               predicted_population = test_predictions$predicted_population)
    
    results_df <- rbind(results_df, fold_results)
    
    
    # Train metrics
    train_metrics <- data.frame(Model = "Model 4",
                                dens_train_rmse = mean(dens_train_rmse_values),
                                dens_train_corr = mean(dens_train_pearson_values),
                                dens_train_mae = mean(dens_train_mae_values),
                                dens_train_bias = mean(dens_train_bias_values),
                                pop_train_rmse = mean(pop_train_rmse_values),
                                pop_train_corr = mean(pop_train_pearson_values),
                                pop_train_mae = mean(pop_train_mae_values),
                                pop_train_bias = mean(pop_train_bias_values)
                                
    )
    
    # Test metrics
    test_metrics <- data.frame(Model = "Model 4",
                               dens_test_rmse = mean(dens_test_rmse_values),
                               dens_test_corr = mean(dens_test_pearson_values),
                               dens_test_mae = mean(dens_test_mae_values),
                               dens_test_bias = mean(dens_test_bias_values),
                               pop_test_rmse = mean(pop_test_rmse_values),
                               pop_test_corr = mean(pop_test_pearson_values),
                               pop_test_mae = mean(pop_test_mae_values),
                               pop_test_bias = mean(pop_test_bias_values)
    )
  }
  
  # Return separate lists for density and population metrics
  list(train_metrics = train_metrics, test_metrics = test_metrics, results_df = results_df)
  
  
}

# Apply function
result2 <- lgocv(data = xval_data, k = 10)

#Train data results
result2$train_metrics %>% 
  kable()

#Test data results
result2$test_metrics %>% 
  kable()

#Get results as a dataframe
result_df <- result2$results_df %>% 
  as_tibble()

#write to file
#write.csv(paste0(output_path1, "spatial_kfold_prediction.csv"), row.names = T)
#################################################################
# Compare all Kfolds

kfold1 <- rbind(result1$train_metrics, result2$train_metrics) %>%  kable()
kfold1

kfold2 <- rbind(result1$test_metrics, result2$test_metrics) %>%  kable()
kfold2


#########################################################################################################
#########################################################################################################
################## Make Predictions #####################################################################

#load covariates
pred_covs <-  read_feather(paste0(input_path, "Malawi_covs_stack.feather"))
r1 <- rast(paste0(input_path, "country_raster.tif"))

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
  select(starts_with("x"))%>% 
  names()

pred_covs[, vars] <- apply(pred_covs[,vars], 2, stdize)

#Get mean and standard devaition from cov_stats
#means <- setNames(cov_stats$Mean, cov_stats$Covariate)
#sds   <- setNames(cov_stats$Std_Dev, cov_stats$Covariate)

# Apply z-score standardization using the means and sds
#pred_covs <- pred_covs %>%
  #mutate(across(all_of(vars), ~ (. - means[cur_column()]) / sds[cur_column()]))

#check scaled covariates
head(pred_covs)

#Replace NAs with 0 to avoid numerical issues
pred_covs <- pred_covs %>% 
  mutate_at(vars(starts_with("x")), ~replace(., is.na(.), 0))

#######################################################################################
######################### EXTRAPOLATE TO 2024 POPULATION ######################

# Model 1 Predictions  --------------------------------------------------------
# Fixed Effect + Urban_Rural_Random_Effect

#Use the generate function to make predictions
mu <- generate(mod1_count, newdata = pred_covs, formula = ~ Intercept +
                x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
                 Random_rural_urban_eval(rural_urban_id),
               n.samples = 100, 
               seed = 2,
               num.threads = "1" )

#Back transform mu
predicted_density <- exp(mu) %>% 
  as_tibble()

#Add building count for each grid
predicted_density <- predicted_density %>% 
  mutate(bcount = pred_covs$bcount)

#Estimate predicted population
predicted_population1 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>% 
  select(-bcount)

#Total Predicted hh count and Uncertainty

pop_model1 <- predicted_population1 %>% 
  apply(2, sum, na.rm = T) %>% 
  as_tibble()%>% 
  summarise(mean_population = round(mean(value)),
            upper_quantile = round(quantile(value, probs=0.975)),
            lower_quantile = round(quantile(value, probs =0.025)))

pop_model1 %>%  kable()

# District HH Estimates ---------------------------------------------

district_names <- pred_covs %>% 
  select(DIST_NAME)

#cbind district to data
district_estimates <- cbind(predicted_population1, district_names) %>% 
  as_tibble()

#Group by district and split data according to district
district_estimates <- district_estimates %>% 
  group_by(DIST_NAME) %>% 
  group_split()


#for loop to get CI for each admin
OUT <- list()
for(dd in 1:length(district_estimates)){
  
  df <- district_estimates[[dd]]
  
  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)
  
  
  df <- df %>% 
    select(starts_with("v")) %>% 
    apply(2, sum, na.rm = T)  
  
  OUT[[dd]] <- c(district_names = typro, mean = mean(df),
                 lower_quantile = quantile(df, 0.025),
                 upper_quantile = quantile(df, 0.975),
                 median = quantile(df, 0.500))
  
  #print(OUT)
}

AA <- do.call(rbind, OUT)
AA

#Convert to tibble and export as a csv
district_pop_count <- AA %>% 
  as_tibble() %>% 
  rename(DIST_NAME = district_names, Estimated_Population = mean, 
         Lower_Population = "lower_quantile.2.5%" ,
         Median_Population = "median.50%", 
         Upper_Population = "upper_quantile.97.5%")

#Write to file

# Summarize pixel level predictions ---------------------------------------

#Summarize predictions
tic() 

mean_count <- rowMeans(predicted_population1, na.rm = T)
Median_Population <- apply(predicted_population1, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_population1, 1, sd)
lower_quantile <- apply(predicted_population1, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm=T))
upper_quantile <- apply(predicted_population1, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty = (upper_quantile - lower_quantile)/mean_count
coe_var = std_count/mean_count

toc()

#sum predictions
sum(Median_Population, na.rm = T)
sum(mean_count, na.rm = T)

#Cbind predictions to xy coord

pixel_predictions1 <- cbind(mean_count, Median_Population, std_count,
                           lower_quantile, upper_quantile, uncertainty, coe_var) %>% 
  as_tibble() %>% 
  mutate(long = pred_covs$long, lat = pred_covs$lat,
         bcount = pred_covs$bcount,
         pop_density = mean_count/bcount)

summary(pixel_predictions1$mean_count)
summary(pixel_predictions1$pop_density)

###############################################################################
# Model 2 Predictions  --------------------------------------------------------
#Model 2 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect

#Use the generate function to make predictions
mu <- generate(mod2_count, newdata = pred_covs, formula = ~ Intercept +
                 x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 + 
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id), 
                 n.samples = 100, 
                 seed = 2,
                 num.threads = "1" )


#Back transform mu
predicted_density <- exp(mu) %>% 
  as_tibble()

#Add building count for each grid
predicted_density <- predicted_density %>% 
  mutate(bcount = pred_covs$bcount)

#Estimate predicted population
predicted_population2 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>% 
  select(-bcount)

#Total Predicted hh count and Uncertainty

pop_model2 <- predicted_population2 %>% 
  apply(2, sum, na.rm = T) %>% 
  as_tibble()%>% 
  summarise(mean_population = round(mean(value)),
            upper_quantile = round(quantile(value, probs=0.975)),
            lower_quantile = round(quantile(value, probs =0.025)))

pop_model2 %>%  kable()

# District HH Estimates ---------------------------------------------

district_names <- pred_covs %>% 
  select(DIST_NAME)

#cbind district to data
district_estimates <- cbind(predicted_population2, district_names) %>% 
  as_tibble()

#Group by district and split data according to district
district_estimates <- district_estimates %>% 
  group_by(DIST_NAME) %>% 
  group_split()


#for loop to get CI for each admin
OUT <- list()
for(dd in 1:length(district_estimates)){
  
  df <- district_estimates[[dd]]
  
  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)
  
  
  df <- df %>% 
    select(starts_with("v")) %>% 
    apply(2, sum, na.rm = T)  
  
  OUT[[dd]] <- c(district_names = typro, mean = mean(df),
                 lower_quantile = quantile(df, 0.025),
                 upper_quantile = quantile(df, 0.975),
                 median = quantile(df, 0.500))
  
  #print(OUT)
}

AA <- do.call(rbind, OUT)
AA

#Convert to tibble and export as a csv
district_pop_count <- AA %>% 
  as_tibble() %>% 
  rename(DIST_NAME = district_names, Estimated_Population = mean, 
         Lower_Population = "lower_quantile.2.5%" ,
         Median_Population = "median.50%", 
         Upper_Population = "upper_quantile.97.5%")

#Write to file

# Summarize pixel level predictions ---------------------------------------

#Summarize predictions
tic() 

mean_count <- rowMeans(predicted_population2, na.rm = T)
Median_Population <- apply(predicted_population2, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_population2, 1, sd)
lower_quantile <- apply(predicted_population2, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm=T))
upper_quantile <- apply(predicted_population2, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty = (upper_quantile - lower_quantile)/mean_count
coe_var = std_count/mean_count

toc()

#sum predictions
sum(Median_Population, na.rm = T)
sum(mean_count, na.rm = T)

#Cbind predictions to xy coord

pixel_predictions2 <- cbind(mean_count, Median_Population, std_count,
                           lower_quantile, upper_quantile, uncertainty, coe_var) %>% 
  as_tibble() %>% 
  mutate(long = pred_covs$long, lat = pred_covs$lat,
         bcount = pred_covs$bcount,
         pop_density = mean_count/bcount)

summary(pixel_predictions2$mean_count)
summary(pixel_predictions2$pop_density)

###############################################################################
# Model 3 Predictions  --------------------------------------------------------
#Model 3 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect + EA Random_Effect

#Use the generate function to make predictions
mu <- generate(mod3_count, newdata = pred_covs, formula = ~ Intercept +
                 x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 + 
                 Random_rural_urban_eval(rural_urban_id)+
                # Random_EA_eval(id)+
                 Random_dist_eval(dist_id), 
               n.samples = 100, 
               seed = 2,
               num.threads = "1" )

#Get iid random effect for EA id
n.samples = 100

iid.sd <- sqrt(1 / mod3_count$summary.hyperpar["Precision for Random_EA",1])
Random_EA_eval <- matrix(rnorm(nrow(pred_covs)*n.samples, 0, iid.sd),
                           nrow = nrow(pred_covs),
                           ncol = n.samples)

#Add random effect to data
mu <- mu + Random_EA_eval


#Back transform mu
predicted_density <- exp(mu) %>% 
  as_tibble()

#Add building count for each grid
predicted_density <- predicted_density %>% 
  mutate(bcount = pred_covs$bcount)

#Estimate predicted population
predicted_population3 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>% 
  select(-bcount)

#Total Predicted hh count and Uncertainty

pop_model3 <- predicted_population3 %>% 
  apply(2, sum, na.rm = T) %>% 
  as_tibble()%>% 
  summarise(mean_population = round(mean(value)),
            upper_quantile = round(quantile(value, probs=0.975)),
            lower_quantile = round(quantile(value, probs =0.025)))

pop_model3 %>%  kable()

# District HH Estimates ---------------------------------------------

district_names <- pred_covs %>% 
  select(DIST_NAME)

#cbind district to data
district_estimates <- cbind(predicted_population3, district_names) %>% 
  as_tibble()

#Group by district and split data according to district
district_estimates <- district_estimates %>% 
  group_by(DIST_NAME) %>% 
  group_split()


#for loop to get CI for each admin
OUT <- list()
for(dd in 1:length(district_estimates)){
  
  df <- district_estimates[[dd]]
  
  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)
  
  
  df <- df %>% 
    select(starts_with("v")) %>% 
    apply(2, sum, na.rm = T)  
  
  OUT[[dd]] <- c(district_names = typro, mean = mean(df),
                 lower_quantile = quantile(df, 0.025),
                 upper_quantile = quantile(df, 0.975),
                 median = quantile(df, 0.500))
  
  #print(OUT)
}

AA <- do.call(rbind, OUT)
AA

#Convert to tibble and export as a csv
district_pop_count <- AA %>% 
  as_tibble() %>% 
  rename(DIST_NAME = district_names, Estimated_Population = mean, 
         Lower_Population = "lower_quantile.2.5%" ,
         Median_Population = "median.50%", 
         Upper_Population = "upper_quantile.97.5%")

#Write to file

# Summarize pixel level predictions ---------------------------------------

#Summarize predictions
tic() 

mean_count <- rowMeans(predicted_population3, na.rm = T)
Median_Population <- apply(predicted_population3, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_population3, 1, sd)
lower_quantile <- apply(predicted_population3, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm=T))
upper_quantile <- apply(predicted_population3, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty = (upper_quantile - lower_quantile)/mean_count
coe_var = std_count/mean_count

toc()

#sum predictions
sum(Median_Population, na.rm = T)
sum(mean_count, na.rm = T)

#Cbind predictions to xy coord

pixel_predictions3 <- cbind(mean_count, Median_Population, std_count,
                            lower_quantile, upper_quantile, uncertainty, coe_var) %>% 
  as_tibble() %>% 
  mutate(long = pred_covs$long, lat = pred_covs$lat,
         bcount = pred_covs$bcount,
         pop_density = mean_count/bcount)

summary(pixel_predictions3$mean_count)
summary(pixel_predictions3$pop_density)


###############################################################################
# Model 4 Predictions  --------------------------------------------------------
#Model 4 - Fixed Effect + Urban_Rural_Random_Effect + Dist_Random_Effect + 
# EA Random_Effect + Spatial Effect

#Use the generate function to make predictions
mu <- generate(mod4_count, newdata = pred_covs, formula = ~ Intercept +
                 x14 + x63 + x59 + x45 + x54 + x52 + x50 + x48 +
                 Random_rural_urban_eval(rural_urban_id)+
                 Random_dist_eval(dist_id)+
                 Random_Spat_eval(cbind(long, lat)),, 
               n.samples = 100, 
               seed = 2,
               num.threads = "1" )

#Get iid random effect for EA id
n.samples = 100

iid.sd <- sqrt(1 / mod4_count$summary.hyperpar["Precision for Random_EA",1])
Random_EA_eval <- matrix(rnorm(nrow(pred_covs)*n.samples, 0, iid.sd),
                         nrow = nrow(pred_covs),
                         ncol = n.samples)

#Add random effect to data
mu <- mu + Random_EA_eval


#Back transform mu
predicted_density <- exp(mu) %>% 
  as_tibble()

#Add building count for each grid
predicted_density <- predicted_density %>% 
  mutate(bcount = pred_covs$bcount)

#Estimate predicted population
predicted_population4 <- predicted_density %>%
  mutate_at(vars(starts_with("v")), ~ . * bcount) %>% 
  select(-bcount)

#Total Predicted hh count and Uncertainty

pop_model4 <- predicted_population4 %>% 
  apply(2, sum, na.rm = T) %>% 
  as_tibble()%>% 
  summarise(mean_population = round(mean(value)),
            upper_quantile = round(quantile(value, probs=0.975)),
            lower_quantile = round(quantile(value, probs =0.025)))

pop_model4 %>%  kable()

# District HH Estimates ---------------------------------------------

district_names <- pred_covs %>% 
  select(DIST_NAME)

#cbind district to data
district_estimates <- cbind(predicted_population4, district_names) %>% 
  as_tibble()

#Group by district and split data according to district
district_estimates <- district_estimates %>% 
  group_by(DIST_NAME) %>% 
  group_split()


#for loop to get CI for each admin
OUT <- list()
for(dd in 1:length(district_estimates)){
  
  df <- district_estimates[[dd]]
  
  # get the ID of the current area being processed
  typro <- unique(df$DIST_NAME)
  print(typro)
  
  
  df <- df %>% 
    select(starts_with("v")) %>% 
    apply(2, sum, na.rm = T)  
  
  OUT[[dd]] <- c(district_names = typro, mean = mean(df),
                 lower_quantile = quantile(df, 0.025),
                 upper_quantile = quantile(df, 0.975),
                 median = quantile(df, 0.500))
  
  #print(OUT)
}

AA <- do.call(rbind, OUT)
AA

#Convert to tibble and export as a csv
district_pop_count <- AA %>% 
  as_tibble() %>% 
  rename(
    DIST_NAME = district_names,
    Estimated_Population = mean, 
    Lower_Population = "lower_quantile.2.5%",
    Median_Population = "median.50%",
    Upper_Population = "upper_quantile.97.5%"
  ) %>%
  mutate(across(-DIST_NAME, ~ as.numeric(.))) %>%        # convert all except DIST_NAME
  mutate(across(where(is.numeric), ~ round(., 2)))       # round numeric columns to 2 decimals


#Write to file
write.csv(district_pop_count, paste0(output_path1, "District_Pop_Estimate.csv"), row.names = F)

# Summarize pixel level predictions ---------------------------------------

#Summarize predictions
tic() 

mean_count <- rowMeans(predicted_population4, na.rm = T)
Median_Population <- apply(predicted_population4, 1, FUN = function(x) quantile(x, probs = 0.5, na.rm = T))
std_count <- apply(predicted_population4, 1, sd)
lower_quantile <- apply(predicted_population4, 1, FUN = function(x) quantile(x, probs = 0.025, na.rm=T))
upper_quantile <- apply(predicted_population4, 1, FUN = function(x) quantile(x, probs = 0.975, na.rm = T))
uncertainty = (upper_quantile - lower_quantile)/mean_count
coe_var = std_count/mean_count

toc()

#sum predictions
sum(Median_Population, na.rm = T)
sum(mean_count, na.rm = T)

#Cbind predictions to xy coord

pixel_predictions4 <- cbind(mean_count, Median_Population, std_count,
                            lower_quantile, upper_quantile, uncertainty, coe_var) %>% 
  as_tibble() %>% 
  mutate(long = pred_covs$long, lat = pred_covs$lat,
         bcount = pred_covs$bcount,
         pop_density = mean_count/bcount)

summary(pixel_predictions4$mean_count)
summary(pixel_predictions4$pop_density)

####################################################################
# Compare predictions

pred<- rbind(pop_model1, pop_model2, pop_model3, pop_model4) 
pred %>%   kable()

hh <- rbind(pop_metrics1, pop_metrics2, pop_metrics3, pop_metrics4)
hh%>%   kable()

dens <- rbind(density_metrics1, density_metrics2, density_metrics3, density_metrics4)
dens %>%   kable()

#############################################################################
#############################################################################

# Rasterize Predictions ---------------------------------------------------
# Base on the model we will rasterize model 4

# #Convert to sf object
pixel_predictions4  <- st_as_sf(pixel_predictions4 , coords = c("long", "lat"))
st_crs(pixel_predictions4 ) <- 4326

#Write as sf object to file
st_write(pixel_predictions4, paste0(output_path1, "mphc_2024_pop.gpkg"))

#Rasterize mean and export to file
mean_raster  <- rasterize(pixel_predictions4, r1, field = "mean_count")
#plot(mean_raster)

#export
writeRaster(mean_raster,  
            paste0(output_path1, "mphc_2024_pop_prediction_mean.tif"), 
            overwrite=TRUE, names="mean")

#Rasterize sd
std_raster   <- rasterize(pixel_predictions4, r1, field = "std_count") 

writeRaster(std_raster,   
            paste0(output_path1, "mphc_2024_pop_prediction_std.tif"), 
            overwrite=TRUE, names="std")

#Lower
lower_raster <- rasterize(pixel_predictions4, r1, field = "lower_quantile") 

writeRaster(lower_raster, 
            paste0(output_path1, "mphc_2024_pop_prediction_lower.tif"), 
            overwrite=TRUE, names="lower")

#upper
upper_raster <- rasterize(pixel_predictions4, r1, field = "upper_quantile")

#Write to file
writeRaster(upper_raster, 
            paste0(output_path1, "mphc_2024_pop_prediction_upper.tif"), 
            overwrite=TRUE, names="upper")


################# END OF SCRIPT ########################################
#######################################################################




