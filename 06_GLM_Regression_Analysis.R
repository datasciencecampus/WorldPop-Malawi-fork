
#GLM Regression Models

#Load packages
library(kableExtra)  #Nice tables
library(janitor)  #Clean variable names
library(corrplot)   #Correlation analysis
library(car)        # Correlation analysis
library(MASS)   #for linear models
library(lme4)  #For GLM models
library(caret)  #For cross-Validation
library(tidyverse)  # For data manipulation

options(scipen=999)

#Specify Drive Path
drive_path <- "C:/Users/oy1r22/OneDrive - University of Southampton/Desktop/Malawi_Workshop/"
input_path <- paste0(drive_path, "Output_Data/")
output_path <- paste0(drive_path, "Output/")

#Load dataset
Malawi_2024_data <- read.csv(paste0(input_path, "Malawi_2024_data.csv"))

#Fix names of variables
Malawi_2024_data <- Malawi_2024_data %>%
  clean_names()

#variable names
names(Malawi_2024_data)

#Drop NA in household count
Malawi_2024_data <- Malawi_2024_data %>% 
  drop_na(hh_count_2024)

# We are going to use data for 2024 for the demonstration

# We want to check a summary of our household count 2024
summary(Malawi_2024_data$hh_count_2024)

# We will remove some outliers and only use household count greater than 17 for analysis

#Filter household count
EA_data <- Malawi_2024_data %>% 
  filter(hh_count_2024 > 17)

# Calculate hh count density
EA_data <- EA_data %>% 
  mutate(hh_density_2024 = hh_count_2024/google_v2_5)

# We will scale the covariates using z-scores

#Select covariates
covs <- EA_data  %>% 
  select(starts_with("x")) %>% 
  select(where(~ !any(is.na(.))))  # Remove covariates with NAs

#Scaling function to scale covariates
stdize <- function(x)
{ stdz <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
return(stdz) }

#apply scaling function
covs <- apply(covs, 2, stdize) %>%    #z-score
  as_tibble()

#cbind scaled covariates for model fitting
EA_data <- EA_data %>% 
  select(-starts_with("x")) %>% 
  cbind(covs) 

#Convert District and Rural Urban Variables to factors
EA_data <- EA_data %>%
  mutate(
    district = as.factor(dist_name),
    rural_urban = as.factor(adm_status)
  )

#We will use the following as covariates as we selected previously
# x13 + x63 + x50 + x44 + x49 + x57 + x55

#################################################################################
##################################################################################
#################################################################################
########## MODELLING HH COUNT 2024  #############################################
#################################################################################

# Fit Total HH Count as a Poisson ---------------------------------------

poisson_model <- glm(hh_count_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + 
                       rural_urban,
                    family = poisson(link = "log"),
                    data = EA_data)

summary(poisson_model)

#Coefficient is on the log-scale so we have to back transform them
#(rate ratio)
exp(coef(poisson_model))


# Create a dataframe with observed and predicted values
model_evaluation <- data.frame(Observed = EA_data$hh_count_2024, 
                               Predicted = poisson_model$fitted.values)

# compute goodness-of-fit metrics
model_evaluation <- model_evaluation %>% 
  mutate(residual = Predicted - Observed)

#Find observed and predicted population sum
sum(model_evaluation$Observed)
sum(model_evaluation$Predicted)

#Checking for model fit
fit_poisson <- model_evaluation %>% 
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(Predicted, Observed)) %>% 
  mutate(Model = "Poisson Model")

fit_poisson %>% 
  kable()

# Count data are prone to overdispersion
#Where variance is not equal to mean
mean(EA_data$hh_count_2024)
var(EA_data$hh_count_2024)

#Check Overdispersion
overdispersion <- sum(residuals(poisson_model, type = "pearson")^2) /
  poisson_model$df.residual

overdispersion

#Rule ≈1 → OK
# 1.5 → Overdispersion → Use Negative Binomial

############################################################################
############################################################################

# Fit A Negative Binomial Model -------------------------------------------

nb_model <- glm.nb(hh_count_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 +  rural_urban,
                     data = EA_data)

summary(nb_model)

#Coefficient is on the log-scale so we have to back transform them
#(rate ratio)
exp(coef(nb_model))

# Create a dataframe with observed and predicted values
model_evaluation <- data.frame(Observed = EA_data$hh_count_2024, 
                               Predicted = nb_model$fitted.values)

# compute goodness-of-fit metrics
model_evaluation <- model_evaluation %>% 
  mutate(residual = Predicted - Observed)

#Find observed and predicted population sum
sum(model_evaluation$Observed)
sum(model_evaluation$Predicted)

#Checking for model fit
fit_nb <- model_evaluation %>% 
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(Predicted, Observed)) %>% 
  mutate(Model = "NB Model")

fit_nb %>% 
  kable()

#Rbind results
rbind(fit_poisson, fit_nb)

#Using AIC
AIC(poisson_model)
AIC(nb_model)

##############################################################################
##############################################################################

# Mixed Effect/Hierarchical Models ----------------------------------------

# Data might be Hierarchical
# EA Nested within Strata and Strata Nested within district. 
# We need to account for variability as a result of such hierarchical nature of data

# A Mixed Effect Model with identity link (gaussian distribution)

mixed_model1 <- lmer(hh_density_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55+   #Fixed Effect
   (1 | district),   #Random Effect
  data = EA_data)

summary(mixed_model1)

#Get the fixed effect part
fixef(mixed_model1)

#Random effect part
ranef(mixed_model1)

#Variance component
VarCorr(mixed_model1)

# From output
#district (Intercept) SD = 0.00000038442  
#Residual SD = 56.95028681165

#Convert SD to variances
var_district <- 0.00000038442^2
var_residual <- 56.95028681165^2

#Compute Intra-Class Correlation Coefficient
icc <- var_district / (var_district + var_residual)
icc

#less than 1% of the variation in total household count is explained by differences between districts.


# Mixed Effect Model - Negative Binomial ----------------------------------

mixed_model2 <- glmer.nb(hh_count_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55+
                       (1 | district),
                     data = EA_data)

summary(mixed_model2)

#Get the fixed effect part
fixef(mixed_model2)

#Random effect part
ranef(mixed_model2)

################################################################################
################### CROSS VALIDATION ###########################################
################################################################################

# Model Validations -------------------------------------------------------

# Making Predictions and Performing Cross Validation ----------------------

set.seed(12345)

# Predicting total hh count
#using training and test data
# Its advisable to divide your dataset into train and test data and check how well your model performance using
# various model metrics. 80% or 70% of the data is used for training and the remaining percent used for testing


# Split dataset into training and test data
train_idx <- sample(1:nrow(EA_data), nrow(EA_data) * 0.8)  # 80% for training

train_data <- EA_data[train_idx, ]
test_data <- EA_data[-train_idx, ]

# Fit a nbinomial using the training data
nb_model_train <- glm.nb(
  hh_count_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55 + district,
  data = train_data
)

summary(nb_model_train)

# Create a dataframe with observed and predicted values
train_evaluation <- data.frame(Observed = train_data$hh_count_2024, Predicted = nb_model_train$fitted.values,
                               Model = "In-Sample")

# compute goodness-of-fit metrics
train_evaluation <- train_evaluation %>% 
  mutate(residual = Predicted - Observed)

In_Sample <- train_evaluation %>%
  summarise(Bias= mean(residual),
            mae = mean(abs(residual)),
            MSE = mean((residual)^2), 
            RMSE = sqrt(MSE), 
            corr = cor(Predicted, Observed))%>% 
  mutate(Model = "In-Sample")


In_Sample

#Out-Sample Predictions
# Make predictions using the test data
predictions <- predict(nb_model_train, newdata = test_data, type = "response")

# Print the predictions
#print(predictions)


# Create a dataframe with observed and predicted values
test_evaluation <- data.frame(Observed = test_data$hh_count_2024, 
                              Predicted = predictions, Model = "Out-Sample")

test_evaluation <- test_evaluation %>% 
  mutate(residual = Predicted - Observed)

# compute goodness-of-fit metrics

Out_Sample <- test_evaluation %>%
  summarise(Bias= mean(residual),
            mae = mean(abs(residual)),
            MSE = mean((residual)^2), 
            RMSE = sqrt(MSE), 
            corr = cor(Predicted, Observed)) %>% 
  mutate(Model = "Out-Sample")

Out_Sample

#Compare In-Sample and Out-Sample Predictions

nb_model_val <- rbind(In_Sample, Out_Sample)
nb_model_val

###########################################################################
##########################################################################
# Mixed Effect Predictions ------------------------------------------------

mixed_model_train <- glmer.nb(hh_count_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55+
                             (1 | district),
                           data = train_data)
#Summary
summary(mixed_model_train)


train_predicted <- predict(mixed_model_train, type = "response")

# Create a dataframe with observed and predicted values
train_evaluation <- data.frame(Observed = train_data$hh_count_2024, Predicted = train_predicted,
                               Model = "In-Sample")

# compute goodness-of-fit metrics
train_evaluation <- train_evaluation %>% 
  mutate(residual = Predicted - Observed)

In_Sample <- train_evaluation %>%
  summarise(Bias= mean(residual),
            mae = mean(abs(residual)),
            MSE = mean((residual)^2), 
            RMSE = sqrt(MSE), 
            corr = cor(Predicted, Observed))%>% 
  mutate(Model = "In-Sample")


In_Sample

#Out-Sample Predictions
# Make predictions using the test data
test_predicted <- predict(mixed_model_train, newdata = test_data, type = "response")

# Print the predictions
#print(predictions)


# Create a dataframe with observed and predicted values
test_evaluation <- data.frame(Observed = test_data$hh_count_2024, 
                              Predicted = test_predicted, Model = "Out-Sample")

test_evaluation <- test_evaluation %>% 
  mutate(residual = Predicted - Observed)

# compute goodness-of-fit metrics

Out_Sample <- test_evaluation %>%
  summarise(Bias= mean(residual),
            mae = mean(abs(residual)),
            MSE = mean((residual)^2), 
            RMSE = sqrt(MSE), 
            corr = cor(Predicted, Observed)) %>% 
  mutate(Model = "Out-Sample")

Out_Sample

#Compare In-Sample and Out-Sample Predictions

mixed_val <- rbind(In_Sample, Out_Sample)
mixed_val

#############################################################################
############################################################################
###############  KFOLD CROSS VALIDATION ####################################

# K-Fold Cross Validation -------------------------------------------------

# function to calculate k-fold
kfold_cv <- function(data, k) {
  n <- nrow(data)
  fold_size <- n %/% k
  folds <- sample(rep(1:k, each = fold_size, length.out = n))
  
  # Create separate dataframes for train and test metrics
  train_metrics <- data.frame()
  test_metrics <- data.frame()
  
  # Place holder for train metrics calculation ---------------------------
  
  train_rmse_values <- numeric(k) # Placeholder for RMSE
  train_pearson_values <- numeric(k) # Placeholder for corr
  train_mae_values <- numeric(k)  # Placeholder for MAE
  train_bias_values <- numeric(k)  # Placeholder for bias
  
  # Place holder for test metrics calculation ------------------------
  test_rmse_values <- numeric(k) # Placeholder for RMSE
  test_pearson_values <- numeric(k) # Placeholder for corr
  test_mae_values <- numeric(k)  # Placeholder for MAE
  test_bias_values <- numeric(k)  # Placeholder for bias
  
  # For loop for implementation ---------------------------------------------
  for (i in 1:k) {
    test_indices <- which(folds == i)
    train_indices <- which(folds != i)
    
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    mixed_model_kfold <- glmer.nb(hh_count_2024 ~ x13 + x63 + x50 + x44 + x49 + x57 + x55+
                                    (1 | district),
                                  data = train_data)
    #Summary
    #summary(mixed_model_train_kfold)
    
    #Train data predictions
    train_predicted <- predict(mixed_model_kfold, type = "response")
    
    # Create a dataframe with observed and predicted values
    train_evaluation <- data.frame(Observed = train_data$hh_count_2024, Predicted = train_predicted)
    
    #Test predictions
    # Make predictions using the test data
    test_predicted <- predict(mixed_model_kfold, newdata = test_data, type = "response")
    
    # Create a dataframe with observed and predicted values
    test_evaluation <- data.frame(Observed = test_data$hh_count_2024, 
                                  Predicted = test_predicted)
    
    #Train data metrics
    train_rmse_values[i] <- sqrt(mean((train_evaluation$Observed - train_evaluation$Predicted)^2))
    train_pearson_values[i] <- cor(train_evaluation$Observed, train_evaluation$Predicted)
    train_mae_values[i] <- mean(abs(train_evaluation$Observed - train_evaluation$Predicted))
    train_bias_values[i] <- mean(train_evaluation$Observed - train_evaluation$Predicted)
    
    #Test data metrics
    test_rmse_values[i] <- sqrt(mean((test_evaluation$Observed - test_evaluation$Predicted)^2))
    test_pearson_values[i] <- cor(test_evaluation$Observed, test_evaluation$Predicted)
    test_mae_values[i] <- mean(abs(test_evaluation$Observed - test_evaluation$Predicted))
    test_bias_values[i] <- mean(test_evaluation$Observed - test_evaluation$Predicted)
    
    
    # Train metrics
    train_metrics <- data.frame(train_rmse = mean(train_rmse_values),
                                train_corr = mean(train_pearson_values),
                                train_mae = mean(train_mae_values),
                                train_bias = mean(train_bias_values)
                                
    )
    
    # Test metrics
    test_metrics <- data.frame(test_rmse = mean(test_rmse_values),
                               test_corr = mean(test_pearson_values),
                               test_mae = mean(test_mae_values),
                               test_bias = mean(test_bias_values)
    )
  }
  
  # Return separate lists for density and population metrics
  list(train_metrics = train_metrics, test_metrics = test_metrics)
}

set.seed(1234)

# Apply function
result1 <- kfold_cv(data = EA_data, k = 10)

#result1

#Train data results
result1$train_metrics %>%
  kable()

#Test data results
result1$test_metrics %>%
  kable()


###################### END OF GLM REGRESSION & CROSS VALIDATION ##################
##################################################################################
#################################################################################
