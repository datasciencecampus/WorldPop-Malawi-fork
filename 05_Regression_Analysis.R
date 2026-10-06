
#Regression Models

#Load packx32s
library(kableExtra)
library(caret)     #Machine learning algorithm
library(MASS)       #For regression
library(corrplot)   #Correlation analysis
library(car)        # Correlation analysis
library(readxl)   # To read excel file
library(janitor)  #Clean variable names
library(tidyverse)  # For data manipulation


options(scipen=999)

#Specify Drive Path
drive_path <- "D:/Malawi/"
input_path <- paste0(drive_path, "Output_Data/")
output_path <- paste0(drive_path, "Output_Data/")

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

############################################################################
############################################################################
# We are going to use data for 2024 for the demonstration

# We want to check a summary of our household count 2024
summary(Malawi_2024_data$hh_count_2024)

##Summary of household count in a table
summary_stats_hh_count <- Malawi_2024_data %>%
  summarise(
    Min    = round(min(hh_count_2024, na.rm = TRUE), 1),
    Q1     = round(quantile(hh_count_2024, 0.25, na.rm = TRUE), 1),
    Median = round(median(hh_count_2024, na.rm = TRUE), 1),
    Mean   = round(mean(hh_count_2024, na.rm = TRUE), 1),
    Q3     = round(quantile(hh_count_2024, 0.75, na.rm = TRUE), 1),
    Max    = round(max(hh_count_2024, na.rm = TRUE), 1)
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

#Distribution of HH Count

ggplot(Malawi_2024_data, aes(x = "2024", y = hh_count_2024)) + 
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  
  # Add summary statistics text
  geom_text(
    data = summary_stats_hh_count,
    aes(
      x = "2024", 
      y = 1000,
      label = label
    ),
    hjust = 1.3,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  labs(
    x = "Year",
    y = "Household Count",
    title = "Distribution of household Count"
  ) +
  theme_minimal()

# We will remove some outliers and only use household count greater than 17 for analysis

#Filter household count
EA_data <- Malawi_2024_data %>% 
  filter(hh_count_2024 > 17)


# We will calculate household density and use it as an outcome in the linear model
EA_data <- EA_data %>% 
  mutate(hh_density_2024 = hh_count_2024/google_v2_5)

#Drop NA in hh_density_2024
EA_data <- EA_data %>% 
  drop_na(hh_density_2024)

#Summarize hh density
summary(EA_data$hh_density_2024)

#We will remove hh density above 30
EA_data <- EA_data %>% 
  filter(hh_density_2024 <= 30)

#Summarize 

# Plot Density Using a Boxplot and Violin Plot

#Summary of household density
summary_stats_density <- EA_data %>%
  summarise(
    Min    = round(min(hh_density_2024, na.rm = TRUE), 1),
    Q1     = round(quantile(hh_density_2024, 0.25, na.rm = TRUE), 1),
    Median = round(median(hh_density_2024, na.rm = TRUE), 1),
    Mean   = round(mean(hh_density_2024, na.rm = TRUE), 1),
    Q3     = round(quantile(hh_density_2024, 0.75, na.rm = TRUE), 1),
    Max    = round(max(hh_density_2024, na.rm = TRUE), 1)
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

#Distribution of HH Density

ggplot(EA_data, aes(x = "2024", y = hh_density_2024)) + 
  geom_violin(alpha = 0.3, trim = FALSE, fill = "blue") +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  
  # Add summary statistics text
  geom_text(
    data = summary_stats_density,
    aes(
      x = "2024", 
      y = 15,
      label = label
    ),
    hjust = 1.3,
    size = 3.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  labs(
    x = "Year",
    y = "Household Density",
    title = "Distribution of household Density"
  ) +
  theme_minimal()



# Model Test and Assumptions ----------------------------------------------
#There are four assumptions associated with a linear regression model:
#Normality: For any fixed value of X, Y is normally distributed.
#Linearity: The relationship between X and the mean of Y is linear.
#Homoscedasticity (Equal variance): The variance of residual is the same for any value of X.
#Independence: Observations are independent of each other.

#1. Assumption of Normality - Your dependent variable must be normally distributed. 
#We can test this using a histogram or a Shapiro-Wilk test

hist(EA_data$hh_density_2024)

#using ggplot
ggplot(EA_data, aes(x = hh_density_2024)) +
  geom_histogram(bins = 50, fill = "#adc178") +
  labs(title = "Histogram of Household Density", x = "Household Density", y = "Frequency")


# 2. Assumption of Linearity - The relationship between the independent and dependent variable must be linear
#We can test this using a scatter plot

#Scatter plot

plot(hh_density_2024 ~ x32, data = EA_data)

#using ggplot
ggplot(EA_data, aes(x = x32, y = hh_density_2024)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add trend line
  labs(x = "x32", y = "hh_density_2024") +
  ggtitle("Scatter Plots of built surface and hh_density_2024") 


# Simple Linear Regression ------------------------------------------------
# Idea : What is the relationship between household density and built-up surface(x32)?

model1 <- lm(hh_density_2024 ~ x32,
             data = EA_data)

summary(model1)

#The results section shows the coefficient
#The p-value - Indicating statistical significance
#The Adjusted R-Square indicating variability in household density as explained by the x32
# Explain the results of the coefficient

#let test assumption of homoscedasticity of the residuals
#Plot residuals vrs fitted values


plot(model1$residuals, model1$fitted.values)
abline(a = 2, b = 0, col = "red")


#using ggplot
ggplot(data = model1, aes(x = model1$residuals, y = model1$fitted.values)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") 


# Multiple Regression Model -----------------------------------------------

# In addition to the assumptions of a linear model tested above. One important thing to consider in multiple linear 
# Regression is the idea of multi-collinearity

# Multi-collinearity - Independent variables should not be correlated.
#We can check this assumption using a correlation matrix or variance inflation factor

#Correlation plot
#We will select a couple of covariates
covs <- EA_data %>% 
  dplyr::select(x26, x32, x60, x35, x53)

#Compute Correlation Matrix
cor_matrix <- cor(covs)
cor_matrix

#Visualize the correlation matrix

corrplot(cor_matrix, method="circle")

col <- colorRampPalette(c("#03071e", "#370617", "#adc178", "#90e0ef", "#caf0f8"))
corrplot(cor_matrix, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )

# From the correlation matrix we see less correlation between the variables
# We can also check multi-collinearity using the variance inflation factor (VIF). 
# Usually people have used a more conservative VIF value of 5 to indicate multi-collinearity. 

# using VIF to check for multi-collinearity

model2 <- lm(hh_density_2024 ~ x26 + x32 + x60 + x35 + x53, 
             data = EA_data)

summary(model2)

#Explain the results

#Check for multicollinearity using vif
vif(model2)
#x26 has a VIF above 5 we will drop it 

#Convert Categorical Variables to factors
EA_data <- EA_data %>%
  mutate(
    district = as.factor(dist_name),
    rural_urban = as.factor(adm_status)
  )

#Improved model with factors

model3 <- lm(hh_density_2024 ~ x32 + x60 + x35 + x53 +
               district +
               rural_urban,
             data = EA_data)

summary(model3)

#Explain results


# Explicitly Setting the Reference Category -------------------------------

# R uses Alphabetical Order to Set the reference category
# However we can set it manually. We will set URBAN as the reference category

unique(EA_data$rural_urban)

EA_data <- EA_data %>%
  mutate(
    rural_urban= relevel(rural_urban, ref = "Urban"),
    district = relevel(district, ref = "Blantyre City")
  )

#Fit the model again
model3b <- lm(hh_density_2024 ~ x32 + x60 + x35 + x53 +
                district +
                rural_urban,
              data = EA_data)

summary(model3b)

###################################################################################
###################################################################################
###################################################################################
###### STEPWISE SELECTION OF COVARIATES ###########################################
##
# We want to do covariate selection ---------------------------------------

#Covs selection
covs <- EA_data  %>% 
  dplyr::select(starts_with("x")) %>% 
  dplyr::select(where(~ !any(is.na(.))))  # Remove covariates with NAs

#Compute Correlation Matrix
cor_matrix <- cor(covs)
cor_matrix

#Visualize the correlation matrix

corrplot(cor_matrix, method="circle")

col <- colorRampPalette(c("#03071e", "#370617", "#adc178", "#90e0ef", "#caf0f8"))
corrplot(cor_matrix, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )

# Calcute mean and standard deviation of covariates
cov_stats <- data.frame(Covariate = colnames(covs),
                        Mean = apply(covs, 2, mean, na.rm = TRUE),
                        Std_Dev = apply(covs, 2, sd, na.rm = TRUE))

#Scaling function to scale covariates
stdize <- function(x)
{ stdz <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
return(stdz) }

#apply scaling function
covs <- apply(covs, 2, stdize) %>%    #z-score
  as_tibble()

#Select response variable and cbind covs
covs_selection <- EA_data %>% 
  dplyr::select(hh_density_2024) %>% 
  cbind(covs) 


# Stepwise Covariate Selection --------------------------------------------
#Stepwise covariates selection

#fit a linear model to select covariates
full_model <- lm(hh_density_2024 ~ ., data = covs_selection)

#Model summary
summary(full_model)

#stepwise selection
step_model1 <- MASS::stepAIC(full_model, direction = "both")
summary(step_model1)

#Check vif
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
    
    # Recalculate VIFs
    vif_values <- vif(model)
  }
  
  return(model)
}


# Apply the function to drop high VIF variables
step1_updated <- drop_high_vif(step_model1)
summary(step1_updated)

#check vif
vif(step1_updated)

# Extract selected variables
selected_vars <- step1_updated$coefficients%>% 
  names()  # Get the selected variables

# Create model formula
formula_string <- paste("hh_density_2024 ~", paste(selected_vars, collapse = " + "))
final_formula <- as.formula(formula_string)

# Print final model formula
print(final_formula)

#function to drop non-significant variables
# Start with full model
current_formula <- as.formula("hh_density_2024 ~  x13 + x35 + x37 + x38 + x40 + 
    x42 + x44 + x49 + x50 + x54 + x55 + x56 + x57 + x58 + x59 + 
    x61 + x62 + x63")

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
  current_formula <- as.formula(paste("hh_density_2024 ~", paste(new_rhs, collapse = " + ")))
}

# Final model
final_model <- model
summary(final_model)
vif(final_model)


# Extract the formula from final model
final_model <- formula(final_model)
final_model 

########### OPTIONAL #############################################

# You can also use lasso regression to rank the covariate importance
#important covariates

#Fit a model using the LASSO with the caret packx32

#drop NAs in covariates for LASSO fitting
covs_selection1 <- covs_selection %>% 
  drop_na() 

#Lasso Regression
fit1_lasso <- caret::train(
  hh_density_2024 ~ x13 + x37 + x42 + x44 + x49 + x50 + x55 + x56 + x57 + x61 + x63,
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

#cbind scaled covariates for model fitting
EA_data <- EA_data %>% 
  dplyr::select(-starts_with("x")) %>% 
  cbind(covs) 


# Fit a multiple linear regression model

#Fit the model again
density_model1 <- lm(hh_density_2024 ~ x13 + x37 + x42 + x44 + x49 + x50 + 
                       x55 + x56 + x57 + x61 + x63,
              data = EA_data)

summary(density_model1)

# Create a dataframe with observed and predicted values
model_evaluation <- data.frame(Observed = EA_data$hh_density_2024, 
                               Predicted = density_model1$fitted.values)

# compute goodness-of-fit metrics
model_evaluation <- model_evaluation %>% 
  mutate(residual = Observed - Predicted)

#Checking for model fit
fit_metrics1 <- model_evaluation %>% 
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(Predicted, Observed)) %>% 
  mutate(Model = "Density Model1")

fit_metrics1 %>% 
  kable()


# Add categorical Factor --------------------------------------------------

#Fit the model again
density_model2 <- lm(hh_density_2024 ~ x13 + x37 + x42 + x44 + x49 + x50 + x55 + x56 + 
                       x57 + x61 + x63 + 
                       district +
                       rural_urban,
                     data = EA_data)

summary(density_model2)

# Create a dataframe with observed and predicted values
model_evaluation <- data.frame(Observed = EA_data$hh_density_2024, 
                               Predicted = density_model2$fitted.values)

# compute goodness-of-fit metrics
model_evaluation <- model_evaluation %>% 
  mutate(residual = Observed - Predicted)

#Checking for model fit
fit_metrics2 <- model_evaluation %>% 
  summarise(
    Bias= mean(residual),
    Imprecision = sd(residual),
    mae = mean(abs(residual)),
    mse = mean((residual)^2),
    rmse = sqrt(mse),
    Corr = cor(Predicted, Observed)) %>% 
  mutate(Model = "Density Model2")

fit_metrics2 %>% 
  kable()

#Rbind results
rbind(fit_metrics1, fit_metrics2)

#Using AIC
AIC(density_model1)
AIC(density_model2)

###################### END OF LINEAR REGRESSION ###################################
##################################################################################
#################################################################################
