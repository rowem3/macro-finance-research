# Script for BUS 420 Final Project: Linear Regression Model

# Load necessary libraries
library(readxl)
library(rsample)  # For data splitting
library(broom)    # For tidy regression results
library(dplyr)    # For data manipulation
library(Metrics)  # For RMSE, MAE, etc.

# Load the data
BSA420ToyotaCorolla <- read_excel("R Projects/BSA 420.R/Final Case Study/Data/BSA420ToyotaCorolla.xlsx")

# Convert categorical variables to factors (if applicable)
BSA420ToyotaCorolla$Fuel_Type <- as.factor(BSA420ToyotaCorolla$Fuel_Type)
BSA420ToyotaCorolla$Automatic <- as.factor(BSA420ToyotaCorolla$Automatic)

# Step 1: Split the data into training (50%) and testing (50%) sets
set.seed(123)  # Set a seed for reproducibility
initial_split <- initial_split(BSA420ToyotaCorolla, prop = 0.5)

# Extract the training and testing sets
training_data <- training(initial_split)
testing_data <- testing(initial_split)

# Step 2: Split the training data further into training (70%) and validation (30%) sets
validation_split <- initial_split(training_data, prop = 0.7)

# Extract the new training and validation sets
train_set <- training(validation_split)
validation_set <- testing(validation_split)

### Step 3: Train and evaluate models for training, validation, and test datasets

## Model 1: Train on the training set
model_train <- lm(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax +
                    Mfg_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows, 
                  data = train_set)

# Get summary for the training data model
summary_train <- summary(model_train)
print("Training Data Model Summary:")
print(summary_train)

## Model 2: Train on the validation set
model_validation <- lm(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax +
                         Mfg_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows, 
                       data = validation_set)

# Get summary for the validation data model
summary_validation <- summary(model_validation)
print("Validation Data Model Summary:")
print(summary_validation)

## Model 3: Train on the test set
model_test <- lm(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax +
                   Mfg_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows, 
                 data = testing_data)

# Get summary for the test data model
summary_test <- summary(model_test)
print("Test Data Model Summary:")
print(summary_test)

### Step 4: Compare the estimated coefficients, p-values, and R-squared for each model

# Coefficients and R-squared comparison using `tidy` and `glance` from `broom`
train_model_summary <- tidy(model_train)
validation_model_summary <- tidy(model_validation)
test_model_summary <- tidy(model_test)

# R-squared values
r_squared_train <- glance(model_train)$r.squared
r_squared_validation <- glance(model_validation)$r.squared
r_squared_test <- glance(model_test)$r.squared

print("Training Model Coefficients and P-values:")
print(train_model_summary)

print("Validation Model Coefficients and P-values:")
print(validation_model_summary)

print("Test Model Coefficients and P-values:")
print(test_model_summary)

cat("R-squared (Training):", r_squared_train, "\n")
cat("R-squared (Validation):", r_squared_validation, "\n")
cat("R-squared (Test):", r_squared_test, "\n")

# Function to print the linear model as a formula and include summary statistics
print_model_formula_and_stats <- function(model, model_name) {
  # Extract the coefficients from the model
  coefficients <- coef(model)
  model_summary <- summary(model)  # Get the summary for the model
  
  # Start with the intercept
  formula_string <- paste0("Price = ", round(coefficients[1], 4))
  
  # Add each predictor variable with its coefficient
  for (i in 2:length(coefficients)) {
    formula_string <- paste0(formula_string, 
                             " + ", round(coefficients[i], 4), 
                             " * ", names(coefficients)[i])
  }
  
  # Extract the additional model statistics
  r_squared <- model_summary$r.squared
  f_statistic <- model_summary$fstatistic[1]
  p_value <- pf(f_statistic, model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
  t_values <- model_summary$coefficients[, "t value"]
  p_values <- model_summary$coefficients[, "Pr(>|t|)"]
  num_obs <- nobs(model)
  
  # Print the model formula
  cat("\n", model_name, "Model Formula:\n", formula_string, "\n\n")
  
  # Print the t-statistics and p-values for each coefficient
  cat("T-statistics and P-values:\n")
  for (i in 1:length(coefficients)) {
    cat(names(coefficients)[i], ": t =", round(t_values[i], 4), ", p =", round(p_values[i], 4), "\n")
  }
  
  # Print the overall model statistics
  cat("\nF-statistic:", round(f_statistic, 4), "\n")
  cat("P-value (overall model):", round(p_value, 4), "\n")
  cat("R-squared:", round(r_squared, 4), "\n")
  cat("Number of observations:", num_obs, "\n\n")
}

# Print the models in written form and include the t-stat, F-stat, p-value, R-squared, and observations for each dataset
print_model_formula_and_stats(model_train, "Training")
print_model_formula_and_stats(model_validation, "Validation")
print_model_formula_and_stats(model_test, "Test")

# Calculate RMSE for the test set
predicted_test <- predict(model_test, newdata = testing_data)  # Predicted values
actual_test <- testing_data$Price  # Actual values

# Calculate RMSE for the test set
predicted_test <- predict(model_test, newdata = testing_data)  # Predicted values
actual_test <- testing_data$Price  # Actual values

# Calculate RMSE for the test set
rmse_test <- rmse(actual_test, predicted_test)
cat("RMSE (Test Set):", round(rmse_test, 4), "\n")

#correlation matrix ----
# Load necessary libraries for 
library(corrplot)  # For correlation matrix visualization
library(dplyr)     # For data manipulation

# Ensure dplyr is loaded
library(dplyr)

# Select numeric variables from the dataset (ignore non-numeric columns like factors)
numeric_data <- BSA420ToyotaCorolla %>%
  select(Age_08_04, KM, HP, Doors, Quarterly_Tax, Mfg_Guarantee, Guarantee_Period, 
         Airco, Automatic_airco, CD_Player, Powered_Windows)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)

# Visualize the correlation matrix with conditional formatting using corrplot
library(corrplot)
corrplot(cor_matrix, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), # Color scale
         type = "upper",           # Display only upper triangle
         tl.col = "black",         # Label color
         tl.srt = 45,              # Rotate labels
         addCoef.col = "black",    # Add correlation coefficients
         number.cex = 0.7,         # Size of coefficient numbers
         diag = FALSE)             # Hide the diagonal (1s)

# Load the necessary library for plotting
library(ggplot2)

# Calculate residuals for the test set
residuals_test <- actual_test - predicted_test  # Residuals are actual - predicted
fitted_values_test <- predicted_test            # Fitted values (predictions)

# Create a data frame for plotting residuals
residuals_data <- data.frame(Fitted_Values = fitted_values_test, Residuals = residuals_test)

# Create a residual plot
ggplot(residuals_data, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue") +  # Scatter plot of residuals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a reference line at y=0
  theme_minimal() +  # Minimal theme for the plot
  labs(title = "Residual Plot for Test Set",
       x = "Fitted Values (Predicted Price)",
       y = "Residuals (Actual - Predicted Price)") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

#
# Install and load the car package for 
if (!require(car)) {
  install.packages("car", dependencies=TRUE)
}
library(car)

# Function to calculate and print Durbin-Watson statistic and VIFs
calculate_dw_vif <- function(model, data_name) {
  # Durbin-Watson statistic
  dw_stat <- durbinWatsonTest(model)
  
  # VIF (Variance Inflation Factor) to detect multicollinearity
  vif_values <- vif(model)
  
  # Print the results
  cat("\nDurbin-Watson Statistic for", data_name, "Data Model:\n")
  print(dw_stat)
  
  cat("\nVariance Inflation Factors (VIF) for", data_name, "Data Model:\n")
  print(vif_values)
  
  # Interpret VIF values
  cat("\nVIF Interpretation Guide:\n")
  cat("1 < VIF < 5: Moderate multicollinearity\n")
  cat("VIF > 5: High multicollinearity (problematic)\n\n")
}

# Calculate and print Durbin-Watson and VIF for the test data model
calculate_dw_vif(model_test, "Test")

