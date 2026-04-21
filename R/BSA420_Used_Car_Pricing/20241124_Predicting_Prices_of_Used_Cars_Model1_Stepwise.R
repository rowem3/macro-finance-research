#Script for BUS 420 Final Project: Step wise Model

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

### Step 3: Perform stepwise regression on the training set

# Full model with all predictors
full_model <- lm(Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax +
                   Mfg_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows, 
                 data = train_set)

# Use stepwise selection to find a reduced model
stepwise_model <- step(full_model, direction = "both")

# Get the summary for the reduced model
summary_stepwise <- summary(stepwise_model)
print("Stepwise Regression Model Summary:")
print(summary_stepwise)

### Step 4: Evaluate the reduced model on the validation set and test set

# Predict on validation set
validation_predictions <- predict(stepwise_model, newdata = validation_set)
validation_rmse <- rmse(validation_set$Price, validation_predictions)
cat("Validation RMSE for Stepwise Model:", validation_rmse, "\n")

# Predict on the test set
test_predictions <- predict(stepwise_model, newdata = testing_data)
test_rmse <- rmse(testing_data$Price, test_predictions)
cat("Test RMSE for Stepwise Model:", test_rmse, "\n")

### Step 5: Print reduced model details

# Print the reduced model formula
cat("Reduced Model Formula:\n")
print(formula(stepwise_model))

# Print the coefficients of the reduced model
cat("Reduced Model Coefficients:\n")
print(coef(stepwise_model))
