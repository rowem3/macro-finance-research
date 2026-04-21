#Script for BUS 420 Final Project: KNN Model

#need to normalize data before breaking it into sub category
#use quanitle formation s


# Load necessary libraries
library(rsample)
library(readxl)
library(caret)   # For k-NN training and data splitting
library(dplyr)   # For data manipulation
library(Metrics) # For RMSE, MAE, etc.

# Load the data
BSA420ToyotaCorolla <- read_excel("R Projects/BSA 420.R/Final Case Study/Data/BSA420ToyotaCorolla.xlsx")

# Convert categorical variables to factors (if applicable)
BSA420ToyotaCorolla$Fuel_Type <- as.factor(BSA420ToyotaCorolla$Fuel_Type)
BSA420ToyotaCorolla$Automatic <- as.factor(BSA420ToyotaCorolla$Automatic)

# Step 1: Normalize the numeric variables using quantile normalization
# Identify the numeric columns
numeric_cols <- c("Age_08_04", "KM", "HP", "Quarterly_Tax", "Guarantee_Period", "Doors")

# Apply quantile normalization using the 'preProcess' function in caret
# 'range' scales values to [0, 1] based on the range of the data, mimicking quantile normalization
preproc <- preProcess(BSA420ToyotaCorolla[, numeric_cols], method = c("range"))

# Normalize the entire dataset based on the pre-processing model
BSA420ToyotaCorolla_norm <- predict(preproc, BSA420ToyotaCorolla)

# Step 2: Split the normalized data into training (50%) and testing (50%) sets
set.seed(123)  # Set a seed for reproducibility
initial_split <- initial_split(BSA420ToyotaCorolla_norm, prop = 0.5)

# Extract the training and testing sets
training_data <- training(initial_split)
testing_data <- testing(initial_split)

# Step 3: Train k-NN using caret
set.seed(123)
knn_model <- train(
  Price ~ Age_08_04 + KM + Fuel_Type + HP + Automatic + Doors + Quarterly_Tax + 
    Mfg_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player + Powered_Windows, 
  data = training_data, 
  method = "knn", 
  tuneGrid = expand.grid(k = seq(1, 15, by = 1)),  # Test more k values
  trControl = trainControl(method = "cv", number = 10)  # 10-fold cross-validation
)

# Print the best k value and the model results
print(knn_model)

# Step 4: Evaluate the model on the test set
predictions <- predict(knn_model, newdata = testing_data)

# Calculate performance metrics (RMSE, MAE, etc.)
rmse_knn <- rmse(testing_data$Price, predictions)
mae_knn <- mae(testing_data$Price, predictions)

cat("k-NN RMSE on test set:", rmse_knn, "\n")
cat("k-NN MAE on test set:", mae_knn, "\n")

# Load necessary libraries (add ggplot2 for plotting)
library(ggplot2)

# Step 5: Create a k-NN plot chart using ggplot2
ggplot(data = testing_data, aes(x = Price, y = predictions)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of actual vs predicted prices
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  # Reference line for perfect predictions
  labs(title = "k-NN Predictions vs Actual Prices",
       x = "Actual Price",
       y = "Predicted Price") +
  theme_minimal()

# Step 6: Extract RMSE values for each k from the training process
rmse_values <- knn_model$results %>% select(k, RMSE)

# Step 7: Create a plot of RMSE vs k using ggplot2
ggplot(data = rmse_values, aes(x = k, y = RMSE)) +
  geom_line(color = "blue", size = 1) +  # Line plot to connect RMSE values
  geom_point(color = "red", size = 3) +  # Points to highlight individual k values
  labs(title = "RMSE vs. k for k-NN Model",
       x = "Number of Neighbors (k)",
       y = "RMSE") +
  theme_minimal()
