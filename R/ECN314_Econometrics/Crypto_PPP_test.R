#PPP Model with not trimmed data

# Load necessary packages
if (!requireNamespace("stargazer", quietly = TRUE)) {
  install.packages("stargazer")
}
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
library(stargazer)
library(readxl)
library(broom)
library(corrplot)
library(RColorBrewer)

#Import data
ECN314_Crypto_PPP <- read_excel("Data.R/ECN314_Crypto_PPP.xlsx")
View(ECN314_Crypto_PPP)

# Fit regression model
PPP_regression_model <- lm(BTC ~ (USINF - JPNINF) + USCPI, data = ECN314_Crypto_PPP)

# Display summary of regression results
  summary(PPP_regression_model)

# Display regression results using stargazer
stargazer(PPP_regression_model, title = "Regression Results", type = "text")

# Display tidy regression results
tidy_results <- tidy(PPP_regression_model)
print(tidy_results)

#Test for multicollinearity -----

# Select only numeric variables
numeric_vars <- ECN314_Crypto_PPP[sapply(ECN314_Crypto_PPP, is.numeric)]

# Compute correlation matrix
Matrix1 <- cor(numeric_vars)

# Print correlation matrix
print(Matrix1)

# Define a color palette using RColorBrewer
color_palette <- brewer.pal(n = 8, name = "RdYlBu")

# Plot correlation matrix with conditional formatting and color scheme
corrplot(Matrix1, method = "color", type = "lower", tl.col = "black", addCoef.col = "black", 
         col = color_palette, 
         bg = "white", 
         addgrid.col = "gray",
         main = "Correlation Matrix")

# Subset of data for variables in the model IF NESSACARY 
model_variables <- c("BTC", "USINF", "JPNINF", "USCPI")
data_subset <- ECN314_Crypto_PPP[model_variables]

# Compute correlation matrix for subset data
Matrix2 <- cor(data_subset)

# Print correlation matrix
print(Matrix2)

# Load the car package
library(car)

# Calculate VIFs for independent variables in the regression model
vif_values <- vif(PPP_regression_model)

# Print the VIF values
print(vif_values)

plot(as.numeric(as.Date(ECN314_Crypto_PPP$observation_date)), residuals(PPP_regression_model),
     xlab = "Observation Date", ylab = "Residuals",
     main = "Scatterplot of Residuals Against Time")

#test for models functional form ----

# Plot residuals against each independent variable
par(mfrow=c(2,2)) # Set up a 2x2 grid for the plots

# Plot against USINF
plot(as.numeric(as.Date(ECN314_Crypto_PPP$USINF)), 
     residuals(ECN314_Crypto_PPP),
     xlab = "USINF", ylab = "Residuals",
     main = "Residuals vs USINF")

# Plot against JPNINF
plot(ECN314_Crypto_PPP$JPNINF, residuals(ECN314_Crypto_PPP),
     xlab = "JPNINF", ylab = "Residuals",
     main = "Residuals vs JPNINF")

# Plot against USCPI
plot(ECN314_Crypto_PPP$USCPI, residuals(ECN314_Crypto_PPP),
     xlab = "USCPI", ylab = "Residuals",
     main = "Residuals vs USCPI")

#Test for autocorrelation

# Test for first order autocorrelation using Durbin-Watson test
durbin_watson_test <- durbinWatsonTest(PPP_regression_model)
print(durbin_watson_test)

# Implement Cochran-Orcutt procedure
cochrane_orcutt <- function(model, data) {
  # Get residuals from the initial model
  residuals <- residuals(model)
  
  # Compute new Y* using residuals and lagged residuals
  residuals_lagged <- c(NA, residuals[-length(residuals)])  # Add NA for lagged residuals
  
  # Exclude NA rows from the dataset
  data <- data[-1, ]
  residuals <- residuals[-1]
  residuals_lagged <- residuals_lagged[-1]
  
  # Fit a new model with Y* as the response
  new_model <- lm(residuals ~ . - 1, data = data)
  
  # Extract residuals from the new model
  new_residuals <- residuals(new_model)
  
  # Compute new rho
  rho <- cor(new_residuals, residuals_lagged, method = "pearson")
  
  # Correct residuals using the estimated rho
  corrected_residuals <- residuals - rho * residuals_lagged
  
  # Fit the final model with corrected residuals
  final_model <- lm(BTC ~ (USINF - JPNINF) + USCPI, data = data)
  
  return(final_model)
}
# Apply the Cochran-Orcutt procedure to the initial model
adjusted_model <- cochrane_orcutt(PPP_regression_model, ECN314_Crypto_PPP)

# View the summary of the adjusted model
summary(adjusted_model)

# Extract coefficients from the adjusted model
tidy_results_adjusted <- tidy(adjusted_model)
print(tidy_results_adjusted)
