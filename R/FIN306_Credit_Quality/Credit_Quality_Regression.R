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

Consumption <- read_excel("Data/bank_customers_simulated.csv")
View(Consumption)

# Fit regression model
Consumption_regression_model <- lm(C ~ DI + CC + Net_Wealth + IR, data = Consumption)

# Display summary of regression results
summary(Consumption_regression_model)

# Display regression results using stargazer
stargazer(Consumption_regression_model, title = "Regression Results", type = "text")

# Display tidy regression results
tidy_results <- tidy(Consumption_regression_model)
print(tidy_results)