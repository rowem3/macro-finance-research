#Main Model------

install.packages ("stargazer")
library(stargazer)

installed.packages("broom")
library(broom)

# Fit linear regression model
regression_model <- lm (GDP_per_Capita_PPP ~ `2022Score`, data = index2022_data_1_)

# Display summary of regression results
summary(regression_model)

# Check if index2022_data_1_ data frame exists
exists("index2022_data_1_")

# Display regression results using stargazer
stargazer (regression_model, title = "Regression Results", type = "text")

# Extract tidy results
tidy_results <- tidy(regression_model)

# Create scatter plot with regression line
plot (index2022_data_1_$`2022Score`, index2022_data_1_$GDP_per_Capita_PPP,
      xlab = "`2022Score`", ylab = "GDP_per_Capita_PPP",
      main = "GDP_per_Capita_PPP and `2022Score`")

# Add regression line to the plot
abline (regression_model, col = "red")

# Extract intercept and slope
intercept <- coef (regression_model)[1]
slope <- coef (regression_model)[2]

# Display intercept and slope on the plot
mtext(paste("Intercept:", round(intercept, 2), "\nSlope:", round(slope, 2)),
      side = 3, line = -3, cex = 1.0)