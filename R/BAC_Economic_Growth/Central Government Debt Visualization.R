# Load required packages
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
df_OECD <- read_csv("23032025 _Initial_Project/Data.R/df_OECD.csv")

# Clean and filter the data
df_clean <- df_OECD %>%
  select(Year, `Country Name`, `Central Government Debt (Percent of GDP)`) %>%
  filter(!is.na(`Central Government Debt (Percent of GDP)`))

# Create the plot
ggplot(df_clean, aes(x = Year, y = `Central Government Debt (Percent of GDP)`, color = `Country Name`)) +
  geom_line() + 
  labs(
    title = "Central Government Debt as a Percentage of GDP Over Time",
    x = "Year",
    y = "Central Government Debt (% of GDP)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Hide the legend if too many countries
  )


