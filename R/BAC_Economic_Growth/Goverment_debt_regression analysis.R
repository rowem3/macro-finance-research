# Load Required Libraries
library(readr)
library(plm)
library(car)

# Import Data Set
df_regression_OECD_heritage_foundation <- read_csv("Data.R/df_regression_OECD_heritage_foundation.csv")

# Convert to Panel Data
pdata <- pdata.frame(df_regression_OECD_heritage_foundation, index = c("Country_Code", "Year"))

# Fixed Effects Model (without multicollinear variable)
model_fe <- plm(Central_Government_Debt ~ 
                  Net_migration +
                  Labor_force_participation_rate_for_ages_15_24 +
                  Public_Social_spending +
                  Foreign_direct_investment_net_inflows +
                  Urban_population +
                  Tax_revenue +
                  Current_account_balance +
                  Inflation_consumer_prices +
                  Profit_tax +
                  Exports_of_goods_and_services +
                  Population_growth,
                data = pdata,
                model = "within")

# Random Effects Model (same updated variables)
model_re <- plm(Central_Government_Debt ~ 
                  Net_migration +
                  Labor_force_participation_rate_for_ages_15_24 +
                  Public_Social_spending +
                  Foreign_direct_investment_net_inflows +
                  Urban_population +
                  Tax_revenue +
                  Current_account_balance +
                  Inflation_consumer_prices +
                  Profit_tax +
                  Exports_of_goods_and_services +
                  Population_growth,
                data = pdata,
                model = "random")

# Summary of both models
summary(model_fe)
summary(model_re)

# Hausman Test: Determines whether Fixed or Random Effects is more appropriate
phtest(model_fe, model_re)

library(dplyr)

df_regression_OECD_heritage_foundation %>%
  group_by(Country_Code) %>%
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE))) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  t()

