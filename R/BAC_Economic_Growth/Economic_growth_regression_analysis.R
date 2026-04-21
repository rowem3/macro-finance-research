# Load Required Libraries
library(readr)
library(plm)
library(car)
library(dplyr)

df_regression_OECD_heritage_foundation <- read_csv(
  "data/df_with_yoy_OECD.csv"
)

# Convert to Panel Data
pdata <- pdata.frame(df_regression_OECD_heritage_foundation, index = c("Country_Code", "Year"))

# Fixed Effects Model
model_fe <- plm(GDP_YOY ~ 
                  Net_migration_YOY +
                  Labor_force_participation_rate_for_ages_15_24_YOY +
                  Public_Social_spending_YOY +
                  Foreign_direct_investment_net_inflows_YOY +
                  Urban_population_YOY +
                  Imports_of_goods_and_services_YOY +
                  Tax_revenue_YOY +
                  Population_growth_YOY +
                  Property_Rights +
                  Government_Integrity +
                  Judicial_Effectiveness +
                  Tax_Burden +
                  Government_Spending +
                  Fiscal_Health +
                  Business_Freedom +
                  Labor_Freedom +
                  Monetary_Freedom +
                  Trade_Freedom +
                  Investment_Freedom +
                  Financial_Freedom,
                data = pdata,
                model = "within")

# Random Effects Model
model_re <- plm(GDP_YOY ~ 
                  Net_migration_YOY +
                  Labor_force_participation_rate_for_ages_15_24_YOY +
                  Public_Social_spending_YOY +
                  Foreign_direct_investment_net_inflows_YOY +
                  Urban_population_YOY +
                  Imports_of_goods_and_services_YOY +
                  Tax_revenue_YOY +
                  Population_growth_YOY +
                  Property_Rights +
                  Government_Integrity +
                  Judicial_Effectiveness +
                  Tax_Burden +
                  Government_Spending +
                  Fiscal_Health +
                  Business_Freedom +
                  Labor_Freedom +
                  Monetary_Freedom +
                  Trade_Freedom +
                  Investment_Freedom +
                  Financial_Freedom,
                data = pdata,
                model = "random")

# Summarize both models
summary(model_fe)
summary(model_re)

# Hausman Test to choose between Fixed and Random effects
phtest(model_fe, model_re)
