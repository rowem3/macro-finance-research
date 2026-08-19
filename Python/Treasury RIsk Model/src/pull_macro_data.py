import os
import pandas as pd
from dotenv import load_dotenv
from fredapi import Fred

load_dotenv()
fred = Fred(api_key=os.getenv("FRED_API_KEY"))

# Pull each series
fed_funds = fred.get_series('FEDFUNDS')
treasury_10y = fred.get_series('DGS10')
unemployment = fred.get_series('UNRATE')

# Combine into one dataframe
macro_df = pd.DataFrame({
    'fed_funds_rate': fed_funds,
    'treasury_10y': treasury_10y,
    'unemployment_rate': unemployment
})

# Forward-fill the monthly series so every day has a value
macro_df['fed_funds_rate'] = macro_df['fed_funds_rate'].ffill()
macro_df['unemployment_rate'] = macro_df['unemployment_rate'].ffill()

# Save to CSV so it persists
macro_df.to_csv('../data/macro_data.csv')

print(macro_df.tail(10))
print(macro_df.shape)