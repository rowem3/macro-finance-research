import yfinance as yf
import pandas as pd

# Define the currencies and the time frame
currencies = ['BRLUSD=X', 'NZDUSD=X', 'CHFUSD=X', 'CNYUSD=X', 'GBPUSD=X', 'EURUSD=X', 'MXNUSD=X', 'RUBUSD=X']
start_date = '2024-09-16'
end_date = '2024-11-15'

# Initialize an empty DataFrame to store the percent changes
percent_changes = pd.DataFrame()

# Fetch the data for each currency
for currency in currencies:
    # Download data from Yahoo Finance
    data = yf.download(currency, start=start_date, end=end_date)['Adj Close']
    
    # Calculate daily percent change
    percent_change = data.pct_change() * 100  # Convert to percentage
    percent_changes[currency] = percent_change

# Clean the DataFrame (remove rows with NaN values, which occur for the first day)
percent_changes = percent_changes.dropna()

# Print the resulting table
print(percent_changes)

