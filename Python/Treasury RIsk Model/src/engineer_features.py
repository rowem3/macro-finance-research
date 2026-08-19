import os
import pandas as pd
from dotenv import load_dotenv
from fredapi import Fred

load_dotenv()
fred = Fred(api_key=os.getenv("FRED_API_KEY"))

# Pull macro data
fed_funds = fred.get_series('FEDFUNDS')
treasury_10y = fred.get_series('DGS10')
unemployment = fred.get_series('UNRATE')

# Combine into dataframe
macro_df = pd.DataFrame({
    'fed_funds_rate': fed_funds,
    'treasury_10y': treasury_10y,
    'unemployment_rate': unemployment
})

# Reset index so date becomes a column
macro_df = macro_df.reset_index()
macro_df.columns = ['date', 'fed_funds_rate', 'treasury_10y', 'unemployment_rate']

# Extract day and month for easier checking
macro_df['day'] = macro_df['date'].dt.day
macro_df['month'] = macro_df['date'].dt.month

# Flag for payday (15th or last day of month)
macro_df['is_payday'] = (
    (macro_df['day'] == 15) |
    (macro_df['day'] == macro_df['date'].dt.daysinmonth)
).astype(int)

# Flag for quarter-end (Mar 31, Jun 30, Sep 30, Dec 31)
macro_df['is_quarter_end'] = (
    ((macro_df['month'] == 3) & (macro_df['day'] == 31)) |
    ((macro_df['month'] == 6) & (macro_df['day'] == 30)) |
    ((macro_df['month'] == 9) & (macro_df['day'] == 30)) |
    ((macro_df['month'] == 12) & (macro_df['day'] == 31))
).astype(int)

# Flag for a few key US federal holidays (fixed dates only for now)
holiday_dates = pd.to_datetime([
    '2024-01-01', '2024-07-04', '2024-11-28', '2024-12-25',
    '2025-01-01', '2025-07-04', '2025-11-27', '2025-12-25',
    '2026-01-01', '2026-07-04', '2026-11-26', '2026-12-25'
])
macro_df['is_holiday'] = macro_df['date'].isin(holiday_dates).astype(int)

# Save the result
macro_df.to_csv('../data/engineered_macro_data.csv', index=False)
print(macro_df.tail(10))
print(f"\nTotal paydays flagged: {macro_df['is_payday'].sum()}")
print(f"Total quarter-ends flagged: {macro_df['is_quarter_end'].sum()}")
print(f"Total holidays flagged: {macro_df['is_holiday'].sum()}")