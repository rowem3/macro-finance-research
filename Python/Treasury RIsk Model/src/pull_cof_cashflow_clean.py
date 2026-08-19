import requests
import pandas as pd

headers = {'User-Agent': 'Mac Research your_email@example.com'}
url = 'https://data.sec.gov/api/xbrl/companyconcept/CIK0000927628/us-gaap/NetCashProvidedByUsedInOperatingActivities.json'

response = requests.get(url, headers=headers)
data = response.json()
records = data['units']['USD']
df = pd.DataFrame(records)

# Clean up: drop duplicates, keep only actual quarterly/annual filings
df = df.drop_duplicates(subset='end')
df = df[df['form'].isin(['10-Q', '10-K'])]

df['end'] = pd.to_datetime(df['end'])
df['year'] = df['end'].dt.year

# Order fiscal periods within each year
fp_order = {'Q1': 1, 'Q2': 2, 'Q3': 3, 'FY': 4}
df['fp_rank'] = df['fp'].map(fp_order)
df = df.sort_values(['year', 'fp_rank']).reset_index(drop=True)

# Subtract prior YTD value to get standalone quarterly figure
df['prior_ytd'] = df.groupby('year')['val'].shift(1).fillna(0)
df['standalone_val'] = df['val'] - df['prior_ytd']

# Relabel FY as Q4 since it's really the 4th quarter's cumulative total
df['quarter_label'] = df['fp'].replace({'FY': 'Q4'})

result = df[['end', 'year', 'quarter_label', 'val', 'standalone_val']]
result.columns = ['period_end', 'year', 'quarter', 'ytd_cumulative', 'standalone_cashflow']

print(result)
result.to_csv('../data/cof_quarterly_cashflow.csv', index=False)


