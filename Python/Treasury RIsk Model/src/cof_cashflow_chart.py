import requests
import pandas as pd
import matplotlib.pyplot as plt

headers = {'User-Agent': 'Mac Research your_email@example.com'}
url = 'https://data.sec.gov/api/xbrl/companyconcept/CIK0000927628/us-gaap/NetCashProvidedByUsedInOperatingActivities.json'

response = requests.get(url, headers=headers)
data = response.json()
records = data['units']['USD']
df = pd.DataFrame(records)

# Clean up
df = df.drop_duplicates(subset='end')
df = df[df['form'].isin(['10-Q', '10-K'])]
df['end'] = pd.to_datetime(df['end'])
df['year'] = df['end'].dt.year

# Only keep 2009 onward
df = df[df['year'] >= 2009]

# Order fiscal periods within each year
fp_order = {'Q1': 1, 'Q2': 2, 'Q3': 3, 'FY': 4}
df['fp_rank'] = df['fp'].map(fp_order)
df = df.sort_values(['year', 'fp_rank']).reset_index(drop=True)

# Convert YTD cumulative to standalone quarterly
df['prior_ytd'] = df.groupby('year')['val'].shift(1).fillna(0)
df['standalone_val'] = df['val'] - df['prior_ytd']
df['quarter_label'] = df['fp'].replace({'FY': 'Q4'})

result = df[['end', 'year', 'quarter_label', 'standalone_val']].copy()
result.columns = ['period_end', 'year', 'quarter', 'operating_cashflow']

# Save to CSV for later use
result.to_csv('../data/cof_quarterly_cashflow_2009_2026.csv', index=False)

# Plot
plt.figure(figsize=(14, 6))
plt.plot(result['period_end'], result['operating_cashflow'], marker='o', linewidth=1.5)
plt.title("Capital One (COF) — Quarterly Operating Cash Flow, 2009–2026")
plt.xlabel("Quarter")
plt.ylabel("Operating Cash Flow (USD)")
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('../data/cof_quarterly_cashflow_chart.png')
plt.show()

print(result)