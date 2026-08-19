import requests
import pandas as pd

headers = {'User-Agent': 'Mac Research your_email@example.com'}
url = 'https://data.sec.gov/api/xbrl/companyconcept/CIK0000927628/us-gaap/NetCashProvidedByUsedInOperatingActivities.json'

response = requests.get(url, headers=headers)
data = response.json()

# The actual reported values live under 'units' -> 'USD'
records = data['units']['USD']
cof_cashflow = pd.DataFrame(records)

print(cof_cashflow[['end', 'val', 'form', 'fp']].tail(15))
cof_cashflow.to_csv('../data/cof_operating_cashflow.csv', index=False)
