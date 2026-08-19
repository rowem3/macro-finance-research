import os
from dotenv import load_dotenv
from fredapi import Fred

load_dotenv()
fred = Fred(api_key=os.getenv("FRED_API_KEY"))

data = fred.get_series('FEDFUNDS')
print(data.tail())