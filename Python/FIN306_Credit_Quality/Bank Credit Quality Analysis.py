import pandas as pd
import statsmodels.api as sm

# Load the dataset
data_path = "path_to_bank_customers_regression.xlsx"  # Replace with the file path
df = pd.read_excel("data/bank_customers_regression.xlsx")

# Prepare the data for logistic regression
# Select predictors and the target variable
predictors = [
    "Income", "Savings", "LoanAmountRequested", "EmploymentStatusSelfemployed",
    "EmploymentStatusUnemployed", "Age", "Associate", "Bachelor", "Master", "PhD",
    "MaritalStatusDivorced", "MaritalStatusSingle", "MaritalStatusWidowed",
    "CreditScore", "PreviousDefaults"
]
X = df[predictors]
y = df["LoanApproved"]

# Add a constant for the intercept term
X = sm.add_constant(X)

# Fit the logistic regression model
model = sm.Logit(y, X)
result = model.fit()

# Print the summary of the regression results
print(result.summary())