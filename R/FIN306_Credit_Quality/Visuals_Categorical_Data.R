library(readr)
bank_customers_simulated <- read_csv("Data.R/bank_customers_simulated.csv")
View(bank_customers_simulated)

# Load ggplot2 for plotting
library(ggplot2)

# Bar Chart for Employment Status
ggplot(bank_customers_simulated, aes(x = EmploymentStatus)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Employment Status", x = "Employment Status", y = "Count") +
  theme_minimal()

# Bar Chart for Education Level (Highschool, Associate, Bachelor, Master, PhD)
ggplot(bank_customers_simulated, aes(x = EducationLevel)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Education Level", x = "Education Level", y = "Count") +
  theme_minimal()

# Bar Chart for Previous Defaults
ggplot(bank_customers_simulated, aes(x = PreviousDefaults)) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Distribution of Previous Defaults", x = "Previous Defaults", y = "Count") +
  theme_minimal()

# Bar Chart for Marital Status
ggplot(bank_customers_simulated, aes(x = MaritalStatus)) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Distribution of Marital Status", x = "Marital Status", y = "Count") +
  theme_minimal()
