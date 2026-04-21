# Load necessary libraries
library(quantmod)
library(ggplot2)
library(dplyr)

# Define currencies and date range
currencies <- c("BRL", "NZD", "CHF", "CNY", "GBP", "EUR", "MXN", "RUB")
start_date <- "2024-08-20"
end_date <- "2024-11-20"

# Function to get exchange rates
get_currency_data <- function(currency, start_date, end_date) {
  symbol <- paste0(currency, "=X")  # Yahoo format for forex rates
  data <- tryCatch({
    getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  }, error = function(e) {
    message(paste("Error fetching data for", currency, ": ", e$message))
    return(NULL)
  })
  return(data)
}

# Get data for all currencies, filtering out NULLs
currency_data <- lapply(currencies, get_currency_data, start_date, end_date)
names(currency_data) <- currencies
currency_data <- Filter(Negate(is.null), currency_data)  # Remove NULL results

# Ensure that all data has the same structure (remove incompatible columns if needed)
prepare_data <- function(data, currency) {
  if (is.null(data)) return(NULL)
  
  # Ensure we are dealing with xts/zoo object with column names
  if (ncol(data) >= 6) {
    data <- data[, 6]  # Use Adjusted closing price
    data <- data.frame(Date = index(data), ExchangeRate = coredata(data), Currency = currency)
    return(data)
  } else {
    return(NULL)
  }
}

# Prepare all currencies data, filtering out NULLs
daily_data <- do.call(rbind, lapply(names(currency_data), function(cur) prepare_data(currency_data[[cur]], cur)))

# Check if we successfully gathered daily data
if (nrow(daily_data) > 0) {
  # Plot daily exchange rates
  p1 <- ggplot(daily_data, aes(x = Date, y = ExchangeRate, color = Currency)) +
    geom_line(size = 1) +
    labs(title = "Daily Exchange Rates (Aug 20, 2024 - Nov 20, 2024)",
         x = "Date", y = "Exchange Rate (to USD)") +
    theme_minimal()
  
  # Print plot to ensure it appears in the plot quadrant
  print(p1)
} else {
  message("No valid data available for plotting.")
}

# Weekly data (every Monday)
if (nrow(daily_data) > 0) {
  weekly_data <- daily_data %>%
    filter(weekdays(Date) == "Monday") %>%
    group_by(Currency) %>%
    mutate(Week = row_number())
  
  # Plot weekly exchange rates
  p2 <- ggplot(weekly_data, aes(x = Date, y = ExchangeRate, color = Currency)) +
    geom_line(size = 1) +
    labs(title = "Weekly Exchange Rates (Mondays Only)",
         x = "Date", y = "Exchange Rate (to USD)") +
    theme_minimal()
  
  # Print plot to ensure it appears in the plot quadrant
  print(p2)
} else {
  message("No valid data available for weekly plotting.")
}

# Produce tables for daily and weekly data
if (nrow(daily_data) > 0) {
  daily_table <- daily_data %>%
    select(Date, Currency, ExchangeRate) %>%
    pivot_wider(names_from = Currency, values_from = ExchangeRate)
  print(daily_table)
  
  weekly_table <- weekly_data %>%
    select(Date, Currency, ExchangeRate) %>%
    pivot_wider(names_from = Currency, values_from = ExchangeRate)
  print(weekly_table)
} else {
  message("No valid data available for tables.")
}
