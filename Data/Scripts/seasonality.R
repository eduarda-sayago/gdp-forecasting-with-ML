### Seasonality adjustment ###

## Identifies which have seasonality, applies X13-ARIMA-SEATS on them.

#rm(list = setdiff(ls(), "base_bruta"))


base_bruta <- read.csv("Data/data_month/base_bruta.csv")

sa_bruta = base_bruta[-281:-282,]
sa_bruta = sa_bruta[,-1:-2]


# Evaluate if seasonality present

library(seastests)

seasonal_tests <- sapply(sa_bruta, function(col) {
  ts_col <- ts(col, frequency = 12)
  qs(ts_col)
})

seasonal_tests <- as.data.frame(t(seasonal_tests))
seasonal_tests$result <- ifelse(as.numeric(seasonal_tests$Pval) < 0.05, 1, 0)

seasonal <- data.frame(
  Variable = colnames(sa_bruta),
  result = seasonal_tests$result
)

########
library(seasonal)

# Create a copy of sa_bruta to store adjusted values
sa_bruta_adj <- sa_bruta

# Loop over all variables except the date column
for (var in seasonal$Variable) {
  
  if (var == "date") next  # skip the date column
  
  # Extract the original series
  series <- sa_bruta[[var]]
  
  # Find the last non-NA value
  last_non_na <- max(which(!is.na(series)))
  series_clean <- series[1:last_non_na]
  
  # Convert to ts object starting from 2002-01, monthly frequency
  ts_series <- ts(series, start = c(2002, 1), frequency = 12)
  
  # Check if this variable is seasonal
  is_seasonal <- seasonal$result[seasonal$Variable == var] == 1
  
  if (is_seasonal) {
    # Run X-13-ARIMA-SEATS
    seas_obj <- seas(ts_series)
    
    # Extract the seasonally adjusted series
    adjusted_series <- final(seas_obj)
    
    # Fill the original vector (keeping NAs at the end)
    series[1:last_non_na] <- as.numeric(adjusted_series)
  }
  
  # Assign back to the adjusted dataframe
  sa_bruta_adj[[var]] <- series
}


###
seasonal_tests <- sapply(sa_bruta_adj, function(col) {
  ts_col <- ts(col, frequency = 12)
  qs(ts_col)
})

seasonal_tests <- as.data.frame(t(seasonal_tests))
seasonal_tests$result <- ifelse(as.numeric(seasonal_tests$Pval) < 0.05, 1, 0)

seasonal <- data.frame(
  Variable = colnames(sa_bruta_adj),
  result = seasonal_tests$result
)

#####
# Evaluate if seasonal component needed

# library(forecast)
# 
# seasonal_diffs_needed <- sapply(base_ponto, function(col) {
#   ts_col <- ts(col, frequency = 4)  # set frequency to match your data period
#   nsdiffs(ts_col)
# })
# 
# seasonal_diffs_needed
# 
# variable_types = data.frame(
#   Variable = colnames(base_ponto),
#   Category = tipo,
#   Seasonal = seasonal_diffs_needed
# )