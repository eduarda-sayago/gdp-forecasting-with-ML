library(forecast)

get_stationarity <- function(dataset, date = TRUE, seasonal_period = 12, adf_alpha = 0.05){
  # requires forecast package
  # if date = TRUE ignore first column
  data <- if (isTRUE(date)) dataset[ , -1, drop = FALSE] else dataset
  
  out <- data.frame(Serie = character(0), ndiffs = integer(0), nsdiffs = integer(0),
                    stringsAsFactors = FALSE)
  
  for(col_name in names(data)){
    x <- as.numeric(data[[col_name]])
    x_ts <- ts(x, frequency = seasonal_period)    # set frequency here
    nd <- forecast::ndiffs(x_ts, alpha = adf_alpha, test = "adf", type = "level")
    ns <- forecast::nsdiffs(x_ts)                  # no m=, avoids the warning
    out <- rbind(out, data.frame(Serie = col_name, ndiffs = nd, nsdiffs = ns,
                                 stringsAsFactors = FALSE))
  }
  
  return(out)
}

month_vars <- get_stationarity(data_m, date = TRUE, seasonal_period = 12)
quart_vars <- get_stationarity(data_q, date = TRUE, seasonal_period = 4)
