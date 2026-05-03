

aggregate_to_quarterly3 <- function(results, info) {
  
  #' Aggregate Monthly Data to Quarterly
  #'
  #' This function transforms a set of time series into quarterly frequency based on aggregation
  #' rules defined in the `info` table. Each variable can be aggregated as a sum or growth rate, 
  #' depending on its type classification.
  #'
  #' @param results -- A `list` or `data.frame` containing time series with a `date` column 
  #' and variables to be aggregated.
  #' @param info -- A `data.frame` with at least two columns:
  #' - `Serie`: the variable names corresponding to those in `results`.
  #' - `type_sw`: an integer code with numbers between 1 and 4, that indicates how the variable
  #'  should be aggregated:
  #'   - 1: Sum over the quarter
  #'   - 2: Compound growth rate over the quarter
  #'   - 3: Sum (same as 1)
  #'   - 4: Exclusively date column. Keeps the last date of the quarter
  #'
  #' @return A list with two elements:
  #' - `results`: A list with the quarterly aggregated time series.
  #' - `info`: The input `info` table (unchanged).
  #'
  #' @details
  #' - Only quarters with exactly 3 observations are retained.
  #'
  #' @examples
  #' set.seed(123)
  #' results <- list(
  #'   date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
  #'   var1 = rnorm(24, 100, 5),  # type 1
  #'   var2 = rnorm(24, 2, 1),    # type 2
  #'   var3 = rnorm(24, 50, 3)    # type 4
  #' )
  #'
  #' info <- data.frame(
  #'   Serie = c("date", "var1", "var2", "var3"),
  #'   type_sw = c(4, 1, 2, 4)
  #' )
  #'
  #' quarterly_data <- aggregate_to_quarterly(results, info)
  #'
  #' @export
  
  # Organize quarters
  dates <- as.Date(results[["date"]])
  stopifnot(inherits(dates, "Date"))
  
  y <- lubridate::year(dates)
  q_num <- lubridate::quarter(dates)
  q <- paste0(y, "Q", q_num) # Quarter ID (ex: 2020Q1)
  valid_q <- names(which(table(q) == 3)) # Keep only quarters with 3 obs
  
  q_last_date <- tapply(dates, q, function(v) max(v)) # Last date of each valid quarter
  q_last_date <- q_last_date[valid_q]
  
  ####### Logistic for transforming to quarterly
  out_results <- list()
  
  for (varname in names(results)) {
    x <- results[[varname]]
    type <- info$type_sw[match(varname, info$Serie)]
    
    if (type == 4) {
      out_results[[varname]] <- as.Date(q_last_date, origin = "1970-01-01")
      
    } else if (type %in% c(1, 3)) {
      agg <- tapply(x, q, function(v) if (length(v) == 3) sum(v) else NA)
      out_results[[varname]] <- as.numeric(agg[valid_q])
      
    } else if (type == 2) {
      agg <- tapply(x, q, function(v) if (length(v) == 3) 100*(prod(v/100 + 1)-1) else NA)
      out_results[[varname]] <- as.numeric(agg[valid_q])
      
    } else {
      warning(paste("Unknown type_sw for", varname))
      next
    }
  }
  
  ## Output
  return(list(results = out_results, info = info))
}
