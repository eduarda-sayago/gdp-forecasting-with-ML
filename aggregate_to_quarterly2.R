aggregate_to_quarterly2 <- function(results, info) {
  # Dates
  dates <- as.Date(results[["date"]])
  stopifnot(inherits(dates, "Date"))
  
  # Year and month
  y <- as.integer(format(dates, "%Y"))
  m <- as.integer(format(dates, "%m"))
  
  # Quarter ID
  q <- paste0(y, "Q", (m - 1) %/% 3 + 1)
  
  # Keep only quarters with 3 obs
  valid_q <- names(which(table(q) == 3))
  
  # Last date of each valid quarter
  q_last_date <- tapply(dates, q, function(v) max(v))
  q_last_date <- q_last_date[valid_q]
  
  # Output container
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
      stop(paste("Unknown type_sw for", varname))
    }
  }
  
  return(list(results = out_results, info = info))
}


# each x will be now (x/100+1)
# each row will be now a product of the last three values




