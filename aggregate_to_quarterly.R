library(lubridate)

aggregate_to_quarterly <- function(results, info) {
  
  quarterly <- list()
  
  # Extract original dates
  dates <- results$date
  
  # Create quarter index for each month
  quarter_index <- paste0(year(dates), "-Q", quarter(dates))
  
  # Count how many months exist in each quarter
  quarter_counts <- table(quarter_index)
  
  # Keep only full quarters (3 months)
  full_quarters <- names(quarter_counts[quarter_counts == 3])
  
  if (length(full_quarters) == 0) stop("No full quarters in dataset.")
  
  # Get last month of each full quarter (Date)
  quarterly[["date"]] <- sapply(split(dates, quarter_index), max)[full_quarters]
  quarterly[["date"]] <- as.Date(quarterly[["date"]], origin = "1970-01-01")  # preserve Date class
  
  # Loop through series
  for (i in seq_len(nrow(info))) {
    serie_name <- info$Serie[i]
    type_sw    <- info$type_sw[i]
    
    if (type_sw == 4) next  # skip date
    
    serie <- results[[serie_name]]
    
    # remove NA at the beginning
    serie <- serie[!is.na(serie)]
    
    # Aggregate by quarter
    q_list <- split(serie, quarter_index)
    
    # Keep only full quarters
    q_list <- q_list[full_quarters]
    
    if (type_sw == 1) {
      # Simple difference -> sum
      q_serie <- sapply(q_list, sum)
      
    } else if (type_sw %in% c(2,3)) {
      # Log differences -> cumulative factor
      q_serie <- sapply(q_list, function(x) prod(exp(x)) - 1)
      
    } else {
      warning(paste("Tipo desconhecido para", serie_name, "- pulando."))
      next
    }
    
    quarterly[[serie_name]] <- as.numeric(q_serie)
  }
  
  return(quarterly)
}
