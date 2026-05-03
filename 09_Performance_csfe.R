f_csfe <- function(x, y_bench, y_real) {
  
  #' Calcula o Cumulative Squared Forecast Error (CSFE)
  #'
  #' This function computes the cumulative squared forecast error relative to a benchmark and
  #' actual values, allowing assessment of model accuracy against the benchmark.
  #'
  #' @param x A numeric vector containing model forecasts.
  #' @param y_bench A numeric vector containing benchmark forecasts.
  #' @param y_real A numeric vector containing observed actual values.
  #' @return A numeric vector with the cumulative squared error at each time point.
  #'
  #' @examples
  #' f_csfe(c(1, 2, 3), c(1.5, 2.5, 3.5), c(1, 2, 3)) # Returns the cumulative squared forecast error
  #'
  
  error_bench <- (y_bench - y_real)^2
  error_x <- (x - y_real)^2
  result <- cumsum(error_bench - error_x)
  return(result)
}

csfe = function(model, benchmarkq, y_real){
  
  #' Calcula CSFE para Diferentes Horizontes
  #'
  #' This function computes the Cumulative Squared Forecast Error (CSFE) for different forecast horizons
  #' from model and benchmark forecasts.
  #'
  #' @param model An object containing model forecasts, with columns representing different horizons.
  #' @param benchmark An object containing benchmark forecasts, with columns corresponding to the same horizons.
  #' @param y_real A numeric vector containing observed actual values.
  #' @return A matrix with the cumulative squared errors for each forecast horizon.
  #'
  #' @examples
  #' csfe_results <- csfe(model, benchmark, y_real)
  #' print(csfe_results)
  #'
  
  h1 = f_csfe(model$forecast[,1], benchmarkq$forecasts[,1], y_real = y_real)
  h4 = f_csfe(model$forecast[,2], benchmarkq$forecasts[,2], y_real = y_real)
  
  cbind(h1, h4)
  
}

csfe1 = function(model, benchmark, y_real){
  
  #' Calcula CSFE para Diferentes Horizontes
  #'
  #' This function computes the Cumulative Squared Forecast Error (CSFE) for different forecast horizons
  #' from model and benchmark forecasts.
  #'
  #' @param model An object containing model forecasts, with columns representing different horizons.
  #' @param benchmark An object containing benchmark forecasts, with columns corresponding to the same horizons.
  #' @param y_real A numeric vector containing observed actual values.
  #' @return A matrix with the cumulative squared errors for each forecast horizon.
  #'
  #' @examples
  #' csfe_results <- csfe(model, benchmark, y_real)
  #' print(csfe_results)
  #'
  
  h1 = f_csfe(model$forecast[,1], benchmark$forecasts[,1], y_real = y_real)
  h12 = f_csfe(model$forecast[,2], benchmark$forecasts[,2], y_real = y_real)
  
  cbind(h1, h12)
  
}
