rolling_window <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  
  #' Rolling Window for Modeling
  #'
  #' This function applies a specified function to a sliding window of data in a `data.frame` to generate forecasts.
  #'
  #' @param fn The function to apply to each sliding window. It must return an object with forecasts and outputs.
  #' @param df A `data.frame` containing the data to be processed.
  #' @param nwindow The size of the sliding window. Default is 1.
  #' @param horizon The forecast horizon, determining how many observations to project ahead.
  #' @param variable The name of the dependent variable to use in the analysis.
  #' @param ... Additional arguments to be passed to function `fn`.
  #' @return A list with two elements:
  #' - forecast: A vector with the forecasts from each application of the function over the sliding windows.
  #' - outputs: A list containing the outputs from each application of the function over the sliding windows.
  #'
  #' @examples
  #' df <- data.frame(data = 1:100, variable = rnorm(100))
  #' result <- rolling_window(fn = my_forecast_function, df = df, nwindow = 5, horizon = 1, variable = 'variable')
  #'
  #' @export
  
  #ind <- seq_len(nrow(df))
  window_size <- nrow(df) - nwindow # rows of df - window size (92 - 31= 61); h4 = (28+4-1)
  indmat <- matrix(NA, window_size, nwindow)
  indmat[1, ] <- seq_len(ncol(indmat))
  for (i in 2:nrow(indmat)) {
    indmat[i, ] <- indmat[i - 1, ] + 1
  }
  
  
  rw <- apply(
    X = indmat,
    MARGIN = 2, # it actually is = 2.
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs <- lapply(rw, function(x) x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}

rolling_window2 <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  
  #' Rolling Window for Modeling
  #'
  #' This function applies a specified function to a sliding window of data in a `data.frame` to generate forecasts.
  #'
  #' @param fn The function to apply to each sliding window. It must return an object with forecasts and outputs.
  #' @param df A `data.frame` containing the data to be processed.
  #' @param nwindow The size of the sliding window. Default is 1.
  #' @param horizon The forecast horizon, determining how many observations to project ahead.
  #' @param variable The name of the dependent variable to use in the analysis.
  #' @param ... Additional arguments to be passed to function `fn`.
  #' @return A list with two elements:
  #' - forecast: A vector with the forecasts from each application of the function over the sliding windows.
  #' - outputs: A list containing the outputs from each application of the function over the sliding windows.
  #'
  #' @examples
  #' df <- data.frame(data = 1:100, variable = rnorm(100))
  #' result <- rolling_window(fn = my_forecast_function, df = df, nwindow = 5, horizon = 1, variable = 'variable')
  #'
  #' @export
  
  n <- nrow(df)
  w <- nwindow            # w == n - b + 1
  h <- horizon
  
  s <- w - h
  b <- n - w + 1

  # index matrix must be (s x b) and iterates through each column
  indmat <- matrix(NA, s, b)
  indmat[1, ] <- seq_len(b) # (first row is filled with numbers from 1 to b)
  for (r in 2:nrow(indmat)) indmat[r, ] <- indmat[r - 1, ] + 1 # from row 2:s, row before +1
  
  rw <- apply(
    X = indmat,
    MARGIN = 2, # iterates through columns.
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  
  # print(rw)
  # View(rw)
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs <- lapply(rw, function(x) x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}
