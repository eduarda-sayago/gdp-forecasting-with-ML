dataprep <- function(type = '...', ind, df, variable, horizon, n_lags = 4, dum_cols = NULL) {
  
  #' Data Preparation for Modeling
  #'
  #' This function prepares data for analysis by creating lag variables and separating dependent and independent variables.
  #'
  #' @param type A character string defining the type of data preparation. Use 'tb' for a table structure or another value as needed.
  #' @param ind A vector of indices to select rows from the `data.frame`.
  #' @param df A `data.frame` containing the data to be prepared.
  #' @param variable The name of the dependent variable to use in the analysis.
  #' @param horizon The forecast horizon, determining how many observations to project ahead.
  #' @param n_lags The number of lags to create. Must be an integer greater than 1. Default is 4.
  #' @return A list with three elements:
  #' - x_in: A `data.frame` containing the input independent variables, excluding the last `horizon` records.
  #' - x_out: A vector containing the last observation of the independent variables (the forecast input).
  #' - y_in: A vector with the corresponding dependent variable values for the input observations.
  #'
  #' @examples
  #' df <- data.frame(data = 1:100, variable = rnorm(100))
  #' result <- dataprep(type = 'tb', ind = 1:80, df = df, variable = 'variable', horizon = 1, n_lags = 4)
  #'
  #' @export
  
  if (n_lags <= 1) stop("n_lags deve ser um inteiro maior do que 1.")
  
  df <- df[ind, ]
  y <- df[, variable]
  
  if (type == 'tb') {
    x_aux <- df[,-1, drop = FALSE]
  } else {
    x_aux <- df
  }
  
  # validate dummy columns
  if (!is.null(dum_cols)) {
    if (!all(dum_cols %in% colnames(x_aux))) {
      stop("Some dum_cols do not exist in x_aux.")
    }
  }
  
  # separate numeric (to be lagged) and dummy (no lags)
  if (is.null(dum_cols)) {
    x_numeric <- as.matrix(x_aux)
    dum_mat <- NULL
    dum_names <- NULL
  } else {
    dum_names <- dum_cols
    dum_mat <- as.matrix(x_aux[, dum_names, drop = FALSE])
    numeric_names <- setdiff(colnames(x_aux), dum_names)
    if (length(numeric_names) > 0) {
      x_numeric <- as.matrix(x_aux[, numeric_names, drop = FALSE])
    } else {
      # no numeric columns to lag
      x_numeric <- matrix(nrow = nrow(x_aux), ncol = 0)
      colnames(x_numeric) <- character(0)
    }
  }
  
  # create lagged matrix for numeric predictors (if any)
  if (ncol(x_numeric) > 0) {
    x_lagged <- embed(x_numeric, n_lags)
    # build names for lagged numeric vars
    names_x <- NULL
    for (i in seq_len(n_lags)) {
      names_x <- c(names_x,
                   paste(colnames(x_numeric), "_lag_", horizon + i - 1, sep = ""))
    }
    colnames(x_lagged) <- names_x
  } else {
    # zero-column matrix with correct number of rows = T - n_lags + 1
    rows_embed <- nrow(x_aux) - n_lags + 1
    if (rows_embed <= 0) stop("Not enough rows to create embed matrix with given n_lags.")
    x_lagged <- matrix(nrow = rows_embed, ncol = 0)
    colnames(x_lagged) <- character(0)
  }
  
  # align dummy rows to embed rows: rows in embed correspond to original indices n_lags..T
  if (!is.null(dum_mat)) {
    aligned_dum <- dum_mat[n_lags:nrow(dum_mat), , drop = FALSE]
    colnames(aligned_dum) <- dum_names
  } else {
    aligned_dum <- NULL
  }
  
  # combine lagged numeric and (non-lagged) dummies
  if (!is.null(aligned_dum) && ncol(x_lagged) > 0) {
    x <- cbind(x_lagged, aligned_dum)
  } else if (!is.null(aligned_dum)) {
    x <- aligned_dum
  } else {
    x <- x_lagged
  }
  
  # splitting
  x_in <- x[-c((nrow(x) - horizon + 1):nrow(x)), , drop = FALSE]
  x_out <- x[nrow(x), , drop = FALSE]
  x_out <- t(as.vector(x_out))
  y_in <- tail(y, nrow(x_in))
  
  return(list(x_in = x_in, x_out = x_out, y_in = y_in))
}