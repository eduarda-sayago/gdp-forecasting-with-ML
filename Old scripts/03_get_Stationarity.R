# ================================================
# ----------------Stationarity--------------------
# ================================================
  # Registers logic: new column with ndiffs
  
  # Logic for first row: if diff was made, means that the series length is reduced in 1 -> add one NA on top
  # Logic for rows: num rows removed from beginning of df == highest diff (usually 1)

# idea: if type = 2 there was no transf needed. and if type = 1 or 3 and diff > 0, then discrete growth, else diff

# One function to determine how much differencing is needed. at the end transform that if type == 4, diffs = 0.
# One function to take difference number and apply either diff (types 0, 2) or disc growth (types 1, 3)

adf_test <- function(df, type_df) {
  #' Determines number of differences needed using ADF
  #'
  #' @param df A data.frame containing the time series (columns named).
  #' @param type_df A data.frame with columns "variable" (names) and "type" (integers 0:4).
  #' 
  #' @return type_df with an added column ndiffs (integers).

  vars <- type_df$variable
  types <- type_df$type
  
  nds <- sapply(seq_along(vars), function(i) {
    if (types[i] == 4) {
      0L
    } else {
      forecast::ndiffs(df[[ vars[i] ]], alpha = 0.05, test = 'adf', type = 'level')
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  type_df$ndiffs <- as.integer(nds)
  type_df
}

get_stationarity <- function(df, type_df) {
  #' Apply ndiffs transformations (diff or discrete growth) exactly ndiffs times,
  #' pad each transformed series with leading NAs to original length,
  #' then drop the first max(ndiffs) rows so the returned df has no NAs at top.
  #'
  #' @param df A data.frame with the original series (columns named).
  #' @param type_df A data.frame with columns variable, type, ndiffs.
  #' @return A list with:
  #'   $df    -> transformed data.frame with first max(ndiffs) rows removed
  #'   $type_df -> the input type_df with an added applied_diffs column
    
    disc_growth <- function(X) {
      X[-1] / X[-length(X)] - 1
    }
    
    n_orig <- nrow(df)
    df_out <- df
    
    type_df <- adf_test(df, type_df)
    
    for (i in seq_len(nrow(type_df))) {
      var <- type_df$variable[i]
      tp  <- type_df$type[i]
      nd  <- as.integer(type_df$ndiffs[i])
      
      x <- as.numeric(df[[var]])
      
      if (nd > 0) {
        for (k in seq_len(nd)) {
          if (tp %in% c(0, 2)) {
            x <- diff(x)
          } else if (tp %in% c(1, 3)) {
            x <- disc_growth(x)
          }
        }
        # pad only if transformed
        x <- c(rep(NA_real_, nd), x)
      }
      
      df_out[[var]] <- x
    }
    
    type_df$applied_diffs <- as.integer(type_df$ndiffs)
    
    # trim top rows if any differencing happened
    max_nd <- max(type_df$ndiffs)
    if (max_nd > 0) {
      df_out <- df_out[(max_nd + 1):n_orig, , drop = FALSE]
      rownames(df_out) <- NULL
    }
    
    list(df = df_out, type_df = type_df)
  }

# ================================================
# -----------------Seasonality--------------------
# ================================================

seas_test <- function(df, type_df, freq) {
  #' Determine number of seasonal differences needed using nsdiffs
  #'
  #' @param df A data.frame containing the time series (columns named).
  #' @param type_df A data.frame with columns "variable" (names) and "type" (integers 0:4).
  #' @param freq Integer: frequency of the series (e.g., 12 for monthly, 4 for quarterly)
  #' @return type_df with an added column sdiffs (integers).
  
  vars <- type_df$variable
  types <- type_df$type
  
  sds <- sapply(seq_along(vars), function(i) {
    if (types[i] == 4) {
      0L
    } else {
      ts_var <- ts(df[[vars[i]]], frequency = freq)
      forecast::nsdiffs(ts_var, test = "ocsb", alpha = 0.05)
    }
  }, simplify = TRUE, USE.NAMES = FALSE)
  
  type_df$sdiffs <- as.integer(sds)
  type_df
}

get_seas_stationarity <- function(df, type_df, freq) {
  #' Apply seasonal differences (or discrete growth) exactly sdiffs times
  #' pad leading NAs, trim top rows by max sdiffs
  #'
  #' @param df A data.frame with the original series (columns named)
  #' @param type_df A data.frame with columns variable, type, sdiffs
  #' @param freq Integer: frequency of the series (e.g., 12 for monthly, 4 for quarterly)
  #' @return A list with:
  #'   $df -> transformed data.frame with first max(sdiffs) rows removed
  #'   $type_df -> input type_df with added applied_sdiffs column
  
  cresc_discreto <- function(X) {
    X[-1] / X[-length(X)] - 1
  }
  
  n_orig <- nrow(df)
  df_out <- df
  
  type_df <- seas_test(df, type_df, freq)
  
  for (i in seq_len(nrow(type_df))) {
    var <- type_df$variable[i]
    tp  <- type_df$type[i]
    sd  <- as.integer(type_df$sdiffs[i])
    
    x <- as.numeric(df[[var]])
    
    if (sd > 0) {
      for (k in seq_len(sd)) {
        if (tp %in% c(0, 2)) {
          x <- diff(x, lag = freq)
        } else if (tp %in% c(1, 3)) {
          # discrete seasonal growth (lagged by freq)
          x <- (x[(freq+1):length(x)] / x[1:(length(x)-freq)]) - 1
        }
      }
      x <- c(rep(NA_real_, sd * freq), x)  # pad with freq*sd leading NAs
    }
    
    df_out[[var]] <- x
  }
  
  type_df$applied_sdiffs <- as.integer(type_df$sdiffs)
  
  max_sd <- max(type_df$sdiffs)
  if (max_sd > 0) {
    df_out <- df_out[(max_sd * freq + 1):n_orig, , drop = FALSE]
    rownames(df_out) <- NULL
  }
  
  list(df = df_out, type_df = type_df)
}

