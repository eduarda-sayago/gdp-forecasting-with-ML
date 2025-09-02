library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)

# Note: this script contains two functions: 
# get_stationary_SW for the transformations and get_stationarity for confirmation with ADF test

get_stationary_SW <- function(dataset, metadata, shift = 1, max_lag = 1){
  
  #' Transformation of Time Series for Stationarity Based on Stock & Watson (2012)
  #'
  #' The function applies variable-specific transformations to induce stationarity, 
  #' following the rules in Stock & Watson (2012). 
  #' 
  #' Originally, the transformations are:  
  #' Interest rates -> first differences
  #' Real activity variables -> first differences of logarithms (growth rates)
  #' Price series -> second differences of logarithms (changes in rates of inflation).
  #' 
  #' Transformations had to be adapted for the occurrence of zeroes and negatives:
  #' - type 1 (Interest rates): first difference
  #' - type 2 (Economic activity):
  #'    * if all values > 0 → first difference of the log
  #'    * otherwise → first difference of log(series + shift)
  #' - type 3 (Prices): second difference of the log
  #'    * if all values > 0 → `max_lag)` differences of the log
  #'    * otherwise → `max_lag)` differences of the log with shift
  #' - type 4 (Date): returns the original dates excluding the first rows (`max_lag`)
  #'
  #' @param dataset A `data.frame` with the time series.
  #' @param metadata A `data.frame` with columns: "Variables" and "type_sw" 
  #'   (1 = interest rates, 2 = activity, 3 = prices, 4 = date).
  #' @param shift A positive constant added before log-transformation when the 
  #'   series contains zeros or negatives, in type 2 or 3 (default = 1).
  #' @param max_lag Maximum number of differences to use in type 3, (default = 1).
  #'
  #' @return A list with:
  #' - `results`: the transformed series
  #' - `info`: a `data.frame` with the variable name and its transformation type
  
  results <- list()
  info <- data.frame(Serie = character(), type_sw = integer())
  
  for (i in seq_len(nrow(metadata))){
    col_name <- metadata$Variables[i]
    type_sw  <- metadata$type_sw[i]
    
    if (!col_name %in% names(dataset)){
      warning(paste(col_name, "not found in dataset"))
      next
    }
    
    serie <- dataset[[col_name]]
    
    # --- Transformation based on Stock & Watson ---
    if (type_sw == 4){
      trans_serie <- as.Date(serie[-seq_len(max_lag)])
        
    } else if (type_sw == 1){ 
      # Interest rates -> first difference
      trans_serie <- diff(serie)
      
    } else if (type_sw == 2){ 
      # Real activity -> growth
      if (all(serie > 0)){
        # positive
        trans_serie <- diff(log(serie))
      } else {
        # negative or zeroes -> log-shift
        trans_serie <- diff(log(serie + shift))
      }
      
    } else if (type_sw == 3){ 
      # Prices -> second difference of log
      if (all(serie > 0)){
        # positive
        trans_serie <- diff(log(serie), differences = max_lag)
      } else {
        # negative or zeroes -> log-shift
        trans_serie <- diff(log(serie + shift), differences = max_lag)
      }
      
    }
    
    # Output
    results[[col_name]] <- trans_serie
    info <- rbind(info, data.frame(Serie = col_name, type_sw = type_sw))
  }
  
  return(list(results = results, info = info))
}

get_stationarity = function(dataset){
  
  #' Stationarity Test with the ADF Test
  #'
  #' This function checks the stationarity of a dataset in a `data.frame` using the Augmented Dickey-Fuller (ADF) Test.
  #'
  #' @param dataset -- A `data.frame` containing the time series to be tested.
  #' @return -- A list with two elements:
  #' - results: A list with the stationary time series, where each series is adjusted according to the number of differences required.
  #' - ndiffs: A `data.frame` with the name of each series and the number of differences needed to achieve stationarity.
  #'
  #' @examples
  #' dataset <- data.frame(serie1 = rnorm(100), serie2 = rnorm(100), serie3 = rnorm(100))
  #' stationarity_results <- get_stationarity(dataset)
  #'
  #' @export
  
  #dataset = dataset[-c(1)]
  
  results = list()
  suport_df = data.frame(Serie = character(), ndiffs = integer())
  
  # Loop através de cada coluna do dataframe
  for (col_name  in names(dataset)){
    serie = dataset[[col_name]]
    
    #Realizando o teste ADF (Questionar ao Hudson)
    adf_test = forecast::ndiffs(serie, alpha = 0.05, test = 'adf', type = 'level')
    
    temp_df = data.frame(Serie = col_name, ndiffs = adf_test)
    suport_df = rbind(suport_df, temp_df)
    
    #Iterando para realizar o número de diferenças necessárias
    if (adf_test !=0){
      diff_serie = serie
      
      for (i in 1:adf_test){
        diff_serie = diff(diff_serie)
      }
      
      results[[col_name]] = diff_serie
      
    } else {
      
      results[[col_name]] = serie
      
    }
  }
  
  return(list(results = results, ndiffs = suport_df))
  
}