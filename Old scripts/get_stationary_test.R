
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)

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