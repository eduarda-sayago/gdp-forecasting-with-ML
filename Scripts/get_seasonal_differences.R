
library(tidyverse)
library(textdata)
library(lubridate)
library(sentometrics)

get_season_diffs <- function(dataset, freq = 12) {
  #' Teste de Diferenças sazonais com nsdiffs
  #'
  #' Esta função verifica a necessidade de realizar diferenças sazonais de um conjunto de dados em um `data.frame` utilizando o teste de raiz unitária sazonal nsdiffs do pacote forecast.
  #'
  #' @param dataset Um `data.frame` contendo as séries temporais a serem testadas.
  #' @param freq Frequência da série temporal (default = 12 para dados mensais).
  #' @return Uma lista com dois elementos:
  #' - results: Uma lista com as séries temporais diferenciadas sazonalmente.
  #' - nsdiffs: Um `data.frame` com o nome de cada série e o número de diferenças necessárias.
  #'
  #' @examples
  #' dataset <- data.frame(serie1 = rnorm(100), serie2 = rnorm(100), serie3 = rnorm(100))
  #' seasonality_results <- get_season_diffs(dataset, freq = 12)
  #'
  #' @export
  
  results <- list()
  support_df <- data.frame(Serie = character(), nsdiffs = integer())
  
  # Loop através de cada coluna do dataframe
  for (col_name in names(dataset)) {
    serie_vec <- dataset[[col_name]]
    
    # Transformar em objeto ts com frequência
    serie <- ts(serie_vec, frequency = freq)
    
    # Realizando o teste de sazonalidade
    seas_test <- forecast::nsdiffs(serie, alpha = 0.05, test = "seas")
    
    temp_df <- data.frame(Serie = col_name, nsdiffs = seas_test)
    support_df <- rbind(support_df, temp_df)
    
    # Iterando para realizar o número de diferenças necessárias
    if (seas_test > 0) {
      diff_serie <- diff(serie, lag = freq, differences = seas_test)
      results[[col_name]] <- diff_serie
    } else {
      results[[col_name]] <- serie
    }
  }
  
  return(list(results = results, nsdiffs = support_df))
}


## Teste de raiz unitária sazonal e aplicação de diferenciação sazonal

source("get_seasonal_differences.R")


date_col <- base_SA[[1]]  
dataset_economic <- base_SA[-c(1)]
dataset_economic <- get_season_diffs(dataset_economic, freq = 12)
seasonality_results <- dataset_economic$nsdiffs
dataset_economic <- do.call(cbind, dataset_economic$results) %>% as.data.frame()
dataset_economic <- cbind(date = date_col[seq_len(nrow(dataset_economic))], dataset_economic)