f_csfe <- function(x, y_bench, y_real) {
  
  #' Calcula o Cumulative Squared Forecast Error (CSFE)
  #'
  #' Esta função calcula o erro quadrático acumulado da previsão em relação a um benchmark e 
  #' os valores reais. O resultado permite avaliar a precisão do modelo em comparação com o benchmark.
  #'
  #' @param x Um vetor numérico contendo as previsões do modelo.
  #' @param y_bench Um vetor numérico contendo as previsões do benchmark.
  #' @param y_real Um vetor numérico contendo os valores reais observados.
  #' @return Um vetor numérico que contém o erro quadrático acumulado para cada ponto no tempo.
  #'
  #' @examples
  #' f_csfe(c(1, 2, 3), c(1.5, 2.5, 3.5), c(1, 2, 3)) # Retorna o erro quadrático acumulado
  #'
  
  error_bench <- (y_bench - y_real)^2
  error_x <- (x - y_real)^2
  result <- cumsum(error_bench - error_x)
  return(result)
}

csfe = function(model, benchmark, y_real){
  
  #' Calcula CSFE para Diferentes Horizontes
  #'
  #' Esta função calcula o Cumulative Squared Forecast Error (CSFE) para diferentes horizontes de previsão
  #' a partir das previsões de um modelo e de um benchmark.
  #'
  #' @param model Um objeto contendo as previsões do modelo, com colunas representando diferentes horizontes.
  #' @param benchmark Um objeto contendo as previsões do benchmark, com colunas correspondendo aos mesmos horizontes.
  #' @param y_real Um vetor numérico contendo os valores reais observados.
  #' @return Uma matriz com os erros quadráticos acumulados para cada horizonte de previsão.
  #'
  #' @examples
  #' csfe_results <- csfe(model, benchmark, y_real)
  #' print(csfe_results)
  #'
  
  h1 = f_csfe(model$forecast[,1], benchmark$forecasts[,1], y_real = y_real)
  h12 = f_csfe(model$forecast[,2], benchmark$forecasts[,2], y_real = y_real)
  
  cbind(h1, h12)
  
}

csfe1 = function(model, benchmark, y_real){
  
  #' Calcula CSFE para Diferentes Horizontes
  #'
  #' Esta função calcula o Cumulative Squared Forecast Error (CSFE) para diferentes horizontes de previsão
  #' a partir das previsões de um modelo e de um benchmark.
  #'
  #' @param model Um objeto contendo as previsões do modelo, com colunas representando diferentes horizontes.
  #' @param benchmark Um objeto contendo as previsões do benchmark, com colunas correspondendo aos mesmos horizontes.
  #' @param y_real Um vetor numérico contendo os valores reais observados.
  #' @return Uma matriz com os erros quadráticos acumulados para cada horizonte de previsão.
  #'
  #' @examples
  #' csfe_results <- csfe(model, benchmark, y_real)
  #' print(csfe_results)
  #'
  
  h1 = f_csfe(model$forecast[,1], benchmark$forecasts[,1], y_real = y_real)
  h4 = f_csfe(model$forecast[,2], benchmark$forecasts[,2], y_real = y_real)
  
  cbind(h1, h4)
  
}
