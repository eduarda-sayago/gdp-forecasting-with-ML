get_stationary_SW <- function(dataset, metadata, shift = 1, max_lag = 1){
  
  #' Transformação de séries para estacionariedade com base em Stock & Watson (2012)
  #' 
  #' Transformações:
  #' 1- Taxas de juros -> primeira diferença
  #' 2- Atividade econômica:
  #'    - positiva -> primeira diferença do log
  #'    - com zeros ou negativos -> primeira diferença do log(serie + shift)
  #' 3- Preços -> segunda diferença do log
  #'
  #' Testa estacionariedade com ndiffs do forecast::ndiffs
  #' Obs: não lida com coluna data. remover antes de usar
  #'
  #' @param dataset Data.frame com séries temporais
  #' @param metadata Data.frame com colunas: "Variables" e "type_sw" (1 = juros, 2 = atividade, 3 = preços)
  #' @param shift Constante positiva para log-shift em séries com zero/negativos (default = 1)
  #' @return Lista com results (séries transformadas) e info (tipo + ndiffs)
  
  results <- list()
  info <- data.frame(Serie = character(), type_sw = integer())
  
  for (i in seq_len(nrow(metadata))){
    col_name <- metadata$Variables[i]
    type_sw  <- metadata$type_sw[i]
    
    if (!col_name %in% names(dataset)){
      warning(paste(col_name, "não encontrada no dataset"))
      next
    }
    
    serie <- dataset[[col_name]]
    
    # --- Transformação segundo Stock & Watson ---
    if (type_sw == 4){
      trans_serie <- as.Date(serie[-seq_len(max_lag)])
        
    } else if (type_sw == 1){ 
      # Taxas de juros -> primeira diferença
      trans_serie <- diff(serie)
      
    } else if (type_sw == 2){ 
      # Atividade real -> crescimento
      if (all(serie > 0)){
        # positiva
        trans_serie <- diff(log(serie))
      } else {
        # contém zeros ou negativos -> log-shift
        trans_serie <- diff(log(serie + shift))
      }
      
    } else if (type_sw == 3){ 
      # Preços -> segunda diferença do log
      if (all(serie > 0)){
        trans_serie <- diff(log(serie), differences = max_lag)
      } else {
        # log-shift caso haja zero/negativos
        trans_serie <- diff(log(serie + shift), differences = max_lag)
      }
      
    }
    
    # Guardar resultados
    results[[col_name]] <- trans_serie
    info <- rbind(info, data.frame(Serie = col_name, type_sw = type_sw))
  }
  
  return(list(results = results, info = info))
}