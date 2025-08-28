aggregate_to_quarterly <- function(results, info){
  #' Agrega séries mensais transformadas para trimestrais
  #' 
  #' @param results Lista com séries transformadas (saída da get_stationary_SW)
  #' @param info Data.frame com colunas: Serie e type_sw
  #' @return Lista com séries trimestrais
  
  quarterly <- list()
  
  if ("date" %in% names(results)) {
    quarterly[["date"]] <- results[["date"]]
  }
  
  for (i in seq_len(nrow(info))){
    serie_name <- info$Serie[i]
    type_sw    <- info$type_sw[i]
    
    if (type_sw == 4) next
    serie <- results[[serie_name]]
    
    # garantir que não tem NA no começo
    serie <- serie[!is.na(serie)]
    
    if (type_sw == 1){
      # Diferença simples -> soma
      q_serie <- zoo::rollapply(serie, width = 3, by = 3, FUN = sum, align = "right")
      
    } else if (type_sw %in% c(2,3)){
      # Diferença do log -> multiplicação de fatores
      # fator mensal ≈ (1 + g), mas usar exp(g) é mais exato
      q_serie <- zoo::rollapply(serie, width = 3, by = 3,
                                FUN = function(x) prod(exp(x)) - 1,
                                align = "right")
      
    } else {
      warning(paste("Tipo desconhecido para", serie_name, "- pulando."))
      next
    }
    
    quarterly[[serie_name]] <- q_serie
  }
  
  return(quarterly)
}
