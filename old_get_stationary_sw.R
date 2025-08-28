
get_stationary_SW <- function(dataset, metadata){
  
  #' Transformação de séries para estacionariedade com base em Stock & Watson (2012)
  #' 
  #' Esta função verifica a transformação a ser aplicada em um conjunto de dados em `data.frame` utilizando um dataframe com o nome das séries  baseada no paper de Stock e Watson (2012).
  #' 1- Interest rates are transformed into first differences.
  #' 2-	Real activity variables (like GDP) are converted to growth rates (first differences of logarithms).	
  #' 3-	Price series are converted into changes in the inflation rate (second differences of logarithms).
  #' 
  #' Finalmente, a estacionariedade é testada com Augmented Dickey-Fuller (ADF) test.
  #'  Obs: não lida com a coluna data. Remover antes de usar.
  #'
  #' @param dataset Um `data.frame` contendo as séries temporais originais.
  #' @param metadata Um `data.frame` (ou csv lido com read.csv) contendo:
  #'   - "Serie": nomes das variáveis no dataset
  #'   - "type_sw": tipo da transformação (1 = taxa de juros, 2 = atividade real, 3 = preços)
  #' @return Uma lista com dois elementos:
  #' - results: lista com as séries transformadas.
  #' - info: data.frame com Série, type_sw e ndiffs do ADF.
  #'
  #' @examples
  #' dataset <- data.frame(serie1 = rnorm(100), serie2 = rnorm(100), serie3 = rnorm(100))
  #' metadata <- data.frame(Serie = c("serie1","serie2","serie3"), type_sw = c(1,2,3))
  #' res <- get_stationary_SW(dataset, metadata)
  #'
  #' @export
  
  results <- list()
  info <- data.frame(Variables = character(), type_sw = integer(), ndiffs = integer())
  
  for (i in seq_len(nrow(metadata))){
    col_name <- metadata$Variables[i]
    type_sw  <- metadata$type_sw[i]
    
    if (!col_name %in% names(dataset)){
      warning(paste("Variável", col_name, "não encontrada no dataset. Pulando..."))
      next
    }
    
    serie <- dataset[[col_name]]
    
    # --- Transformação segundo Stock & Watson ---
    if (type_sw == 1){ 
      # Taxas de juros -> primeira diferença
      trans_serie <- diff(serie)
    } else if (type_sw == 2){ 
      # Atividade real -> crescimento (primeira diferença do log)
      trans_serie <- diff(log(serie))
    } else if (type_sw == 3){ 
      # Preços -> inflação (segunda diferença do log)
      trans_serie <- diff(log(serie), differences = 1)
    } else {
      warning(paste("Tipo desconhecido para", col_name, "- retornando série sem transformação."))
      trans_serie <- serie
    }
    
    # --- Teste ADF ---
    adf_test <- forecast::ndiffs(trans_serie, alpha = 0.05, test = 'adf', type = 'level')
    
    # Guardar resultados
    results[[col_name]] <- trans_serie
    
    temp_df <- data.frame(Serie = col_name, type_sw = type_sw, ndiffs = adf_test)
    info <- rbind(info, temp_df)
  }
  
  return(list(results = results, info = info))
}



metadata_sw <- as.data.frame(read.csv2("Stock_watson.csv"))

base_NSA <- base_NSA[-((nrow(base_NSA)-1):nrow(base_NSA)), ]
colSums(is.na(base_NSA))

date_col <- base_NSA[[1]]  
mdataset_sw <- base_NSA[-c(1)]
dataset <- get_stationary_SW(mdataset_sw, metadata_sw)
series_results <- dataset$info
dataset <- do.call(cbind, dataset$results) %>% as.data.frame()
dataset <- cbind(date = date_col[seq_len(nrow(dataset))], dataset)
