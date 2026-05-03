get_log_transform <- function(dataset) {
  #' Transformação inicial dos dados - Log
  #' 
  #' Detecta o tipo de cada variável e aplica transformação inicial, seguindo a regra:
  #'
  #' 0: estritamente positiva → log
  #' 1: contém negativos → não transforma
  #' 2: contém zeros → log(X + 2)
  #' 3: zeros e negativos → shift +1
  #' 4: coluna de data → não transforma
  #'
  #' Obs: Leva em consideração a presença de coluna data.
  #' 
  #' @param dataset Um data.frame contendo as séries temporais.
  #' @return Uma lista com:
  #' - results: séries transformadas
  #' - tipo_df: data.frame com nome da variável e tipo detectado
  #' 
  #' @examples
  #' dataset <- data.frame(Data = 1:10, serie1 = rnorm(10), serie2 = rpois(10,5))
  #' result <- get_log_transform(dataset)
  
  results <- list()
  tipo <- c()
  
  # Loop por cada coluna
  for (i in seq_along(dataset)) {
    X <- dataset[[i]]
    
    # Detecta tipo
    if (i == 1) {
      # Presumindo que a primeira coluna é data
      tipo_i <- 4
      X_trans <- X
    } else if (any(X < 0, na.rm = TRUE)) {
      if (any(X == 0, na.rm = TRUE)) {
        tipo_i <- 3
        X_trans <- X + 1
      } else {
        tipo_i <- 1
        X_trans <- X
      }
    } else if (any(X == 0, na.rm = TRUE)) {
      tipo_i <- 2
      X_trans <- log(X + 2)
    } else {
      tipo_i <- 0
      X_trans <- log(X)
    }
    
    tipo <- c(tipo, tipo_i)
    results[[names(dataset)[i]]] <- X_trans
  }
  
  tipo_df <- data.frame(
    Variable = names(dataset),
    Tipo = tipo
  )
  
  return(list(results = results, tipo_df = tipo_df))
}

dataset <- base_SA
dataset <- get_log_transform(dataset)
log_results <- dataset$tipo_df
dataset <- do.call(cbind, dataset$results) %>% as.data.frame()
