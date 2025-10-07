# ================================================
# --------------Log Transformations---------------
# ================================================
# * Based on https://github.com/pedroskorin/L2_Boosting/blob/master/l2_boosting/history/data_adjustment.R

# tipo 4 = date col
# tipo 0 = só positivo → log(X)
# tipo 1 = tem zeros → log(X + 2)
# tipo 2 = tem negativos → nada
# tipo 3 = tem negativos e zeros → faz X + 1

get_logs <- function(df) {
  # df: data.frame com as séries (cada coluna é uma variável)
  # devolve: lista com $results (data.frame) e $type_df (data.frame: variable, tipo)
  
  n <- ncol(df)
  results <- df # inicialmente copia (vamos substituir só as colunas transformáveis)
  types <- integer(n)
  names(types) <- colnames(df)
  
  for (i in seq_len(n)) {
    x <- df[[i]]
    colname <- colnames(df)[i]
    
    # 4 = coluna de data (mantemos sem transformação)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      types[i] <- 4
      results[[i]] <- x
      next
    }
    
    has_neg <- any(x < 0, na.rm = TRUE)
    has_zero <- any(x == 0, na.rm = TRUE)
    
    # Mapping:
    # type 0 = positive → log(X)
    # type 1 = zeroes → log(X + 2)
    # type 2 = negatives → nothing
    # type 3 = zeroes and neg → X + 1
    
    if (!has_neg && !has_zero) {
      types[i] <- 0L
      results[[i]] <- log(x)
    } else if (!has_neg && has_zero) {
      types[i] <- 1L
      results[[i]] <- log(x + 2)
    } else if (has_neg && !has_zero) {
      types[i] <- 2L
      results[[i]] <- x
    } else { # has_neg && has_zero
      types[i] <- 3L
      results[[i]] <- x + 1
    }
  }
  
  type_df <- data.frame(
    variable = colnames(df),
    type = as.integer(types),
    stringsAsFactors = FALSE
  )
  
  return(list(results = as.data.frame(results), type_df = type_df))
}
