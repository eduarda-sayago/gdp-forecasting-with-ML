add_stars <- function(pvalues, alpha_levels = c(0.05, 0.01, 0.001)) {
  
  #' Adiciona Estrelas para Indicar Níveis de Significância
  #'
  #' Esta função atribui estrelas (*) aos valores-p com base em níveis de significância especificados.
  #'
  #' @param pvalues Um vetor de valores-p.
  #' @param alpha_levels Um vetor de níveis de significância (padrão: 0.05, 0.01, 0.001).
  #'
  #' @return Um vetor de caracteres com estrelas correspondentes aos valores-p.
  #'
  #' @examples
  #' stars <- add_stars(c(0.02, 0.001, 0.07))
  #'
  
  stars <- character(length(pvalues))
  
  for (i in seq_along(alpha_levels)) {
    stars[pvalues <= alpha_levels[i]] <- paste(rep("*", i), collapse = "")
  }
  
  return(stars)
}

#QUARTERLY
compute_dm = function(model_names, model_dataframes, horizons, orig_data){
  
  #' Computa o Teste de Diebold-Mariano para Modelos de Previsão
  #'
  #' Esta função calcula o teste de Diebold-Mariano para diferentes modelos de previsão e horizontes de previsão.
  #'
  #' @return Uma lista contendo matrizes de valores-p formatadas com estrelas para três conjuntos de modelos:
  #'   - pvalues_tb: valores-p para modelos baseados em texto.
  #'   - pvalues_eco: valores-p para modelos econômicos.
  #'   - pvalues_eco_tb: valores-p para modelos que combinam dados econômicos e baseados em texto.
  #'
  #' @examples
  #' dm_results <- compute_dm()
  
  pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
  results <- list()
  
  results_tb = results
  pvalues_tb = pvalues
  
  for (m in seq_along(model_names)) {
    model_name <- model_names[m]
    model_df <- model_dataframes[[m]]
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      x <- data.frame(benchmarkp$forecasts)[[i]] #the only thing that changes from quart to month
      y <- data.frame(model_df$forecasts)[[i]]
      
      #residual = y - ŷ
      dm = dm.test(
        e1 = orig_data - y,
        e2 = orig_data - x,
        h = h,
        alternative = 'two.sided',
        varestimator = 'bartlett'
      )
      
      pvalues[m, i] <- dm$p.value
      results[[paste0(model_name, '- horizon ', h)]] <- dm
    }
  }
  
  rownames(pvalues) <- model_names
  colnames(pvalues) <- horizons
  pvalues_eco_stars <- add_stars(pvalues)
  pvalues <- matrix(paste0(format(pvalues, nsmall = 10), pvalues_eco_stars), nrow = nrow(pvalues), dimnames = dimnames(pvalues))
  pvalues
  
  list = list(
    pvalues = pvalues)
  
  return(list)
  
}

#MONTHLY
compute_dm1 = function(model_names, model_dataframes, horizons, orig_data){
  
  #' Computa o Teste de Diebold-Mariano para Modelos de Previsão
  #'
  #' Esta função calcula o teste de Diebold-Mariano para diferentes modelos de previsão e horizontes de previsão.
  #'
  #' @return Uma lista contendo matrizes de valores-p formatadas com estrelas para três conjuntos de modelos:
  #'   - pvalues_tb: valores-p para modelos baseados em texto.
  #'   - pvalues_eco: valores-p para modelos econômicos.
  #'   - pvalues_eco_tb: valores-p para modelos que combinam dados econômicos e baseados em texto.
  #'
  #' @examples
  #' dm_results <- compute_dm()
  
  pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
  results <- list()
  
  results_tb = results
  pvalues_tb = pvalues
  
  for (m in seq_along(model_names)) {
    model_name <- model_names[m]
    model_df <- model_dataframes[[m]]
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      x <- data.frame(benchmark$forecasts)[[i]] #the only thing that changes from quart to month
      y <- data.frame(model_df$forecasts)[[i]]
      
      #residual = y - ŷ
      dm = dm.test(
        e1 = orig_data - y,
        e2 = orig_data - x,
        h = h,
        alternative = 'two.sided',
        varestimator = 'bartlett'
      )
      
      pvalues[m, i] <- dm$p.value
      results[[paste0(model_name, '- horizon ', h)]] <- dm
    }
  }
  
  rownames(pvalues) <- model_names
  colnames(pvalues) <- horizons
  pvalues_eco_stars <- add_stars(pvalues)
  pvalues <- matrix(paste0(format(pvalues, nsmall = 10), pvalues_eco_stars), nrow = nrow(pvalues), dimnames = dimnames(pvalues))
  pvalues
  
  list = list(
    pvalues = pvalues)
  
  return(list)
  
}


#' #' A more general version that would include benchmark as an input. not tested yet.
#' compute_dm = function(model_names, model_dfs, benchmark_df, horizons, orig_data){
#'   
#'   #' Computa o Teste de Diebold-Mariano para Modelos de Previsão
#'   #'
#'   #' Esta função calcula o teste de Diebold-Mariano para diferentes modelos de previsão e horizontes de previsão.
#'   #'
#'   #' @param model_names -- A vector of strings containing the name of each model.
#'   #' @param model_dfs -- A list with the output lists of the models to be tested. The output_model list must contain forecasts.
#'   #' @param benchmark_df -- The benchmark output list. Must contain forecasts.
#'   #' @param horizons -- A vector of integers with the horizons that were estimated in the models.
#'   #' @param orig_data -- The target variable original dataset, cut to match the forecast length.
#'   #' 
#'   #' @return Uma lista contendo matrizes de valores-p formatadas com estrelas para três conjuntos de modelos:
#'   #'   - pvalues_tb: valores-p para modelos baseados em texto.
#'   #'   - pvalues_eco: valores-p para modelos econômicos.
#'   #'   - pvalues_eco_tb: valores-p para modelos que combinam dados econômicos e baseados em texto.
#'   #'
#'   #' @examples
#'   #' dm_results <- compute_dm(c("model1", "model2"), list(out_model1, out_model2), out_benchmark, c(1,3), df$target[70:100])
#'   
#'   model_dataframes <- model_dfs 
#'   benchmark <- benchmark_df
#'   
#'   # Check that orig_data length matches each model's forecast length
#'   for (m in seq_along(model_names)) {
#'     model_df <- model_dataframes[[m]]
#'     
#'     for (i in seq_along(horizons)) {
#'       if (length(orig_data) != length(model_df$forecasts[[i]])) {
#'         stop(paste0("Length of original data (", length(orig_data), 
#'                     ") does not match length of forecasts for model '", 
#'                     model_names[m], "' at horizon ", horizons[i], "."))
#'       }
#'     }
#'   }
#'   
#'   pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
#'   results <- list()
#'   
#'   results_tb = results
#'   pvalues_tb = pvalues
#'   
#'   for (m in seq_along(model_names)) {
#'     model_name <- model_names[m]
#'     model_df <- model_dataframes[[m]]
#'     
#'     for (i in seq_along(horizons)) {
#'       h <- horizons[i]
#'       
#'       x <- data.frame(benchmark$forecasts)[[i]]
#'       y <- data.frame(model_df$forecasts)[[i]]
#'       
#'       #residual = y - ŷ
#'       dm = dm.test(
#'         e1 = orig_data - y,
#'         e2 = orig_data - x,
#'         h = h,
#'         alternative = 'two.sided',
#'         varestimator = 'bartlett'
#'       )
#'       
#'       pvalues[m, i] <- dm$p.value
#'       results[[paste0(model_name, '- horizon ', h)]] <- dm
#'     }
#'   }
#'   
#'   rownames(pvalues) <- model_names
#'   colnames(pvalues) <- horizons
#'   pvalues_eco_stars <- add_stars(pvalues)
#'   pvalues <- matrix(paste0(format(pvalues, nsmall = 10), pvalues_eco_stars), nrow = nrow(pvalues), dimnames = dimnames(pvalues))
#'   pvalues
#'   
#'   list = list(
#'     pvalues = pvalues)
#'   
#'   return(list)
#'   
#' }