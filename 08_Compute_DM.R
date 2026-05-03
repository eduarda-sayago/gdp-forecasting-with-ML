add_stars <- function(pvalues, alpha_levels = c(0.05, 0.01, 0.001)) {
  
  #' Add Stars to Indicate Significance Levels
  #'
  #' This function assigns stars (*) to p-values based on specified significance levels.
  #'
  #' @param pvalues A vector of p-values.
  #' @param alpha_levels A vector of significance levels (default: 0.05, 0.01, 0.001).
  #'
  #' @return A character vector with stars corresponding to each p-value.
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
  
  #' Compute the Diebold-Mariano Test for Forecasting Models
  #'
  #' This function computes the Diebold-Mariano test for different forecasting models and forecast horizons.
  #'
  #' @return A list containing p-value matrices formatted with stars for three sets of models:
  #'   - pvalues_tb: p-values for text-based models.
  #'   - pvalues_eco: p-values for economic models.
  #'   - pvalues_eco_tb: p-values for models combining economic and text-based data.
  #'
  #' @examples
  #' dm_results <- compute_dm()
  
  library(forecast)
  
  pvalues <- matrix(nrow = length(model_names), ncol = length(horizons))
  results <- list()
  
  results_tb = results
  pvalues_tb = pvalues
  
  for (m in seq_along(model_names)) {
    model_name <- model_names[m]
    model_df <- model_dataframes[[m]]
    
    for (i in seq_along(horizons)) {
      h <- horizons[i]
      
      x <- data.frame(benchmarkq$forecasts)[[i]] #the only thing that changes from quart to month
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
  
  #' Compute the Diebold-Mariano Test for Forecasting Models
  #'
  #' This function computes the Diebold-Mariano test for different forecasting models and forecast horizons.
  #'
  #' @return A list containing p-value matrices formatted with stars for three sets of models:
  #'   - pvalues_tb: p-values for text-based models.
  #'   - pvalues_eco: p-values for economic models.
  #'   - pvalues_eco_tb: p-values for models combining economic and text-based data.
  #'
  #' @examples
  #' dm_results <- compute_dm()
  
  library(forecast)
  
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
#'   #' Compute the Diebold-Mariano Test for Forecasting Models
#'   #'
#'   #' This function computes the Diebold-Mariano test for different forecasting models and forecast horizons.
#'   #'
#'   #' @param model_names -- A vector of strings containing the name of each model.
#'   #' @param model_dfs -- A list with the output lists of the models to be tested. The output_model list must contain forecasts.
#'   #' @param benchmark_df -- The benchmark output list. Must contain forecasts.
#'   #' @param horizons -- A vector of integers with the horizons that were estimated in the models.
#'   #' @param orig_data -- The target variable original dataset, cut to match the forecast length.
#'   #'
#'   #' @return A list containing p-value matrices formatted with stars for three sets of models:
#'   #'   - pvalues_tb: p-values for text-based models.
#'   #'   - pvalues_eco: p-values for economic models.
#'   #'   - pvalues_eco_tb: p-values for models combining economic and text-based data.
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