call_models = function(data, model_name, model_function, variable, type = "default"){
  
  #' Chamadas para Modelos de Previsão
  #'
  #' Esta função aplica um modelo de previsão a um conjunto de dados, utilizando uma janela deslizante para gerar previsões.
  #'
  #' @param data Um `data.frame` ou matriz contendo os dados a serem modelados.
  #' @param model_name O nome do modelo a ser utilizado (apenas um rótulo).
  #' @param model_function A função que implementa o modelo de previsão a ser aplicado.
  #' @param variable O nome da variável dependente que será prevista.
  #' @param type O tipo de dados. O padrão é "default".
  #' @return Uma lista contendo os seguintes elementos:
  #' - mae: O erro absoluto médio das previsões.
  #' - rmse: A raiz do erro quadrático médio das previsões.
  #' - forecasts: As previsões geradas pelo modelo para o horizonte definido.
  #'
  #' @examples
  #' df <- data.frame(time = 1:100, variable = rnorm(100))
  #' results <- call_models(data = df, model_name = "MyModel", model_function = my_model_function, variable = "variable")
  #'
  #' @export
  
  data = as.matrix(data)
  
  model_name <- model_name
  model_function <- model_function
  
  n <- nrow(data)
  b = round(n*0.3)
  y_out <- tail(data[, variable], b)
  
  model_list <- list()
  for_ind <- c(1, 4)
  
  for (i in for_ind) {
    model <- rolling_window2(
      fn = model_function,
      df = data,
      nwindow = n - b + 1,
      horizon = i,
      variable = variable,
      n_lags = 4
    )
    model_list[[i]] <- model
    cat(i, "\n")
  }
  
  forecasts <- Reduce(
    f = cbind,
    x = lapply(model_list, function(x) head(x$forecast, b))
  ) %>% as.matrix()
  
  for (i in for_ind) {
    filename <- paste0("forecast ",model_name,"-horizon-" , i, ".png")
    filepath <- file.path("Plots", filename)
    png(filepath, width = 1000, height = 600)
    
    col_idx <- match(i, for_ind)  # find which column corresponds to horizon i
    
    plot.ts(y_out, 
            main = paste0("Forecast with ", model_name, " (horizon = ", i, ")"),
            lwd = 2)
    lines(forecasts[, col_idx], col = 2, lwd = 2)
    dev.off()
  }
  
  rmse <- apply(forecasts, 2, f_rmse, y = y_out) %>% print()
  mae <- apply(forecasts, 2, f_mae, y = y_out) %>% print()
  mape <- apply(forecasts, 2, f_mape, y = y_out) %>% print()
  
  results = list(mae = mae, rmse = rmse, mape = mape, forecasts = forecasts)
  
  return(results)
}

call_models1 = function(data, model_name, model_function, variable, type = "default"){
  
  #' Chamadas para Modelos de Previsão - *MONTHLY VERSION*
  #'
  #' Esta função aplica um modelo de previsão a um conjunto de dados, utilizando uma janela deslizante para gerar previsões.
  #'
  #' @param data Um `data.frame` ou matriz contendo os dados a serem modelados.
  #' @param model_name O nome do modelo a ser utilizado (apenas um rótulo).
  #' @param model_function A função que implementa o modelo de previsão a ser aplicado.
  #' @param variable O nome da variável dependente que será prevista.
  #' @param type O tipo de dados. O padrão é "default".
  #' @return Uma lista contendo os seguintes elementos:
  #' - mae: O erro absoluto médio das previsões.
  #' - rmse: A raiz do erro quadrático médio das previsões.
  #' - forecasts: As previsões geradas pelo modelo para o horizonte definido.
  #'
  #' @examples
  #' df <- data.frame(time = 1:100, variable = rnorm(100))
  #' results <- call_models(data = df, model_name = "MyModel", model_function = my_model_function, variable = "variable")
  #'
  #' @export
  
  data = as.matrix(data)
  
  model_name <- model_name
  model_function <- model_function
  
  n <- nrow(data)
  b = round(n*0.3)
  y_out <- tail(data[, variable], b)
  
  model_list <- list()
  for_ind <- c(1, 12)
  
  for (i in for_ind) {
    model <- rolling_window2(
      fn = model_function,
      df = data,
      nwindow = n - b + 1,
      horizon = i,
      variable = variable,
      n_lags = 4
    )
    model_list[[i]] <- model
    cat(i, "\n")
  }
  
  forecasts <- Reduce(
    f = cbind,
    x = lapply(model_list, function(x) head(x$forecast, b))
  ) %>% as.matrix()
  
  for (i in for_ind) {
    filename <- paste0("forecast ",model_name,"-horizon-" , i, ".png")
    filepath <- file.path("Plots", filename)
    png(filepath, width = 1000, height = 600)
    
    col_idx <- match(i, for_ind)  # find which column corresponds to horizon i
    
    plot.ts(y_out, 
            main = paste0("Forecast with ", model_name, " (horizon = ", i, ")"),
            lwd = 2)
    lines(forecasts[, col_idx], col = 2, lwd = 2)
    dev.off()
  }
  
  rmse <- apply(forecasts, 2, f_rmse, y = y_out) %>% print()
  mae <- apply(forecasts, 2, f_mae, y = y_out) %>% print()
  mape <- apply(forecasts, 2, f_mape, y = y_out) %>% print()
  
  results = list(mae = mae, rmse = rmse, mape = mape, forecasts = forecasts)
  
  return(results)
}

#' call_models_old = function(data, model_name, model_function, variable, type = "default"){
#'   
#'   #' Chamadas para Modelos de Previsão
#'   #'
#'   #' Esta função aplica um modelo de previsão a um conjunto de dados, utilizando uma janela deslizante para gerar previsões.
#'   #'
#'   #' @param data Um `data.frame` ou matriz contendo os dados a serem modelados.
#'   #' @param model_name O nome do modelo a ser utilizado (apenas um rótulo).
#'   #' @param model_function A função que implementa o modelo de previsão a ser aplicado.
#'   #' @param variable O nome da variável dependente que será prevista.
#'   #' @param type O tipo de dados. O padrão é "default".
#'   #' @return Uma lista contendo os seguintes elementos:
#'   #' - mae: O erro absoluto médio das previsões.
#'   #' - rmse: A raiz do erro quadrático médio das previsões.
#'   #' - forecasts: As previsões geradas pelo modelo para o horizonte definido.
#'   #'
#'   #' @examples
#'   #' df <- data.frame(time = 1:100, variable = rnorm(100))
#'   #' results <- call_models(data = df, model_name = "MyModel", model_function = my_model_function, variable = "variable")
#'   #'
#'   #' @export
#'   
#'   data = as.matrix(data)
#'   
#'   model_name <- model_name
#'   model_function <- model_function
#'   
#'   nwindows = round(nrow(data)*0.3)
#'   y_out <- tail(data[, variable], nwindows)
#'   
#'   #if (type == 'tb'){
#'   #  data <- as.data.frame(data) %>%
#'   #    select(-c(1)) %>% 
#'   #    as.matrix()
#'   #}
#'   
#'   model_list <- list()
#'   for_ind <- c(1, 4)
#'   
#'   for (i in for_ind) {
#'     model <- rolling_window(
#'       fn = model_function,
#'       df = data,
#'       nwindow = nwindows + i - 1,
#'       horizon = i,
#'       variable = variable,
#'       n_lags = 4
#'     )
#'     model_list[[i]] <- model
#'     cat(i, "\n")
#'   }
#'   
#'   forecasts <- Reduce(
#'     f = cbind,
#'     x = lapply(model_list, function(x) head(x$forecast, nwindows))
#'   ) %>% as.matrix()
#'   
#'   
#'   for (i in for_ind) {
#'     filename <- paste0("forecast ",model_name,"-horizon-" , i, ".png")
#'     filepath <- file.path("Plots", filename)
#'     png(filepath, width = 1000, height = 600)
#'     
#'     col_idx <- match(i, for_ind)  # find which column corresponds to horizon i
#'     
#'     plot.ts(y_out, 
#'             main = paste0("Forecast with ", model_name, " (horizon = ", i, ")"),
#'             lwd = 2)
#'     lines(forecasts[, col_idx], col = 2, lwd = 2)
#'     dev.off()
#'   }
#'   
#'   
#'   rmse <- apply(forecasts, 2, f_rmse, y = y_out) %>% print()
#'   mae = apply(forecasts, 2, f_mae, y = y_out) %>% print()
#'   
#'   results = list(mae = mae, rmse = rmse, forecasts = forecasts)
#'   
#'   return(results)
#' }