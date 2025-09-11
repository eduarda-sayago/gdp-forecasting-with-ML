
# with oob
get_rforest <- function(ind, df, variable, horizon, n_lags) {
  
  library(randomForest)
  library(forecast)
  
  # Data preparation
  data_in <- dataprep(
    type = "default",
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- as.data.frame(data_in$x_in)
  x_out <- data_in$x_out
  x_out <- as.data.frame(matrix(x_out, nrow = 1))
  colnames(x_out) <- colnames(x_in)
  
  
  # Tune mtry using OOB error
  p <- ncol(x_in)
  mtry_grid <- c(1, floor(sqrt(p)), floor(p/3), floor(p/2), p)
  
  rf_list <- lapply(mtry_grid, function(m) {
    model <- randomForest(
      x = x_in,
      y = y_in,
      mtry = m,
      ntree = 500,
      nodesize = 5
    )
    
    list(model = model, oob_error = model$mse[500])
  })
  
  # Pick the best model
  best <- rf_list[[which.min(sapply(rf_list, function(x) x$oob_error))]]
  rf_opt <- best$model
  
  # Forecast
  rf_forecast <- predict(rf_opt, newdata = as.data.frame(t(x_out)))
  
  return(list(
    forecast = rf_forecast,
    outputs = list(
      rf_opt = rf_opt,
      best_mtry = rf_opt$mtry
    )
  ))
}

#with kf cv
get_rf <- function(ind, df, variable, horizon, n_lags) {
  
  #' Ajuste de Modelo de Random Forest
  #'
  #' Esta função ajusta um modelo de Random Forest aos dados fornecidos e gera previsões.
  #'
  #' @param ind Índices das observações a serem utilizadas.
  #' @param df Um data.frame contendo os dados.
  #' @param variable Nome da variável dependente a ser modelada.
  #' @param horizon Horizonte de previsão.
  #' @param n_lags Número de defasagens a serem usadas na modelagem.
  #' @return Uma lista contendo as previsões do modelo de Random Forest e informações sobre o modelo ajustado.
  #'
  #' @examples
  #' results <- get_rf(ind = 1:100, df = my_data, variable = "sales", horizon = 10, n_lags = 4)
  
  library(caret)
  library(randomForest)
  library(forecast)
  
  set.seed(100)
  
  # Preparação dos dados (mesmo estilo do boosting)
  data_in <- dataprep(
    type = 'default',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  # Definição do grid de parâmetros para tuning
  p <- ncol(x_in)
  mtry_grid <- expand.grid(mtry = c(1, floor(sqrt(p)), floor(p/3), floor(p/2), p))
  
  # Controle de treino com k-fold CV (ex: 5 folds)
  ctrl <- trainControl(method = "cv", number = 5)
  
  # Ajuste do modelo com CV
  rf_fit <- train(
    x = x_in,
    y = y_in,
    method = "rf",
    tuneGrid = mtry_grid,
    trControl = ctrl,
    ntree = 500,
    nodesize = 5
  )
  
  # Melhor modelo
  rf_opt <- rf_fit$finalModel
  best_mtry <- rf_fit$bestTune$mtry
  
  # Previsão
  rf_forecast <- predict(rf_fit, newdata = as.data.frame(t(x_out)))
  
  # Saída
  results <- list(
    forecast = rf_forecast,
    outputs = list(
      best_mtry = best_mtry,
      rf_fit = rf_fit
    )
  )
  
  return(results)
}

# another try
get_random_forest <- function(ind, df, variable, horizon, n_lags) {
  
  #' Ajuste de Modelo Random Forest
  #'
  #' Esta função ajusta um modelo de Random Forest aos dados fornecidos e gera previsões.
  #'
  #' @param ind Índices das observações a serem utilizadas.
  #' @param df Um data.frame contendo os dados.
  #' @param variable Nome da variável dependente a ser modelada.
  #' @param horizon Horizonte de previsão.
  #' @param n_lags Número de defasagens a serem usadas na modelagem.
  #' @return Uma lista contendo as previsões do modelo Random Forest e informações sobre o modelo ajustado.
  #'
  #' @examples
  #' results <- get_random_forest(ind = 1:100, df = my_data, variable = "sales", horizon = 10, n_lags = 4)
  
  library(randomForest)
  
  # --- PREPARAÇÃO DOS DADOS
  data_in <- dataprep(
    type = 'default',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- as.data.frame(data_in$x_in)
  x_out <- as.data.frame(data_in$x_out)
  
  # --- CROSS-VALIDATION PARA ESCOLHA DO MTRY
  set.seed(100)
  n <- nrow(x_in)
  k <- 5  # 5-fold CV
  
  folds <- sample(rep(1:k, length.out = n))
  mtry_values <- 1:ncol(x_in)
  cv_rmse <- numeric(length(mtry_values))
  
  for (j in seq_along(mtry_values)) {
    fold_errors <- c()
    for (fold in 1:k) {
      train_idx <- which(folds != fold)
      test_idx <- which(folds == fold)
      
      rf_model <- randomForest(
        x = x_in[train_idx, , drop = FALSE],
        y = y_in[train_idx],
        mtry = mtry_values[j],
        ntree = 500
      )
      
      preds <- predict(rf_model, newdata = x_in[test_idx, , drop = FALSE])
      fold_errors <- c(fold_errors, sqrt(mean((y_in[test_idx] - preds)^2)))
    }
    cv_rmse[j] <- mean(fold_errors)
  }
  
  best_mtry <- mtry_values[which.min(cv_rmse)]
  
  # --- AJUSTE FINAL COM MELHOR MTRY
  rf_final <- randomForest(
    x = x_in,
    y = y_in,
    mtry = best_mtry,
    ntree = 500
  )
  
  # --- PREVISÃO PARA A JANELA DE TESTE
  rf_forecast <- predict(rf_final, newdata = x_out) %>% as.vector()
  
  # --- RESULTADOS
  results <- list(
    forecast = rf_forecast,
    outputs = list(
      best_mtry = best_mtry,
      rf_model = rf_final,
      cv_errors = cv_rmse
    )
  )
  
  return(results)
}
