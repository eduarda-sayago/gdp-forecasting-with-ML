
# ================================================
# ------------ROOT MEAN SQUARED ERROR-------------
# ================================================

f_rmse = function(x, y){
  
  #' Cálculo do Erro Quadrático Médio (RMSE)
  #'
  #' Esta função calcula a raiz do erro quadrático médio entre duas séries de valores.
  #'
  #' @param x Um vetor de valores previstos.
  #' @param y Um vetor de valores reais.
  #' @return O valor do erro quadrático médio entre as duas séries de valores.
  #'
  #' @examples
  #' f_rmse(c(2, 3, 5), c(1, 2, 6)) # Retorna a RMSE entre os vetores
  
  sqrt(mean((y - x)^2))
}

# ================================================
# --------------MEAN ABSOLUTE ERROR---------------
# ================================================

f_mae = function(x, y){
  
  #' Cálculo do Erro Médio Absoluto (MAE)
  #'
  #' Esta função calcula o erro médio absoluto entre duas séries de valores.
  #'
  #' @param x Um vetor de valores previstos.
  #' @param y Um vetor de valores reais.
  #' @return O valor do erro médio absoluto entre as duas séries de valores.
  #'
  #' @examples
  #' f_mae(c(2, 3, 5), c(1, 2, 6)) # Retorna o MAE entre os vetores
  #'
  
  mean(abs(y - x))
}

# ================================================
# ---------MEAN ABSOLUTE PERCENTAGE ERROR---------
# ================================================

f_mape = function(x, y){
  
  #' Cálculo do Erro Percentual Médio Absoluto (MAPE)
  #'
  #' Esta função calcula o erro percentual médio absoluto entre duas séries de valores.
  #'
  #' @param x Um vetor de valores previstos.
  #' @param y Um vetor de valores reais.
  #' @return O valor do erro percentual médio absoluto (em %).
  #'
  #' @examples
  #' f_mape(c(2, 3, 5), c(1, 2, 6)) # Retorna o MAPE entre os vetores
  #'
  
  mean(abs((y - x) / y)) * 100
}

# ================================================
# ---------------MEAN MODEL (test)----------------
# ================================================

get_mean = function(ind, df, variable, horizon, n_lags){
  data_in = dataprep(
    type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )

  y_in = as.numeric(data_in$y_in)
  mu = mean(y_in)

  results = list(
    forecast = rep(mu, horizon),
    outputs = list(mean = mu)
  )
  return(results)
}

# ================================================
# ------------------SARIMA MODEL------------------
# ================================================

get_sarima = function(ind, df, variable, horizon, n_lags){
  
  #' Ajuste de Modelo SARIMA
  #'
  #' Esta função ajusta um modelo SARIMA aos dados fornecidos e gera previsões.
  #'
  #' @param ind Índices das observações a serem utilizadas.
  #' @param df Um data.frame contendo os dados.
  #' @param variable Nome da variável dependente a ser modelada.
  #' @param horizon Horizonte de previsão.
  #' @param n_lags Número de defasagens a serem usadas na modelagem.
  #' @return Uma lista contendo as previsões do modelo SARIMA.
  #'
  #' @examples
  #' results <- get_sarima(ind = 1:100, df = my_data, variable = "sales", horizon = 10, n_lags = 4)
  
  
  library(tidyverse)
  library(forecast)
  
  data_in = dataprep(
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags)
  
  #INICIANDO AS VARIAVEIS
  y_in = data_in$y_in
  as.ts(y_in, frequency = 4)
  
  reg_arima = auto.arima(
    y = y_in, 
    stepwise = F, 
    approximation = F, 
    stationary = T,
    seasonal = T,
    start.p = 0,
    start.q = 0)
  
  print(reg_arima)
  
  for_arima_aux = forecast(
    object = reg_arima, 
    h = horizon)
  
  forecasts = for_arima_aux$mean
  forecast = tail(forecasts, 1)
  
  results = list(forecast = forecast)
  
  return (results)
  
}

# ================================================
# ------------------LASSO MODEL-------------------
# ================================================

get_lasso = function(ind, df, variable, horizon, n_lags){
  
  #' Ajuste de Modelo Lasso
  #'
  #' Esta função ajusta um modelo de regressão Lasso aos dados fornecidos e gera previsões.
  #'
  #' @param ind Índices das observações a serem utilizadas.
  #' @param df Um data.frame contendo os dados.
  #' @param variable Nome da variável dependente a ser modelada.
  #' @param horizon Horizonte de previsão.
  #' @param n_lags Número de defasagens a serem usadas na modelagem.
  #' @return Uma lista contendo as previsões do modelo Lasso.
  #'
  #' @examples
  #' results <- get_lasso(ind = 1:100, df = my_data, variable = "sales", horizon = 10, n_lags = 4)
    
  set.seed(100)
  
  library(glmnet)
  library(forecast)
  
  #PREPARANDO OS DADOS
  data_in = dataprep(
    type = 'default',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = 4)
  
  #INICIANDO AS VARIAVEIS
  y_in = data_in$y_in
  x_in = data_in$x_in
  x_out = data_in$x_out
  
  set.seed(100)
  #ESTIMANDO O MODELO
  cv_lasso = cv.glmnet(
    x = as.matrix(x_in),
    y = y_in,
    alpha = 1,
    intercept = T,
    standardize = T,
    nfolds = 5)
  
  grid <- 10^seq(10, -2, length = 100)
  out = glmnet(x_in, y_in, alpha = 1, lambda = grid)
  
  lasso.coef = predict(
    out, 
    type = "coefficients",
    s = cv_lasso$lambda.min)[1:41,]
  
  coef_mat <- as.matrix(lasso.coef)                     # ensure it's a matrix
  coef_vals <- as.numeric(coef_mat)                     # numeric values
  coef_names <- rownames(coef_mat)                      # names (intercept + features)
  
  nonzero <- which(coef_vals != 0)
  
  # window identifier (keeps track of which rolling window produced it)
  window_id <- paste0("ind_", min(ind), "_to_", max(ind))
  
  if (length(nonzero) > 0) {
    df_coefs <- data.frame(
      window = window_id,
      name   = coef_names[nonzero],
      coef   = coef_vals[nonzero],
      stringsAsFactors = FALSE
    )
  } else {
    df_coefs <- data.frame(
      window = window_id,
      name   = NA_character_,
      coef   = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  
  # store in a global R object (list) so each call appends
  if (!exists("lasso_coefs_history", envir = .GlobalEnv)) {
    assign("lasso_coefs_history", list(), envir = .GlobalEnv)
  }
  hist_list <- get("lasso_coefs_history", envir = .GlobalEnv)
  hist_list[[length(hist_list) + 1]] <- df_coefs
  assign("lasso_coefs_history", hist_list, envir = .GlobalEnv)
  
  # also append to CSV so you can open it later without R running
  fn <- "lasso_coefs_history.csv"
  write.table(
    df_coefs,
    file = fn,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(fn),
    append = TRUE
  )
  
  # (optional) still print a short summary to console:
  message("Saved ", nrow(df_coefs), " nonzero coef(s) for window ", window_id)
  
  #PREVISAO
  opt_lasso = predict(
    cv_lasso,
    s = cv_lasso$lambda.min,
    newx = as.matrix(x_out, nrow = 1))
  
  results = list(forecast = opt_lasso)
  
  return(results)
  
}

# ================================================
# ---------------ELASTIC NET MODEL----------------
# ================================================

get_elasticnet <- function(ind, df, variable, horizon, n_lags) {
  
  
  #' Ajuste de Modelo Elastic Net
  #'
  #' Esta função ajusta um modelo Elastic Net aos dados fornecidos e gera previsões.
  #'
  #' @param ind Índices das observações a serem utilizadas.
  #' @param df Um data.frame contendo os dados.
  #' @param variable Nome da variável dependente a ser modelada.
  #' @param horizon Horizonte de previsão.
  #' @param n_lags Número de defasagens a serem usadas na modelagem.
  #' @return Uma lista contendo as previsões do modelo Elastic Net.
  #'
  #' @examples
  #' re
  
  library(forecast)
  library(glmnet)
  library(caret)
  
  # PREPARANDO OS DADOS
  data_in <- dataprep(
    type = 'default',
    #type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  # INICIANDO AS VARIAVEIS
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  set.seed(100)
  cv_5 <- trainControl(method = "cv", number = 5) # CROSS VALIDATION
  
  myGrid <- expand.grid(
    alpha = seq(0, 1, length = 10), # range for alpha
    lambda = exp(seq(from = log(0.5), to = log(25000), length = 200)) # Broad range for lambda
  )
  
  cv_enet <- train(
    x = as.data.frame(x_in),
    y = as.numeric(y_in),
    method = "glmnet",
    trControl = cv_5,
    tuneGrid = myGrid,
    metric = "RMSE",
    intercept = T,
    standardize = T
  )
  
  # Assuming 'cv_enet' is your trained model object
  # best_lambda <- cv_enet$finalModel$lambdaOpt %>% print()
  # best_alpha <- cv_enet$finalModel$tuneValue[1] %>% as.numeric() %>% print()
  best_lambda <- cv_enet$finalModel$lambdaOpt
  best_alpha  <- as.numeric(cv_enet$finalModel$tuneValue[1])
  opt_enet <- coef(cv_enet$finalModel, cv_enet$finalModel$lambdaOpt)
  
  #---
  coef_mat <- as.matrix(opt_enet)
  coef_vals <- as.numeric(coef_mat)
  coef_names <- rownames(coef_mat)
  nonzero <- which(coef_vals != 0)
  
  # identifier for the rolling window
  window_id <- paste0("ind_", min(ind), "_to_", max(ind))
  
  if (length(nonzero) > 0) {
    df_coefs <- data.frame(
      window = window_id,
      name   = coef_names[nonzero],
      coef   = coef_vals[nonzero],
      alpha  = best_alpha,
      lambda = best_lambda,
      stringsAsFactors = FALSE
    )
  } else {
    df_coefs <- data.frame(
      window = window_id,
      name   = NA_character_,
      coef   = NA_real_,
      alpha  = best_alpha,
      lambda = best_lambda,
      stringsAsFactors = FALSE
    )
  }
  
  # store in a global list so each call appends (no change to function signature)
  if (!exists("elasticnet_coefs_history", envir = .GlobalEnv)) {
    assign("elasticnet_coefs_history", list(), envir = .GlobalEnv)
  }
  hist_list <- get("elasticnet_coefs_history", envir = .GlobalEnv)
  hist_list[[length(hist_list) + 1]] <- df_coefs
  assign("elasticnet_coefs_history", hist_list, envir = .GlobalEnv)
  
  # also append to CSV for persistence
  fn <- "elasticnet_coefs_history.csv"
  write.table(
    df_coefs,
    file = fn,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(fn),
    append = TRUE
  )
  
  message("Saved ", nrow(df_coefs), " coeff row(s) for window ", window_id)
  #---
  x <- rep(0, ncol(x_in) + 1)
  x[opt_enet@i + 1] <- opt_enet@x
  names(x) <- c("cte", colnames(x_in))
  
  glmnet_aux <- glmnet(
    x = x_in,
    y = y_in,
    family = "gaussian",
    lambda = best_lambda,
    alpha = best_alpha,
    standardize = T,
    intercept = T
  )
  
  opt_elasticnet <- predict(
    glmnet_aux,
    s = best_lambda,
    newx = x_out,
    type = "response"
  ) # acho que retorna "the fitted values"
  
  results <- list(forecast = opt_elasticnet)
  
  return(results)
}

# ================================================
# --------------RANDOM FOREST MODEL---------------
# ================================================

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
  
  # Preparação dos dados (mesmo estilo do boosting)
  data_in <- dataprep(
    type = 'default',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- as.numeric(data_in$y_in)
  x_in <- as.data.frame(data_in$x_in)
  x_out <- as.data.frame(data_in$x_out)
  
  # Ensure x_out has the same column names as x_in
  if (ncol(x_out) == ncol(x_in)) {
    colnames(x_out) <- colnames(x_in)
  } else {
    x_out <- as.data.frame(t(x_out))
    colnames(x_out) <- colnames(x_in)
  }
  
  # CV setup
  set.seed(100)
  ctrl <- trainControl(method = "cv", number = 5)
  
  p <- ncol(x_in)
  grid <- expand.grid(
    mtry = unique(pmax(1, c(1L, floor(sqrt(p)), floor(p/3), floor(p/2), p)))
  )
  
  # Ajuste do modelo com CV
  rf_cv <- train(
    x = x_in,
    y = y_in,
    method = "rf",
    trControl = ctrl,
    tuneGrid = grid,
    ntree = 500,
    metric = "RMSE"
  )
  
    print(rf_cv$bestTune)
  
  # Previsão
  rf_forecast <- predict(rf_cv, newdata = x_out)
  
  # Saída
  results <- list(
    forecast = as.numeric(rf_forecast),
    outputs = list(
      rf_cv = rf_cv,
      best_mtry = rf_cv$bestTune$mtry
    )
  )
  
  return(results)
}

# ================================================
# -----------------BOOSTING MODEL-----------------
# ================================================

get_boosting <- function(ind, df, variable, horizon, n_lags) {
  
  #' Ajuste de Modelo de Boosting
  #'
  #' Esta função ajusta um modelo de boosting aos dados fornecidos e gera previsões.
  #'
  #' @param ind Índices das observações a serem utilizadas.
  #' @param df Um data.frame contendo os dados.
  #' @param variable Nome da variável dependente a ser modelada.
  #' @param horizon Horizonte de previsão.
  #' @param n_lags Número de defasagens a serem usadas na modelagem.
  #' @return Uma lista contendo as previsões do modelo de boosting e informações sobre o modelo ajustado.
  #'
  #' @examples
  #' results <- get_boosting(ind = 1:100, df = my_data, variable = "sales", horizon = 10, n_lags = 4)
  
  
  library(mboost)
  library(forecast)
  
  # INICIALIZACAO DE VARIAVEIS
  set.seed(100)
  
  data_in <- dataprep(
    type = 'default',
    #type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags
  )
  
  y_in <- data_in$y_in
  x_in <- data_in$x_in
  x_out <- data_in$x_out
  
  # AJUSTE DO MODELO DE BOOSTING
  reg_full <- glmboost(
    y = y_in,
    x = as.matrix(x_in),
    offset = 0, # mean(y_in),
    center = TRUE,
    control = boost_control(mstop = 300, nu = 0.1)
  )
  
  # DETERMINACAO DO NUMERO OTIMO DE ITERACOES
  cv5f <- cv(model.weights(reg_full), type = "kfold", B = 5)
  cv_seq <- cvrisk(reg_full, folds = cv5f, papply = lapply)
  m_opt <- mstop(cv_seq)
  
  # AJUSTE DO MODELO COM O NUMERO OTIMO DE ITERACOES
  
  reg_opt <- reg_full[m_opt]
  
  # PREVISAO PARA A JANELA DE TESTE
  opt_boosting <- predict(
    object = reg_opt,
    newdata = matrix(x_out, nrow = 1)
  ) %>% as.vector() + mean(y_in)
  
  # RESULTADOS
  results <- list(
    forecast = opt_boosting,
    outputs = list(
      m_opt = m_opt,
      reg_opt = reg_opt
    )
  )
  return(results)
}
