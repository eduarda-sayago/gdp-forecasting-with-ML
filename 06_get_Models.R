
# ================================================
# --------------ERRO QUADRATICO MEDIO-------------
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
  
  sqrt(mean((x - y)^2))
}

# ================================================
# ------------------ERRO MEDIO--------------------
# ================================================

f_me = function(x,y){
  
  #' Cálculo do Erro Médio
  #'
  #' Esta função calcula o erro médio entre duas séries de valores.
  #'
  #' @param x Um vetor de valores previstos.
  #' @param y Um vetor de valores reais.
  #' @return O valor do erro médio entre as duas séries de valores.
  #'
  #' @examples
  #' f_me(c(2, 3, 5), c(1, 2, 6)) # Retorna o erro médio entre os vetores
  
  
  mean(y - x)
}

# ================================================
# ------------------SARIMA MODEL------------------
# ================================================

get_sarima = function(ind, df, variable, horizon, n_lags, verbose = TRUE){
  
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
  
  reg_arima = auto.arima(
    y = y_in, 
    stepwise = F, 
    approximation = F, 
    stationary = T,
    seasonal = T,
    start.p = 0,
    start.q = 0)
  
  if (isTRUE(verbose)) print(reg_arima)
  
  for_arima_aux = forecast(
    object = reg_arima, 
    h = horizon)
  
  forecasts = for_arima_aux$mean
  
  # Provide lightweight model details as outputs
  outputs <- list(
    coef = tryCatch(coef(reg_arima), error = function(e) NULL),
    aic = tryCatch(AIC(reg_arima), error = function(e) NULL)
  )
  
  results = list(forecast = as.numeric(forecasts), outputs = outputs)
  
  return (results)
  
}

# ================================================
# ------------------LASSO MODEL-------------------
# ================================================

get_lasso = function(ind, df, variable, horizon, n_lags, verbose = TRUE){
  
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
    #type = 'tb',
    ind = ind,
    df = df,
    variable = variable,
    horizon = horizon,
    n_lags = n_lags)
  
  #INICIANDO AS VARIAVEIS
  y_in = data_in$y_in
  x_in = data_in$x_in
  x_out = data_in$x_out
  
  #ESTIMANDO O MODELO
  cv_lasso = cv.glmnet(
    x = as.matrix(x_in),
    y = y_in,
    alpha = 1,
    intercept = T,
    standardize = T,
    nfolds = 5)
  
  # Only compute and print coefficient paths when verbose to save time
  if (isTRUE(verbose)) {
  grid <- 10^seq(10, -2, length = 100)
  out = glmnet(x_in, y_in, alpha = 1, lambda = grid)
  lasso.coef = predict(
    out, 
    type = "coefficients",
    s = cv_lasso$lambda.min)[1:41,]
  print(lasso.coef[lasso.coef != 0])
  }
  
  #PREVISAO
  opt_lasso = predict(
    cv_lasso,
    s = cv_lasso$lambda.min,
    newx = as.matrix(x_out, nrow = 1))
  
  lasso_coef_mat <- tryCatch(coef(cv_lasso, s = cv_lasso$lambda.min), error = function(e) NULL)
  lasso_coef <- tryCatch({
    v <- rep(0, nrow(lasso_coef_mat)); names(v) <- rownames(lasso_coef_mat); v[lasso_coef_mat@i + 1] <- lasso_coef_mat@x; v
  }, error = function(e) NULL)
  outputs <- list(lambda_min = cv_lasso$lambda.min, coef = lasso_coef)
  
  results = list(forecast = opt_lasso, outputs = outputs)
  
  return(results)
  
}

# ================================================
# ---------------ELASTIC NET MODEL----------------
# ================================================

get_elasticnet <- function(ind, df, variable, horizon, n_lags, verbose = TRUE) {
  
  
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
  best_lambda <- cv_enet$finalModel$lambdaOpt %>% print()
  best_alpha <- cv_enet$finalModel$tuneValue[1] %>% as.numeric()
  if (isTRUE(verbose)) {
    print(best_lambda)
    print(best_alpha)
  }
  opt_enet <- coef(cv_enet$finalModel, cv_enet$finalModel$lambdaOpt)
  
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
  
  # Outputs: best alpha/lambda and coefficients
  enet_coef_mat <- tryCatch(coef(glmnet_aux, s = best_lambda), error = function(e) NULL)
  enet_coef <- tryCatch({
    v <- rep(0, nrow(enet_coef_mat)); names(v) <- rownames(enet_coef_mat); v[enet_coef_mat@i + 1] <- enet_coef_mat@x; v
  }, error = function(e) NULL)
  outputs <- list(best_lambda = best_lambda, best_alpha = best_alpha, coef = enet_coef)
  
  results <- list(forecast = opt_elasticnet, outputs = outputs)
  
  return(results)
}

# ================================================
# --------------RANDOM FOREST MODEL---------------
# ================================================

# with oob
get_rforest <- function(ind, df, variable, horizon, n_lags, verbose = TRUE) {
  
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
  
  y_in <- as.numeric(data_in$y_in)
  x_in <- as.data.frame(data_in$x_in)
  x_out <- as.data.frame(data_in$x_out)
  
  x_out <- x_out[, colnames(x_in), drop = FALSE]
  
  
  # Tune mtry using OOB error
  p <- ncol(x_in)
  mtry_grid <- unique(pmax(1, c(1L, floor(sqrt(p)), floor(p/3), floor(p/2), p)))
  
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
  rf_forecast <- predict(rf_opt, newdata = x_out)
  
  return(list(
    forecast = rf_forecast,
    outputs = list(
      rf_opt = rf_opt,
      best_mtry = rf_opt$mtry
    )
  ))
}

#with k-fold cross-validation (caret)
get_rf <- function(ind, df, variable, horizon, n_lags, verbose = TRUE) {
  
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
  
  x_out <- x_out[, colnames(x_in), drop = FALSE]
  
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
  
  if (isTRUE(verbose)) {
    print(rf_cv$bestTune)
  }
  
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
