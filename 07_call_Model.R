
call_models = function(data, model_name, model_function, variable, verbose = TRUE){
  
  #' Chamadas para Modelos de Previsão
  #'
  #' Esta função aplica um modelo de previsão a um conjunto de dados, utilizando uma janela deslizante para gerar previsões.
  #'
  #' @param data Um `data.frame` ou matriz contendo os dados a serem modelados.
  #' @param model_name O nome do modelo a ser utilizado (apenas um rótulo).
  #' @param model_function A função que implementa o modelo de previsão a ser aplicado.
  #' @param variable O nome da variável dependente que será prevista.
  #' @return Uma lista contendo os seguintes elementos:
  #' - me: O erro médio das previsões.
  #' - rmse: A raiz do erro quadrático médio das previsões.
  #' - forecasts: As previsões geradas pelo modelo para o horizonte definido.
  #'
  #' @examples
  #' df <- data.frame(time = 1:100, variable = rnorm(100))
  #' results <- call_models(data = df, model_name = "MyModel", model_function = my_model_function, variable = "variable")
  #'
  #' @export
  library(ggplot2)
  
  date_col <- data[, 1]
  data <- data[, -1]
  data <- as.matrix(data)
  
  model_name <- model_name
  model_function <- model_function
  
  window_prop <- 0.30
  nwindows = max(1L, as.integer(floor(nrow(data) * window_prop)))
  if (isTRUE(verbose)) message(sprintf("[%s] Starting with %d rolling windows (window_prop=%.2f)", model_name, nwindows, window_prop))
  y_out <- tail(data[, variable], nwindows)
  y_time <- tail(date_col, nwindows)
  
  model_list <- list()
  for_ind <- c(1, 4)
  
  for (i in for_ind) {
    model <- rolling_window(
      fn = model_function,
      df = data,
      nwindow = nwindows + i - 1,
      horizon = i,
      variable = variable,
      n_lags = 4,
      verbose = verbose
    )
    model_list[[i]] <- model
    if (isTRUE(verbose)) message(sprintf("[%s] Finished horizon h=%d", model_name, i))
  }
  
  forecasts <- Reduce(
    f = cbind,
    x = lapply(model_list, function(x) head(x$forecast, nwindows))
  ) %>% as.matrix()
  
  # Compute metrics before plotting so we can display them
  rmse <- apply(forecasts, 2, f_rmse, y = y_out) %>% print()
  me = apply(forecasts, 2, f_me, y = y_out) %>% print()
  
  
  # Prepare enhanced plotting
  actual_col <- "#1f77b4"   # blue
  h1_col     <- "#d62728"   # red
  h4_col     <- "#2ca02c"   # green
  main_title <- sprintf("%s Forecasts for %s", model_name, variable)
  sub_title  <- tryCatch(
    sprintf("RMSE h=1: %.3f, h=4: %.3f  |  ME h=1: %.3f, h=4: %.3f",
            rmse[1], ifelse(length(rmse) >= 2, rmse[2], NA),
            me[1],   ifelse(length(me) >= 2,   me[2],   NA)),
    error = function(e) ""
  )
  # Legend labels (use GDP wording when variable == 'pib_rs')
  obs_label <- if (identical(variable, "pib_rs")) "Observed GDP" else "Observed"
  h1_label  <- if (identical(variable, "pib_rs")) "Predicted GDP (h=1)" else "Forecast h=1"
  h4_label  <- if (identical(variable, "pib_rs")) "Predicted GDP (h=4)" else "Forecast h=4"
  
  if (interactive()) {
    plot(y_time, y_out, type = "l", col = actual_col, lwd = 2,
         xlab = "Date", ylab = if (identical(variable, "pib_rs")) "GDP (pib_rs)" else variable,
         main = main_title, sub = sub_title)
    if (ncol(forecasts) >= 1) lines(y_time, forecasts[, 1], col = h1_col, lwd = 2)
    if (ncol(forecasts) >= 2) lines(y_time, forecasts[, 2], col = h4_col, lwd = 2)
    legend("topleft",
           legend = c(obs_label, h1_label, if (ncol(forecasts) >= 2) h4_label else NULL),
           col    = c(actual_col, h1_col, if (ncol(forecasts) >= 2) h4_col else NULL),
           lty = 1, lwd = 2, bty = "n")
  } else {
    plots_dir <- file.path("plots")
    if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
    png(file.path(plots_dir, paste0(model_name, "_forecast.png")), width = 1000, height = 600)
    plot(y_time, y_out, type = "l", col = actual_col, lwd = 2,
         xlab = "Date", ylab = if (identical(variable, "pib_rs")) "GDP (pib_rs)" else variable,
         main = main_title, sub = sub_title)
    if (ncol(forecasts) >= 1) lines(y_time, forecasts[, 1], col = h1_col, lwd = 2)
    if (ncol(forecasts) >= 2) lines(y_time, forecasts[, 2], col = h4_col, lwd = 2)
    legend("topleft",
           legend = c(obs_label, h1_label, if (ncol(forecasts) >= 2) h4_label else NULL),
           col    = c(actual_col, h1_col, if (ncol(forecasts) >= 2) h4_col else NULL),
           lty = 1, lwd = 2, bty = "n")
    grid(col = "#dddddd")
    dev.off()
  }
  if (isTRUE(verbose)) message(sprintf("[%s] RMSE: %s | ME: %s", model_name, paste(round(rmse,4), collapse=", "), paste(round(me,4), collapse=", ")))
  
  # Collect model outputs per horizon (list of per-window outputs)
  outputs_by_h <- setNames(lapply(for_ind, function(h) model_list[[h]]$outputs), paste0("h", for_ind))
  results = list(me = me, rmse = rmse, forecasts = forecasts, outputs = outputs_by_h)
  
  return(results)
}