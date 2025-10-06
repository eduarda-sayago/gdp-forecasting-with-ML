rolling_window <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  
  #' Janela Movel para Modelagem
  #'
  #' Esta função aplica uma função especificada a uma janela deslizante de dados em um `data.frame` para realizar previsões.
  #'
  #' @param fn A função a ser aplicada em cada janela deslizante. Esta função deve retornar um objeto com as previsões e saídas.
  #' @param df Um `data.frame` contendo os dados a serem processados.
  #' @param nwindow O tamanho da janela deslizante. O padrão é 1.
  #' @param horizon O horizonte de previsão, que determina quantas observações devem ser projetadas para frente.
  #' @param variable O nome da variável dependente a ser utilizada na análise.
  #' @param ... Argumentos adicionais a serem passados para a função `fn`.
  #' @return Uma lista com dois elementos:
  #' - forecast: Um vetor com as previsões resultantes de cada aplicação da função nas janelas deslizantes.
  #' - outputs: Uma lista contendo as saídas resultantes de cada aplicação da função nas janelas deslizantes.
  #'
  #' @examples
  #' df <- data.frame(data = 1:100, variable = rnorm(100))
  #' result <- rolling_window(fn = my_forecast_function, df = df, nwindow = 5, horizon = 1, variable = 'variable')
  #'
  #' @export
  
  #ind <- seq_len(nrow(df))
  window_size <- nrow(df) - nwindow # tamanho da janela h1 = 64, h4 = 61
  indmat <- matrix(NA, window_size, nwindow) # (64,28)
  indmat[1, ] <- seq_len(ncol(indmat))
  for (i in 2:nrow(indmat)) {
    indmat[i, ] <- indmat[i - 1, ] + 1
  }
  
  rw <- apply(
    X = indmat,
    MARGIN = 2,
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs <- lapply(rw, function(x) x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}

expanding_window <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  #' Janela Expansiva para Modelagem (expanding window)
  #'
  #' Versão mínima alterada para produzir janelas expansivas: cada janela começa
  #' em 1 e cresce até o final, começando com tamanho `nwindow`.
  
  # número de janelas (inclui a janela final que termina em nrow(df))
  window_size <- nrow(df) - nwindow + 1
  if (window_size < 1) stop("nwindow is larger than number of rows in df")
  
  # lista de índices: 1:(nwindow), 1:(nwindow+1), ..., 1:(nrow(df))
  indlist <- lapply(seq_len(window_size), function(i) seq_len(nwindow + i - 1))
  
  # aplica fn a cada janela (cada elemento de indlist é passado como primeiro arg)
  rw <- lapply(indlist, function(idx) fn(idx, df = df, horizon = horizon, variable = variable, ...))
  
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs <- lapply(rw, function(x) x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}

expanding_window2 <- function(fn, df, nwindow = 1, horizon, variable, ...) {
  #' Janela Expansiva para Modelagem
  #'
  #' Mesma interface do rolling_window, mas cada janela cresce a partir do início (1:K).
  #' @param fn A função aplicada em cada janela (deve retornar $forecast e $outputs).
  #' @param df Data frame / matrix.
  #' @param nwindow Número de janelas finais (mantido igual ao rolling_window).
  #' @param horizon Horizonte de previsão.
  #' @param variable Nome/índice da variável dependente.
  #' @return lista com $forecast e $outputs (mesmo formato do rolling_window).
  
  # número base que o rolling_window também usa
  window_size <- nrow(df) - nwindow
  
  # construir lista de índices de treino: para j em 1:nwindow -> 1:(window_size + j - 1)
  indlist <- vector("list", length = nwindow)
  for (j in seq_len(nwindow)) {
    indlist[[j]] <- seq_len(window_size + j - 1)
  }
  
  # aplicar a função fn a cada conjunto de índices (mantendo os argumentos iguais)
  rw <- lapply(
    X = indlist,
    FUN = fn,
    df = df,
    horizon = horizon,
    variable = variable,
    ...
  )
  
  forecast <- unlist(lapply(rw, function(x) x$forecast))
  outputs  <- lapply(rw, function(x) x$outputs)
  return(list(forecast = forecast, outputs = outputs))
}