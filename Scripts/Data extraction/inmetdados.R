## Processamento dados inmet

# Libraries
library(dplyr)
library(purrr)
library(readr)

# Função para mapear nomes originais para novos nomes
mapear_nomes_colunas <- function(nomes_originais) {
  # Dicionário de mapeamento (sem o prefixo cidade_)
  mapeamento <- c(
    "DIRECAO PREDOMINANTE DO VENTO, MENSAL(° (gr))" = "dirvento",
    "INSOLACAO TOTAL, MENSAL(h)" = "insolacaototal", 
    "NEBULOSIDADE, MEDIA MENSAL(décimos)" = "nebulosmedia",
    "PRECIPITACAO TOTAL, MENSAL(mm)" = "precipitacao",
    "PRESSAO ATMOSFERICA, MEDIA MENSAL(mB)" = "pressaomed",
    "TEMPERATURA MAXIMA MEDIA, MENSAL(°C)" = "tempmax",
    "TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)" = "tempmedcomp",
    "TEMPERATURA MINIMA MEDIA, MENSAL(°C)" = "tempmin",
    "UMIDADE RELATIVA DO AR, MEDIA MENSAL(%)" = "umidade",
    "VENTO, VELOCIDADE MAXIMA MENSAL(m/s)" = "ventovelmax",
    "VENTO, VELOCIDADE MEDIA MENSAL(m/s)" = "ventovelmed"
  )
  
  # Substituir apenas os nomes conhecidos, manter outros como estão
  nomes_mapeados <- ifelse(nomes_originais %in% names(mapeamento), 
                           mapeamento[nomes_originais], 
                           nomes_originais)
  
  return(nomes_mapeados)
}

# Função para processar cada CSV individual
processar_csv <- function(caminho_arquivo) {
  # Extrair nome da cidade do nome do arquivo (sem extensão)
  nome_cidade <- tools::file_path_sans_ext(basename(caminho_arquivo))
  
  # Ler o CSV a partir da linha 11 (onde estão os headers)
  df <- read_delim(caminho_arquivo, 
                   delim = ";",
                   skip = 10,  # pula as primeiras 10 linhas (header na 11)
                   locale = locale(decimal_mark = "."),
                   na = "null",  # transforma "null" em NA automaticamente
                   col_types = cols(.default = "c"))  # lê tudo como character primeiro
  
  # Mapear nomes das colunas para versões simplificadas
  nomes_originais <- names(df)
  nomes_mapeados <- mapear_nomes_colunas(nomes_originais)
  
  # Adicionar prefixo da cidade (exceto para a primeira coluna que é data)
  nomes_finais <- c(nomes_mapeados[1],  # mantém nome da coluna data
                    paste0(nome_cidade, "_", nomes_mapeados[-1]))  # adiciona prefixo cidade
  
  names(df) <- nomes_finais
  
  # Converter tipos de dados
  # Assumindo que a primeira coluna é sempre data
  df[[1]] <- as.Date(df[[1]])
  
  # Converter outras colunas para numeric
  df[,-1] <- lapply(df[,-1], as.numeric)
  
  return(df)
}

# Método principal
consolidar_dados_meteorologicos <- function(pasta_csvs) {
  # Listar todos os arquivos CSV na pasta
  arquivos_csv <- list.files(pasta_csvs, pattern = "\\.csv$", full.names = TRUE)
  
  if(length(arquivos_csv) == 0) {
    stop("Nenhum arquivo CSV encontrado na pasta especificada")
  }
  
  # Processar cada CSV
  lista_dfs <- map(arquivos_csv, processar_csv)
  
  # Obter nome da coluna de data (assumindo que é igual em todos)
  nome_coluna_data <- names(lista_dfs[[1]])[1]
  
  # Fazer merge de todos os dataframes pela coluna de data
  df_final <- lista_dfs[[1]]
  
  if(length(lista_dfs) > 1) {
    for(i in 2:length(lista_dfs)) {
      df_final <- full_join(df_final, lista_dfs[[i]], by = nome_coluna_data)
    }
  }
  
  # Ordenar por data
  df_final <- df_final[order(df_final[[nome_coluna_data]]), ]
  
  return(df_final)
}

# Exemplo de uso:
pasta_dados <- "Data/inmetdatars/"
dados_consolidados <- consolidar_dados_meteorologicos(pasta_dados)

# Verificar resultado
# str(dados_consolidados)
# head(dados_consolidados)

# Versão alternativa usando reduce() - mais elegante
consolidar_dados_v2 <- function(pasta_csvs) {
  arquivos_csv <- list.files(pasta_csvs, pattern = "\\.csv$", full.names = TRUE)
  
  # Processar e fazer join em uma só operação
  dados_consolidados <- arquivos_csv %>%
    map(processar_csv) %>%
    reduce(~ full_join(.x, .y, by = names(.x)[1])) %>%
    arrange(across(1))  # ordena pela primeira coluna (data)
  
  return(dados_consolidados)
}

colSums(is.na(dados_consolidados))

dados_consolidados <- dados_consolidados %>%
  select(-bage_...13, -bomjesus_...13, -caxias_...13, -cruzalta_...13, -encruzilhada_...13, -passofundo_...13, -pelotas_...13, -portoalegre_...13, -santamaria_...13, -santavpalmar_...13, -sluizgonzaga_...13,
         -torres_...13, -uruguaiana_...13)

colnames(dados_consolidados)

dados_consolidados <- dados_consolidados %>%
  select('Data Medicao', 'bage_dirvento', 'bage_insolacaototal', 'bage_nebulosmedia', 'bage_precipitacao', 'bage_pressaomed', 'bage_tempmax', 'bage_tempmedcomp', 'bage_tempmin', 'bage_umidade', 'bage_ventovelmax', 'bage_ventovelmed', 'cruzalta_dirvento', 'cruzalta_insolacaototal', 'cruzalta_nebulosmedia', 'cruzalta_precipitacao', 'cruzalta_pressaomed', 'cruzalta_tempmax', 'cruzalta_tempmedcomp', 'cruzalta_tempmin', 'cruzalta_umidade', 'cruzalta_ventovelmax', 'cruzalta_ventovelmed', 'passofundo_dirvento', 'passofundo_insolacaototal', 'passofundo_nebulosmedia', 'passofundo_precipitacao', 'passofundo_pressaomed', 'passofundo_tempmax', 'passofundo_tempmedcomp', 'passofundo_tempmin', 'passofundo_umidade', 'passofundo_ventovelmax', 'passofundo_ventovelmed', 'santamaria_dirvento', 'santamaria_insolacaototal', 'santamaria_nebulosmedia', 'santamaria_precipitacao', 'santamaria_pressaomed', 'santamaria_tempmax', 'santamaria_tempmedcomp', 'santamaria_tempmin', 'santamaria_umidade', 'santamaria_ventovelmax', 'santamaria_ventovelmed'
)

write.csv(dados_consolidados, "dadosinmett.csv")


