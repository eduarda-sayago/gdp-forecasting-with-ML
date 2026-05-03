# Preenchimento NA dados meteorologicos

# Pacotes necessários
library(forecast)
library(dplyr)
library(tidyr)
library(zoo)
library(tibble)

# Passo 1: Converter a coluna de data
dados_consolidados$`Data Medicao` <- as.yearmon(dados_consolidados$`Data Medicao`)

# Passo 2: Interpolação em cada coluna (exceto a data)
# Guardar a data separadamente
datas <- dados_consolidados$`Data Medicao`

# Aplicar na.interp() em todas as colunas, exceto a data
df_interp <- dados_consolidados[, -1] |> 
  lapply(function(x) {
    ts_x <- ts(x, frequency = 12)   # define a série mensal
    as.numeric(na.interp(ts_x))     # interpolação automática
  }) |> 
  as_tibble()

# Passo 3: Reunir tudo em um novo data.frame
df_final <- bind_cols(`Data Medicao` = datas, df_interp)

# Visualizar
print(head(df_final))

write.csv(df_final, "meteorologicos_mensal.csv")
