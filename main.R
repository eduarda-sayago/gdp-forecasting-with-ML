####### MAIN ########

# -------- Packages --------

library(dplyr)
library(lubridate)

# -------- Get data --------

base_NSA <- readRDS("Data/base_NSA.rds")

base_NSA <- base_NSA[-((nrow(base_NSA)-1):nrow(base_NSA)), ] # removendo linhas vazias
sum(is.na(base_NSA))

# -------- Stationarize --------

source("get_stationarity_SW.R")

metadata_sw <- as.data.frame(read.csv2("Stock_watson.csv"))

data_set <- get_stationary_SW(base_NSA, metadata_sw)
series_results <- data_set$info
dataset <- do.call(cbind, data_set$results) %>% as.data.frame()

# -------- Make it quarterly --------

source("aggregate_to_quarterly.R")

quarter_ds <- aggregate_to_quarterly(data_set$results, data_set$info)
quarter_ds2 <- aggregate_to_quarterly2(data_set$results, data_set$info)

plot(quarter_ds2[["results"]][["bage_precipitacao"]], type = "l")
plot(quarter_ds[["bage_precipitacao"]], type = "l")
plot(dataset$bage_precipitacao, type = "l")
plot(base_NSA$bage_precipitacao, type = "l")

# -------- a --------

## depois disso eu vou ter que transformar em estacionárias:
# as séries novas tri -> só tipo 03
# as séries originais tri -> aplicar função stationary
# juntar e testar no adf

