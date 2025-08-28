####### MAIN ########

base_SA <- readRDS("base_SA.rds")
base_NSA <- readRDS("base_NSA.rds")

base_NSA <- base_NSA[-((nrow(base_NSA)-1):nrow(base_NSA)), ] # removendo linhas vazias
sum(is.na(base_NSA))

####### Make series Stationary ########

source("get_stationarity_SW.R")

metadata_sw <- as.data.frame(read.csv2("Stock_watson.csv"))

data_set <- get_stationary_SW(base_NSA, metadata_sw)
series_results <- data_set$info
dataset <- do.call(cbind, data_set$results) %>% as.data.frame()


######## Make series Quarterly #####

quarter_ds <- aggregate_to_quarterly(data_set$results, data_set$info)
quarter_ds <- as.data.frame(quarter_ds)
