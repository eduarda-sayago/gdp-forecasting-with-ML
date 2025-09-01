####### MAIN ########

# -------- Packages --------

library(dplyr)
library(lubridate)

# -------- Get data --------

data_m <- readRDS("Data/base_NSA.rds")
data_q <- read.csv2("Data/quarterly_NSA.csv")
data_q$date <- as.Date(data_q$date)

data_m <- data_m[-((nrow(data_m)-1):nrow(data_m)), ] # remove empty rows
sum(is.na(data_m))
sum(is.na(data_q))

# -------- Stationarize --------

source("get_stationary_SW.R")

sw_list <- as.data.frame(read.csv2("Stock_watson.csv"))

stdata_m <- get_stationary_SW(data_m, sw_list)
info_stm <- stdata_m$info
datamon <- do.call(cbind, stdata_m$results) %>% as.data.frame()

stdata_q <- get_stationary_SW(data_q, sw_list)
info_stq <- stdata_q$info
dataqrt <- do.call(cbind, stdata_q$results) %>% as.data.frame()

# -------- Make it quarterly --------

source("aggregate_to_quarterly.R")

quarter_ds <- aggregate_to_quarterly(stdata_m$results, stdata_m$info)


# plot(quarter_ds[["results"]][["bage_precipitacao"]], type = "l") #for list within list
# plot(data_q$pib_rs, type = "l") #for dataframe



# ------- Joining data ---------

mq_results <- do.call(cbind, quarter_ds2$results) %>% as.data.frame()
mq_results$date <- as.Date(mq_results$date, origin = "1970-01-01")
dataqrt$date <- as.Date(dataqrt$date, origin = "1970-01-01")

dataset <- merge(mq_results, dataqrt, by = "date")


#plot(data_q$pib_rs, type = "l")
#plot(dataqrt$pib_rs, type = "l")

# ------- Making sure data is stationary ---------

source("get_stationary_test.R")

test <- get_stationarity(dataset)

# -------- a --------
rm(list = c("quarter_ds2"))
