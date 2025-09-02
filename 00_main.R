####### MAIN ########

# ================================================
# ------------------Packages----------------------
# ================================================

library(dplyr)
library(lubridate)

# ================================================
# ------------------Functions---------------------
# ================================================

source("02_get_Stationary_SW.R")
source("03_get_Quarterly_Data.R")
source("04_get_Data_Prep.R")
source("05_get_Rolling_Window.R")
source("06_get_Models.R")
source("07_call_Model.R")

# ================================================
# ---------------Calling Dataset------------------
# ================================================

data_m <- readRDS("Data/base_NSA.rds")
data_q <- read.csv2("Data/quarterly_NSA.csv")
data_q$date <- as.Date(data_q$date)

data_m <- data_m[-((nrow(data_m)-1):nrow(data_m)), ] # remove empty rows
sum(is.na(data_m))
sum(is.na(data_q))

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================

sw_list <- as.data.frame(read.csv2("Stock_watson.csv"))

stdata_m <- get_stationary_SW(data_m, sw_list)
info_stm <- stdata_m$info
datamon <- do.call(cbind, stdata_m$results) %>% as.data.frame()

stdata_q <- get_stationary_SW(data_q, sw_list)
info_stq <- stdata_q$info
dataqrt <- do.call(cbind, stdata_q$results) %>% as.data.frame()

# ================================================
# --------Transforming to Quarterly data----------
# ================================================


quarter_ds <- aggregate_to_quarterly(stdata_m$results, stdata_m$info)


# plot(quarter_ds[["results"]][["bage_precipitacao"]], type = "l") #for list within list
# plot(data_q$pib_rs, type = "l") #for dataframe

# ================================================
# --------------Merging datasets------------------
# ================================================

mq_results <- do.call(cbind, quarter_ds$results) %>% as.data.frame()
mq_results$date <- as.Date(mq_results$date, origin = "1970-01-01")
dataqrt$date <- as.Date(dataqrt$date, origin = "1970-01-01")

dataset <- merge(dataqrt, mq_results, by = "date")


#plot(data_q$pib_rs, type = "l")
#plot(dataqrt$pib_rs, type = "l")

# ------- Making sure data is stationary ---------

test <- get_stationarity(dataset)
#rm("test")

# ================================================
# -----------------Forecasting--------------------
# ================================================


prepresult <- dataprep(type = 'tb', ind = 1:80, df = df, variable = 'variable', horizon = 1, n_lags = 4)

df <- data.frame(data = 1:100, variable = rnorm(100))
result <- rolling_window(fn = get_sarima, df = df, nwindow = 5, horizon = 1, variable = 'variable')


#BENCHMARK
benchmark = call_models(data_q, 'Sarima', get_sarima, "pib_rs")
benchmark = get_sarima(ind = 1:100, df = my_data, variable = "sales", horizon = 10, n_lags = 4)

