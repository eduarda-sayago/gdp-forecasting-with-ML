# ================================================
# ---------------------Main-----------------------
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
#saveRDS(dataset, "qdataset.rds")

# plot(quarter_ds[["results"]][["bage_precipitacao"]], type = "l") #for list within list
# plot(data_q$pib_rs, type = "l") #for dataframe

# ================================================
# --------------Merging datasets------------------
# ================================================

mq_results <- do.call(cbind, quarter_ds$results) %>% as.data.frame()
mq_results$date <- as.Date(mq_results$date, origin = "1970-01-01")
dataqrt$date <- as.Date(dataqrt$date, origin = "1970-01-01")

dataset <- merge(dataqrt, mq_results, by = "date")

#Adding dummies
dataset$d_pandemic <- ifelse(dataset$date >= as.Date("2020-03-01") &
                               dataset$date <= as.Date("2020-06-01"), 1, 0)
dataset$d_rsflood <- ifelse(dataset$date == as.Date("2024-06-01"), 1, 0)

## seasonal
df <- data.frame(1:92)

df$quarter <- lubridate::quarter(dataset$date)
dataset$Q2 <- ifelse(df$quarter == 2, 1, 0)
dataset$Q3 <- ifelse(df$quarter == 3, 1, 0)
dataset$Q4 <- ifelse(df$quarter == 4, 1, 0)

#plot(data_q$pib_rs, type = "l")
#plot(dataqrt$pib_rs, type = "l")

# --- Making sure data is stationary

test <- get_stationarity(dataset)
#rm(list= c("test"))

# ================================================
# -----------------Forecasting--------------------
# ================================================

# Benchmark (SARIMA)
benchmark = call_models(dataset, 'Sarima', get_sarima, "pib_rs")

# Lasso model
lasso_model = call_models(dataset, 'Lasso', get_lasso, "pib_rs")

# Elastic net model
enet_model = call_models(dataset, 'Enet', get_elasticnet, "pib_rs")

# Random Forest model
rf_model1 = call_models(dataset, 'RandomForestOOB', get_rforest, "pib_rs")
rf_model2 = call_models(dataset, 'RandomForestCV', get_rf, "pib_rs")
rf_model3 = call_models(dataset, 'RF 3', get_random_forest, "pib_rs")

# Neural Networks model


# ================================================