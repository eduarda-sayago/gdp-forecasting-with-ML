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
rm(setdiff(list(c("ipea", ))))

# ================================================
# ---------------Calling Dataset------------------
# ================================================
message("[1/7] Loading data...")

data_m <- readRDS("Data/base_NSA.rds")
data_q <- read.csv2("Data/quarterly_NSA.csv")

data_q$date <- as.Date(data_q$date)

data_m <- data_m[-((nrow(data_m)-1):nrow(data_m)), ] # remove empty rows
sum(is.na(data_m))
sum(is.na(data_q))

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================
message("[2/7] Applying Stock-Watson transforms (monthly)...")

sw_list <- as.data.frame(read.csv2("Stock_watson.csv"))


message("[3/7] Applying Stock-Watson transforms (quarterly)...")

stdata_m <- get_stationary_SW(data_m, sw_list)
info_stm <- stdata_m$info
datamon <- do.call(cbind, stdata_m$results) %>% as.data.frame()

stdata_q <- get_stationary_SW(data_q, sw_list)
info_stq <- stdata_q$info
dataqrt <- do.call(cbind, stdata_q$results) %>% as.data.frame()

# ================================================
# --------Transforming to Quarterly data----------
# ================================================
message("[4/7] Aggregating monthly series to quarterly...")

quarter_ds <- aggregate_to_quarterly(stdata_m$results, stdata_m$info)


# plot(quarter_ds[["results"]][["bage_precipitacao"]], type = "l") #for list within list
# plot(data_q$pib_rs, type = "l") #for dataframe

# ================================================
# --------------Merging datasets------------------
# ================================================
message("[5/7] Merging datasets and building features...")

mq_results <- do.call(cbind, quarter_ds$results) %>% as.data.frame()
mq_results$date <- as.Date(mq_results$date, origin = "1970-01-01")
dataqrt$date <- as.Date(dataqrt$date, origin = "1970-01-01")

dataset <- merge(dataqrt, mq_results, by = "date")


#Adding dummies
n_obs <- nrow(dataset)
dummies <- data.frame(matrix(ncol = 0, nrow = n_obs))
dummies$quarter <- lubridate::quarter(dataset$date)
dataset$Q2 <- ifelse(dummies$quarter == 2, 1, 0)
dataset$Q3 <- ifelse(dummies$quarter == 3, 1, 0)
dataset$Q4 <- ifelse(dummies$quarter == 4, 1, 0)
dataset$d_pandemic <- ifelse(dataset$date >= as.Date("2020-03-01") &
                               dataset$date <= as.Date("2020-06-01"), 1, 0)
dataset$d_rsflood <- ifelse(dataset$date == as.Date("2024-06-01"), 1, 0)

saveRDS(dataset, "dataset.rds")

# --- Making sure data is stationary
message("[6/7] Checking stationarity (ADF ndiffs) on merged dataset...")


test <- get_stationarity(dataset)
rm(list= c("test"))

acf(dataset$pib_rs, lag = 24)

readrd

# ================================================
# --------------Test data handling----------------
# ================================================

ts.plot(df$y)


sw_test <- as.data.frame(read.csv2("SW_test.csv"))

df_st <- get_stationary_SW(df, sw_test)
df_info_st <- df_st$info
df_st <- do.call(cbind, df_st$results) %>% as.data.frame()

dataprep_test <- dataprep(type = 'tb', ind = 1:80, df = df, variable = 'y', horizon = 4, n_lags = 2)
x_in <- as.data.frame(dataprep_test[["x_in"]])
x_out <- as.data.frame(dataprep_test[["x_out"]])
y_in <- as.data.frame(dataprep_test[["y_in"]])



# ================================================
# -----------------Forecasting--------------------
# ================================================

message("Mean")
# Mean model
mean_model <- call_models(df, 'Mean', get_mean, "y")

message("SARIMA")
# Benchmark (SARIMA)
benchmark <- call_models(df, 'Sarima', get_sarima, "y")

message("LASSO")
# Lasso model
lasso_model <- call_models(df, 'Lasso', get_lasso, "y")

message("Elastic Net")
# Elastic net model
enet_model <- call_models(df, 'Enet', get_elasticnet, "y")

message("Random Forest")
# Random Forest model
rf_model1 <- call_models(df, 'RandomForestOOB', get_rforest, "y")
rf_model2 <- call_models(df, 'RandomForestCV', get_rf, "y")

# ================================================