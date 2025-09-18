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
#rm(setdiff(list(c("ipea", ))))

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
dummies <- data.frame(matrix(ncol = 0, nrow = 92))
dummies$quarter <- lubridate::quarter(dataset$date)
dataset$Q2 <- ifelse(dummies$quarter == 2, 1, 0)
dataset$Q3 <- ifelse(dummies$quarter == 3, 1, 0)
dataset$Q4 <- ifelse(dummies$quarter == 4, 1, 0)
dataset$d_pandemic <- ifelse(dataset$date >= as.Date("2020-03-01") &
                               dataset$date <= as.Date("2020-06-01"), 1, 0)
dataset$d_rsflood <- ifelse(dataset$date == as.Date("2024-06-01"), 1, 0)

saveRDS(dataset, "dataset.rds")

date = dataset$date
dataset$date <- NULL
dataset[] <- lapply(dataset, as.numeric)


# --- Making sure data is stationary
message("[6/7] Checking stationarity (ADF ndiffs) on merged dataset...")


test <- get_stationarity(dataset)
rm(list= c("test"))




# ================================================
# --------------Test data handling----------------
# ================================================

df <- readRDS("df_test.rds")

n_obs <- nrow(df)
dummies <- data.frame(matrix(ncol = 0, nrow = n_obs))
dummies$quarter <- lubridate::quarter(df$date)
df$Q2 <- ifelse(dummies$quarter == 2, 1, 0)

datest = df$date
df$date <- NULL
df[] <- lapply(df, as.numeric)

# dataprep_test <- dataprep(type = 'default', ind = 1:80, df = df, 
#                           variable = 'y', horizon = 4, n_lags = 2, dum_cols = "Q2") 
# x_in <- as.data.frame(dataprep_test[["x_in"]])
# x_out <- as.data.frame(dataprep_test[["x_out"]])
# y_in <- as.data.frame(dataprep_test[["y_in"]])






# ================================================
# -----------------Forecasting--------------------
# ================================================

message("Mean")
# Mean model
#mean_model <- call_models(df, 'Mean (test)', get_mean, "y")
mean_model <- call_models(dataset, 'Mean', get_mean, "pib_rs")
# h=1 RMSE: 0.08924119; MAE: 0.08880100
# h=4 RMSE: 0.06789313; MAE: 0.06757122


message("SARIMA")
# Sarima model
#sarima_model <- call_models(df, 'Sarima (test)', get_sarima, "y")
sarima_model <- call_models(dataset, 'Sarima', get_sarima, "pib_rs")
# h=1 RMSE: 0.07011117; MAE: 0.12220252 
# h=4 RMSE: 0.04844195; MAE: 0.10155142 

sarimah1 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = sarima_model$forecasts[,1])
sarimah4 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = sarima_model$forecasts[,2])

# anchor (value before first diff row)
y0 <- 129.1651999
log_y0 <- log(y0)

# transformed series in the dataframe (assumed to be diff(log(y)))
z_orig  <- sarimah1$original
z_pred  <- sarimah1$predito

# undo differencing: cumulative sum starting at log(y0)
log_recon_orig <- log_y0 + cumsum(z_orig)
log_recon_pred <- log_y0 + cumsum(z_pred)

# undo log
y_recon_orig <- exp(log_recon_orig)
y_recon_pred <- exp(log_recon_pred)


# add back to dataframe
sarimah1$orig_recon  <- y_recon_orig
sarimah1$pred_recon  <- y_recon_pred

plot(tail(data_q[,2], 28), sarimah1$orig_recon)

matplot(sarimah1$date, sarimah1[, c("orig_recon", "pred_recon")], 
        type = "l", lty = 1, lwd = 2, col = c("black","red"),
        ylab = "Value", xlab = "Date", main = "Observed vs Forecast")
legend("topleft", legend = c("Original", "Forecast"),
       col = c("black","red"), lty = 1, lwd = 2)

message("LASSO")
# Lasso model
#lasso_model <- call_models(df, 'Lasso (test)', get_lasso, "y")
lasso_model <- call_models(dataset, 'Lasso', get_lasso, "pib_rs")
# h=1 RMSE: 0.05619033; MAE: 0.06089886  
# h=4 RMSE: 0.03669128; MAE: 0.04466303 


message("Elastic Net")
# Elastic net model
#enet_model <- call_models(df, 'Enet', get_elasticnet, "y")
enet_model <- call_models(dataset, 'Enet', get_elasticnet, "pib_rs")
# h=1 RMSE: 0.05440103; MAE: 0.05253469   
# h=4 RMSE: 0.04112094; MAE: 0.03919072 

message("Random Forest")
# Random Forest model
#rf_model1 <- call_models(df, 'RandomForestOOB', get_rforest, "y")
#rf_model2 <- call_models(df, 'RandomForestCV', get_rf, "y")

# ================================================