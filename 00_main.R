# ================================================
# ---------------------Main-----------------------
# ================================================

library(dplyr)
library(lubridate)

# ================================================
# ------------------Functions---------------------
# ================================================

source("01_get_Data.R")
source("02_get_Stationary_SW.R")
source("03_get_Quarterly_Data.R")
source("04_get_Data_Prep.R")
source("05_get_Rolling_Window.R")
source("06_get_Models.R")
source("07_call_Model.R")
source("08_Compute_DM.R")
source("09_Performance_csfe.R")

#what to keep
#rm(list = setdiff(ls(), c("",)))


# ================================================
# ---------------Calling Dataset------------------
# ================================================
message("[1] Loading data")

data_m <- readRDS("Data/base_NSA.rds")
data_q <- read.csv2("Data/quarterly_NSA.csv")

data_q$date <- as.Date(data_q$date)

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================

sw_list <- as.data.frame(read.csv2("Stock_watson.csv"))

message("[2] Stock-Watson transforms (monthly)")

stdata_m <- get_stationary_SW(data_m, sw_list)
info_stm <- stdata_m$info

datamon <- do.call(cbind, stdata_m$results) %>% as.data.frame()

message("[3] Stock-Watson transforms (quarterly)")

stdata_q <- get_stationary_SW(data_q, sw_list)

info_stq <- stdata_q$info
dataqrt <- do.call(cbind, stdata_q$results) %>% as.data.frame()

# ================================================
# --------Transforming to Quarterly data----------
# ================================================

message("[4] Aggregating monthly series to quarterly")

quarter_ds <- aggregate_to_quarterly(stdata_m$results, stdata_m$info)

# ================================================
# --------------Merging datasets------------------
# ================================================
message("[5] Merging datasets and including dummies")

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
# dataset$d_shift <- ifelse(dataset$date < as.Date("2013-03-01"), 
#                           seq_len(sum(dataset$date < as.Date("2013-03-01"))),0)
# ================================================
# ------------------Checkpoint--------------------
# ================================================

saveRDS(dataset, "dataset.rds")
rm(data_q, data_m, sw_list, stdata_m, info_stm, stdata_q, info_stq, datamon, dataqrt, quarter_ds, mq_results, dummies)

dataset <- readRDS("dataset.rds")
date = dataset$date
dataset$date <- NULL
dataset[] <- lapply(dataset, as.numeric)

datasetn <- dataset[1:71,]
daten <- dataset$date[1:71,]
datasetn$date <- NULL
datasetn[] <- lapply(datasetn, as.numeric)
# --- Making sure data is stationary

message("[6] Checking stationarity (ADF ndiffs) on merged dataset...")
test <- get_stationarity(dataset)
rm(test)


# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
message("Mean")

mean_modelp <- call_models(dataset, 'Mean', get_mean, "pib_rs")
# h=1 RMSE: 0.08924119; MAE: 0.06789313 
# h=4 RMSE: 0.08880100; MAE: 0.06757122

#=====
message("SARIMA")

benchmarkp <- call_models(dataset, 'SARIMA', get_sarima, "pib_rs")
# h=1 RMSE: 0.07011117; MAE: 0.04844195; MAPE: 99.58476
# h=4 RMSE: 0.07980111; MAE: 0.05570668; MAPE: 127.10611 
benchmarkn <- call_models(datasetn, 'SARIMA - no pandemic', get_sarima, "pib_rs")
# h=1 RMSE: 0.03664326; MAE: 0.02755062; MAPE: 120.6778 
# h=4 RMSE: 0.03803226; MAE: 0.03033111; MAPE: 100.7422  

# test <- as.data.frame(benchmark$forecasts)
# test <- cbind(date[65:92], dataset$pib_rs[65:92],test)
# library(writexl)
# write_xlsx(test, "sarimaresults.xlsx")

#=====
message("LASSO")

lasso_modelp <- call_models(dataset, 'LASSO', get_lasso, "pib_rs")
# h=1 RMSE: 0.05619033 ; MAE: 0.03669128; MAPE: 97.0483 
# h=4 RMSE: 0.06089886 ; MAE: 0.04466303; MAPE: 101.9442 

lasso_modeln <- call_models(datasetn, 'LASSO - no pandemic', get_lasso, "pib_rs")
# h=1 RMSE: 0.04617606 ; MAE: 0.03713081 ; MAPE: 128.9973 
# h=4 RMSE: 0.04662754 ; MAE: 0.03607676 ; MAPE: 172.0447   

#=====
message("Elastic Net")

enet_modelp <- call_models(dataset, 'Elastic Net', get_elasticnet, "pib_rs")
# h=1 RMSE: 0.05440103  ; MAE: 0.04112094; MAPE: 91.74626 
# h=4 RMSE: 0.05253469  ; MAE: 0.03919072 ; MAPE: 85.84087 

enet_modeln <- call_models(datasetn, 'Elastic Net - no pandemic', get_elasticnet, "pib_rs")
# h=1 RMSE: 0.04589452  ; MAE: 0.03648708 ; MAPE: 117.5919 
# h=4 RMSE: 0.04606393 ; MAE: 0.03566886 ; MAPE: 122.5221  

#=====
message("Random Forest")

rf_modelp <- call_models(dataset, 'Random Forest', get_rf, "pib_rs")
# h=1 RMSE: 0.05637370 ; MAE: 0.04002083 ; MAPE: 92.02282 
# h=4 RMSE: 0.05071925 ; MAE: 0.03560388 ; MAPE: 84.35485

#=====
# message("Boosting")
# 
# boost_modelp <- call_models(dataset, 'Boosting', get_boosting, "pib_rs")
# h=1 RMSE: 0.05718182 ; MAE: 0.03720618 ; MAPE: 101.8835 
# h=4 RMSE: 0.06833421 ; MAE: 0.05230112 ; MAPE: 118.3273 

# ================================================
# ---------Graphs with original series------------
# ================================================

#=====
# sarimah1 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = sarima_model$forecasts[,1])
# sarimah4 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = sarima_model$forecasts[,2])
# 
# # Checking series untransformed (back to original)
# y0 <- 129.1651999
# log_y0 <- log(y0)
# z_orig  <- sarimah1$original
# z_pred  <- sarimah1$predito
# log_recon_orig <- log_y0 + cumsum(z_orig)
# log_recon_pred <- log_y0 + cumsum(z_pred)
# y_recon_orig <- exp(log_recon_orig)
# y_recon_pred <- exp(log_recon_pred)
# sarimah1$orig_recon  <- y_recon_orig
# sarimah1$pred_recon  <- y_recon_pred
# matplot(sarimah1$date, sarimah1[, c("orig_recon", "pred_recon")], 
#         type = "l", lty = 1, lwd = 2, col = c("black","red"),
#         ylab = "Value", xlab = "Date", main = "SARIMA - Observed vs Forecast")

#=====
# lassoh1 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = lasso_model$forecasts[,1])
# lassoh4 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = lasso_model$forecasts[,2])
# 
# # Checking series untransformed (back to original)
# z_orig  <- lassoh1$original
# z_pred  <- lassoh1$predito
# log_recon_orig <- log_y0 + cumsum(z_orig)
# log_recon_pred <- log_y0 + cumsum(z_pred)
# y_recon_orig <- exp(log_recon_orig)
# y_recon_pred <- exp(log_recon_pred)
# lassoh1$orig_recon  <- y_recon_orig
# lassoh1$pred_recon  <- y_recon_pred
# matplot(lassoh1$date, lassoh1[, c("orig_recon", "pred_recon")], 
#         type = "l", lty = 1, lwd = 2, col = c("black","red"),
#         ylab = "Value", xlab = "Date", main = "LASSO - Observed vs Forecast")

#=====
# eneth1 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = enet_model$forecasts[,1])
# eneth4 <- data.frame(date = tail(date, 28), original = tail(dataset[, 1], 28), predito = enet_model$forecasts[,2])
# 
# # Checking series untransformed (back to original)
# 
# z_orig  <- eneth1$original
# z_pred  <- eneth1$predito
# log_recon_orig <- log_y0 + cumsum(z_orig)
# log_recon_pred <- log_y0 + cumsum(z_pred)
# y_recon_orig <- exp(log_recon_orig)
# y_recon_pred <- exp(log_recon_pred)
# eneth1$orig_recon  <- y_recon_orig
# eneth1$pred_recon  <- y_recon_pred
# matplot(eneth1$date, eneth1[, c("orig_recon", "pred_recon")], 
#         type = "l", lty = 1, lwd = 2, col = c("black","red"),
#         ylab = "Value", xlab = "Date", main = "Elastic Net - Observed vs Forecast")


# ================================================
# ---------------Diebold-Mariano test-------------
# ================================================

yp <- dataset$`pib_rs`[65:92]
dm_tests <- compute_dm(model_names = c("LASSO", "Elastic Net", "Random Forest"),
                           model_dataframes = list(lasso_modelp, enet_modelp, rf_modelp),
                           horizons = c(1, 4),
                           orig_data = yp)

#meandm_test = compute_dmv2()

# ================================================
# ------Performance evaluation through CSFE-------
# ================================================

csfep_lasso = csfe1(lasso_modelp, benchmarkp, yp)
csfep_enet = csfe1(enet_modelp, benchmarkp, yp)
csfep_rf = csfe1(rf_modelp, benchmarkp, yp)
#csfep_boosting = csfe1(boost_modelp, benchmarkp, yp)

csfep_lasso <- as.data.frame(csfep_lasso)
csfep_enet <- as.data.frame(csfep_enet)
csfep_rf <- as.data.frame(csfep_rf)
#csfep_boosting <- as.data.frame(csfep_boosting)

# ================================================
# --------------------Graphs----------------------
# ================================================
y_ax <- date[65:92]
CSFE_q <- data.frame(date = y_ax,
                      lasso_h1 = csfep_lasso$h1,
                      lasso_h12 = csfep_lasso$h4,
                      enet_h1 = csfep_enet$h1,
                      enet_h12 = csfep_enet$h4,
                      rf_h1 = csfep_rf$h1,
                      rf_h12 = csfep_rf$h4) 

# c("#F57C00", "#1ABC9C", "#1F497D")

# ================================================