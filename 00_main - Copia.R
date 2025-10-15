# ================================================
# ---------------------Main-----------------------
# ================================================

library(dplyr)
library(lubridate)

# ================================================
# ------------------Functions---------------------
# ================================================

source("01_get_Data.R")
source("03_get_Quarterly_Data.R")
source("02_get_Log_Transformations.R")
source("03_get_Stationarity.R")
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

rawm_gdp <- readRDS("Data/base_NSA.rds")
rawq_gdp <- read.csv2("Data/quarterly_NSA.csv")

# ================================================
# --------Transforming to Quarterly data----------
# ================================================

message("[2] Aggregating monthly series to quarterly")

rawm_gdp$date <- as.Date(rawm_gdp$date)
rawm_gdp <- to_quarterly(rawm_gdp, agg = "mean")
rawm_gdp <- rawm_gdp[1:93,]

message("[3] Merging datasets")
rawq_gdp$date <- as.Date(rawq_gdp$date)
raw_gdp <- merge(rawq_gdp, rawm_gdp, by = "date")

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================
message("[2] Applying log transforms")

raw_gdp_log <- get_logs1(raw_gdp)
type_dfq <- raw_gdp_log$type_df
logq_results <- do.call(cbind, raw_gdp_log$results) %>% as.data.frame()

# ================================================
# --------------Stationarity tests----------------
# ================================================

message("[3] Applying ADF test and differencing")

rawq_st <- get_stationarity(logq_results, type_df = type_dfq)

type_dfq <- rawq_st$type_df
rawq_st <- as.data.frame(rawq_st$df)
rawq_st$date <- as.Date(rawq_st$date, origin = "1970-01-01")

message("[4] Applying seasonal differencing")

datasetq <- get_seas_stationarity(rawq_st, type_df = type_dfq, freq = 4)

type_dfq <- datasetq$type_df
datasetq <- as.data.frame(datasetq$df)
datasetq$date <- as.Date(datasetq$date, origin = "1970-01-01")

# ================================================
# ------------------Checkpoint--------------------
# ================================================

#Adding dummies
dummies <- data.frame(matrix(ncol = 0, nrow = nrow(raw_gdp)))
dummies$quarter <- lubridate::quarter(raw_gdp$date)

raw_gdp$Q2 <- ifelse(dummies$quarter == 2, 1, 0)
raw_gdp$Q3 <- ifelse(dummies$quarter == 3, 1, 0)
raw_gdp$Q4 <- ifelse(dummies$quarter == 4, 1, 0)

raw_gdp$d_pandemic <- ifelse(raw_gdp$date >= as.Date("2020-03-01") &
                               raw_gdp$date <= as.Date("2020-06-01"), 1, 0)
raw_gdp$d_rsflood <- ifelse(raw_gdp$date == as.Date("2024-06-01"), 1, 0)
raw_gdp$d_shift <- ifelse(raw_gdp$date < as.Date("2013-03-01"),
                          seq_len(sum(raw_gdp$date < as.Date("2013-03-01"))),0)


saveRDS(datasetq, "datasetq.rds")
rm(data_q, data_m, sw_list, stdata_m, info_stm, stdata_q, info_stq, datamon, dataqrt, quarter_ds, mq_results, dummies)


dateq = raw_gdp$date
raw_gdp$date <- NULL
raw_gdp[] <- lapply(raw_gdp, as.numeric)

# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
message("Mean")

mean_modelq <- call_models(datasetq, 'Mean', get_mean, "pib_rs")
# h=1 RMSE: 0.08924119; MAE: 0.06789313 
# h=4 RMSE: 0.08880100; MAE: 0.06757122

#=====
message("SARIMA")

benchmarkq <- call_models(raw_gdp, 'q - SARIMA', get_sarima, "pib_rs")
# h=1 RMSE: 0.07011117; MAE: 0.04844195; MAPE: 99.58476
# h=4 RMSE: 0.07980111; MAE: 0.05570668; MAPE: 127.10611 

# test <- as.data.frame(benchmark$forecasts)
# test <- cbind(date[65:92], dataset$pib_rs[65:92],test)
# library(writexl)
# write_xlsx(test, "sarimaresults.xlsx")

#=====
message("LASSO")

lasso_modelq <- call_models(datasetq, 'q - LASSO', get_lasso, "pib_rs")
# h=1 RMSE: 0.06341426  ; MAE: 0.04486765 ; MAPE: 127.1816  
# h=4 RMSE: 0.06132939  ; MAE: 0.04509913 ; MAPE: 113.4071  

rawq_st$d <- NULL

lasso_modlq <- call_models(rawq_st, 'nother - LASSO', get_lasso, "pib_rs")
# h=1 RMSE: 5.361284  ; MAE: 4.273517  ; MAPE: 3.189909   
# h=4 RMSE: 7.646238   ; MAE: 5.868104  ; MAPE: 4.320331   

# h=1 RMSE: 0.07038481   ; MAE: 0.04795207   ; MAPE: 137.1094    
# h=4 RMSE: 0.05648580    ; MAE: 0.04192961   ; MAPE: 106.4342    


#lasso_modeld <- call_models(dataset, 'LASSOd', get_lasso, "pib_rs")
# h=1 RMSE: 0.05630044; MAE: 0.03681486; MAPE: 97.17642
# h=4 RMSE: 0.06074664; MAE: 0.04515107; MAPE: 102.24471  

#=====
message("Elastic Net")

enet_modelq <- call_models(datasetq, 'q - Elastic Net', get_elasticnet, "pib_rs")
# h=1 RMSE: 0.05440103  ; MAE: 0.04112094; MAPE: 91.74626 
# h=4 RMSE: 0.05253469  ; MAE: 0.03919072 ; MAPE: 85.84087 

#=====
message("Random Forest")

rf_modelq <- call_models(datasetq, 'q - Random Forest', get_rf, "pib_rs")
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