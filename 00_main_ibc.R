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

## ORIGINAL, UNTRANSFORMED DATA.
rawm_ibc <- readRDS("rawM_ibc.rds")
raww_ibc <- readRDS("rawW_ibc.rds")

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================
message("[2] Applying log transforms")

rawm_ibc_log <- get_logs(rawm_ibc)
type_dfm <- rawm_ibc_log$type_df
logm_results <- do.call(cbind, rawm_ibc_log$results) %>% as.data.frame()

# Diagnosic: remove santavpalmar insolacaototal - possibly contains mistakes in data

raww_ibc_log <- get_logs(raww_ibc)
type_dfw <- raww_ibc_log$type_df
logw_results <- do.call(cbind, raww_ibc_log$results) %>% as.data.frame()

# ================================================
# --------------Stationarity tests----------------
# ================================================

message("[3] Applying ADF test and differencing")

rawm_stry <- get_stationarity(logm_results, type_df = type_dfm)
raww_stry <- get_stationarity(logw_results, type_df = type_dfw)

type_dfm <- rawm_stry$type_df
rawm_stry <- as.data.frame(rawm_stry$df)

type_dfw <- raww_stry$type_df
raww_stry <- as.data.frame(raww_stry$df)

message("[4] Applying seasonal differencing")

datasetm <- get_seas_stationarity(logm_results, type_df = type_dfm, freq = 12)
datasetw <- get_seas_stationarity(logw_results, type_df = type_dfw, freq = 12)

type_dfm <- datasetm$type_df
datasetm <- as.data.frame(datasetm$df)

type_dfw <- datasetw$type_df
datasetw <- as.data.frame(datasetw$df)

# Storing and removing Date column
date = datasetm$date
dfdate = as.Date(date, origin = "1970-01-01")

datasetm$date <- NULL
datasetm[] <- lapply(datasetm, as.numeric)

datasetw$date <- NULL
datasetw[] <- lapply(datasetw, as.numeric)

rm(rawm_ibc_log, raww_ibc_log, logm_results, logw_results, raww_stry, rawm_stry)

# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
message("Mean")

mean_model <- call_models(dataset, 'Mean - PIB_RS', get_mean, "pib_rs")
# h=1 RMSE: 0.08924119 ; MAE: 0.06789313  
# h=4 RMSE: 0.08885499 ; MAE: 0.06764367  

mean_model <- call_models(datasetm, 'Mean - IBC-m', get_mean, "ibc_rs")
# h=1 RMSE: 0.1026092; MAE: 0.06800124  
# h=3 RMSE: 0.1166696; MAE: 0.08106962 
# h=6 RMSE: 0.1209282; MAE: 0.08549032 
# h=12 RMSE: 0.1220261; MAE: 0.08669113  
mean_model <- call_models(datasetw, 'Mean - IBC-w', get_mean, "ibc_rs")
# h=1 RMSE: 0.1026092  ; MAE:  0.06800124 
# h=3 RMSE: 0.1166696  ; MAE:  0.08106962 
# h=6 RMSE: 0.1209282  ; MAE:  0.08549032 
# h=12 RMSE: 0.1220261 ; MAE:  0.08669113 

#=====
message("SARIMA")

benchmark <- call_models(dataset, 'SARIMA - PIB_RS', get_sarima, "pib_rs")
# h=1 RMSE: 0.07011117; MAE: 0.04844195    
# h=4 RMSE: 0.12220252 ; MAE: 0.10155142   
benchmarkm <- call_models(datasetm, 'SARIMA - IBC-m', get_sarima, "ibc_rs")
# h=1 RMSE: 0.06629558  ; MAE:  0.04880438  
# h=3 RMSE: 0.10957398  ; MAE:  0.08588896  
# h=6 RMSE: 0.10211441  ; MAE:  0.08086174  
# h=12 RMSE: 0.10242225  ; MAE: 0.07750083 


#=====
message("LASSO")

lasso_model <- call_models(dataset, 'LASSO - PIB_RS', get_lasso, "pib_rs")
# h=1 RMSE: 0.05920526  ; MAE: 0.04084625  
# h=4 RMSE: 0.05988952  ; MAE: 0.04352576  
lasso_mmodel <- call_models(datasetm, 'LASSO - IBC-m', get_lasso, "ibc_rs")
# h=1 RMSE: 0.04012251 ; MAE:  0.03249728 
# h=3 RMSE: 0.05493627 ; MAE:  0.03931377 
# h=6 RMSE: 0.07379232 ; MAE:  0.05730493 
# h=12 RMSE: 0.06383114 ; MAE: 0.04716723 
lasso_wmodel <- call_models(datasetw, 'LASSO - IBC-w', get_lasso, "ibc_rs")
# h=1 RMSE: 0.03860373 ; MAE:  0.02976507 
# h=3 RMSE: 0.06234229 ; MAE:  0.04534379 
# h=6 RMSE: 0.09056368 ; MAE:  0.06564034 
# h=12 RMSE: 0.07399347 ; MAE: 0.05377384 

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
message("Elastic Net")

enet_model <- call_models(dataset, 'Elastic Net', get_elasticnet, "pib_rs")
# h=1 RMSE: 0.05790124 ; MAE: 0.04586020    
# h=4 RMSE: 0.12473872 ; MAE: 0.09587616 
enet_model <- call_models(datasetm, 'Elastic Net', get_elasticnet, "ibc_rs")
# h=1 RMSE: ; MAE:    
# h=4 RMSE: ; MAE:  
enet_model <- call_models(datasetw, 'Elastic Net', get_elasticnet, "ibc_rs")
# h=1 RMSE: ; MAE:    
# h=4 RMSE: ; MAE:  

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

#=====
message("Random Forest")

rf_model <- call_models(datasetm, 'Random Forest', get_rf, "ibc_rs")
# h=1 RMSE: ; MAE:     
# h=4 RMSE: ; MAE:  
rf_model <- call_models(datasetw, 'Random Forest', get_rf, "ibc_rs")
# h=1 RMSE: ; MAE:     
# h=4 RMSE: ; MAE:  

message("Boosting")

boosting_model <- call_models(datasetm, 'Boosting', get_boosting, "ibc_rs")
# h=1 RMSE: ; MAE:      
# h=4 RMSE: ; MAE:   
boosting_model <- call_models(datasetm, 'Boosting', get_boosting, "ibc_rs")
# h=1 RMSE: ; MAE:      
# h=4 RMSE: ; MAE:  

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
# ================================================
# ---------------Diebold-Mariano test-------------
# ================================================

dm_tests = compute_dm()
meandm_test = compute_dmv2()

# ================================================
# ------Performance evaluation through CSFE-------
# ================================================

csfem_lassom = csfe(lasso_mmodel, benchmarkm, datasetm$"ibc_rs"[181:257])
csfem_lassow = csfe(lasso_wmodel, benchmarkm, datasetm$"ibc_rs"[181:257])

csfem_enet = csfe(enet_model, benchmark, datasetm$"ibc_rs"[181:257])
csfem_rf = csfe(rf_model, benchmark, datasetm$"ibc_rs"[181:257])
csfem_boosting = csfe(boosting_model, benchmark, datasetm$"ibc_rs"[181:257])

csfem_lassom <- as.data.frame(csfem_lassom)
csfem_lassow <- as.data.frame(csfem_lassow)

csfem_enet <- as.data.frame(csfe_enet)
csfem_rf <- as.data.frame(csfe_rf)
csfem_boosting <- as.data.frame(csfe_boosting)

# Base R plotting
plot(dfdate[181:257], csfem_lassom$h1, type = "l", col = "orange", lwd = 2, ylim = c(-0.01, 0.056),
     ylab = "h1", xlab = "Index", main = "Comparison of h1 across models")

lines(date[181:257], csfem_lassow$h1, col = "red",   lwd = 2)
#lines(date[181:257], csfem_rf$h1,   col = "blue",  lwd = 2)
#lines(date[181:257], csfem_boosting$h1, col = "green", lwd = 2)

legend("topleft",
       legend = c("Lasso", "Elastic Net", "Random Forest", "Boosting"),
       col = c("orange", "red", "blue", "green"),
       lty = 1, lwd = 2)

# Base R plotting
plot(date[181:257], csfem_lasso$h3, type = "l", col = "orange", lwd = 2, ylim = c(0.05, 0.04),
     ylab = "h3", xlab = "Index", main = "Comparison of h4 across models")

lines(date[181:257], csfem_enet$h3, col = "red",   lwd = 2)
#lines(date[181:257], csfem_rf$h3,   col = "blue",  lwd = 2)
#lines(date[181:257], csfem_boosting$h3, col = "green", lwd = 2)

legend("topleft",
       legend = c("Lasso", "Elastic Net", "Random Forest", "Boosting"), 
       col = c("orange", "red", "blue", "green"),
       lty = 1, lwd = 2)

CSFEm <- cbind(csfem_lasso, csfem_enet, csfem_rf, csfem_boosting)

# ================================================