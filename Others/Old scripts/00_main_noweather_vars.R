# ================================================
# -------Main - several vars removed version------
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

#what to keep
#rm(list = setdiff(ls(), c("",)))

# ================================================
# ---------------Calling Dataset------------------
# ================================================
message("[1/7] Loading data...")

## ORIGINAL, UNTRANSFORMED DATA.
data_m <- readRDS("Data/base_NSA.rds")
data_q <- read.csv2("Data/quarterly_NSA.csv")

meteorologicos <- c("bage_dirvento", "bage_insolacaototal", "bage_nebulosmedia", "bage_precipitacao", "bage_pressaomed", 
                    "bage_tempmax", "bage_tempmedcomp", "bage_tempmin", "bage_umidade", "bage_ventovelmax", "bage_ventovelmed", 
                    "bomjesus_dirvento", "bomjesus_insolacaototal", "bomjesus_nebulosmedia", "bomjesus_precipitacao", 
                    "bomjesus_pressaomed", "bomjesus_tempmax", "bomjesus_tempmedcomp", "bomjesus_tempmin", "bomjesus_umidade", 
                    "bomjesus_ventovelmax", "bomjesus_ventovelmed", "caxias_dirvento", "caxias_insolacaototal", "caxias_nebulosmedia", 
                    "caxias_precipitacao", "caxias_pressaomed", "caxias_tempmax", "caxias_tempmedcomp", "caxias_tempmin", "caxias_umidade", 
                    "caxias_ventovelmax", "caxias_ventovelmed", "cruzalta_dirvento", "cruzalta_insolacaototal", "cruzalta_nebulosmedia", 
                    "cruzalta_precipitacao", "cruzalta_pressaomed", "cruzalta_tempmax", "cruzalta_tempmedcomp", "cruzalta_tempmin", 
                    "cruzalta_umidade", "cruzalta_ventovelmax", "cruzalta_ventovelmed", "encruzilhada_dirvento", "encruzilhada_insolacaototal", 
                    "encruzilhada_nebulosmedia", "encruzilhada_precipitacao", "encruzilhada_pressaomed", "encruzilhada_tempmax", 
                    "encruzilhada_tempmedcomp", "encruzilhada_tempmin", "encruzilhada_umidade", "encruzilhada_ventovelmax", 
                    "encruzilhada_ventovelmed", "passofundo_dirvento", "passofundo_insolacaototal", "passofundo_nebulosmedia", 
                    "passofundo_precipitacao", "passofundo_pressaomed", "passofundo_tempmax", "passofundo_tempmedcomp", "passofundo_tempmin", 
                    "passofundo_umidade", "passofundo_ventovelmax", "passofundo_ventovelmed", "pelotas_dirvento", "pelotas_insolacaototal", 
                    "pelotas_nebulosmedia", "pelotas_precipitacao", "pelotas_pressaomed", "pelotas_tempmax", "pelotas_tempmedcomp", 
                    "pelotas_tempmin", "pelotas_umidade", "pelotas_ventovelmax", "pelotas_ventovelmed", "portoalegre_dirvento", 
                    "portoalegre_insolacaototal", "portoalegre_nebulosmedia", "portoalegre_precipitacao", "portoalegre_pressaomed", 
                    "portoalegre_tempmax", "portoalegre_tempmedcomp", "portoalegre_tempmin", "portoalegre_umidade", "portoalegre_ventovelmax", 
                    "portoalegre_ventovelmed", "santamaria_dirvento", "santamaria_insolacaototal", "santamaria_nebulosmedia", 
                    "santamaria_precipitacao", "santamaria_pressaomed", "santamaria_tempmax", "santamaria_tempmedcomp", "santamaria_tempmin", 
                    "santamaria_umidade", "santamaria_ventovelmax", "santamaria_ventovelmed", "santavpalmar_dirvento", 
                    "santavpalmar_insolacaototal", "santavpalmar_nebulosmedia", "santavpalmar_precipitacao", "santavpalmar_pressaomed", 
                    "santavpalmar_tempmax", "santavpalmar_tempmedcomp", "santavpalmar_tempmin", "santavpalmar_umidade", "santavpalmar_ventovelmax", 
                    "santavpalmar_ventovelmed", "sluizgonzaga_dirvento", "sluizgonzaga_insolacaototal", "sluizgonzaga_nebulosmedia", 
                    "sluizgonzaga_precipitacao", "sluizgonzaga_pressaomed", "sluizgonzaga_tempmax", "sluizgonzaga_tempmedcomp", 
                    "sluizgonzaga_tempmin", "sluizgonzaga_umidade", "sluizgonzaga_ventovelmax", "sluizgonzaga_ventovelmed", "torres_dirvento", 
                    "torres_insolacaototal", "torres_nebulosmedia", "torres_precipitacao", "torres_pressaomed", "torres_tempmax", 
                    "torres_tempmedcomp", "torres_tempmin", "torres_umidade", "torres_ventovelmax", "torres_ventovelmed", "uruguaiana_dirvento", 
                    "uruguaiana_insolacaototal", "uruguaiana_nebulosmedia", "uruguaiana_precipitacao", "uruguaiana_pressaomed", 
                    "uruguaiana_tempmax", "uruguaiana_tempmedcomp", "uruguaiana_tempmin", "uruguaiana_umidade", "uruguaiana_ventovelmax", 
                    "uruguaiana_ventovelmed")
data_m <- data_m[ , !(names(data_m) %in% meteorologicos)]

#Transform date column, remove empty rows
data_q$date <- as.Date(data_q$date)
data_m <- data_m[-((nrow(data_m)-1):nrow(data_m)), ]

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================
sw_list <- as.data.frame(read.csv2("Stock_watson.csv"))

message("[2/7] Applying Stock-Watson transforms (monthly)...")

stdata_m <- get_stationary_SW(data_m, sw_list)
info_stm <- stdata_m$info
datamon <- do.call(cbind, stdata_m$results) %>% as.data.frame()

message("[3/7] Applying Stock-Watson transforms (quarterly)...")

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

#dataset <- dataset[-(1:4),] #REMOVE 4 ROWS BC OF LAG 4 DIFFERENCING

saveRDS(dataset, "dataset_noweather.rds")

date = dataset$date
dataset$date <- NULL
dataset[] <- lapply(dataset, as.numeric)

# --- Making sure data is stationary
message("[6/7] Checking stationarity (ADF ndiffs) on merged dataset...")


test <- get_stationarity(dataset)
rm(list = c("test"))

# ================================================
# --------------Test data handling----------------
# ================================================

# df <- readRDS("df_test.rds")
# 
# n_obs <- nrow(df)
# dummies <- data.frame(matrix(ncol = 0, nrow = n_obs))
# dummies$quarter <- lubridate::quarter(df$date)
# df$Q2 <- ifelse(dummies$quarter == 2, 1, 0)
# 
# 
# datest = df$date
# df$date <- NULL
# df[] <- lapply(df, as.numeric)


# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
message("Mean")

mean_model <- call_models(dataset, 'Mean', get_mean, "pib_rs")
# h=1 RMSE: 0.08924119; MAE: 0.08880100
# h=4 RMSE: 0.06789313; MAE: 0.06757122

#=====
message("SARIMA")

sarima_model <- call_models(dataset, 'Sarima', get_sarima, "pib_rs")
# h=1 RMSE: 0.07011117; MAE: 0.12220252 
# h=4 RMSE: 0.04844195; MAE: 0.10155142 

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
message("LASSO")

lasso_NWmodel <- call_models(dataset, 'Lasso', get_lasso, "pib_rs")
# h=1 RMSE: 0.05619033; MAE: 0.06089886  
# h=4 RMSE: 0.03669128; MAE: 0.04466303


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

# h=1 RMSE: 0.05440103; MAE: 0.05253469   
# h=4 RMSE: 0.04112094; MAE: 0.03919072 
enet_NWmodel <- call_models(dataset, 'Enet', get_elasticnet, "pib_rs")



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

rf_NWmodel <- call_models(dataset, 'RandomForest', get_rf, "pib_rs")
# init            
# 0.05512607 0.05501403 
# init            
# 0.04061721 0.03860391          

# ================================================
# ---------------Diebold-Mariano test-------------
# ================================================

#dm_tests = compute_dm()

# ================================================
# ------Performance evaluation through CSFE-------
# ================================================

# csfe_lasso = csfe(lasso_model, sarima_model, dataset$"pib_rs"[65:92])
# csfe_lasso_tb = csfe(lasso_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# csfe_enet_tb = csfe(enet_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# 
# csfe_boosting_economic = csfe(boosting_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# csfe_lasso_economic = csfe(lasso_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# csfe_enet_economic = csfe(enet_economic, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# 
# csfe_boosting_economic_tb = csfe(boosting_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# csfe_lasso_economic_tb = csfe(lasso_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])
# csfe_enet_economic_tb = csfe(enet_economic_tb, benchmark, dataset_economic$`BZEAMOM%`[124:164])

# ================================================