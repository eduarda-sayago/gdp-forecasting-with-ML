# ================================================
# ---------Main - Monthly Analysis - IBCR-RS------
# ================================================

library(dplyr)
library(lubridate)

# ================================================
# ------------------Functions---------------------
# ================================================

source("01_get_Data.R")
source("03_get_Log_Transformations.R")
source("04_get_Data_Prep.R")
source("05_get_Rolling_Window.R")
source("06_get_Models.R")
source("07_call_Model.R")
source("08_Compute_DM.R")
source("09_Performance_csfe.R")

# ================================================
# ---------------Calling Dataset------------------
# ================================================
message("[1] Loading data")

## ORIGINAL, UNTRANSFORMED DATA.
rawm_ibc <- readRDS("rawM_ibc.rds")

# ================================================
# -----------------Preprocessing------------------
# ================================================
message("[2] Applying log transformation")

rawm_ibc_log <- get_logs(rawm_ibc)
type_dfm <- rawm_ibc_log$type_df
datasetm <- do.call(cbind, rawm_ibc_log$results) %>% as.data.frame()
datasetm$date <- as.Date(datasetm$date, origin = "1970-01-01")

# ================================================
# --------------Addition of dummies---------------
# ================================================
message("[3] Adding dummies to dataset")

dummies <- data.frame(matrix(ncol = 0, nrow = nrow(datasetm)))
dummies$month <- lubridate::month(datasetm$date)

datasetm$M2 <- ifelse(dummies$month == 2, 1, 0)
datasetm$M3 <- ifelse(dummies$month == 3, 1, 0)
datasetm$M4 <- ifelse(dummies$month == 4, 1, 0)
datasetm$M5 <- ifelse(dummies$month == 5, 1, 0)
datasetm$M6 <- ifelse(dummies$month == 6, 1, 0)
datasetm$M7 <- ifelse(dummies$month == 7, 1, 0)
datasetm$M8 <- ifelse(dummies$month == 8, 1, 0)
datasetm$M9 <- ifelse(dummies$month == 9, 1, 0)
datasetm$M10 <- ifelse(dummies$month == 10, 1, 0)
datasetm$M11 <- ifelse(dummies$month == 11, 1, 0)
datasetm$M12 <- ifelse(dummies$month == 12, 1, 0)
datasetm$d_pandemic <- ifelse(datasetm$date >= as.Date("2020-01-01") &
                                    datasetm$date <= as.Date("2020-10-01"), 1, 0)
datasetm$d_shift <- ifelse(datasetm$date < as.Date("2013-01-01"), 
                               seq_len(sum(datasetm$date < as.Date("2013-01-01"))),0)

saveRDS(datasetm, "Dataset_MonthlyAnalysis.rds")
datasetm <- readRDS("Dataset_MonthlyAnalysis.rds")

datem = datasetm$date
datasetm$date <- NULL
datasetm[] <- lapply(datasetm, as.numeric)

rm(dummies, rawm_ibc_log)

# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
#message("[3] Calculating Mean model")

#mean_model <- call_models(datasetm, 'Mean', get_mean, "ibc_rs")

#=====
message("[4] Calculating Benchmark (SARIMA) model")

benchmark <- call_models1(datasetm, 'SARIMA', get_sarima, "ibc_rs")
# h=1  RMSE: 0.06738957; MAE: 0.05007342; MAPE: 1.076639  
# h=12 RMSE: 0.09635736; MAE: 0.06892825; MAPE: 1.478484 

#=====
message("[5] Calculating LASSO model")

lasso_model <- call_models1(datasetm, 'LASSO', get_lasso, "ibc_rs")
# h=1  RMSE: 0.04931252; MAE: 0.02732362; MAPE: 0.5921543  
# h=12 RMSE: 0.05768894; MAE: 0.04170441; MAPE: 0.9073074 

#=====
message("[6] Calculating Elastic Net model")

enet_model <- call_models1(datasetm, 'Elastic Net', get_elasticnet, "ibc_rs")
# h=1  RMSE: 0.05072413; MAE: 0.03758021; MAPE: 0.8103573  
# h=12 RMSE: 0.06213666; MAE: 0.04957504; MAPE: 1.0750708 

#=====
message("[7] Calculating Random Forest model")

rf_model <- call_models1(datasetm, 'Random Forest', get_rf, "ibc_rs")
# h=1  RMSE: 0.04854916; MAE: 0.03360036; MAPE: 0.7254949  
# h=12 RMSE: 0.05374739; MAE: 0.03919684; MAPE: 0.8461323 

# ================================================
# ---------------Diebold-Mariano test-------------
# ================================================
message("[8] Computing Diebold-Mariano test")

ym <- datasetm$`ibc_rs`[189:269]
dm_tests_ibc <- compute_dm1(model_names = c("LASSO", "Elastic Net", "Random Forest"),
                           model_dataframes = list(lasso_model, enet_model, rf_model),
                           horizons = c(1, 12),
                           orig_data = ym)

# ================================================
# ------Performance evaluation through CSFE-------
# ================================================
message("[9] Evaluating through CSFE (Welch and Goyal, 2008)")

mcsfe_lasso = csfe1(lasso_model, benchmark, ym)
mcsfe_enet = csfe1(enet_model, benchmark, ym)
mcsfe_rf = csfe1(rf_model, benchmark, ym)

mcsfe_lasso <- as.data.frame(mcsfe_lasso)
mcsfe_enet <- as.data.frame(mcsfe_enet)
mcsfe_rf <- as.data.frame(mcsfe_rf)

# ================================================
# -----------------DF - Graphs--------------------
# ================================================
message("[10] Collecting results for graph building (see 10_get_Graphs.R)")

y_axis <- datem[189:269]
csfe_m <- data.frame(date = y_axis,
                      lasso_h1 = mcsfe_lasso$h1,
                      lasso_h12 = mcsfe_lasso$h12,
                      enet_h1 = mcsfe_enet$h1,
                      enet_h12 = mcsfe_enet$h12,
                      rf_h1 = mcsfe_rf$h1,
                      rf_h12 = mcsfe_rf$h12) 

forecast_bench <- as.data.frame(benchmark$forecasts)
forecast_lasso <- as.data.frame(lasso_model$forecasts)
forecast_enet <- as.data.frame(enet_model$forecasts)
forecast_rf <- as.data.frame(rf_model$forecasts)

results_m <- data.frame(date = y_axis,
                        ibcrs_h1 = ym,
                        ibcrs_h12 = ym,
                        benchmark_h1 = forecast_bench$init,
                        benchmark_h12 = forecast_bench$V2,
                        lasso_h1 = forecast_lasso$init,
                        lasso_h12 = forecast_lasso$V2,
                        enet_h1 = forecast_enet$init,
                        enet_h12 = forecast_enet$V2,
                        rf_h1 = forecast_rf$init,
                        rf_h12 = forecast_rf$V2)

# c("#F57C00", "#1ABC9C", "#1F497D")

message("Process finished. Thank you!")
# ================================================