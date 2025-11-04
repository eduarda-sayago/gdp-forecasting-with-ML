# ================================================
# ---------Main - Quarterly Analysis - GDP-RS-----
# ================================================

library(dplyr)
library(lubridate)

# ================================================
# ------------------Functions---------------------
# ================================================

source("01_get_Data.R")
source("02_get_Quarterly_Data.R")
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

datasetq <- readRDS("dataset_gdp.rds") # And skip to --Forecasting--
                                       # Or run lines 30:68

rawm_gdp <- readRDS("Data/base_NSA.rds")
rawq_gdp <- read.csv2("Data/quarterly_NSA.csv")

# ================================================
# --------Transforming to Quarterly data----------
# ================================================
message("[2] Aggregating monthly series to quarterly and merging datasets")

rawm_gdp$date <- as.Date(rawm_gdp$date)
rawm_gdp <- to_quarterly(rawm_gdp, agg = "mean")
rawm_gdp <- rawm_gdp[1:93,]

rawq_gdp$date <- as.Date(rawq_gdp$date)
datasetq <- merge(rawq_gdp, rawm_gdp, by = "date")

# ================================================
# -----------------Preprocessing------------------
# ================================================
message("[3] Adding dummies to dataset")

dummies <- data.frame(matrix(ncol = 0, nrow = nrow(datasetq)))
dummies$quarter <- lubridate::quarter(datasetq$date)

datasetq$Q2 <- ifelse(dummies$quarter == 2, 1, 0)
datasetq$Q3 <- ifelse(dummies$quarter == 3, 1, 0)
datasetq$Q4 <- ifelse(dummies$quarter == 4, 1, 0)

datasetq$d_pandemic <- ifelse(datasetq$date >= as.Date("2020-03-01") &
                               datasetq$date <= as.Date("2020-06-01"), 1, 0)
datasetq$d_shift <- ifelse(datasetq$date < as.Date("2013-03-01"),
                          seq_len(sum(datasetq$date < as.Date("2013-03-01"))),0)

#saveRDS(datasetq, "dataset_gdp.rds")

dateq = datasetq$date
datasetq$date <- NULL
datasetq[] <- lapply(datasetq, as.numeric)

rm(rawm_gdp, rawq_gdp, dummies)

# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
#message("[3] Calculating Mean model")

#mean_model <- call_models(datasetq, 'Mean', get_mean, "pib_rs")

#=====
message("[4] Calculating Benchmark (SARIMA) model")

benchmarkq <- call_models(datasetq, 'SARIMA', get_sarima, "pib_rs")
# h=1 RMSE: 10.13834; MAE: 7.387079; MAPE: 5.463893
# h=4 RMSE: 11.06123; MAE: 7.043361; MAPE: 5.09347

#=====
message("[5] Calculating LASSO model")

lasso_modelq <- call_models(datasetq, 'LASSO', get_lasso, "pib_rs")
# h=1 RMSE: 5.361284 ; MAE: 4.273517; MAPE: 3.189909 
# h=4 RMSE: 7.646238 ; MAE: 5.868104; MAPE: 4.320331    

#=====
message("[6] Calculating Elastic Net model")

enet_modelq <- call_models(datasetq, 'Elastic Net', get_elasticnet, "pib_rs")
# h=1 RMSE: 5.970406  ; MAE: 4.545163; MAPE: 3.388999 
# h=4 RMSE: 7.632532  ; MAE: 5.898247 ; MAPE: 4.343466

#=====
message("[7] Calculating Random Forest model")

rf_modelq <- call_models(datasetq, 'Random Forest', get_rf, "pib_rs")
# h=1 RMSE: 7.479729  ; MAE: 4.601721  ; MAPE: 3.325140  
# h=4 RMSE: 7.506680  ; MAE: 5.593859  ; MAPE: 4.095269 

# ================================================
# ---------------Diebold-Mariano test-------------
# ================================================
message("[8] Computing Diebold-Mariano test")

yq <- datasetq$`pib_rs`[66:93]
dm_tests <- compute_dm(model_names = c("LASSO", "Elastic Net", "Random Forest"),
                           model_dataframes = list(lasso_modelq, enet_modelq, rf_modelq),
                           horizons = c(1, 4),
                           orig_data = yq)

# ================================================
# ------Performance evaluation through CSFE-------
# ================================================
message("[9] Evaluating through CSFE (Welch and Goyal, 2008)")

qcsfe_lasso = csfe(lasso_modelq, benchmarkq, yq)
qcsfe_enet = csfe(enet_modelq, benchmarkq, yq)
qcsfe_rf = csfe(rf_modelq, benchmarkq, yq)

qcsfe_lasso <- as.data.frame(qcsfe_lasso)
qcsfe_enet <- as.data.frame(qcsfe_enet)
qcsfe_rf <- as.data.frame(qcsfe_rf)

# ================================================
# --------------------Graphs----------------------
# ================================================
message("[10] Collecting results for graph building (see 10_get_Graphs.R)")

y_ax <- dateq[66:93]
csfe_q <- data.frame(date = y_ax,
                      lasso_h1 = qcsfe_lasso$h1,
                      lasso_h4 = qcsfe_lasso$h4,
                      enet_h1 = qcsfe_enet$h1,
                      enet_h4 = qcsfe_enet$h4,
                      rf_h1 = qcsfe_rf$h1,
                      rf_h4 = qcsfe_rf$h4) 

forecastq_bench <- as.data.frame(benchmarkq$forecasts)
forecastq_lasso <- as.data.frame(lasso_modelq$forecasts)
forecastq_enet <- as.data.frame(enet_modelq$forecasts)
forecastq_rf <- as.data.frame(rf_modelq$forecasts)

results_q <- data.frame(date = y_ax,
                        GDP_RS = yq,
                        benchmark_h1 = forecastq_bench$init,
                        benchmark_h4 = forecastq_bench$V2,
                        lasso_h1 = forecastq_lasso$init,
                        lasso_h4 = forecastq_lasso$V2,
                        enet_h1 = forecastq_enet$init,
                        enet_h4 = forecastq_enet$V2,
                        rf_h1 = forecastq_rf$init,
                        rf_h4 = forecastq_rf$V2)

# c("#F57C00", "#1ABC9C", "#1F497D")

message("Process finished. Thank you!")
# ================================================