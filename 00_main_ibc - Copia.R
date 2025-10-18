# ================================================
# ---------------------Main-----------------------
# ================================================

#to do after: unify labels as "m" and "q" at beginning
#transf graphs in function
#maybe "if dont have it install packages" function

library(dplyr)
library(lubridate)

# ================================================
# ------------------Functions---------------------
# ================================================

source("01_get_Data.R")
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

## ORIGINAL, UNTRANSFORMED DATA.
rawm_ibc <- readRDS("rawM_ibc.rds")

# ================================================
# --------Preprocessing for Stationarity----------
# ================================================
message("[2] Applying log transforms")

rawm_ibc_log <- get_logs(rawm_ibc)
type_dfm <- rawm_ibc_log$type_df
logm_results <- do.call(cbind, rawm_ibc_log$results) %>% as.data.frame()

# santavpalmar insolacaototal - possibly contains mistakes in data

raww_ibc_log <- get_logs1(rawm_ibc)
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
datasetw <- get_seas_stationarity(raww_stry, type_df = type_dfw, freq = 12)

type_dfm <- datasetm$type_df
datasetm <- as.data.frame(datasetm$df)

type_dfw <- datasetw$type_df
datasetw <- as.data.frame(datasetw$df)

# Adding dummies in datasetm

logw_results$date <- as.Date(logw_results$date, origin = "1970-01-01")

dummies <- data.frame(matrix(ncol = 0, nrow = nrow(logw_results)))
dummies$month <- lubridate::month(logw_results$date)

logw_results$M2 <- ifelse(dummies$month == 2, 1, 0)
logw_results$M3 <- ifelse(dummies$month == 3, 1, 0)
logw_results$M4 <- ifelse(dummies$month == 4, 1, 0)
logw_results$M5 <- ifelse(dummies$month == 5, 1, 0)
logw_results$M6 <- ifelse(dummies$month == 6, 1, 0)
logw_results$M7 <- ifelse(dummies$month == 7, 1, 0)
logw_results$M8 <- ifelse(dummies$month == 8, 1, 0)
logw_results$M9 <- ifelse(dummies$month == 9, 1, 0)
logw_results$M10 <- ifelse(dummies$month == 10, 1, 0)
logw_results$M11 <- ifelse(dummies$month == 11, 1, 0)
logw_results$M12 <- ifelse(dummies$month == 12, 1, 0)
logw_results$d_pandemic <- ifelse(logw_results$date >= as.Date("2020-01-01") &
                               logw_results$date <= as.Date("2020-10-01"), 1, 0)
#logw_results$d_rsflood <- ifelse(logw_results$date == as.Date("2024-06-01"), 1, 0)
logw_results$d_shift <- ifelse(logw_results$date < as.Date("2013-01-01"), 
                           seq_len(sum(logw_results$date < as.Date("2013-01-01"))),0)

# Adding dummies in datasetw
# ...
#saveRDS(datasetm,"datasetm.rds")

# Storing and removing Date column
dfdate = datasetm$date
dfdate = as.Date(date, origin = "1970-01-01")

datasetm$date <- NULL
datasetm[] <- lapply(datasetm, as.numeric)

dfwdate = logw_results$date
logw_results$date <- NULL
logw_results[] <- lapply(logw_results, as.numeric)

rm(dummies, rawm_ibc_log, logm_results, rawm_stry)
#rm(raww_ibc_log, logw_results, raww_stry)

# ================================================
# -----------------1st try - logmresults ----------- old log fun
# ================================================
# logm = old log
# logw = new log
# add all dummies excpt dflood cause fuck that sht
rawm_ibc_log <- get_logs(rawm_ibc)
logm_results <- do.call(cbind, rawm_ibc_log$results) %>% as.data.frame()
logm_results$date <- as.Date(logm_results$date, origin = "1970-01-01")

dummies <- data.frame(matrix(ncol = 0, nrow = nrow(logm_results)))
dummies$month <- lubridate::month(logm_results$date)

logm_results$M2 <- ifelse(dummies$month == 2, 1, 0)
logm_results$M3 <- ifelse(dummies$month == 3, 1, 0)
logm_results$M4 <- ifelse(dummies$month == 4, 1, 0)
logm_results$M5 <- ifelse(dummies$month == 5, 1, 0)
logm_results$M6 <- ifelse(dummies$month == 6, 1, 0)
logm_results$M7 <- ifelse(dummies$month == 7, 1, 0)
logm_results$M8 <- ifelse(dummies$month == 8, 1, 0)
logm_results$M9 <- ifelse(dummies$month == 9, 1, 0)
logm_results$M10 <- ifelse(dummies$month == 10, 1, 0)
logm_results$M11 <- ifelse(dummies$month == 11, 1, 0)
logm_results$M12 <- ifelse(dummies$month == 12, 1, 0)
logm_results$d_pandemic <- ifelse(logm_results$date >= as.Date("2020-01-01") &
                                    logm_results$date <= as.Date("2020-10-01"), 1, 0)
logm_results$d_shift <- ifelse(logm_results$date < as.Date("2013-01-01"), 
                               seq_len(sum(logm_results$date < as.Date("2013-01-01"))),0)
datelm = logm_results$date
logm_results$date <- NULL
logm_results[] <- lapply(logm_results, as.numeric)

benchmark1w <- call_models1(logm_results, 'Logmold - SARIMA', get_sarima, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

lasso_model1w <- call_models1(logm_results, 'Logmold - LASSO', get_lasso, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE: 

enet_model1w <- call_models1(logm_results, 'Logmold - Elastic Net', get_elasticnet, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

rf_modelw <- call_models1(logm_results, 'Logmold Random Forest - IBC-w', get_rf, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

# ================================================
# -----------------2nd try - logwresults ----------- new log fun
# ================================================
raww_ibc_log <- get_logs1(rawm_ibc)
logw_results <- do.call(cbind, raww_ibc_log$results) %>% as.data.frame()
logw_results$date <- as.Date(logw_results$date, origin = "1970-01-01")

dummies <- data.frame(matrix(ncol = 0, nrow = nrow(logw_results)))
dummies$month <- lubridate::month(logw_results$date)

logw_results$M2 <- ifelse(dummies$month == 2, 1, 0)
logw_results$M3 <- ifelse(dummies$month == 3, 1, 0)
logw_results$M4 <- ifelse(dummies$month == 4, 1, 0)
logw_results$M5 <- ifelse(dummies$month == 5, 1, 0)
logw_results$M6 <- ifelse(dummies$month == 6, 1, 0)
logw_results$M7 <- ifelse(dummies$month == 7, 1, 0)
logw_results$M8 <- ifelse(dummies$month == 8, 1, 0)
logw_results$M9 <- ifelse(dummies$month == 9, 1, 0)
logw_results$M10 <- ifelse(dummies$month == 10, 1, 0)
logw_results$M11 <- ifelse(dummies$month == 11, 1, 0)
logw_results$M12 <- ifelse(dummies$month == 12, 1, 0)
logw_results$d_pandemic <- ifelse(logw_results$date >= as.Date("2020-01-01") &
                                    logw_results$date <= as.Date("2020-10-01"), 1, 0)
#logw_results$d_rsflood <- ifelse(logw_results$date == as.Date("2024-06-01"), 1, 0)
logw_results$d_shift <- ifelse(logw_results$date < as.Date("2013-01-01"), 
                               seq_len(sum(logw_results$date < as.Date("2013-01-01"))),0)
datelw = logw_results$date
logw_results$date <- NULL
logw_results[] <- lapply(logw_results, as.numeric)

benchmark2w <- call_models1(logw_results, 'Logwnew - SARIMA', get_sarima, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE: 

lasso_model2w <- call_models1(logw_results, 'Logwnew - LASSO', get_lasso, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

enet_model2w <- call_models1(logw_results, 'Logwnew - Elastic Net', get_elasticnet, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

#rf_modelw <- call_models1(logw_results, 'Random Forest - IBC-w', get_rf, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

# ================================================
# -----------------3rd try - rawm_ibc ----------- no log, raw data
# ================================================

rawm_ibc$date <- as.Date(rawm_ibc$date, origin = "1970-01-01")

dummies <- data.frame(matrix(ncol = 0, nrow = nrow(rawm_ibc)))
dummies$month <- lubridate::month(rawm_ibc$date)

rawm_ibc$M2 <- ifelse(dummies$month == 2, 1, 0)
rawm_ibc$M3 <- ifelse(dummies$month == 3, 1, 0)
rawm_ibc$M4 <- ifelse(dummies$month == 4, 1, 0)
rawm_ibc$M5 <- ifelse(dummies$month == 5, 1, 0)
rawm_ibc$M6 <- ifelse(dummies$month == 6, 1, 0)
rawm_ibc$M7 <- ifelse(dummies$month == 7, 1, 0)
rawm_ibc$M8 <- ifelse(dummies$month == 8, 1, 0)
rawm_ibc$M9 <- ifelse(dummies$month == 9, 1, 0)
rawm_ibc$M10 <- ifelse(dummies$month == 10, 1, 0)
rawm_ibc$M11 <- ifelse(dummies$month == 11, 1, 0)
rawm_ibc$M12 <- ifelse(dummies$month == 12, 1, 0)
rawm_ibc$d_pandemic <- ifelse(rawm_ibc$date >= as.Date("2020-01-01") &
                                rawm_ibc$date <= as.Date("2020-10-01"), 1, 0)
#rawm_ibc$d_rsflood <- ifelse(rawm_ibc$date == as.Date("2024-06-01"), 1, 0)
rawm_ibc$d_shift <- ifelse(rawm_ibc$date < as.Date("2013-01-01"), 
                               seq_len(sum(rawm_ibc$date < as.Date("2013-01-01"))),0)
daterm = rawm_ibc$date
rawm_ibc$date <- NULL
rawm_ibc[] <- lapply(rawm_ibc, as.numeric)

benchmark3w <- call_models1(rawm_ibc, 'Rawdata - SARIMA', get_sarima, "ibc_rs")  
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

lasso_model3w <- call_models1(rawm_ibc, 'Rawdata - LASSO', get_lasso, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

enet_model3w <- call_models1(rawm_ibc, 'Rawdata - Elastic Net', get_elasticnet, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

#rf_modelw <- call_models1(rawm_ibc, 'Random Forest - IBC-w', get_rf, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

# ================================================
# -----------------4th try - rawm_stry --------- no log fun, but adf test
# ================================================
rawm_ibc <- readRDS("rawM_ibc.rds")

rawm_stry <- get_stationarity(rawm_ibc, type_df = type_dfm)
type_dfm <- rawm_stry$type_df
rawm_stry <- as.data.frame(rawm_stry$df)

rawm_stry$date <- as.Date(rawm_stry$date, origin = "1970-01-01")


dummies <- data.frame(matrix(ncol = 0, nrow = nrow(rawm_stry)))
dummies$month <- lubridate::month(rawm_stry$date)

rawm_stry$M2 <- ifelse(dummies$month == 2, 1, 0)
rawm_stry$M3 <- ifelse(dummies$month == 3, 1, 0)
rawm_stry$M4 <- ifelse(dummies$month == 4, 1, 0)
rawm_stry$M5 <- ifelse(dummies$month == 5, 1, 0)
rawm_stry$M6 <- ifelse(dummies$month == 6, 1, 0)
rawm_stry$M7 <- ifelse(dummies$month == 7, 1, 0)
rawm_stry$M8 <- ifelse(dummies$month == 8, 1, 0)
rawm_stry$M9 <- ifelse(dummies$month == 9, 1, 0)
rawm_stry$M10 <- ifelse(dummies$month == 10, 1, 0)
rawm_stry$M11 <- ifelse(dummies$month == 11, 1, 0)
rawm_stry$M12 <- ifelse(dummies$month == 12, 1, 0)
rawm_stry$d_pandemic <- ifelse(rawm_stry$date >= as.Date("2020-01-01") &
                                rawm_stry$date <= as.Date("2020-10-01"), 1, 0)
rawm_stry$d_shift <- ifelse(rawm_stry$date < as.Date("2013-01-01"), 
                           seq_len(sum(rawm_stry$date < as.Date("2013-01-01"))),0)
datest = rawm_stry$date
rawm_stry$date <- NULL
rawm_stry[] <- lapply(rawm_stry, as.numeric)

benchmark4w <- call_models1(rawm_stry, 'Nlogadf - SARIMA', get_sarima, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

lasso_model4w <- call_models1(rawm_stry, 'Nlogadf - LASSO', get_lasso, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

enet_model4w <- call_models1(rawm_stry, 'Nlogadf - Elastic Net', get_elasticnet, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

#rf_modelw <- call_models1(rawm_stry, 'Random Forest - IBC-w', get_rf, "ibc_rs")
# h=1  RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:


# ================================================
# ---------------Diebold-Mariano test-------------
# ================================================

yi <- datasetm$`ibc_rs`[181:257]
dm_tests_ibc <- compute_dm1(model_names = c("LASSO", "Elastic Net", "Random Forest"),
                           model_dataframes = list(lasso_model, enet_model, rf_model),
                           horizons = c(1, 12),
                           orig_data = yi)

#meandm_test = compute_dmv2()

# ================================================
# ------Performance evaluation through CSFE-------
# ================================================

csfem_lasso = csfe(lasso_model, benchmark, yi)
csfem_enet = csfe(enet_model, benchmark, yi)
csfem_rf = csfe(rf_model, benchmark, yi)
#csfem_boosting = csfe(boost_model, benchmark, yi)

csfem_lasso <- as.data.frame(csfem_lasso)
csfem_enet <- as.data.frame(csfem_enet)
csfem_rf <- as.data.frame(csfem_rf)
#csfem_boosting <- as.data.frame(csfem_boosting)

# ================================================
# --------------------Graphs----------------------
# ================================================
y_axis <- dfdate[181:257]
CSFE_df <- data.frame(date = y_axis,
                      lasso_h1 = csfem_lasso$h1,
                      lasso_h12 = csfem_lasso$h12,
                      enet_h1 = csfem_enet$h1,
                      enet_h12 = csfem_enet$h12,
                      rf_h1 = csfem_rf$h1,
                      rf_h12 = csfem_rf$h12) 

# c("#F57C00", "#1ABC9C", "#1F497D")

# ================================================