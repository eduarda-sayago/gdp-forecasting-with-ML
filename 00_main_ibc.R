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
#raww_stry <- get_stationarity(logw_results, type_df = type_dfw)

type_dfm <- rawm_stry$type_df
rawm_stry <- as.data.frame(rawm_stry$df)

# type_dfw <- raww_stry$type_df
# raww_stry <- as.data.frame(raww_stry$df)

message("[4] Applying seasonal differencing")

datasetm <- get_seas_stationarity(logm_results, type_df = type_dfm, freq = 12)

type_dfm <- datasetm$type_df
datasetm <- as.data.frame(datasetm$df)

# type_dfw <- datasetw$type_df
# datasetw <- as.data.frame(datasetw$df)

# Adding dummies in datasetm

datasetm$date <- as.Date(datasetm$date, origin = "1970-01-01")

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
#datasetm$d_rsflood <- ifelse(datasetm$date == as.Date("2024-06-01"), 1, 0)
datasetm$d_shift <- ifelse(datasetm$date < as.Date("2013-01-01"), 
                           seq_len(sum(datasetm$date < as.Date("2013-01-01"))),0)

# Adding dummies in datasetw
# ...
saveRDS(datasetm,"datasetm.rds")

# Storing and removing Date column
dfdate = datasetm$date
dfdate = as.Date(date, origin = "1970-01-01")

datasetm$date <- NULL
datasetm[] <- lapply(datasetm, as.numeric)

# datasetw$date <- NULL
# datasetw[] <- lapply(datasetw, as.numeric)

rm(dummies, rawm_ibc_log, logm_results, rawm_stry)
#rm(raww_ibc_log, logw_results, raww_stry)
# ================================================
# -----------------Forecasting--------------------
# ================================================

#=====
message("Mean")

mean_modelp <- call_models(dataset, 'Mean - PIB_RS', get_mean, "pib_rs") #not saved yet
# h=1 RMSE: 0.08924119 ; MAE: 0.06789313  
# h=4 RMSE: 0.08885499 ; MAE: 0.06764367  

mean_model <- call_models(datasetm, 'Mean - IBC-m', get_mean, "ibc_rs")
# h=1 RMSE: 0.1026092; MAE: 0.06800124  
# h=12 RMSE: 0.1220261; MAE: 0.08669113  

#mean_model <- call_models(datasetw, 'Mean - IBC-w', get_mean, "ibc_rs")
# h=1 RMSE: 0.1026092  ; MAE:  0.06800124 
# h=12 RMSE: 0.1220261 ; MAE:  0.08669113 

#=====
message("SARIMA")

benchmarkp <- call_models(dataset, 'SARIMA - PIB_RS', get_sarima, "pib_rs")
# h=1 RMSE: 0.07011117; MAE: 0.04844195; MAPE: 99.58476     
# h=4 RMSE: 0.07980111; MAE: 0.05570668; MAPE: 127.10611  

benchmark <- call_models1(datasetm, 'SARIMA - IBC-m', get_sarima, "ibc_rs")
# h=1 RMSE: 0.06629558; MAE: 0.04880438; MAPE: 1.048171 
# h=12 RMSE: 0.09837429; MAE: 0.07007968; MAPE: 1.501778  

#=====
message("LASSO")

lasso_modelp <- call_models(dataset, 'LASSO - PIB_RS', get_lasso, "pib_rs")
# h=1 RMSE: 0.05920526  ; MAE: 0.04084625; MAPE: 119.5896 
# h=4 RMSE: 0.05988952  ; MAE: 0.04352576; MAPE: 105.9388 

lasso_model <- call_models1(datasetm, 'LASSO - IBC-m', get_lasso, "ibc_rs")
# h=1 RMSE: 0.03649598; MAE: 0.02839302; MAPE: 0.6150090 
# h=12 RMSE: 0.05989764; MAE: 0.04312109; MAPE: 0.9387088 

#lasso_wmodel <- call_models1(datasetw, 'LASSO - IBC-w', get_lasso, "ibc_rs")
# h=1 RMSE: 0.03860373 ; MAE:  0.02976507; MAPE:
# h=12 RMSE: 0.07399347 ; MAE: 0.05377384; MAPE:

#=====
message("Elastic Net")

enet_modelp <- call_models(dataset, 'Elastic Net', get_elasticnet, "pib_rs") #not saved
# h=1 RMSE: 0.05790124 ; MAE: 0.04586020; MAPE:    
# h=4 RMSE: 0.12473872 ; MAE: 0.09587616; MAPE:

enet_model <- call_models1(datasetm, 'Elastic Net - IBC-m', get_elasticnet, "ibc_rs")
# h=1 RMSE: 0.05072413 ; MAE: 0.03893112; MAPE: 0.8390616 
# h=12 RMSE: 0.06412265 ; MAE: 0.05130680; MAPE: 1.1120433 

#enet_wmodel <- call_models1(datasetw, 'Elastic Net', get_elasticnet, "ibc_rs")
# h=1 RMSE: ; MAE:    ; MAPE:
# h=12 RMSE: ; MAE:    ; MAPE:

#=====
message("Random Forest")

rf_model <- call_models1(datasetm, 'Random Forest - IBC-m', get_rf, "ibc_rs")
# h=1 RMSE: 0.05013537 ; MAE: 0.03508641; MAPE: 0.7569036 
# h=12 RMSE: 0.05428087 ; MAE:0.04022022; MAPE: 0.8674320 

#rf_wmodel <- call_models1(datasetw, 'Random Forest', get_rf, "ibc_rs")
# h=1 RMSE: ; MAE:     
# h=4 RMSE: ; MAE:  

#message("Boosting")
# boost_model <- call_models1(datasetm, 'Boosting', get_boosting, "ibc_rs")
# # h=1 RMSE: 0.1026092; MAE: 0.06800124 ; MAPE:  1.445637 
# # h=12 RMSE:0.1002764; MAE: 0.06661304 ; MAPE:  1.417023  
# boost_wmodel <- call_models1(datasetw, 'Boosting', get_boosting, "ibc_rs")
# # h=1 RMSE: ; MAE:      
# # h=4 RMSE: ; MAE:  

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