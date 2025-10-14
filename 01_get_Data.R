# ================================================
# ------------------Gather Data-------------------
# ================================================

library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

nacional <- read.csv("Data/data_NSA/nacional_mensal.csv")
regional <- read.csv2("Data/data_NSA/regional_mensal.csv")
meteorologicos <- read.csv("Data/data_NSA/meteorologicos_mensal.csv")
ipea <- read.csv("Data/data_NSA/dadosipeaNSA.csv")

varmonth <- read.csv2("Data/data_NSA/Varmonth.csv")
weather <- read.csv2("Data/data_NSA/precipitation_rs.csv")

# ================================================
# -----Cut data - IBC-RS [2003-01 to 2025-05]-----
# ================================================

varmonth_1 <- varmonth[13:281, -1] #
meteorologicos_1 <- meteorologicos[25:293, -(1:2)] # -2
weather_1 <- weather[13:281, -1] # -1
regional_1 <- regional[14:282, -(1:2)] # -2
nacional_1 <- nacional[14:282, -(1:2)] # -2
ipea_1 <- ipea[13:281, -1]

rawm_ibc <- cbind(varmonth_1, meteorologicos_1, ipea_1, regional_1, nacional_1)
raww_ibc <- cbind(varmonth_1, weather_1, ipea_1, regional_1, nacional_1)

rawm_ibc <- rawm_ibc %>% select(date, everything())
raww_ibc <- raww_ibc %>% select(date, everything())

rawm_ibc[,1] <- as.Date(rawm_ibc[,1])
raww_ibc[,1] <- as.Date(raww_ibc[,1])

saveRDS(rawm_ibc, "rawM_ibc.rds")
saveRDS(raww_ibc, "rawW_ibc.rds")

rm(varmonth_1, meteorologicos_1, weather_1, regional_1, nacional_1, ipea_1)
#rm(varmonth,meteorologicos, weather, regional, nacional, ipea)

# ================================================
# -----Cut data - PIB_RS [2002-01 to 2025-06]-----
# ================================================

# library(dplyr)
# library(lubridate)
# 
# nacional <- read.csv("Data/data_NSA/nacional_mensal.csv")
# regional <- read.csv("Data/data_NSA/regional_mensal.csv")
# meteorologicos <- read.csv("Data/data_NSA/meteorologicos_mensal.csv")
# ipea <- read.csv("Data/data_NSA/dadosipeaNSA.csv")
# 
# nacional2 = nacional[2:281, -(1:2)] #
# regional2 = regional[2:281,-(1:2)] #
# meteorologicos2 = meteorologicos[13:292, -(1:2)] #
# ipea2 = ipea[1:280,-1]
# 
# base_NSA = cbind(regional2, nacional2, meteorologicos2, ipea2)
# base_NSA <- base_NSA %>% select(date, everything())
# base_NSA[,1] <- as.Date(base_NSA[,1])
# 
# saveRDS(base_NSA, "base_NSA.rds")
#rm(nacional2, regional2, meteorologicos2, ipea2)

rm(varmonth,meteorologicos, weather, regional, nacional, ipea)