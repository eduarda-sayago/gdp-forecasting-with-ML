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

# ================================================
# -----Cut data - PIB_RS [2002-01 to 2025-06]----- obs: fix later. gotta save data_m otherwise it wont go
# ================================================

nacional = nacional[-1,-1]
meteorologicos = meteorologicos[-1:-12,-1:-2]
regional = regional[-1,-1]
ipea = ipea[-283:-284,-1]

nacional = nacional[-283,] 
regional = regional[-283,]

# ALL THE 

rm(varmonth, meteorologicos, weather, regional, nacional, ipea)

# base_NSA = cbind(ipea,
#                  nacional,
#                  meteorologicos,
#                  regional)

#base_NSA[,1] <- as.Date(base_NSA[,1])
#base_NSA <- base_NSA %>% arrange(base_NSA[,1])

#saveRDS(data_m, "data_m.rds")
#rm(list = setdiff(ls(), c("base_NSA")))

# base_NSA with no meteorologicos

##base_noweather = cbind(ipea,nacional, regional)

##base_noweather[,1] <- as.Date(base_noweather[,1])
##base_noweather <- base_noweather %>% arrange(base_noweather[,1])

##saveRDS(base_noweather, "base_noweather.rds")

#rm(list = setdiff(ls(), c("base_NSA")))

# Raw data w/ Seasonal adjustment (if available)

# nacional_SA <- read.csv2("Data/data_SA/nacional_mensal.csv")
# regional_SA <- read.csv2("Data/data_SA/regional_mensal.csv")
# meteorologicos <- read.csv("Data/data_SA/meteorologicos_mensal.csv")
# ipea_SA <- read.csv("Data/data_SA/dadosipeaSA.csv")
# 
# nacional_SA = nacional_SA[-1,-1]
# regional_SA = regional_SA[-1,-1]
# ipea_SA = ipea_SA[-283:-284,-1]
# 
# nacional_SA = nacional_SA[-283,] 
# regional_SA = regional_SA[-283,]
# 
# base_SA = cbind(ipea_SA,
#                 nacional_SA,
#                 meteorologicos,
#                 regional_SA)
# 
# base_SA[,1] <- as.Date(base_SA[,1])
# base_SA <- base_SA %>% arrange(base_SA[,1])
# 
# saveRDS(base_SA, "base_SA.rds")