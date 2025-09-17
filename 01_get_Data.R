#### Gather monthly data
# 
# # Packages
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# Raw data

nacional <- read.csv("Data/data_NSA/nacional_mensal.csv")
regional <- read.csv("Data/data_NSA/regional_mensal.csv")
meteorologicos <- read.csv("Data/data_NSA/meteorologicos_mensal.csv")
ipea <- read.csv("Data/data_NSA/dadosipeaNSA.csv")

nacional = nacional[-1,-1]
meteorologicos = meteorologicos[-1:-12,-1:-2]
regional = regional[-1,-1]
ipea = ipea[-283:-284,-1]

nacional = nacional[-283,] 
regional = regional[-283,]

# base_NSA = cbind(ipea,
#                  nacional,
#                  meteorologicos,
#                  regional)

#base_NSA[,1] <- as.Date(base_NSA[,1])
#base_NSA <- base_NSA %>% arrange(base_NSA[,1])

#saveRDS(base_NSA, "base_NSA.rds")
#rm(list = setdiff(ls(), c("base_NSA")))

# base_NSA with no meteorologicos

base_noweather = cbind(ipea,nacional, regional)

base_noweather[,1] <- as.Date(base_noweather[,1])
base_noweather <- base_noweather %>% arrange(base_noweather[,1])

saveRDS(base_noweather, "base_noweather.rds")
# rm(list = setdiff(ls(), c("base_noweather", "base_NSA")))

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