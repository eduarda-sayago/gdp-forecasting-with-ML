#### Gather monthly data

# Packages
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# Raw data
nacional_NSA <- read.csv2("Data/data_month_NSA/nacional_mensal.csv")
regional_NSA <- read.csv2("Data/data_month_NSA/regional_mensal.csv")
meteorologicos <- read.csv("Data/data_month_NSA/meteorologicos_mensal.csv")
ipea_NSA <- read.csv("Data/data_month_NSA/dadosipeaNSA.csv")

nacional_SA <- read.csv2("Data/data_month_SA/nacional_mensal.csv")
regional_SA <- read.csv2("Data/data_month_SA/regional_mensal.csv")
meteorologicos <- read.csv("Data/data_month_SA/meteorologicos_mensal.csv")
ipea_SA <- read.csv("Data/data_month_SA/dadosipeaSA.csv")


nacional_NSA = nacional_NSA[-1,-1]
meteorologicos = meteorologicos[-1:-12,-1:-2]
regional_NSA = regional_NSA[-1,-1]
ipea_NSA = ipea_NSA[-283:-284,-1]

nacional_SA = nacional_SA[-1,-1]
regional_SA = regional_SA[-1,-1]
ipea_SA = ipea_SA[-283:-284,-1]

nacional_SA = nacional_SA[-283,] 
regional_SA = regional_SA[-283,]

nacional_NSA = nacional_NSA[-283,] 
regional_NSA = regional_NSA[-283,]

base_SA = cbind(ipea_SA,
                   nacional_SA,
                   meteorologicos,
                   regional_SA)
base_NSA = cbind(ipea_NSA,
                nacional_NSA,
                meteorologicos,
                regional_NSA)

saveRDS(base_SA, "base_SA.rds")
saveRDS(base_NSA, "base_NSA.rds")

base_SA[,1] <- as.Date(base_SA[,1])
base_NSA[,1] <- as.Date(base_NSA[,1])

# Order by date
base_SA <- base_SA %>% arrange(base_SA[,1])
base_NSA <- base_NSA %>% arrange(base_NSA[,1])

# Save again
saveRDS(base_SA, "base_SA.rds")
saveRDS(base_NSA, "base_NSA.rds")

base_SA <- readRDS("base_SA.rds")
base_NSA <- readRDS("base_NSA.rds")

rm(list = setdiff(ls(), c("base_SA", "base_NSA")))

