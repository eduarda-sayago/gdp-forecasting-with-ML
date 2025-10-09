# ================================================
# ------------Gather Monthly Data-----------------
# ================================================

# library(dplyr)
# library(lubridate)
# 
# nacional <- read.csv("Data/data_NSA/nacional_mensal.csv")
# regional <- read.csv("Data/data_NSA/regional_mensal.csv")
# meteorologicos <- read.csv("Data/data_NSA/meteorologicos_mensal.csv")
# ipea <- read.csv("Data/data_NSA/dadosipeaNSA.csv")
# 
# nacional = nacional[2:281, -(1:2)] #
# regional = regional[2:281,-(1:2)] #
# meteorologicos = meteorologicos[13:292, -(1:2)] #
# ipea = ipea[1:280,-1]
# 
# raw_base = cbind(regional, nacional, meteorologicos, ipea)
# raw_base <- raw_base %>% select(date, everything())
# raw_base[,1] <- as.Date(raw_base[,1])
# 
# saveRDS(raw_base, "raw_base.rds")

get_raw_base <- function(rds_path = "raw_base.rds") {
  # If RDS already exists, return it
  if (file.exists(rds_path)) {
    message("Loading RDS from disk")
    raw_base <- readRDS(rds_path)
    return(raw_base)
  }
  
  # Otherwise, process the CSVs
  message("Gathering RDS from CSV files...")
  
  # Load necessary libraries
  library(dplyr)
  library(lubridate)
  
  # Read CSV files
  nacional <- read.csv("Data/data_NSA/nacional_mensal.csv")
  regional <- read.csv("Data/data_NSA/regional_mensal.csv")
  meteorologicos <- read.csv("Data/data_NSA/meteorologicos_mensal.csv")
  ipea <- read.csv("Data/data_NSA/dadosipeaNSA.csv")
  
  # Apply transformations
  nacional <- nacional[2:281, -(1:2)]
  regional <- regional[2:281, -(1:2)]
  meteorologicos <- meteorologicos[13:292, -(1:2)]
  ipea <- ipea[1:280, -1]
  
  # Combine all data
  raw_base <- cbind(regional, nacional, meteorologicos, ipea)
  raw_base <- raw_base %>% select(date, everything())
  raw_base[, 1] <- as.Date(raw_base[, 1])
  
  # Save the processed data
  saveRDS(raw_base, rds_path)
  message("RDS file created and saved as raw_base.rds")
  
  # Return the final data
  return(raw_base)
}

rm(nacional, regional, meteorologicos,ipea)
