### Quarterly conversion

# Packages
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# Raw data
base_bruta <- read.csv("Data/data_month/base_bruta.csv")


# Make quarterly
to_quarterly <- function(df, date_col = "date") {
  df %>%
    mutate(
      !!date_col := as.Date(.data[[date_col]]),
      # Get quarter-end dates: 03, 06, 09, 12
      date = ceiling_date(.data[[date_col]], "quarter") - days(1)
    ) %>%
    group_by(date) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
}

tri_bruta <- to_quarterly(base_bruta, date_col = "date")
tri_bruta <- tri_bruta[-94,]


# Add other quarter data
data_tri <- read.csv2("Data/data_quarter/datareg_tri.csv")
data_tri <- data_tri[,-1]
tri_bruta = cbind(tri_bruta,
                  data_tri)

write.csv(tri_bruta, file = "Data/data_quarter/tri_data.csv", row.names = FALSE)
