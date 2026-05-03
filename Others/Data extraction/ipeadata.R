#IPEA dataset

if(!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "tidyr", "purrr", "ipeadatar", "stringr", "lubridate")


ipea_search <- ipeadatar::available_series() %>%
 dplyr::filter(
   stringr::str_detect(name, stringr::regex("Ibovespa", ignore_case = TRUE)) &
     freq != "Yearly"
 )

get_ipea_data <- function(codes, start_date) {
  start_date <- as.Date(start_date)
  
  ipeacods <- ipeadatar::available_series() %>%
    filter(code %in% codes)
  
  dados_ipea <- map_dfr(ipeacods$code, ~ipeadata(.x))
  
  dados_largos <- dados_ipea %>%
    select(date, code, value) %>%
    pivot_wider(names_from = code, values_from = value) %>%
    arrange(date)
  
  linha_data <- dados_largos %>% filter(date == start_date)
  
  colunas_na <- names(linha_data)[which(is.na(linha_data) & names(linha_data) != "date")]
  clean_df <- dados_largos %>%
    select(-all_of(colunas_na)) %>%
    filter(date >= start_date)
  
  return(clean_df)
}


# Code list
ipeacods <- c('IFS12_BEEFB12','IFS12_MAIZE12','IFS12_PETROLEUM12','IFS12_SOJAGP12','PAN12_DTSPY12','BM12_DTEEY12','BM12_DTEMY12','BM12_DTGFN12','BM12_DTGFY12','BM12_DTSPY12','GM12_IBVSP12','FCESP12_IICA12','FCESP12_IIC12','CNC12_ICEC12','CNI12_ICEICA12','CNI12_ICEIEXP12','CNI12_ICEIGER12','DIMAC_ICTI3','FCESP12_IICF12','GAC12_INDFBCFDESSAZ12','PAN12_NFSPNYS12','PAN12_NFSPPYS12','ABPO12_PAPEL12','BM12_ERCF12','BM12_TJOVER12','ANBIMA12_TJTLN112','ANBIMA12_TJTLN1212','ANBIMA12_TJTLN312','ANBIMA12_TJTLN612','BMF12_SWAPDI180F12','BMF12_SWAPDI360F12','BMF12_SWAPDI36012'
)
ipeacodsNSA <- c('IFS12_BEEFB12','IFS12_MAIZE12','IFS12_PETROLEUM12','IFS12_SOJAGP12','PAN12_DTSPY12','BM12_DTEEY12','BM12_DTEMY12','BM12_DTGFN12','BM12_DTGFY12','BM12_DTSPY12','GM12_IBVSP12','FCESP12_IICA12','FCESP12_IIC12','CNC12_ICEC12','CNI12_ICEICA12','CNI12_ICEIEXP12','CNI12_ICEIGER12','DIMAC_ICTI3','FCESP12_IICF12','GAC12_INDFBCF12','PAN12_NFSPNYS12','PAN12_NFSPPYS12','ABPO12_PAPEL12','BM12_ERCF12','BM12_TJOVER12','ANBIMA12_TJTLN112','ANBIMA12_TJTLN1212','ANBIMA12_TJTLN312','ANBIMA12_TJTLN612','BMF12_SWAPDI180F12','BMF12_SWAPDI360F12','BMF12_SWAPDI36012'
)

ipeadailycods <- c("EIA366_PWTI366", "EIA366_PBRENT366")

# Start date
st_date <- "2002-01-01"


df_result <- get_ipea_data(ipeacods, st_date)
df_day <- get_ipea_data(ipeadailycods, st_date)

df_monthly <- df_day %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(across(-date, ~ mean(.x, na.rm = TRUE)), .groups = "drop")
df_monthly <- df_monthly %>%
  rename(date = month)

df_ipea <- df_result %>%
  left_join(df_monthly, by = "date")

write.csv(df_ipea, "dadosipeaSA.csv")

colnames(df_ipea)
