# State GDP Forecasting with Machine Learning: A Case Study of Rio Grande do Sul

**Previsão do PIB Estadual com Aprendizado de Máquina: Um Estudo de Caso do Rio Grande do Sul**

Eduarda Teixeira Sayago and Marcelo Savino Portugal  
Undergraduate thesis, Department of Economic Sciences and International Relations  
Federal University of Rio Grande do Sul (UFRGS), 2025

---

## Overview

This thesis evaluates the performance of machine learning methods (LASSO, Elastic Net, and Random Forest) relative to a SARIMA benchmark for forecasting Rio Grande do Sul's GDP. Forecasts are generated at 1-step-ahead and 1-year-ahead horizons using both monthly and quarterly data, with a rolling window scheme and formal statistical evaluation via the Diebold-Mariano test. LASSO achieved the lowest short-term errors while Random Forest was most accurate at long horizons. All ML methods outperformed SARIMA in most scenarios.

---

## Results

Relative RMSE with respect to the SARIMA benchmark (values below 1.0 indicate improvement over the benchmark):

| Model | Monthly h=1 | Monthly h=12 | Quarterly h=1 | Quarterly h=4 |
|---|---|---|---|---|
| LASSO | 0.53 | 0.60 | 0.53 | 0.69 |
| Elastic Net | 0.73 | 0.64 | 0.59 | 0.69 |
| Random Forest | 0.72 | 0.56 | 0.74 | 0.68 |

MAPE:

| Model | Monthly h=1 | Monthly h=12 | Quarterly h=1 | Quarterly h=4 |
|---|---|---|---|---|
| SARIMA (benchmark) | 1.08% | 1.48% | 5.46% | 3.19% |
| LASSO | 0.59% | 0.91% | 5.09% | 4.32% |
| Elastic Net | 0.81% | 1.08% | 3.39% | 4.34% |
| Random Forest | 0.73% | 0.85% | 3.33% | 4.10% |

---

## Data

**Monthly series:** January 2003 to June 2025 (257 observations). Target variable: IBCR-RS (Regional Economic Activity Index from the Brazilian Central Bank), base 01/2022 = 100.

**Quarterly series:** Q1 2002 to Q1 2025 (93 observations). Target variable: Rio Grande do Sul GDP from the Departamento de Economia e Estatística (DEE), base Q1 2002 = 100.

**Predictors:** 217-231 features across three levels:
- Subnational (166 features): real economy, industry, agribusiness, services, trade, and weather data from 13 economically significant municipalities in Rio Grande do Sul
- National (58 features): price indexes, money supply, interest rates, public sector, uncertainty indicators, and financial markets
- International (7 features): commodities and external sector

Weather data accounts for 143 of the subnational features, reflecting the state's agribusiness-dependent economy and its sensitivity to climate events.

All series were transformed for stationarity following Stock and Watson (2012). Four lags of each predictor were included. Seasonal dummies and outlier dummies for the Covid-19 pandemic and a 2013 structural trend shift were added to both datasets.

---

## Methodology

**Forecast strategy:** Direct forecasting with a fixed rolling window.  
**Evaluation period:** Last 30% of the sample (approximately 77 monthly and 28 quarterly observations).  
**Horizons tested:** h = 1 and h = 12 months; h = 1 and h = 4 quarters.  
**Hyperparameter selection:** 5-fold cross-validation minimizing RMSE for all ML models.  
**Performance evaluation:** RMSE, MAPE, Cumulative Squared Forecast Error (CSFE), and Diebold-Mariano test for statistical significance.

---

## Project Structure

| Script | Description |
|--------|-------------|
| `00_main.R` | Master script, sources all modules and runs the full quarterly pipeline |
| `00_main.R` | Master script 2, sources all modules and runs the full monthly pipeline |
| `01_get_Data.R` | Loads or rebuilds the raw monthly dataset from CSV files |
| `02_get_stationary_SW.R` | Applies Stock and Watson (2012) stationarity transformations; includes ADF-based stationarity check |
| `03_get_Log_Transformations.R` | Applies log transformations to series based on sign/zero characteristics |
| `04_get_Data_Prep.R` | Creates lagged predictors and splits data into training inputs/targets |
| `05_get_Rolling_Window.R` | Implements rolling-window evaluation (`rolling_window`, `rolling_window2`) |
| `06_get_Models.R` | Defines forecast models: SARIMA, LASSO, Elastic Net, Random Forest, Boosting; and error metrics (RMSE, MAE, MAPE) |
| `07_call_Model.R` | Wraps model functions into a unified rolling-window call and saves forecast plots |
| `08_Compute_DM.R` | Computes the Diebold-Mariano test for pairwise model comparison |
| `09_Performance_csfe.R` | Cumulative Squared Forecast Error (CSFE) functions for graphical performance evaluation |
| `10_get_Graphs.R` | Generates result plots |

---

## Requirements

```r
install.packages(c(
  "dplyr", "lubridate", "tidyverse", "forecast",
  "glmnet", "caret", "randomForest", "mboost",
  "sentometrics", "textdata", "readr", "tidyr"
))
```

Software: R version 4.2.2

## Usage

Open `TCC_RSecon.Rproj` in RStudio and run `00_main.R` and `00_main_ibc.R`.

---

## Citation

Sayago, E. T.; Portugal, M. S. State GDP Forecasting with Machine Learning: A Case Study of Rio Grande do Sul. Undergraduate thesis, Federal University of Rio Grande do Sul, Porto Alegre, 2025. Available at Lume UFRGS: https://lume.ufrgs.br/handle/10183/302668

## Acknowledgements

Thank you to Prof. Dr. Marcelo Portugal and Prof. Dr. Flávio A. Ziegelmann for all the counseling for this thesis. 

Thank you to my friend Nathan Ramos, who helped me so much with the code. Check out his repository at https://github.com/E30895/economics-forecast-using-text

obs: Claude code was utilized post-thesis for the organization of this repository.
