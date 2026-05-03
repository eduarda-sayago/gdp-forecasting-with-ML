# GDP Forecasting with Machine Learning

This project forecasts the GDP of Rio Grande do Sul using machine learning models applied to a large dataset of macroeconomic, regional, and meteorological variables. The pipeline covers data collection, stationarity transformations, quarterly aggregation, model estimation, and forecast evaluation.

## Project Structure

| Script | Description |
|--------|-------------|
| `00_main.R` | Master script — sources all modules and runs the full pipeline |
| `01_get_Data.R` | Loads or rebuilds the raw monthly dataset from CSV files |
| `02_get_stationary_SW.R` | Applies Stock & Watson (2012) stationarity transformations; includes ADF-based stationarity check |
| `03_get_Log_Transformations.R` | Applies log transformations to series based on sign/zero characteristics |
| `04_get_Data_Prep.R` | Creates lagged predictors and splits data into training inputs/targets |
| `05_get_Rolling_Window.R` | Implements rolling-window evaluation (`rolling_window`, `rolling_window2`) |
| `06_get_Models.R` | Defines forecast models: SARIMA, LASSO, Elastic Net, Random Forest, Boosting; and error metrics (RMSE, MAE, MAPE) |
| `07_call_Model.R` | Wraps model functions into a unified rolling-window call and saves forecast plots |
| `08_Compute_DM.R` | Computes the Diebold-Mariano test for pairwise model comparison |
| `09_Performance_csfe.R` | Cumulative Squared Forecast Error (CSFE) functions for graphical performance evaluation |
| `10_get_Graphs.R` | Generates result plots |
| `arima_tests.R` | Standalone ARIMA/SARIMA rolling-window experiments |

## Models

- **Benchmark:** Historical mean
- **SARIMA:** Seasonal ARIMA via `auto.arima`
- **LASSO:** L1-penalized regression via `glmnet` with 5-fold cross-validation
- **Elastic Net:** Combined L1/L2 penalty via `caret` + `glmnet` with grid search over α and λ
- **Random Forest:** `randomForest` via `caret` with CV-tuned `mtry`
- **Boosting:** Gradient boosting via `mboost` with CV-selected number of iterations

## Evaluation

Forecasts are generated at horizons **h = 1** and **h = 4** quarters (and **h = 1** and **h = 12** months for the monthly pipeline) using an expanding/rolling window over 30% of the sample as the test set. Models are compared using:

- **RMSE** — Root Mean Squared Error
- **MAE** — Mean Absolute Error
- **MAPE** — Mean Absolute Percentage Error
- **CSFE** — Cumulative Squared Forecast Error (graphical)
- **Diebold-Mariano test** — Statistical significance of forecast differences

## Data

The dataset combines monthly series from multiple sources:

- **Nacional / Regional** — national and regional economic activity indicators
- **Meteorológicos** — weather variables (INMET)
- **IPEA** — macroeconomic series from IPEADATA
- **IBC-RS** — Banco Central's economic activity index for Rio Grande do Sul
- **Stock-Watson metadata** (`Data/Stock_watson.csv`) — transformation codes for each variable

Raw data is stored under `Data/` and preprocessed into `.rds` files for reproducibility.

## Requirements

```r
install.packages(c(
  "dplyr", "lubridate", "tidyverse", "forecast",
  "glmnet", "caret", "randomForest", "mboost",
  "sentometrics", "textdata", "readr", "tidyr"
))
```

## Usage

Open `TCC_RSecon.Rproj` in RStudio and run `00_main.R`.
