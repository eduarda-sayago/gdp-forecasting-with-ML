
library(forecast)
set.seed(123)

n <- 95  # number of observations

# Function to simulate an AR(1) series
sim_ar1 <- function(n, phi = 0.7, sd = 1) {
  arima.sim(model = list(ar = phi), n = n, sd = sd)
}

# Simulate main X variables
x1 <- sim_ar1(n)
x2 <- sim_ar1(n)

# Simulate other 18 X variables as AR1s correlated with x1 or x2
other_X <- matrix(NA, nrow = n, ncol = 18)

for (i in 1:18) {
  # Linear combination with some noise to induce correlation
  other_X[, i] <- 0.5 * x1 + 0.3 * x2 + sim_ar1(n, phi = 0.5, sd = 0.5)
}

colnames(other_X) <- paste0("x", 3:20)

# Simulate y using AR1 + 2 main regressors
phi_y <- 0.6
beta1 <- 1.2
beta2 <- -0.8

y <- numeric(n)
# Start with initial values for y (first 4 lags)
y[1:4] <- rnorm(4)

for (t in 5:n) {
  y[t] <- phi_y * y[t-1] + beta1 * x1[t-1] + beta2 * x2[t-2] + rnorm(1, 0, 1)
}

library(zoo)
start_date <- as.Date("2002-03-01")
quarter_dates <- seq(start_date, by = "3 months", length.out = n)
quarter_dates

# Combine into a data frame
df <- data.frame(date = quarter_dates, y = y, x1 = x1, x2 = x2, other_X)
head(df)

# y = 0.6 * y[t-1] + 1.2 * x1[t-1] -0.8 * x2[t-2] + erro(rnorm(1, 0, 1))