# 3) A tiny test function that is easy to verify:
#    - it returns the sum of the variable in the window as "forecast"
#    - it returns the indices in outputs so we can inspect them
test_fn <- function(idx, df, horizon, variable, ...) {
  value <- sum(df[[variable]][idx])
  list(forecast = value, outputs = list(idx = idx))
}

# 4) Create a simple dataset to test with
df <- data.frame(variable = 1:10)  # predictable numbers
nwindow <- 3
horizon <- 1  # not used by test_fn but kept for signature

# Run rolling and expanding
roll_res <- rolling_window(test_fn, df, nwindow = nwindow, horizon = horizon, variable = "variable")
roll_res2 <- rolling_windowv2(test_fn, df, nwindow = nwindow, horizon = horizon, variable = "variable")
exp_res  <- expanding_window(test_fn, df, nwindow = nwindow, horizon = horizon, variable = "variable")

# 5) Expected values (computed explicitly) ----

# Rolling expected sums: windows 1:3, 2:4, ..., 8:10
expected_roll_sums <- sapply(1:(nrow(df) - nwindow + 1),
                             function(i) sum(df$variable[i:(i + nwindow - 1)]))

# Expanding expected sums: 1:3, 1:4, ..., 1:10
expected_exp_sums <- sapply(0:(nrow(df) - nwindow),
                            function(k) sum(df$variable[1:(nwindow + k)]))

# 6) Automatic checks ----
stopifnot(identical(as.numeric(roll_res2$forecast), as.numeric(expected_roll_sums)))
stopifnot(identical(as.numeric(exp_res$forecast),  as.numeric(expected_exp_sums)))
stopifnot(length(roll_res2$forecast) == length(expected_roll_sums))
stopifnot(length(exp_res$forecast)  == length(expected_exp_sums))

# 7) Visual inspection output ----
cat("ROLLING windows (each outputs$idx):\n")
print(lapply(roll_res2$outputs, function(o) o$idx))

cat("\nROLLING forecasts:\n")
print(roll_res2$forecast)
cat("EXPECTED rolling sums:\n")
print(expected_roll_sums)

cat("\nEXPANDING windows (each outputs$idx):\n")
print(lapply(exp_res$outputs, function(o) o$idx))

cat("\nEXPANDING forecasts:\n")
print(exp_res$forecast)
cat("EXPECTED expanding sums:\n")
print(expected_exp_sums)
