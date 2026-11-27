# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R, Development Phase
# Student : Majella Nkadi
# File    : run_all_tests.R
# Purpose : Sources every indicator, runs the assignment examples, and cross-checks against base R
# Note    : Implemented with base R core functions only (no packages).
# =============================================================================

# HOW TO RUN: put all .R files in the same folder (working directory), then
#   source("run_all_tests.R")
files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R",
           "rsi.R", "stoch_rsi.R", "crossover.R", "crossunder.R")
for (f in files) source(f)

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

cat("\n--- SMA (period 3) ---\n");  print(sma(data, period = 3))
cat("\n--- EMA (period 3) ---\n");  print(ema(data, period = 3))
cat("\n--- MACD (3, 5, 2) ---\n")
print(macd(c(100, 105, 110, 115, 120, 125, 130), short_period = 3,
           long_period = 5, signal_period = 2))
cat("\n--- STDEV ---\n");           print(stdev(data))
cat("\n--- LINREG (length 5, offset 0) ---\n"); print(linreg(data, 5, 0))
rdata <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
cat("\n--- RSI (period 5) ---\n");  print(rsi(rdata, period = 5))
cat("\n--- StochRSI (14, 3, 3) ---\n")
print(stoch_rsi(rdata, period = 14, k_period = 3, d_period = 3))
cat("\n--- StochRSI (3, 3, 2) ---\n")
print(stoch_rsi(rdata, period = 3, k_period = 3, d_period = 2))
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
cat("\n--- Crossover ---\n");  print(crossover(arr1, arr2))
cat("\n--- Crossunder ---\n"); print(crossunder(arr1, arr2))

# ---- Independent cross-checks (base R used ONLY to verify) ----
cat("\n--- Verification ---\n")
tol <- 1e-9
x <- data
stopifnot(abs(sma(x, 3)[1] - (10 + 12 + 15) / 3) < tol)
stopifnot(all(abs(sma(x, 3) - stats::filter(x, rep(1/3, 3), sides = 1)[3:9]) < tol))
stopifnot(abs(stdev(x) - sd(x) * sqrt((length(x) - 1) / length(x))) < tol)
lr <- linreg(x, 5, 0); sub <- x[(length(x) - 5):length(x)]
fit <- lm(sub ~ seq_along(sub))
stopifnot(abs(lr$slope - coef(fit)[[2]]) < tol, abs(lr$intercept - coef(fit)[[1]]) < tol)
m <- macd(x, 3, 5, 2)
stopifnot(all(abs(m$histogram - (m$macd_line - m$signal_line)) < tol))
stopifnot(all(is.na(rsi(rdata, 5)[1:5])), all(rsi(rdata, 5)[6:10] >= 0 & rsi(rdata, 5)[6:10] <= 100))
stopifnot(identical(crossover(c(1, 3), c(2, 2)), c("None", "Up")))
stopifnot(identical(crossunder(c(3, 1), c(2, 2)), c("None", "True")))
stopifnot(inherits(try(sma(1:2, 3), silent = TRUE), "try-error"))      # error check
stopifnot(inherits(try(crossover(1:3, 1:2), silent = TRUE), "try-error"))
stopifnot(inherits(try(linreg(1:5, 6, 0), silent = TRUE), "try-error"))
stopifnot(inherits(try(linreg(1:5, 3, 3), silent = TRUE), "try-error"))
cat("All verification checks passed.\n")
