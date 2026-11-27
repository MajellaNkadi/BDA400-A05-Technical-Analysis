# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R, Development Phase
# Student : Majella Nkadi
# File    : stoch_rsi.R
# Purpose : Stochastic RSI (%K and %D). Requires rsi.R and sma.R
# Note    : Implemented with base R core functions only (no packages).
# =============================================================================

if (!exists("rsi")) source("rsi.R")
if (!exists("sma")) source("sma.R")

# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI (normalize RSI between 0 and 1)
  if (all(is.na(rsi_values))) {
    k_values <- rsi_values                         # not enough data: keep NA
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
    if (max_rsi == min_rsi) {
      k_values <- ifelse(is.na(rsi_values), NA, 0) # flat RSI: avoid 0/0
    } else {
      k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
    }
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)

  return(result)
}
