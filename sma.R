# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R, Development Phase
# Student : Majella Nkadi
# File    : sma.R
# Purpose : Simple Moving Average (SMA)
# Note    : Implemented with base R core functions only (no packages).
# =============================================================================

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (!is.numeric(data)) stop("data must be a numeric vector")
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values
  n_windows  <- length(data) - period + 1
  sma_values <- numeric(n_windows)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:n_windows) {
    current_window <- data[i:(i + period - 1)]        # current window
    mean_value     <- sum(current_window) / period    # average of the window
    sma_values[i]  <- mean_value                      # store the result
  }

  return(sma_values)
}
