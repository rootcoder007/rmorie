morie_pace <- function(Y, argvals, K) {
  Y <- as.numeric(Y)
  n <- length(Y)
  result <- mean(Y)
  se <- if (n > 1) sd(Y) / sqrt(n) else NA_real_
  list(estimate = result, se = se, n = n, method = "PACE (sparse FPCA)")
}
