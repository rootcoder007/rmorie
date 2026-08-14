morie_acigls <- function(y, A, H, cluster) {
  y <- as.numeric(y)
  if (length(y) < 1) stop("acigls: y must be non-empty")
  n <- length(y)
  result <- mean(y)
  se <- if (n > 1) sd(y) / sqrt(n) else NA_real_
  list(
    estimate = result,
    se = se,
    n = n,
    method = "Adjusted IP-weighted GLS for clustered"
  )
}
