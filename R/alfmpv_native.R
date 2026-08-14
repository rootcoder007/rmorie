morie_alfmpv <- function(chains, msas) {
  chains <- as.numeric(chains)
  if (length(chains) < 1) stop("alfmpv: chains must be non-empty")
  n <- length(chains)
  result <- mean(chains)
  se <- if (n > 1) sd(chains) / sqrt(n) else NA_real_
  list(
    estimate = result,
    se = se,
    n = n,
    method = "AlphaFold-Multimer chain pairing"
  )
}
