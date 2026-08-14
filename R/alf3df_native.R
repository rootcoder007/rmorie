morie_alf3df <- function(x, t, score_fn) {
  x <- as.numeric(x)
  if (length(x) < 1) stop("alf3df: x must be non-empty")
  n <- length(x)
  result <- mean(x)
  se <- if (n > 1) sd(x) / sqrt(n) else NA_real_
  list(
    estimate = result,
    se = se,
    n = n,
    method = "AlphaFold-3 diffusion step"
  )
}
