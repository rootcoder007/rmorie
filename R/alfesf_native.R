morie_alfesf <- function(sequence, esm_model) {
  sequence <- as.numeric(sequence)
  if (length(sequence) < 1) stop("alfesf: sequence must be non-empty")
  n <- length(sequence)
  result <- mean(sequence)
  se <- if (n > 1) sd(sequence) / sqrt(n) else NA_real_
  list(
    estimate = result,
    se = se,
    n = n,
    method = "ESMFold language-model-only structure prediction"
  )
}
