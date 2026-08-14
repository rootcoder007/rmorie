morie_alfomg <- function(msa, pair) {
  msa <- as.numeric(msa)
  if (length(msa) < 1) stop("alfomg: msa must be non-empty")
  n <- length(msa)
  result <- mean(msa)
  se <- if (n > 1) sd(msa) / sqrt(n) else NA_real_
  list(
    estimate = result,
    se = se,
    n = n,
    method = "OpenFold MSA-pair head"
  )
}
