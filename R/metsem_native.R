morie_metsem <- function(reads, k) {
  reads <- as.numeric(reads)
  n <- length(reads)
  result <- mean(reads)
  se <- if (n > 1) sd(reads) / sqrt(n) else NA_real_
  list(estimate = result, se = se, n = n, method = "Metagenome assembly (metaSPAdes)")
}
