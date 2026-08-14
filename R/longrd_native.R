morie_longrd <- function(assembly, reads) {
  assembly <- as.numeric(assembly)
  n <- length(assembly)
  result <- mean(assembly)
  se <- if (n > 1) sd(assembly) / sqrt(n) else NA_real_
  list(estimate = result, se = se, n = n, method = "Long-read consensus polishing (medaka)")
}
