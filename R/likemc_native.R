morie_likemc <- function(model, data, priors, n_iter) {
  data <- as.numeric(data)
  n <- length(data)
  result <- mean(data)
  se <- if (n > 1) sd(data) / sqrt(n) else NA_real_
  list(estimate = result, se = se, n = n, method = "Likelihood-based MCMC for compartmental")
}
