test_that("psis_loo smooths the tail with the fitted generalized Pareto", {
  S <- 60
  mu <- seq(-1, 1, length.out = S)
  y <- c(0.3, -0.8, 2.4, 0.1)
  ll <- sapply(y, function(v) dnorm(v, mean = mu, sd = 0.7, log = TRUE))
  r <- psis_loo(ll)
  lppd <- sum(apply(ll, 2, function(v) log(mean(exp(v)))))
  expect_equal(r$p_loo, lppd - r$elpd_loo, tolerance = 1e-12)
  expect_equal(r$looic, -2 * r$elpd_loo, tolerance = 1e-14)
  expect_equal(r$se, sqrt(4 * var(r$elpd_loo_pointwise)), tolerance = 1e-12)
  # with tail length ceiling(min(12, 3 sqrt(60))) = 12 >= 5 every k is estimated
  expect_true(all(is.finite(r$k_hat)))
  # raw importance sampling for one observation, by hand, is the unsmoothed
  # estimate; smoothing may only move weights inside the tail
  w <- exp(-ll[, 3] - max(-ll[, 3]))
  raw <- log(sum(w * exp(ll[, 3])) / sum(w))
  expect_true(abs(r$elpd_loo_pointwise[3] - raw) < 1)
})
