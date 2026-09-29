test_that("Bayesian samplers: gamma moments, reproducibility and reliability", {
  st <- .br_stream(9)
  d <- vapply(1:4000, function(i) .br_gamma(st, 2.5), 0)
  expect_lt(abs(mean(d) - 2.5), 4 * sqrt(2.5 / 4000))
  z <- .morie_random_normal(80, seed = 2)
  X <- cbind(1, z[1:30])
  y <- 1 + 2 * z[1:30] + 0.3 * z[41:70]
  a <- BayesLinearHalfcauchy(y, X, ndraw = 200, burn_in = 50, seed = 5)
  b <- BayesLinearHalfcauchy(y, X, ndraw = 200, burn_in = 50, seed = 5)
  expect_identical(a$draws, b$draws)
  expect_lt(abs(a$beta[2] - 2), 0.3)
  expect_equal(GenomicReliability(c(0.1, 0.4), 0.8)$reliability, c(0.875, 0.5))
})
