test_that("spatial count models recompute", {
  n <- 16
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) W[i, i + 1] <- W[i + 1, i] <- 1
  W <- W / rowSums(W)
  u <- .morie_random_uniform(3 * n, seed = 21, stream = 0)
  X <- cbind(1, 2 * u[1:n] - 1)
  y <- stats::qpois(u[(n + 1):(2 * n)], exp(0.8 + 0.6 * X[, 2]))
  r <- SarPoisson(y, X, W, rho_bounds = c(0.3, 0.3))
  Xt <- solve(diag(n) - 0.3 * W, X)
  expect_equal(r$coefficients, unname(coef(glm(y ~ Xt - 1, family = poisson))), tolerance = 1e-8)
  ic <- CountModelIc(r$loglik, r$k, n)
  expect_equal(ic$AIC, -2 * r$loglik + 2 * r$k, tolerance = 1e-15)
  expect_equal(BymVarianceFraction(1, 3)$fraction, 0.25, tolerance = 1e-15)
})
