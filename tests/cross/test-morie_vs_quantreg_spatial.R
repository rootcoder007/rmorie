test_that("regression quantiles and the two-stage spatial estimator equal quantreg::rq", {
  skip_if_not_installed("quantreg")
  n <- 40
  u <- .morie_random_uniform(2 * n, seed = 61)
  D <- as.matrix(stats::dist(cbind(u[1:n], u[n + 1:n])))
  A <- matrix(0, n, n)
  for (i in 1:n) A[i, order(D[i, ])[2:4]] <- 1
  A <- pmax(A, t(A))
  W <- A / rowSums(A)
  z <- .morie_random_normal(3 * n, seed = 62)
  X <- cbind(z[1:n], z[n + 1:n])
  y <- as.vector(solve(diag(n) - 0.4 * W, 1 + X[, 1] - 0.5 * X[, 2] + z[2 * n + 1:n]))
  wy <- as.vector(W %*% y)
  for (tau in c(0.25, 0.5, 0.8)) {
    expect_equal(QuantileRegressionLp(y, cbind(1, X), tau)$coefficients, unname(stats::coef(quantreg::rq(y ~ X, tau = tau))),
                 tolerance = 1e-9)
    s1 <- quantreg::rq(wy ~ X + I(W %*% X), tau = tau)
    s2 <- quantreg::rq(y ~ X + stats::fitted(s1), tau = tau)
    ours <- SpatialQuantileIv(y, X, W, tau)
    expect_equal(c(ours$coefficients, ours$rho), unname(stats::coef(s2)), tolerance = 1e-9)
  }
})
