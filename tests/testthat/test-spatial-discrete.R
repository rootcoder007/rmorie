test_that("spatial discrete-choice estimators satisfy their defining equations", {
  n <- 30
  u <- .morie_random_uniform(2 * n, seed = 9)
  D <- as.matrix(stats::dist(cbind(u[1:n], u[n + 1:n])))
  A <- matrix(0, n, n)
  for (i in 1:n) A[i, order(D[i, ])[2:4]] <- 1
  A <- pmax(A, t(A))
  W <- A / rowSums(A)
  z <- .morie_random_normal(3 * n, seed = 10)
  X <- cbind(1, z[1:n], z[n + 1:n])
  y <- as.numeric(solve(diag(n) - 0.4 * W, 0.1 + X[, 2] - 0.6 * X[, 3] + z[2 * n + 1:n]) > 0)
  lg <- BinaryGlm(y, X)
  expect_lt(max(abs(crossprod(X, y - lg$fitted))), 1e-8)
  r <- SpatialLogitGmm(y, X, W)
  expect_length(r$se, 4)
  pr <- SpatialProbitGmm(y, X, W)
  expect_equal(SpatialProbitGmm(y, X, W, start_rho = pr$rho, tol = 1e-12)$rho, pr$rho, tolerance = 1e-8)
  t <- DiscreteMoranTest(y, X, W, "logit")
  s2 <- lg$fitted * (1 - lg$fitted)
  M <- (W^2 + W * t(W)) * outer(s2, s2)
  diag(M) <- 0
  e <- y - lg$fitted
  expect_equal(t$statistic, sum(e * (W %*% e)) / sqrt(sum(M)), tolerance = 1e-10)
})
