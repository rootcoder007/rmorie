test_that("spatial panel likelihoods satisfy their first-order conditions", {
  N <- 7
  T <- 4
  W <- matrix(0, N, N)
  for (i in 1:N) for (j in c(i - 1, i + 1, i + 2)) if (j >= 1 && j <= N) W[i, j] <- 1
  W <- W / rowSums(W)
  z <- .morie_random_normal(4 * N * T, seed = 21)
  k <- seq_len(N * T)
  X <- cbind(z[k], z[N * T + k] + 0.2 * ((k - 1) %% N))
  y <- 0.5 + X[, 1] - 0.4 * X[, 2] + 0.6 * z[2 * N * T + (k - 1) %% N + 1] + 0.4 * z[3 * N * T + k]
  r <- SpatialPanelMl(y, X, W, N)
  s2 <- sum(r$residuals^2) / (N * T)
  yt <- as.vector(matrix(y, N) - rowMeans(matrix(y, N)))
  tr <- sum(diag(solve(diag(N) - r$rho * W, W)))
  expect_equal(sum(r$residuals * as.vector(W %*% matrix(yt, N))), s2 * T * tr, tolerance = 1e-8)
  ly <- SpatialPanelMl(y, X, W, N, lee_yu = TRUE)
  expect_equal(ly$sigma2, r$sigma2 * T / (T - 1), tolerance = 1e-14)
  re <- SpatialPanelReLag(y, X, W, N)
  d <- as.vector(y - re$rho * as.vector(W %*% matrix(y, N)) - cbind(1, X) %*% re$coefficients)
  md <- rowMeans(matrix(d, N))
  expect_equal(re$phi, min(1, sqrt(sum((matrix(d, N) - md)^2) / ((T - 1) * T * sum(md^2)))), tolerance = 1e-8)
  ic <- InformationCriteria(-40, 3, 50)
  expect_equal(ic$bic, 80 + 3 * log(50), tolerance = 1e-15)
})
