test_that("spatial panel ML equals splm::spml", {
  skip_if_not_installed("splm")
  skip_if_not_installed("spdep")
  N <- 9
  T <- 5
  W <- matrix(0, N, N)
  for (i in 1:N) for (j in c(i - 1, i + 1, i + 3, i - 3)) if (j >= 1 && j <= N) W[i, j] <- 1
  W <- W / rowSums(W)
  z <- .morie_random_normal(4 * N * T, seed = 13)
  k <- seq_len(N * T)
  X <- cbind(z[k], 0.5 * z[N * T + k] + 0.1 * ((k - 1) %% N))
  y <- 1 + 0.8 * X[, 1] - 0.5 * X[, 2] + 0.7 * z[2 * N * T + (k - 1) %% N + 1] + 0.5 * z[3 * N * T + k]
  df <- data.frame(id = rep(1:N, T), time = rep(1:T, each = N), y = y, x1 = X[, 1], x2 = X[, 2])
  lw <- spdep::mat2listw(W, style = "W")
  ev <- range(Re(eigen(W)$values))
  iv <- c(1 / ev[1], 1 / ev[2]) + c(1e-9, -1e-9)
  for (eff in c("individual", "time", "twoways")) {
    for (mm in c("lag", "error")) {
      s <- splm::spml(y ~ x1 + x2, data = df, index = c("id", "time"), listw = lw, model = "within", effect = eff,
                      lag = mm == "lag", spatial.error = if (mm == "lag") "none" else "b")
      o <- SpatialPanelMl(y, X, W, N, mm, eff, interval = iv)
      expect_equal(c(o$rho, o$coefficients), unname(stats::coef(s)), tolerance = 1e-6)
      expect_equal(o$sigma2, s$sigma2, tolerance = 1e-6)
      if (mm == "lag") expect_equal(o$loglik, s$logLik, tolerance = 1e-9)
    }
  }
  s <- splm::spml(y ~ x1 + x2, data = df, index = c("id", "time"), listw = lw, model = "random", lag = TRUE,
                  spatial.error = "none")
  o <- SpatialPanelReLag(y, X, W, N, interval = iv)
  expect_equal(o$coefficients, unname(stats::coef(s)), tolerance = 1e-6)
  expect_equal(o$rho, unname(s$arcoef), tolerance = 1e-6)
  expect_equal((1 / o$phi^2 - 1) / T, unname(s$errcomp), tolerance = 1e-6)
  expect_equal(o$loglik, s$logLik, tolerance = 1e-8)
})
