d <- .spdiag_data()

test_that("Sdmdet, Sdmjac, Sdmconv, Sdmlrt, Sdmwald and Sdmr2", {
  expect_equal(Sdmdet(d$W, 0.3)$statistic, log(abs(det(diag(8) - 0.3 * d$W))), tolerance = 1e-12)
  expect_equal(Sdmjac(d$W, -0.3)$statistic, log(abs(det(diag(8) + 0.3 * d$W))), tolerance = 1e-12)
  expect_equal(Sdmconv(d$W3, 1.2)$statistic, 0)
  expect_equal(Sdmlrt(-10, -12)$p_value, exp(-2), tolerance = 1e-12)
  expect_equal(Sdmwald(0.5, 0.2)$statistic, 6.25, tolerance = 1e-14)
  expect_equal(Sdmr2(-12, -12, 30)$statistic, 0)
})

test_that("Sdmcf, Sdmflt, Sdmwx and Sdmolsi", {
  V <- diag(c(0.01, 0.04, 0.05, 0.09, 0.07))
  expect_equal(Sdmcf(c(1, -0.5), c(-0.4, 0.2), V, 0.4)$statistic, 0, tolerance = 1e-14)
  g <- c(-0.2 + 0.4, 0.3 - 0.2)
  G <- rbind(c(1, 0.4, 0, 1, 0), c(-0.5, 0, 0.4, 0, 1))
  expect_equal(Sdmcf(c(1, -0.5), c(-0.2, 0.3), V, 0.4)$statistic,
               as.numeric(g %*% solve(G %*% V %*% t(G), g)), tolerance = 1e-12)
  expect_equal(Sdmflt(d$y, d$W)$filtered, as.vector(d$y - 0.3 * d$W %*% d$y), tolerance = 1e-14)
  w <- Sdmwx(d$X, d$W)
  wx <- as.vector(d$W %*% d$X[, 2])
  expect_equal(w$means, mean(wx), tolerance = 1e-14)
  expect_equal(w$correlations, cor(d$X[, 2], wx), tolerance = 1e-12)
  o <- Sdmolsi(d$y, d$X)
  f <- lm(d$y ~ d$X[, 2])
  expect_equal(o$beta, unname(coef(f)), tolerance = 1e-12)
  expect_equal(o$statistic, as.numeric(logLik(f)), tolerance = 1e-12)
})

test_that("Sdmres, Gnsres and Sdemres are residual Moran tests", {
  expect_equal(Sdmres(d$e, d$W)$statistic, .spdiag_moran(d$e, d$W), tolerance = 1e-13)
  u <- lm.fit(d$X, d$y)$residuals
  r <- Gnsres(u, d$W, d$X)
  M <- diag(8) - d$X %*% solve(crossprod(d$X), t(d$X))
  expect_equal(r$expected, 8 / sum(d$W) * sum(diag(M %*% d$W)) / 6, tolerance = 1e-13)
  expect_equal(Sdemres(u, d$W, d$X)$statistic, .spdiag_moran(u, d$W), tolerance = 1e-13)
})

test_that("Sdmvar inverts the Fisher information of the Durbin design", {
  WX <- d$W %*% d$X[, 2]
  r <- Sdmvar(d$X, WX, d$W, 0.3, 0.5, c(1, 2, 0.4))
  Fm <- .spdiag_fisher(cbind(d$X, WX), d$W, c(1, 2, 0.4), 0.3, 0, 0.5, TRUE, FALSE)
  expect_equal(r$information, Fm, tolerance = 1e-9)
})

test_that("Sdmboot estimate is the lag-model rho on the Durbin design", {
  r <- Sdmboot(d$y, d$X, d$W, B = 4, seed = 9)
  Z <- cbind(d$X, d$W %*% d$X[, 2])
  expect_equal(r$statistic, SpatialRegressionML(d$y, Z, d$W, "lag")$rho, tolerance = 1e-6)
})
