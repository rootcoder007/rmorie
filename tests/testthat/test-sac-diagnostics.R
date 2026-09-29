d <- .spdiag_data()

test_that("Sacdet, Sacjac, Sacconv, Saclrt and Sacwald", {
  a <- log(abs(det(diag(8) - 0.4 * d$W)))
  b <- log(abs(det(diag(8) + 0.3 * d$W)))
  expect_equal(Sacdet(d$W, 0.4, -0.3)$statistic, a + b, tolerance = 1e-12)
  expect_equal(Sacjac(d$W, 0.4, -0.3)$logdet_lambda, b, tolerance = 1e-12)
  r <- Sacconv(d$W3, 0.4, -0.3)
  expect_equal(c(r$lower, r$upper), c(-1, 1), tolerance = 1e-12)
  expect_equal(r$statistic, 1)
  expect_equal(Saclrt(-3, -4)$statistic, 2)
  V <- rbind(c(0.01, 0.002), c(0.002, 0.02))
  w <- Sacwald(c(0.3, -0.2), V)
  expect_equal(w$statistic, as.numeric(c(0.3, -0.2) %*% solve(V, c(0.3, -0.2))), tolerance = 1e-12)
  expect_equal(w$p_value, exp(-w$statistic / 2), tolerance = 1e-12)
})

test_that("Sacres, Sacimp and Sacrob", {
  expect_equal(Sacres(d$e, d$W)$statistic, .spdiag_moran(d$e, d$W), tolerance = 1e-13)
  Ai <- solve(diag(8) - 0.4 * d$W)
  r <- Sacimp(2, 0.4, 0.7, d$W)
  expect_equal(r$direct, 2 * sum(diag(Ai)) / 8, tolerance = 1e-12)
  expect_equal(r$lambda, 0.7)
  r0 <- Sacrob(d$y, d$X, d$W)
  r1 <- Sacrob(d$y, d$X, d$W, robust = "HC1")
  expect_equal(r1$se, r0$se * sqrt(8 / 5), tolerance = 1e-12)
  expect_equal(r0$se, GS2SLSSAC(d$y, d$X, d$W, robust = "HC0")$se)
})

test_that("Sacvar inverts the Gaussian Fisher information", {
  r <- Sacvar(d$X, d$W, 0.3, -0.2, 0.5, c(1, 2))
  Fm <- .spdiag_fisher(d$X, d$W, c(1, 2), 0.3, -0.2, 0.5, TRUE, TRUE)
  expect_equal(r$information, Fm, tolerance = 1e-9)
  expect_equal(r$cov %*% Fm, diag(5), tolerance = 1e-9)
})

test_that("Sacboot estimate is the SAC maximum likelihood rho", {
  r <- Sacboot(d$y, d$X, d$W, B = 3, seed = 1)
  expect_equal(r$statistic, SpatialRegressionML(d$y, d$X, d$W, "sac")$rho, tolerance = 1e-6)
  expect_equal(r$ci_lower, unname(quantile(r$draws, 0.025)), tolerance = 1e-14)
})
