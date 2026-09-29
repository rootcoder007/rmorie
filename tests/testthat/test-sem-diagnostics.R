d <- .spdiag_data()

test_that("Semjac, Semconv, Semlrt and Semwald", {
  expect_equal(Semjac(d$W, 0.6)$statistic, log(abs(det(diag(8) - 0.6 * d$W))), tolerance = 1e-12)
  C4 <- matrix(c(0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0), 4)
  r <- Semconv(C4, 0.4)
  expect_equal(c(r$lower, r$upper), c(-0.5, 0.5), tolerance = 1e-12)
  expect_equal(r$statistic, 1)
  expect_equal(Semconv(C4, 0.6)$statistic, 0)
  expect_equal(Semconv(C4, -0.3)$logdet, log(abs(det(diag(4) + 0.3 * C4))), tolerance = 1e-12)
  expect_equal(Semlrt(-5, -7.25, 2)$p_value, exp(-4.5 / 2), tolerance = 1e-12)
  expect_equal(Semwald(-0.2, 0.25)$statistic, 0.64, tolerance = 1e-14)
})

test_that("Semflt, Semres, Semlm and Semsc", {
  expect_equal(Semflt(d$y, d$W, 0.2)$filtered, as.vector(d$y - 0.2 * d$W %*% d$y), tolerance = 1e-14)
  f <- as.vector(d$e - 0.3 * d$W %*% d$e)
  expect_equal(Semres(d$e, d$W, 0.3)$statistic, .spdiag_moran(f, d$W), tolerance = 1e-13)
  Tr <- sum(d$W * d$W + d$W * t(d$W))
  lm <- (sum(d$e * d$W %*% d$e) / (sum(d$e^2) / 8))^2 / Tr
  expect_equal(Semlm(d$e, d$W)$statistic, lm, tolerance = 1e-12)
  expect_equal(Semsc(d$e, d$W)$p_value, 1 - pchisq(lm, 1), tolerance = 1e-12)
})

test_that("Semspec is the delta-method common-factor Wald test", {
  V <- rbind(c(0.01, 0, 0), c(0, 0.04, 0.01), c(0, 0.01, 0.09))
  G <- c(1, 0.4, 1)
  expect_equal(Semspec(1, -0.2, V, 0.4)$statistic, 0.2^2 / sum(G * V %*% G), tolerance = 1e-13)
})

test_that("Semvar inverts the Gaussian Fisher information", {
  r <- Semvar(d$X, d$W, 0.3, 0.5)
  Fm <- .spdiag_fisher(d$X, d$W, c(0, 0), 0, 0.3, 0.5, FALSE, TRUE)
  expect_equal(r$information, Fm, tolerance = 1e-9)
  expect_equal(r$statistic, r$cov[3, 3])
})

test_that("Semboot draws refit the error model", {
  r <- Semboot(d$y, d$X, d$W, B = 4, seed = 2)
  expect_equal(r$statistic, SpatialRegressionML(d$y, d$X, d$W, "error")$lambda, tolerance = 1e-6)
  expect_length(r$draws, 4)
  expect_equal(r$se_boot, sd(r$draws))
})
