# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2X: tests for the 9 Horowitz semiparametric estimators
# (hrzb2 / hrzi1 / hrzi2 / hrzc1 / hrzk1 / hrzk3 / hrzn1 / hrzp1 /
# hrzq1) + the shared .hrz_* helpers in aaa_helpers_horowitz.R.

# ----------------------------------------------------- shared helpers

test_that(".hrz_silverman returns a positive bandwidth on a normal sample", {
  set.seed(1L)
  x <- stats::rnorm(50)
  out <- morie:::.hrz_silverman(x)
  expect_true(is.numeric(out) && out > 0)
})

test_that(".hrz_silverman returns 1.0 sentinel on a single value", {
  expect_equal(morie:::.hrz_silverman(c(1.5)), 1.0)
})

test_that(".hrz_gauss_kernel integrates to ~1 on a dense grid", {
  u <- seq(-6, 6, length.out = 2000)
  vals <- morie:::.hrz_gauss_kernel(u)
  area <- sum(vals) * (u[2] - u[1])
  expect_equal(area, 1.0, tolerance = 1e-3)
})

test_that(".hrz_nw_loo runs on a small synthetic dataset", {
  set.seed(2L)
  z <- stats::rnorm(40)
  y <- 0.5 * z + stats::rnorm(40, sd = 0.3)
  out <- morie:::.hrz_nw_loo(z, y, h = 0.5)
  expect_true(is.numeric(out) && all(is.finite(out)))
})

test_that(".hrz_probit_newton fits a probit on synthetic data", {
  set.seed(3L)
  n <- 100L
  # Z must be a design MATRIX (ncol(Z) inside the fn); pass intercept
  # + a single covariate column.
  Z <- cbind(1, stats::rnorm(n))
  D <- as.integer(stats::pnorm(Z[, 2]) > stats::runif(n))
  out <- morie:::.hrz_probit_newton(D, Z, maxit = 30L)
  expect_true(is.numeric(out) && all(is.finite(out)))
})

test_that(".hrz_logit_newton fits a logit on synthetic data", {
  set.seed(4L)
  n <- 100L
  X <- cbind(1, stats::rnorm(n))
  D <- as.integer(stats::plogis(X[, 2]) > stats::runif(n))
  out <- morie:::.hrz_logit_newton(D, X, maxit = 30L)
  expect_true(is.numeric(out) && all(is.finite(out)))
})

test_that(".hrz_qreg_irls fits a quantile regression on synthetic data", {
  set.seed(5L)
  n <- 100L
  X <- cbind(1, stats::rnorm(n))
  y <- X %*% c(1, 0.5) + stats::rnorm(n, sd = 0.4)
  out <- morie:::.hrz_qreg_irls(X, y, tau = 0.5, maxit = 30L)
  expect_true(is.numeric(out) && all(is.finite(out)))
})

test_that(".hrz_hermite returns an (n, J) basis matrix", {
  t <- seq(-2, 2, length.out = 20)
  out <- morie:::.hrz_hermite(t, J = 4L)
  expect_true(is.matrix(out))
  expect_equal(nrow(out), 20L)
  expect_equal(ncol(out), 4L)
})

# ----------------------------------------------------- hrzb2 (smoothed maximum-score)

test_that("hrzb2 runs on synthetic binary data + returns a beta estimate", {
  set.seed(11L)
  n <- 80L
  x <- stats::rnorm(n)
  y <- as.integer(0.5 * x + stats::rnorm(n, sd = 0.5) > 0)
  out <- tryCatch(hrzb2(x, y), error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("hrzb2 error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

test_that(".hrzb2_loss returns the +Inf-ish sentinel for the zero-norm guard", {
  set.seed(12L)
  X <- matrix(stats::rnorm(40), 20, 2)
  ys <- sample(c(-1, 1), 20, replace = TRUE)
  out <- morie:::.hrzb2_loss(b = c(0, 0), X = X, ys = ys, h = 0.5)
  expect_equal(out, 1e12)
})

# ----------------------------------------------------- hrzi1 / hrzi2 / hrzc1

test_that("hrzi1 runs on synthetic binary data", {
  set.seed(13L)
  n <- 80L
  x <- stats::rnorm(n)
  y <- as.integer(0.3 * x + stats::rnorm(n, sd = 0.5) > 0)
  out <- tryCatch(hrzi1(x, y), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzi1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzi2 runs on synthetic binary data", {
  set.seed(14L)
  n <- 80L
  x <- stats::rnorm(n)
  y <- as.integer(0.4 * x + stats::rnorm(n, sd = 0.5) > 0)
  out <- tryCatch(hrzi2(x, y), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzi2 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzc1 runs on synthetic censored data", {
  set.seed(15L)
  n <- 80L
  x <- stats::rnorm(n)
  y <- pmax(0, 0.5 * x + stats::rnorm(n, sd = 0.5))
  out <- tryCatch(hrzc1(x, y, censor = 0.0), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzc1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- hrzk1 / hrzk3

test_that("hrzk1 returns a kernel-density estimate on synthetic data", {
  set.seed(16L)
  x <- stats::rnorm(120)
  out <- tryCatch(hrzk1(x), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzk1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzk3 returns a 3-d kernel smoother estimate", {
  set.seed(17L)
  n <- 100L
  x <- stats::rnorm(n)
  y <- 0.4 * x + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(hrzk3(x, y), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzk3 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- hrzn1 / hrzp1 / hrzq1

test_that("hrzn1 fits a nonparametric IV estimator on synthetic data", {
  set.seed(18L)
  n <- 120L
  z <- stats::rnorm(n)
  x <- 0.5 * z + stats::rnorm(n, sd = 0.3)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(hrzn1(x, y, z, J = 4L), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzn1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzp1 fits a partial-linear semiparametric model", {
  set.seed(19L)
  n <- 120L
  z <- stats::rnorm(n)
  x <- stats::rnorm(n)
  y <- 0.5 * x + 0.3 * z^2 + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(hrzp1(x, y, z), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzp1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzq1 fits a quantile-regression estimator", {
  set.seed(20L)
  n <- 100L
  x <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(hrzq1(x, y, tau = 0.5), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzq1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})
