# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Ramsey RESET test. The R side
# (ramsey_reset_test, R/diagnostics.R) predates the Python side
# (morie.fn.rsetf); the anchors below are the values printed by the
# Python implementation to full double precision.
#
# The fixture uses a linear congruential generator with exact integer
# arithmetic and a division by a power of two, so R doubles and Python
# integers agree bit for bit. No qnorm anywhere.

reset_fixture <- function(n = 200L, seed = 20260730) {
  s <- seed
  nxt <- function() {
    s <<- (1664525 * s + 1013904223) %% 4294967296
    (s + 0.5) / 4294967296
  }
  x1 <- numeric(n); x2 <- numeric(n); e <- numeric(n)
  for (i in seq_len(n)) x1[i] <- 2 * nxt() - 1
  for (i in seq_len(n)) x2[i] <- 2 * nxt() - 1
  for (i in seq_len(n)) e[i] <- 2 * nxt() - 1
  X <- cbind(1, x1, x2)
  list(X = X, x1 = x1, x2 = x2,
       y_linear = 1 + 2 * x1 - 0.5 * x2 + e,
       y_quadratic = 1 + 2 * x1 - 0.5 * x2 + e + 1.5 * x1^2)
}

test_that("the fixture is bit-identical across languages", {
  fx <- reset_fixture()
  expect_equal(sum(fx$x1), -15.0063638389111, tolerance = 1e-12)
  expect_equal(sum(fx$x2), -6.12452876567841, tolerance = 1e-12)
})

test_that("RESET matches the Python core on a correctly specified model", {
  fx <- reset_fixture()
  out <- ramsey_reset_test(fx$y_linear, fx$X)
  expect_equal(out$statistic, 4.5355070195310185, tolerance = 1e-9)
  expect_equal(out$p_value, 0.011876696606914755, tolerance = 1e-9)
  expect_equal(out$df, 2L)
})

test_that("RESET matches the Python core on a misspecified model", {
  fx <- reset_fixture()
  out <- ramsey_reset_test(fx$y_quadratic, fx$X)
  expect_equal(out$statistic, 55.857599060373275, tolerance = 1e-9)
  expect_lt(out$p_value, 1e-15)
})

test_that("the omitted quadratic term is detected and the linear one is not", {
  set.seed(20)
  n <- 500L
  x <- stats::rnorm(n)
  X <- cbind(1, x)
  lin <- drop(X %*% c(1, 2)) + stats::rnorm(n, sd = 0.5)
  expect_gt(ramsey_reset_test(lin, X)$p_value, 0.01)
  expect_lt(ramsey_reset_test(lin + 1.5 * x^2, X)$p_value, 1e-6)
})

test_that("the test is invariant to rescaling the response", {
  # the auxiliary regressors are powers of the fitted values, so
  # rescaling y rescales their column span but does not change it
  fx <- reset_fixture()
  a <- ramsey_reset_test(fx$y_quadratic, fx$X)$statistic
  b <- ramsey_reset_test(1000 * fx$y_quadratic, fx$X)$statistic
  expect_equal(a, b, tolerance = 1e-6)
})

test_that("power is absent against misspecification orthogonal to y-hat", {
  # an omitted variable uncorrelated with the fitted values leaves no
  # curvature in the single index, so RESET cannot see it. This is a
  # property of the test, not a defect of the implementation.
  set.seed(21)
  n <- 2000L
  x <- stats::rnorm(n)
  u <- stats::rnorm(n)          # orthogonal to x by construction
  X <- cbind(1, x)
  y <- drop(X %*% c(1, 2)) + 3 * u + stats::rnorm(n, sd = 0.5)
  expect_gt(ramsey_reset_test(y, X)$p_value, 0.01)
})

test_that("the false-positive rate is near nominal under the null", {
  set.seed(22)
  p <- vapply(seq_len(400L), function(i) {
    n <- 200L
    x <- stats::rnorm(n)
    X <- cbind(1, x)
    ramsey_reset_test(drop(X %*% c(1, 2)) + stats::rnorm(n), X)$p_value
  }, numeric(1))
  expect_lt(mean(p < 0.05), 0.10)
  expect_gt(mean(p < 0.05), 0.02)
})
