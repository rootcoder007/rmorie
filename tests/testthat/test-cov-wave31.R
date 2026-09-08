# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 31 -- morie_dcc_multivariate_garch().
#
# This used to mock rmgarch::dccfit / rugarch::likelihood and assert the
# glue around them. That branch no longer exists: the estimator is
# Engle's two-step DCC computed natively end to end, so the mocks bound
# functions nobody called and the expectations (method "DCC(1,1) via
# rmgarch", loglik -123.45) described a code path that had been removed.
#
# The data below carries a deliberate correlation regime shift. That
# matters for more than realism: on i.i.d. draws the fitted recursion
# sits at Q_bar and the correlation path is flat, so a 1% error in the
# persistence term b moves it by only 7e-14 and no tolerance worth
# setting would catch it. With the shift in place, 1% errors in a and b
# move the path by 2e-3 and 6e-3, which these tolerances do catch.

.wave31_data <- function() {
  set.seed(31)
  n <- 60L
  e <- matrix(stats::rnorm(n * 3), n, 3)
  f <- stats::rnorm(n)
  w <- c(rep(0, n / 2), rep(1.5, n / 2))
  X <- e
  for (j in seq_len(3)) X[, j] <- e[, j] + w * f
  X
}

test_that("the native DCC estimator reports a stationary DCC(1,1) fit", {
  X <- .wave31_data()
  res <- morie_dcc_multivariate_garch(X)

  expect_equal(res$method, "DCC(1,1) two-step Gaussian QMLE")
  expect_equal(res$n, 60L)
  expect_equal(res$k, 3L)
  expect_equal(dim(res$conditional_variance), c(60L, 3L))
  expect_equal(dim(res$conditional_correlation), c(60L, 3L, 3L))
  expect_true(all(res$conditional_variance > 0))
  expect_true(is.finite(res$loglik))
  # a, b are the DCC news and persistence terms: both non-negative and
  # jointly stationary, or the correlation recursion would not converge.
  expect_gte(res$a, 0)
  expect_gte(res$b, 0)
  expect_lt(res$a + res$b, 1)
})

test_that("the fitted path tracks the correlation regime shift", {
  X <- .wave31_data()
  res <- morie_dcc_multivariate_garch(X)
  r12 <- res$conditional_correlation[, 1, 2]

  # Without this the recursion checks below could pass on a flat path.
  expect_gt(diff(range(r12)), 0.2)
  # The shift is at t = 30, so the second half must be the correlated
  # half. This fails if the recursion runs backwards or ignores Z.
  expect_gt(mean(r12[31:60]), mean(r12[1:30]) + 0.1)
})

test_that("the correlation path matches the recursion recomputed here", {
  X <- .wave31_data()
  res <- morie_dcc_multivariate_garch(X)

  # Independent route: rebuild the standardised residuals from the
  # reported conditional variances, then walk Engle's recursion with the
  # reported a and b. Nothing below reuses the estimator's own path, so
  # a wrong recursion, a transposed update or an off-by-one in t fails.
  n <- nrow(X)
  k <- ncol(X)
  Z <- matrix(NA_real_, n, k)
  for (j in seq_len(k)) {
    rj <- X[, j] - mean(X[, j])
    Z[, j] <- rj / sqrt(res$conditional_variance[, j] + 1e-12)
  }
  Q_bar <- crossprod(Z) / n
  expect_equal(res$unconditional_correlation, Q_bar, tolerance = 1e-10)

  Q <- Q_bar
  for (t in seq_len(n)) {
    d <- sqrt(pmax(diag(Q), 1e-12))
    expect_equal(res$conditional_correlation[t, , ], Q / outer(d, d),
                 tolerance = 1e-10)
    Q <- (1 - res$a - res$b) * Q_bar + res$a * tcrossprod(Z[t, ]) +
      res$b * Q
  }
})

test_that("every reported correlation matrix is a correlation matrix", {
  R <- morie_dcc_multivariate_garch(.wave31_data())$conditional_correlation
  for (t in seq_len(dim(R)[1])) {
    Rt <- R[t, , ]
    expect_equal(diag(Rt), rep(1, ncol(Rt)), tolerance = 1e-12)
    expect_equal(Rt, t(Rt), tolerance = 1e-12)
    expect_true(all(abs(Rt) <= 1 + 1e-9))
  }
})

test_that("the estimator rejects a sample it cannot fit", {
  expect_error(morie_dcc_multivariate_garch(matrix(stats::rnorm(20), 10, 2)),
               "n>=30")
  expect_error(morie_dcc_multivariate_garch(matrix(stats::rnorm(40), 40, 1)),
               "k>=2")
})
