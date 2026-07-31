# SPDX-License-Identifier: AGPL-3.0-or-later
# Ch 2 definitional family. Schabenberger & Gotway (2005), Secs 2.3, 2.4.

EXPO  <- function(h) exp(-3 * h)
GAUSS <- function(h) exp(-3 * h^2)
SPH   <- function(h) ifelse(h <= 1, 1 - 1.5 * h + 0.5 * h^3, 0)
NUG   <- function(h) ifelse(h == 0, 1.3, exp(-3 * h))

test_that("continuous covariances give MS continuity", {
  for (f in list(EXPO, GAUSS, SPH)) expect_true(spcont(f)$is_continuous)
})

test_that("a nugget destroys mean-square continuity", {
  r <- spcont(NUG)
  expect_false(r$is_continuous)
  # probed at a finite h = 1e-6, so the estimate is the nugget plus the
  # continuous part's decay over that lag (3e-6 for exp(-3h))
  expect_equal(r$nugget, 0.3, tolerance = 1e-4)
})

test_that("the decision is shrinkage, not a fixed threshold", {
  expect_lt(spcont(EXPO)$gap_ratio, 0.1)   # shrinking
  expect_gt(spcont(NUG)$gap_ratio, 0.5)    # plateau
})

test_that("the gaussian covariance is MS differentiable with C''(0) = -6", {
  r <- spmsd(GAUSS, m = 1)
  expect_true(r$is_differentiable)
  expect_equal(r$derivative_2m, -6, tolerance = 1e-4)
  expect_true(spmsd(GAUSS, m = 2)$is_differentiable)
})

test_that("kinked covariances are not MS differentiable", {
  for (f in list(EXPO, SPH)) {
    r <- spmsd(f, m = 1)
    expect_false(r$is_differentiable)
    expect_gt(r$growth_ratio, 1.5)         # diverges under refinement
  }
})

test_that("the step size adapts to the order and precision is reported", {
  h1 <- spmsd(GAUSS, m = 1)$h
  h2 <- spmsd(GAUSS, m = 2)$h
  expect_gt(h2, h1)                        # coarser stencil for higher order
  expect_gt(spmsd(GAUSS, m = 1)$significant_digits,
            spmsd(GAUSS, m = 3)$significant_digits)
})

test_that("derivative-field covariance is (-1)^m times the derivative", {
  r <- spmsd(GAUSS, m = 1)
  expect_equal(r$derivative_cov, -r$derivative_2m)
  expect_gt(r$derivative_cov, 0)           # it is a variance
})

test_that("empirical covariance recovers the sill", {
  set.seed(0)
  co <- matrix(stats::runif(800), 400, 2) * 10
  r <- spcovf(co, stats::rnorm(400, 0, 2), n_bins = 6)
  expect_equal(r$sill, 4, tolerance = 0.25)
})

test_that("implied semivariogram matches the direct estimate", {
  set.seed(1)
  co <- matrix(stats::runif(1000), 500, 2) * 10
  r <- spcovf(co, stats::rnorm(500, 0, 2), n_bins = 6)
  ok <- !is.na(r$covariance) & !is.na(r$semivariogram)
  expect_lt(max(abs(r$semivariogram[ok] - r$implied_semivariogram[ok])), 0.5)
})

test_that("input validation", {
  expect_error(spcont("nope"), "must be a function")
  expect_error(spmsd("nope"), "must be a function")
  expect_error(spmsd(GAUSS, m = 0), "`m` must be")
  expect_error(spmsd(GAUSS, h = 0), "`h` must be")
  expect_error(spcovf(matrix(0, 5, 2), numeric(4)), "same number of rows")
})
