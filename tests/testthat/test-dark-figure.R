# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P1: the dark-figure functions must agree with research/lean/P1DarkFigure.lean.

set.seed(1)

test_that("Lincoln-Petersen recovers the true count under independence (lincoln_petersen)", {
  set.seed(1)
  N <- 1250; p1 <- 0.32; p2 <- 0.2
  n1 <- N * p1; n2 <- N * p2; m <- N * p1 * p2
  out <- morie_dark_figure_two_source(n1, n2, m)
  expect_equal(out$point, N, tolerance = 1e-12)
  expect_equal(out$lower, N, tolerance = 1e-12)
  expect_equal(out$theorem, "Research.P1.TwoSource.lincoln_petersen")
})

test_that("dependence box gives the proved interval and both ends are attained (petersen_bounds)", {
  set.seed(1)
  N <- 1250; p1 <- 0.32; p2 <- 0.2; kappa <- 2
  for (theta in c(1 / kappa, 0.8, 1, 1.5, kappa)) {
    n1 <- N * p1; n2 <- N * p2; m <- N * p1 * p2 * theta
    out <- morie_dark_figure_two_source(n1, n2, m, kappa = kappa)
    expect_lte(out$lower, N + 1e-9)
    expect_gte(out$upper, N - 1e-9)
    expect_equal(out$point * theta, N, tolerance = 1e-12)   # petersen_identity
  }
  # sharpness: at theta = 1/kappa the lower end equals N, at theta = kappa the upper end does
  m_lo <- N * p1 * p2 / kappa
  expect_equal(morie_dark_figure_two_source(N * p1, N * p2, m_lo, kappa = kappa)$lower, N, tolerance = 1e-12)
  m_hi <- N * p1 * p2 * kappa
  expect_equal(morie_dark_figure_two_source(N * p1, N * p2, m_hi, kappa = kappa)$upper, N, tolerance = 1e-12)
})

test_that("chapman's estimator is returned on request", {
  set.seed(1)
  out <- morie_dark_figure_two_source(400, 250, 80, chapman = TRUE)
  expect_equal(out$chapman, 401 * 251 / 81 - 1)
})

test_that("dark-figure interval contains the truth for every admissible noise pair (dark_figure_bounds)", {
  set.seed(1)
  v <- 0.06; r <- 0.02; a <- 0.01; b <- 0.30
  for (i in 1:300) {
    al <- stats::runif(1, 0, a); be <- stats::runif(1, 0, b)
    v_obs <- v * (1 - be) + (1 - v) * al
    d <- morie_dark_figure_bounds(v_obs, r, a, b)
    expect_lte(d$v_lower, v + 1e-12)
    expect_gte(d$v_upper, v - 1e-12)
    expect_lte(d$dark_lower, v - r + 1e-12)
    expect_gte(d$dark_upper, v - r - 1e-12)
    expect_gte(d$v_lower, r)          # recording only loses events
  }
})

test_that("the recorded rate tightens the survey interval when it is informative", {
  set.seed(1)
  # survey 0.03 with 30 percent under-reporting allowed, police recorded 0.05: v >= 0.05
  d <- morie_dark_figure_bounds(v_obs = 0.03, r = 0.05, alpha_max = 0.01, beta_max = 0.30)
  expect_equal(d$v_lower, 0.05)
  expect_gte(d$v_upper, d$v_lower)
  expect_equal(d$dark_lower, 0)
})

test_that("argument checks", {
  set.seed(1)
  expect_error(morie_dark_figure_two_source(10, 10, 20), "smaller list")
  expect_error(morie_dark_figure_two_source(10, 10, 5, kappa = 0.5), "at least 1")
  expect_error(morie_dark_figure_bounds(0.5, 0.1, 0.6, 0.5), "below 1")
  expect_error(morie_dark_figure_bounds(c(0.5, 0.4), c(0.1, 0.1, 0.1), 0.1, 0.1), "length")
})

test_that("breakdown value separates surviving from failing conclusions (conclusion_*_breakdown)", {
  set.seed(1)
  b <- morie_dark_figure_breakdown(0.06, threshold = 0.10)
  expect_equal(b$breakdown_beta_max, 0.4)
  # below the breakdown every admissible noise pair keeps v below the threshold
  for (i in 1:200) {
    al <- stats::runif(1, 0, 0.01); be <- stats::runif(1, 0, 0.39)
    v <- (0.06 - al) / (1 - al - be)
    expect_lt(v, 0.10)
  }
  # at or above it the witness alpha = 0, beta = beta_max reaches the threshold
  expect_gte(0.06 / (1 - 0.4), 0.10 - 1e-12)
  expect_error(morie_dark_figure_breakdown(0.06, threshold = 0.05), "above v_obs")
})
