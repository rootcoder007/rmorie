# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P5: the fairness functions must agree with research/lean/P5Fairness.lean.

set.seed(1)

test_that("Chouldechova's identity holds on any confusion table", {
  set.seed(1)
  for (i in 1:50) {
    cells <- stats::runif(4, 1, 500)
    r <- morie_fairness_rates(cells[1], cells[2], cells[3], cells[4])
    expect_equal(morie_fairness_implied_fpr(r$p, r$ppv, r$fnr), r$fpr, tolerance = 1e-12)
  }
})

test_that("equal ppv and fnr with different base rates force different fpr (impossibility)", {
  set.seed(1)
  f1 <- morie_fairness_implied_fpr(0.30, 0.6, 0.25)
  f2 <- morie_fairness_implied_fpr(0.50, 0.6, 0.25)
  expect_false(isTRUE(all.equal(f1, f2)))
  expect_gt(f2, f1)   # higher base rate, higher implied fpr, other things equal
})

test_that("base-rate bounds are sharp: the corner noise rates attain them", {
  set.seed(1)
  p <- 0.37; a <- 0.08; b <- 0.15
  # lower end: alpha = a, beta = 0
  p_obs_low <- p * (1 - 0) + (1 - p) * a
  bl <- morie_fairness_base_rate_bounds(p_obs_low, a, b)
  expect_equal(bl$lower, p, tolerance = 1e-12)
  # upper end: alpha = 0, beta = b
  p_obs_up <- p * (1 - b) + (1 - p) * 0
  bu <- morie_fairness_base_rate_bounds(p_obs_up, a, b)
  expect_equal(bu$upper, p, tolerance = 1e-12)
  # any admissible noise pair keeps the truth inside the interval
  for (i in 1:200) {
    al <- stats::runif(1, 0, a); be <- stats::runif(1, 0, b)
    p_obs <- p * (1 - be) + (1 - p) * al
    bb <- morie_fairness_base_rate_bounds(p_obs, a, b)
    expect_lte(bb$lower, p + 1e-12)
    expect_gte(bb$upper, p - 1e-12)
  }
})

test_that("known noise rates invert exactly", {
  set.seed(1)
  p <- c(0.1, 0.4, 0.8)
  p_obs <- p * (1 - 0.2) + (1 - p) * 0.1
  expect_equal(morie_fairness_true_rate(p_obs, 0.1, 0.2), p, tolerance = 1e-12)
})

test_that("argument checks", {
  set.seed(1)
  expect_error(morie_fairness_rates(0, 1, 1, 1), "positive")
  expect_error(morie_fairness_implied_fpr(1, 0.5, 0.1), "in \\(0, 1\\)")
  expect_error(morie_fairness_base_rate_bounds(0.5, 0.6, 0.5), "below 1")
  expect_error(morie_fairness_true_rate(0.5, 0.6, 0.5), "alpha \\+ beta")
})
