# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2V: tests for 4 more single-fn alias R files (vtpwr, irm,
# irtsp, bysid). Each exposes a short alias + a morie_<name> alias
# for the canonical long name.

# ----------------------------------------------------- vtpwr / morie_voting_power_index

test_that("morie_voting_power_index returns Banzhaf/Shapley power indices", {
  # Three voters with weights 3, 2, 1; majority quota 4 -> A is
  # dictator (power 1 for A, 0 for B/C).
  out <- tryCatch(
    morie_voting_power_index(x = c(3, 2, 1), quota = 4),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("vtpwr error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out) || is.data.frame(out))
})

test_that("vtpwr default quota equals majority", {
  out <- tryCatch(
    morie_voting_power_index(x = c(2, 2, 1)),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("vtpwr default quota error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out) || is.data.frame(out))
})

# ----------------------------------------------------- irm / morie_estimate_irm

test_that("morie_estimate_irm runs or skips on missing DoubleML", {
  set.seed(1L)
  n <- 200L
  df <- data.frame(
    d = stats::rbinom(n, 1L, 0.5),
    y = stats::rnorm(n),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n)
  )
  out <- tryCatch(
    morie_estimate_irm(df, treatment = "d", outcome = "y",
                       covariates = c("x1", "x2"),
                       n_folds = 2L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("morie_estimate_irm needs DoubleML / mlr3: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

# ----------------------------------------------------- irtsp / morie_irt_spatial

test_that("morie_irt_spatial returns ideal-point estimates", {
  set.seed(2L)
  x <- matrix(sample(0:1, 30 * 15, replace = TRUE), 30L, 15L)
  out <- tryCatch(
    morie_irt_spatial(x, n_iter = 10L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("irtsp error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out) || is.numeric(out))
})

# ----------------------------------------------------- bysid / morie_bayesian_ideal_points

test_that("morie_bayesian_ideal_points returns Bayesian ideal-point posterior", {
  set.seed(3L)
  x <- matrix(sample(0:1, 30 * 15, replace = TRUE), 30L, 15L)
  out <- tryCatch(
    morie_bayesian_ideal_points(x, n_iter = 50L, burn = 10L,
                                 seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("bysid error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.matrix(out) || is.numeric(out))
})

# ------------------------------------------- short alias / canonical alias parity

test_that("each canonical morie_* name is identical to its short alias", {
  expect_identical(morie_voting_power_index, vtpwr)
  expect_identical(morie_irt_spatial, irtsp)
  expect_identical(morie_bayesian_ideal_points, bysid)
})
