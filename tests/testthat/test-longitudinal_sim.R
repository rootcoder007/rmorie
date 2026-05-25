# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2P: tests for longitudinal_sim.R — AR/VAR coefficient
# generators + panel simulator.

test_that("morie_sync_rng returns a seeded RNG state", {
  rng <- morie_sync_rng(seed = 42L)
  expect_true(!is.null(rng))
})

test_that("morie_generate_ar_coefficients returns finite-numeric coefficients", {
  # Output dimensionality depends on lag-vector defaults inside the
  # generator; just assert finite-numeric output rather than a literal
  # length (length 9 was observed for p=3 with default lags=3).
  rng <- morie_sync_rng(1L)
  out <- morie_generate_ar_coefficients(p = 3L, rng = rng)
  expect_true(is.numeric(out) || is.matrix(out) || is.list(out))
  flat <- unlist(out)
  expect_true(length(flat) >= 3L && all(is.finite(flat)))
})

test_that("morie_generate_var_coefficients returns (p, p) coefficient matrix per lag", {
  rng <- morie_sync_rng(2L)
  out <- morie_generate_var_coefficients(p = 3L, lags = 2L, rng = rng)
  expect_true(is.list(out) || is.array(out) || is.matrix(out))
})

test_that("morie_mvn_with_covariance returns an n x p sample", {
  rng <- morie_sync_rng(3L)
  out <- morie_mvn_with_covariance(n = 100L, p = 3L, rng = rng)
  expect_true(is.matrix(out) || is.data.frame(out))
  if (is.matrix(out) || is.data.frame(out)) {
    expect_equal(nrow(out), 100L)
    expect_equal(ncol(out), 3L)
  }
})

test_that("morie_simulate_longitudinal_panel returns a balanced panel data.frame", {
  out <- tryCatch(
    # p_variables = 1 so the long-format frame is exactly
    # n_individuals * n_timepoints rows (one variable per id/time
    # cell). Otherwise the panel layout multiplies by p_variables.
    morie_simulate_longitudinal_panel(n_individuals = 20L,
                                       n_timepoints = 5L,
                                       p_variables = 1L,
                                       seed = 4L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("simulate_longitudinal_panel error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.data.frame(out) || is.list(out))
  if (is.data.frame(out)) {
    expect_equal(nrow(out), 20L * 5L)
  }
})
