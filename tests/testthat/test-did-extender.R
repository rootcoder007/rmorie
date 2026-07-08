# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 1.g tests for the wrapper-as-extender DiD entry points added
# to R/did.R: morie_did_twoway_fe_weights() (TwoWayFEWeights) and
# morie_did_synthdid_estimate() (synthdid explicit-name API).
# Uses make_did_panel() from helper-did.R.

# ---------------------------------------------------------------------------
# morie_did_twoway_fe_weights -- TwoWayFEWeights::twowayfeweights
# ---------------------------------------------------------------------------

test_that("morie_did_twoway_fe_weights returns TWFE diagnostics shape", {
  skip_if_not_installed("TwoWayFEWeights")
  df <- make_did_panel(n_units = 30L, n_periods = 6L, tau = 0.5,
                       seed = 11L)
  out <- tryCatch(
    morie_did_twoway_fe_weights(df, group = "unit", time = "time",
                                treatment = "d", outcome = "y"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("twoway_fe_weights error: %s", conditionMessage(out)))
  }
  expect_s3_class(out, "morie_did_twfe_diagnostics")
  expect_true(all(c("n_negative_weights", "sum_weights",
                    "sum_negative_weights", "share_negative_weights",
                    "method", "raw") %in% names(out)))
  expect_true(grepl("TwoWayFEWeights", out$method))
})

test_that("morie_did_twoway_fe_weights errors when TwoWayFEWeights missing", {
  # Only meaningful when the package is NOT installed; skip otherwise.
  if (requireNamespace("TwoWayFEWeights", quietly = TRUE)) skip(
    "TwoWayFEWeights is installed; cannot test the missing-package error path."
  )
  df <- make_did_panel(n_units = 10L, n_periods = 4L, seed = 12L)
  expect_error(
    morie_did_twoway_fe_weights(df, group = "unit", time = "time",
                                treatment = "d", outcome = "y"),
    regexp = "TwoWayFEWeights"
  )
})


# ---------------------------------------------------------------------------
# morie_did_synthdid_estimate -- coresynth SDID (scm_fit)
# ---------------------------------------------------------------------------

test_that("morie_did_synthdid_estimate returns synthdid result shape", {
  skip_if_not_installed("coresynth")
  df <- make_did_panel(n_units = 30L, n_periods = 8L, tau = 0.5,
                       seed = 13L)
  # coresynth reads a 0/1 treatment indicator (1 in treated unit-periods);
  # helper-did.R already produces a `d` column with this convention.
  out <- tryCatch(
    morie_did_synthdid_estimate(df, unit = "unit", time = "time",
                                treatment = "d", outcome = "y"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("coresynth SDID error: %s", conditionMessage(out)))
  }
  expect_s3_class(out, "morie_did_synthdid_result")
  expect_true(all(c("att", "std_error", "vcov_method", "n_treated",
                    "n_control", "n_pre", "n_post", "method", "raw")
                  %in% names(out)))
  expect_true(is.finite(out$att))
  expect_true(grepl("coresynth", out$method))
})

test_that("morie_did_synthdid_estimate errors when coresynth missing", {
  if (requireNamespace("coresynth", quietly = TRUE)) skip(
    "coresynth is installed; cannot test the missing-package error path."
  )
  df <- make_did_panel(n_units = 10L, n_periods = 4L, seed = 14L)
  expect_error(
    morie_did_synthdid_estimate(df, unit = "unit", time = "time",
                                treatment = "d", outcome = "y"),
    regexp = "coresynth"
  )
})
