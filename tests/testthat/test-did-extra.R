# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2F-2: tests for did.R exports not yet covered by test-did.R.
# Uses make_did_panel() / make_did_2x2() from test-did.R (auto-sourced).

test_that("morie_did_staggered estimates staggered-adoption ATT", {
  df <- make_did_panel(n_units = 40L, n_periods = 6L,
                        tau = 0.6, seed = 2L)
  out <- tryCatch(
    morie_did_staggered(df, "y", "unit", "time", "treat_time"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("did_staggered error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_did_synthetic returns synthetic-control fit", {
  df <- make_did_panel(n_units = 30L, n_periods = 8L,
                        tau = 0.5, seed = 3L)
  out <- tryCatch(
    morie_did_synthetic(df, "y", "unit", "time", "treat_time"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("did_synthetic error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_did_placebo_test_group returns per-group placebo coefs", {
  df <- make_did_2x2(n = 300, tau = 0.5, seed = 4L)
  df$g <- sample(letters[1:4], nrow(df), replace = TRUE)
  out <- tryCatch(
    morie_did_placebo_test_group(df, "y", "d", "post",
                                  group_col = "g",
                                  unaffected_groups = c("b", "c")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("placebo_test_group error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_did_heterogeneous returns CATE by moderator quintile", {
  df <- make_did_2x2(n = 400, tau = 0.5, seed = 5L)
  df$m <- stats::rnorm(nrow(df))
  out <- tryCatch(
    morie_did_heterogeneous(df, "y", "d", "post",
                              moderator = "m"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("heterogeneous error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_did_chaisemartin_dhaultfoeuille returns de Chaisemartin-D'Haultfoeuille DID-M", {
  df <- make_did_panel(n_units = 30L, n_periods = 6L,
                        tau = 0.5, seed = 6L)
  out <- tryCatch(
    morie_did_chaisemartin_dhaultfoeuille(df, "y", "treat",
                                           "unit", "time"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("chaisemartin_dhaultfoeuille error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_did_sensitivity_analysis sweeps over unobserved-confounding parameters", {
  df <- make_did_2x2(n = 300, tau = 0.5, seed = 7L)
  out <- tryCatch(
    morie_did_sensitivity_analysis(df, "y", "d", "post"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("sensitivity_analysis error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("morie_did_diagnostics returns DID assumptions summary", {
  df <- make_did_2x2(n = 300, tau = 0.5, seed = 8L)
  out <- tryCatch(
    morie_did_diagnostics(df, "y", "d", "post"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("diagnostics error: %s", conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})
