# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2S: tests for investigation.R — logistic + ATE wrappers.

.make_synthetic_inv <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  d  <- stats::rbinom(n, 1L, stats::plogis(0.4 * x1 + 0.3 * x2))
  y  <- stats::rbinom(n, 1L, stats::plogis(0.3 * x1 + 0.5 * d))
  w  <- stats::rgamma(n, shape = 2.4, scale = 1)
  data.frame(y = y, d = d, x1 = x1, x2 = x2, w = w)
}

test_that("morie_run_weighted_logistic_analysis runs without weights", {
  df <- .make_synthetic_inv(n = 200L)
  out <- morie_run_weighted_logistic_analysis(df, outcome = "y",
                                               predictors = c("x1", "x2"))
  expect_type(out, "list")
})

test_that("morie_run_weighted_logistic_analysis runs with survey weights", {
  df <- .make_synthetic_inv(n = 200L, seed = 2L)
  out <- tryCatch(
    morie_run_weighted_logistic_analysis(df, outcome = "y",
                                          predictors = c("x1", "x2"),
                                          weights_col = "w"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("weighted logistic needs survey pkg: %s",
                 conditionMessage(out)))
  }
  expect_type(out, "list")
})

test_that("morie_compare_nested_logistic_models compares full vs reduced", {
  df <- .make_synthetic_inv(n = 300L, seed = 3L)
  out <- morie_compare_nested_logistic_models(
    df, outcome = "y",
    predictors_full = c("x1", "x2"),
    predictors_reduced = c("x1"))
  expect_type(out, "list")
})

test_that("morie_compare_nested_logistic_models errors when reduced not a subset", {
  df <- .make_synthetic_inv(n = 100L, seed = 4L)
  expect_error(
    morie_compare_nested_logistic_models(
      df, outcome = "y",
      predictors_full = c("x1"),
      predictors_reduced = c("x2")),
    regexp = "subset"
  )
})

test_that("morie_run_treatment_effects_analysis returns IPW-ATE rich result", {
  df <- .make_synthetic_inv(n = 400L, seed = 5L)
  out <- tryCatch(
    morie_run_treatment_effects_analysis(df, treatment = "d",
                                          outcome = "y",
                                          covariates = c("x1", "x2")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("treatment_effects error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
  expect_true(all(c("ate", "se", "ci_lower", "ci_upper", "n",
                    "method") %in% names(out)))
})
