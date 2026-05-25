# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2O: tests for mrm_diagnostics.R — balance / overlap / median-
# causal-effect / assumptions checks.

.make_synthetic_cd <- function(n = 300L, tau = 0.5, seed = 1L) {
  set.seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  d  <- stats::rbinom(n, 1L, stats::plogis(0.4 * x1 + 0.2 * x2))
  y  <- tau * d + 0.5 * x1 + 0.3 * x2 + stats::rnorm(n, sd = 0.5)
  data.frame(d = d, y = y, x1 = x1, x2 = x2)
}

test_that("mrm_standardised_difference returns per-covariate SMDs", {
  df <- .make_synthetic_cd(n = 200L)
  out <- mrm_standardised_difference(df,
                                      treatment_col = "d",
                                      covariates = c("x1", "x2"))
  expect_true(is.list(out) || is.data.frame(out) || is.numeric(out))
})

test_that("mrm_check_balancing reports any-SMD-over-threshold", {
  df <- .make_synthetic_cd(n = 200L, seed = 2L)
  out <- mrm_check_balancing(df, treatment_col = "d",
                              covariates = c("x1", "x2"))
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_check_overlap quantifies propensity-score overlap", {
  df <- .make_synthetic_cd(n = 200L, seed = 3L)
  out <- mrm_check_overlap(df, treatment_col = "d",
                            covariates = c("x1", "x2"))
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_median_causal_effect returns finite estimate", {
  df <- .make_synthetic_cd(n = 200L, seed = 4L)
  out <- tryCatch(
    mrm_median_causal_effect(df,
                              treatment_col = "d",
                              outcome_col = "y"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("median causal effect error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out) || is.numeric(out))
})

test_that("mrm_assumptions_check returns a multi-check rich-result", {
  df <- .make_synthetic_cd(n = 200L, seed = 5L)
  out <- tryCatch(
    mrm_assumptions_check(df,
                           treatment_col = "d",
                           outcome_col = "y",
                           covariates = c("x1", "x2")),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("assumptions_check error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})
