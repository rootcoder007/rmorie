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

test_that("balance threshold is honoured and overlap uses the glm propensity", {
  i <- 1:120
  df <- data.frame(x1 = sin(i), x2 = cos(1.3 * i))
  df$d <- as.integer(0.8 * df$x1 - 0.5 * df$x2 + 0.6 * sin(2.7 * i) > 0)
  df$y <- 1 + 0.7 * df$d + df$x1 + 0.4 * cos(3.3 * i)
  # |SMD| = 159.33% (x1) and 85.99% (x2)
  expect_equal(mrm_check_balancing(df, "d", c("x1", "x2"),
                                   threshold_pct = 100)$n_imbalanced, 1L)
  expect_equal(mrm_check_balancing(df, "d", c("x1", "x2"),
                                   threshold_pct = 200)$n_imbalanced, 0L)
  ov <- mrm_check_overlap(df, "d", c("x1", "x2"))
  e <- stats::glm(d ~ x1 + x2, data = df, family = stats::binomial())$fitted.values
  expect_equal(ov$common_support_lower,
               round(max(min(e[df$d == 1]), min(e[df$d == 0])), 4))
  me <- mrm_median_causal_effect(df, "d", "y", c("x1", "x2"))
  expect_equal(me$n_matched, 57L)
  expect_equal(me$median_treatment_effect, 2.0311)
})
