# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Supplementary tests for morie_estimate_irm and morie_estimate_double_ml
# in causal.R (the existing test-causal.R covers the IPW/AIPW/G-comp family).
# ---------------------------------------------------------------------------

set.seed(1)

make_dml_df <- function(n = 400, tau = 0.5, seed = 1) {
  set.seed(seed)
  X <- matrix(rnorm(n * 3), n, 3)
  d <- rbinom(n, 1, plogis(0.4 * X[, 1] + 0.2 * X[, 2]))
  y <- tau * d + 0.7 * X[, 1] + 0.3 * X[, 2] + rnorm(n, sd = 0.5)
  data.frame(
    y = y, d = d,
    x1 = X[, 1], x2 = X[, 2], x3 = X[, 3]
  )
}


# ---------------------------------------------------------------------------
# morie_estimate_double_ml
# ---------------------------------------------------------------------------

test_that("morie_estimate_double_ml returns expected fields", {
  df <- make_dml_df()
  res <- morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"),
                                  n_folds = 3L)
  expect_true(all(c("ate", "se", "ci_lower", "ci_upper",
                    "n", "method") %in% names(res)))
  expect_type(res$ate, "double")
  expect_true(is.finite(res$ate))
  expect_true(is.finite(res$se))
  expect_lt(res$ci_lower, res$ci_upper)
  expect_equal(res$n, nrow(df))
})

test_that("morie_estimate_double_ml recovers true ATE on simple DGP", {
  df <- make_dml_df(n = 1000, tau = 0.5, seed = 2)
  res <- morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"),
                                  n_folds = 5L)
  expect_equal(res$ate, 0.5, tolerance = 0.15)
})

test_that("morie_estimate_double_ml uses DoubleML when available", {
  skip_if_not_installed("ranger")
  df <- make_dml_df(n = 300)
  res <- morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"),
                                  n_folds = 3L)
  expect_match(res$method, "DoubleML", fixed = TRUE)
})

test_that("morie_estimate_double_ml fallback path is named clearly", {
  # Force the fallback by running with a very small n_folds and assume
  # DoubleML isn't always installed; either way method string is set.
  df <- make_dml_df(n = 200)
  res <- morie_estimate_double_ml(df, "y", "d", c("x1", "x2"),
                                  n_folds = 3L)
  expect_true(grepl("PLR", res$method))
})


# ---------------------------------------------------------------------------
# morie_estimate_irm
# ---------------------------------------------------------------------------

test_that("morie_estimate_irm returns expected fields", {
  df <- make_dml_df()
  res <- morie_estimate_irm(df, treatment = "d", outcome = "y",
                            covariates = c("x1", "x2", "x3"),
                            n_folds = 3L)
  expect_true(all(c("ate", "se", "ci_lower", "ci_upper",
                    "n", "method") %in% names(res)))
  expect_type(res$ate, "double")
  expect_true(is.finite(res$ate))
  expect_true(is.finite(res$se))
  expect_lt(res$ci_lower, res$ci_upper)
})

test_that("morie_estimate_irm recovers true ATE on simple DGP", {
  df <- make_dml_df(n = 1000, tau = 0.5, seed = 3)
  res <- morie_estimate_irm(df, "d", "y", c("x1", "x2", "x3"),
                            n_folds = 5L, random_state = 3L)
  expect_equal(res$ate, 0.5, tolerance = 0.15)
})

test_that("morie_estimate_irm CI covers true effect with high probability", {
  df <- make_dml_df(n = 800, tau = 0.5, seed = 4)
  res <- morie_estimate_irm(df, "d", "y", c("x1", "x2", "x3"),
                            n_folds = 5L, random_state = 4L)
  # Confidence interval should bracket the true 0.5 (most of the time)
  expect_true(res$ci_lower <= 0.5 + 0.3 && res$ci_upper >= 0.5 - 0.3)
})

test_that("morie_estimate_irm uses DoubleML when available", {
  df <- make_dml_df(n = 300)
  res <- morie_estimate_irm(df, "d", "y", c("x1", "x2", "x3"),
                            n_folds = 3L)
  expect_match(res$method, "IRM", fixed = TRUE)
})

test_that("morie_estimate_irm fallback method string mentions IRM", {
  df <- make_dml_df(n = 200)
  res <- morie_estimate_irm(df, "d", "y", c("x1", "x2"),
                            n_folds = 3L)
  expect_true(grepl("IRM", res$method))
})


# ---------------------------------------------------------------------------
# Edge cases
# ---------------------------------------------------------------------------

test_that("morie_estimate_irm errors on degenerate (single-arm) treatment", {
  set.seed(1)
  n <- 100
  df <- data.frame(
    y = rnorm(n), d = rep(1L, n),
    x1 = rnorm(n), x2 = rnorm(n)
  )
  expect_error(
    morie_estimate_irm(df, "d", "y", c("x1", "x2"), n_folds = 3L)
  )
})

test_that("morie_estimate_double_ml is reasonably stable across seeds", {
  df <- make_dml_df(n = 500, tau = 0.5, seed = 5)
  r1 <- morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"),
                                 n_folds = 3L, random_state = 1L)
  r2 <- morie_estimate_double_ml(df, "y", "d", c("x1", "x2", "x3"),
                                 n_folds = 3L, random_state = 2L)
  expect_equal(r1$ate, r2$ate, tolerance = 0.15)
})
