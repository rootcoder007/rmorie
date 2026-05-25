# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/effects.R

set.seed(2026L)
mk_data <- function(n = 60L) {
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  z  <- rbinom(n, 1, 0.5)
  d  <- as.integer(plogis(0.3 + 0.8 * z + 0.4 * x1) > runif(n))
  y  <- 1 + 0.5 * d + 0.3 * x1 + 0.2 * x2 + rnorm(n)
  data.frame(y = y, d = d, z = z, x1 = x1, x2 = x2,
             wt = 1 + abs(rnorm(n, mean = 1)))
}

test_that("estimate_ate returns ate and se", {
  df <- mk_data(60L)
  res <- estimate_ate(df, "y", "d", "wt")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_true(is.numeric(res$se))
  expect_true(res$se > 0)
})

test_that("estimate_plr falls back to base R cross-fit ridge", {
  df <- mk_data(80L)
  res <- estimate_plr(df, "d", "y", c("x1", "x2"), n_folds = 3L)
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_true(is.numeric(res$se))
  expect_true(is.numeric(res$ci_lower))
  expect_true(is.numeric(res$ci_upper))
  expect_true(is.numeric(res$pval))
  expect_equal(res$n_obs, 80L)
  expect_true(is.character(res$method))
})

test_that("estimate_plr errors on missing columns", {
  df <- mk_data(40L)
  expect_error(estimate_plr(df, "d", "y", c("x1", "missing")),
               "missing")
})

test_that("estimate_plr errors on n_folds < 2", {
  df <- mk_data(40L)
  expect_error(estimate_plr(df, "d", "y", c("x1"), n_folds = 1L),
               "n_folds")
})

test_that("estimate_pliv 2SLS fallback works", {
  df <- mk_data(80L)
  # Suppress the fallback warning
  res <- suppressWarnings(
    estimate_pliv(df, "d", "y", "z", c("x1", "x2"))
  )
  expect_true(is.list(res))
  expect_true(is.numeric(res$late))
  expect_true(is.numeric(res$se))
  expect_equal(res$n_obs, 80L)
})

test_that("estimate_pliv errors on missing columns", {
  df <- mk_data(40L)
  expect_error(estimate_pliv(df, "d", "y", "z", c("missing")),
               "missing")
})

test_that("estimate_ate_gcomputation linear path", {
  df <- mk_data(60L)
  res <- estimate_ate_gcomputation(df, "d", "y", c("x1", "x2"),
                                    outcome_model = "linear")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_equal(res$n_obs, 60L)
  expect_equal(res$outcome_model, "linear")
})

test_that("estimate_ate_gcomputation logistic path", {
  df <- mk_data(60L)
  df$y_bin <- as.integer(df$y > median(df$y))
  res <- estimate_ate_gcomputation(df, "d", "y_bin", c("x1", "x2"),
                                    outcome_model = "logistic")
  expect_true(is.list(res))
  expect_true(is.numeric(res$ate))
  expect_equal(res$outcome_model, "logistic")
})

test_that("estimate_ate_gcomputation rejects bad outcome_model", {
  df <- mk_data(40L)
  expect_error(estimate_ate_gcomputation(df, "d", "y", c("x1"),
                                         outcome_model = "poisson"),
               "outcome_model")
})

test_that("estimate_ate_gcomputation errors on missing columns", {
  df <- mk_data(40L)
  expect_error(estimate_ate_gcomputation(df, "d", "y", c("missing")),
               "missing")
})

test_that("estimate_ate_gcomputation errors on too few obs", {
  df <- mk_data(5L)
  expect_error(estimate_ate_gcomputation(df, "d", "y", c("x1")),
               "10")
})

test_that("sensitivity_rosenbaum returns data frame of gammas", {
  df <- mk_data(40L)
  res <- sensitivity_rosenbaum(df, "d", "y", c("x1"),
                               gamma_range = c(1, 2), n_gamma = 5L)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5L)
  expect_named(res, c("Gamma", "p_lower", "p_upper"))
})

test_that("sensitivity_rosenbaum errors on bad inputs", {
  df <- mk_data(40L)
  expect_error(sensitivity_rosenbaum(df, "d", "missing", c("x1")),
               "missing")
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(0.5, 2)),
               "Minimum")
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(2, 1)),
               "gamma_range")
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(1, 2),
                                     n_gamma = 1L),
               "n_gamma")
})

test_that("sensitivity_rosenbaum errors on too few treated/control", {
  df <- mk_data(20L)
  df$d <- 0L  # no treated
  expect_error(sensitivity_rosenbaum(df, "d", "y", c("x1"),
                                     gamma_range = c(1, 2),
                                     n_gamma = 3L),
               "treated")
})

test_that("e_value returns scalar >= 1", {
  expect_equal(e_value(0, 1), 1)
  v <- e_value(0.5, 0.1)
  expect_true(v >= 1)
  v2 <- e_value(2, 1, null = 0)
  expect_true(v2 >= 1)
})

test_that("e_value errors on non-positive se", {
  expect_error(e_value(0.5, 0), "se must be > 0")
  expect_error(e_value(0.5, -1), "se must be > 0")
})
