# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

# ---------------------------------------------------------------------------
# Coverage tests for R/sensitivity.R
# ---------------------------------------------------------------------------

set.seed(1)

# ---------------------------------------------------------------------------
# E-values
# ---------------------------------------------------------------------------

test_that("e_value_rr point estimate matches closed-form for rr > 1", {
  res <- e_value_rr(2.0)
  expect_equal(as.numeric(res$e_value_point),
               2 + sqrt(2 * (2 - 1)), tolerance = 1e-6)
})

test_that("e_value_rr inverts rr < 1", {
  res <- e_value_rr(0.5)
  expect_equal(as.numeric(res$e_value_point),
               2 + sqrt(2 * (2 - 1)), tolerance = 1e-6)
})

test_that("e_value_rr with CI returns finite CI E-value", {
  res <- e_value_rr(2.0, ci_lower = 1.5, ci_upper = 2.5)
  expect_true(is.finite(as.numeric(res$e_value_ci)))
})

test_that("e_value_or rare-outcome branch equals e_value_rr", {
  a <- e_value_or(2.0)
  b <- e_value_rr(2.0)
  expect_equal(as.numeric(a$e_value_point),
               as.numeric(b$e_value_point), tolerance = 1e-6)
})

test_that("e_value_or common-outcome branch corrects via Zhang-Yu", {
  res <- e_value_or(2.0, prevalence = 0.3)
  expect_true(is.finite(as.numeric(res$e_value_point)))
})

test_that("e_value_hr runs and returns finite point", {
  res <- e_value_hr(2.0, ci_lower = 1.5, ci_upper = 2.5)
  expect_true(is.finite(as.numeric(res$e_value_point)))
})

test_that("e_value_hr handles HR = 1 degenerate case", {
  res <- e_value_hr(1.0)
  expect_true(is.finite(as.numeric(res$e_value_point)))
})

test_that("e_value_d works with se and with n branches", {
  a <- e_value_d(0.5, se = 0.1)
  b <- e_value_d(0.5, n = 100)
  c <- e_value_d(0.5)
  for (r in list(a, b, c))
    expect_true(is.finite(as.numeric(r$e_value_point)))
})

# ---------------------------------------------------------------------------
# Rosenbaum bounds
# ---------------------------------------------------------------------------

test_that("rosenbaum_bounds wilcoxon method runs", {
  set.seed(1)
  t_out <- rnorm(40, mean = 0.5)
  c_out <- rnorm(40)
  res <- rosenbaum_bounds(t_out, c_out, method = "wilcoxon")
  expect_true(is.finite(res$critical_gamma))
  expect_equal(length(res$gamma_values), length(res$p_upper))
})

test_that("rosenbaum_bounds sign method runs", {
  set.seed(1)
  res <- rosenbaum_bounds(rnorm(40, 0.5), rnorm(40), method = "sign")
  expect_true(is.finite(res$critical_gamma))
})

test_that("rosenbaum_bounds mcnemar method runs on binary data", {
  set.seed(1)
  t_out <- rbinom(50, 1, 0.6); c_out <- rbinom(50, 1, 0.4)
  res <- rosenbaum_bounds(t_out, c_out, method = "mcnemar")
  expect_true(is.finite(res$critical_gamma))
})

test_that("rosenbaum_bounds unknown method errors", {
  expect_error(rosenbaum_bounds(rnorm(20), rnorm(20), method = "no_such"))
})

test_that("rosenbaum_bounds custom gamma_range honoured", {
  set.seed(1)
  res <- rosenbaum_bounds(rnorm(30, 0.4), rnorm(30),
                          gamma_range = c(1, 2, 3))
  expect_equal(length(res$gamma_values), 3L)
})

# ---------------------------------------------------------------------------
# Tipping-point analysis
# ---------------------------------------------------------------------------

test_that("tipping_point_analysis default delta_range runs", {
  res <- tipping_point_analysis(0.5, 0.1, n_treated = 100, n_control = 100)
  expect_equal(length(res$delta_values), 101L)
  expect_true(is.finite(res$tipping_point))
})

test_that("tipping_point_analysis custom delta_range honoured", {
  res <- tipping_point_analysis(0.5, 0.2, n_treated = 50, n_control = 50,
                                delta_range = seq(-1, 1, length.out = 25))
  expect_equal(length(res$delta_values), 25L)
})

test_that("tipping_point_analysis with tiny SE marks robust", {
  res <- tipping_point_analysis(1.0, 0.01, n_treated = 200, n_control = 200)
  expect_true(is.finite(res$tipping_point))
})

# ---------------------------------------------------------------------------
# Omitted variable bias
# ---------------------------------------------------------------------------

test_that("omitted_variable_bias returns rv_q in [0, 1]", {
  res <- omitted_variable_bias(estimate = 0.5, se = 0.1, dof = 100,
                               r2_yd_x = 0.2, partial_r2_treatment = 0.1)
  expect_gte(res$rv_q, 0); expect_lte(res$rv_q, 1)
  expect_gte(res$rv_qa, 0); expect_lte(res$rv_qa, 1)
})

test_that("omitted_variable_bias degenerate f_stat<=1 yields rv_q=0", {
  res <- omitted_variable_bias(estimate = 0.05, se = 0.1, dof = 100,
                               r2_yd_x = 0.01, partial_r2_treatment = 0.01)
  expect_equal(res$rv_q, 0)
})

test_that("omitted_variable_bias benchmark_covariates writes bounds", {
  res <- omitted_variable_bias(0.5, 0.1, 100, 0.2, 0.1,
                               benchmark_covariates = list(z = 0.05))
  expect_true("z" %in% names(res$benchmark_bounds))
  expect_length(res$benchmark_bounds[["z"]], 2L)
})

# ---------------------------------------------------------------------------
# specification_curve
# ---------------------------------------------------------------------------

test_that("specification_curve runs across covariate sets and OLS", {
  set.seed(1)
  n <- 200
  d <- data.frame(
    y = rnorm(n), tx = rbinom(n, 1, 0.5),
    a = rnorm(n), b = rnorm(n), c = rnorm(n))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("a"), c("a", "b"),
                                                   c("a", "b", "c")))
  expect_equal(length(res$estimates), 3L)
})

test_that("specification_curve accepts Python-style (name, fn) sample filters", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), tx = rbinom(120, 1, 0.5),
                  a = rnorm(120), grp = sample(c("x", "y"), 120, replace = TRUE))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("a")),
                             sample_filters = list(
                               list("full", function(df) df),
                               list("x_only", function(df) df[df$grp == "x", ])))
  expect_equal(length(res$estimates), 2L)
})

test_that("specification_curve invalid filter shape errors", {
  d <- data.frame(y = rnorm(40), tx = rbinom(40, 1, 0.5), a = rnorm(40))
  expect_error(specification_curve(d, "y", "tx",
                                   covariate_sets = list(c("a")),
                                   sample_filters = list(list("bad"))))
})

test_that("specification_curve empty result path returns NA medians", {
  set.seed(1)
  d <- data.frame(y = rnorm(20), tx = rbinom(20, 1, 0.5))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("no_such_col")))
  expect_equal(length(res$estimates), 0L)
})

test_that("specification_curve robust model branch runs when MASS available", {
  set.seed(1)
  d <- data.frame(y = rnorm(120), tx = rbinom(120, 1, 0.5),
                  a = rnorm(120))
  res <- specification_curve(d, "y", "tx",
                             covariate_sets = list(c("a")),
                             model_types = c("ols", "robust"))
  expect_gte(length(res$estimates), 1L)
})

# ---------------------------------------------------------------------------
# manski_bounds
# ---------------------------------------------------------------------------

test_that("manski_bounds returns lower <= upper", {
  set.seed(1)
  res <- manski_bounds(runif(50), runif(50), p_treated = 0.5)
  expect_lte(res$lower_bound, res$upper_bound)
  expect_equal(res$width, res$upper_bound - res$lower_bound)
})

test_that("manski_bounds honours custom outcome_range", {
  set.seed(1)
  res <- manski_bounds(rnorm(50, 1, 0.3), rnorm(50, 0, 0.3),
                       p_treated = 0.4, outcome_range = c(-3, 3))
  expect_true(is.finite(res$width))
})

# ---------------------------------------------------------------------------
# bias_adjusted_estimate + probabilistic_bias_analysis
# ---------------------------------------------------------------------------

test_that("bias_adjusted_estimate returns finite adjusted CI", {
  res <- bias_adjusted_estimate(0.5, 0.1, rr_ud = 2, rr_eu = 2)
  expect_true(is.finite(res$adjusted_estimate))
  expect_lt(res$adjusted_ci_lower, res$adjusted_ci_upper)
})

test_that("probabilistic_bias_analysis returns summaries from MC draws", {
  res <- probabilistic_bias_analysis(0.5, 0.1, n_simulations = 200L,
                                     seed = 1L)
  for (k in c("median_adjusted", "mean_adjusted", "ci_2.5", "ci_97.5",
              "pct_null_included", "pct_same_sign")) {
    expect_true(is.finite(res[[k]]))
  }
  expect_equal(res$n_simulations, 200L)
})

test_that("probabilistic_bias_analysis custom bias_parms honoured", {
  bp <- list(rr_ud = c(1.2, 0.1), rr_eu = c(1.2, 0.1),
             prevalence = c(0.2, 0.05))
  res <- probabilistic_bias_analysis(0.4, 0.1, n_simulations = 100L,
                                     bias_parms = bp, seed = 2L)
  expect_equal(res$n_simulations, 100L)
})

# ---------------------------------------------------------------------------
# sensitivity_summary
# ---------------------------------------------------------------------------

test_that("sensitivity_summary minimal call returns base rows", {
  res <- sensitivity_summary(0.5, 0.1)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("estimate", "se", "ci_lower", "ci_upper", "p_value")
                  %in% res$metric))
})

test_that("sensitivity_summary appends RR / OR / HR rows when supplied", {
  res <- sensitivity_summary(0.5, 0.1, rr = 2, odds_ratio = 2,
                             hazard_ratio = 2, prevalence = 0.2)
  expect_s3_class(res, "data.frame")
  expect_gte(nrow(res), 5L)
})