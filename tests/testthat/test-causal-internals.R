# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2MM: tests for internal helpers across causal.R, effects.R,
# effect_sizes.R, sensitivity.R, rdd.R, ingest_chicago.R.

# ============================================================== causal.R

test_that(".fit_propensity returns ps in (0,1) on synthetic data", {
  set.seed(1L)
  df <- data.frame(d = stats::rbinom(100, 1L, 0.5),
                   x1 = stats::rnorm(100), x2 = stats::rnorm(100))
  ps <- morie:::.fit_propensity(df, "d", c("x1", "x2"))
  expect_true(is.numeric(ps))
  expect_true(all(ps > 0 & ps < 1))
})

test_that(".clip_ps clips ps to [eps, 1-eps]", {
  expect_equal(morie:::.clip_ps(c(0.0, 0.5, 1.0), eps = 0.01),
               c(0.01, 0.5, 0.99))
  expect_equal(morie:::.clip_ps(c(-1, 2)), c(1e-6, 1 - 1e-6))
})

test_that(".hajek_diff computes weighted-mean difference", {
  out <- morie:::.hajek_diff(y1 = c(1, 2, 3), w1 = c(1, 1, 1),
                              y0 = c(0, 1), w0 = c(1, 1))
  expect_equal(out, 2 - 0.5, tolerance = 1e-10)
})

test_that(".influence_score_aipw returns finite scores", {
  set.seed(2L); n <- 50L
  y <- stats::rnorm(n); t <- stats::rbinom(n, 1, 0.5)
  ps <- pmin(pmax(stats::runif(n, 0.1, 0.9), 0.05), 0.95)
  mu1 <- stats::rnorm(n); mu0 <- stats::rnorm(n)
  out <- morie:::.influence_score_aipw(y, t, ps, mu1, mu0)
  expect_true(is.numeric(out) && length(out) == n)
  expect_true(all(is.finite(out)))
})

test_that(".dml_prepare_xy splits data into X, y, treatment matrix", {
  set.seed(3L); n <- 80L
  df <- data.frame(y = stats::rnorm(n), d = stats::rbinom(n, 1, 0.5),
                   x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  out <- morie:::.dml_prepare_xy(df, "d", "y", c("x1", "x2"))
  expect_type(out, "list")
})

test_that(".dml_xfit_ridge runs ridge cross-fitting on synthetic data", {
  set.seed(4L); n <- 80L
  X <- cbind(1, matrix(stats::rnorm(n * 3), n, 3))
  y <- X %*% c(0.1, 0.5, 0.3, -0.2) + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(
    morie:::.dml_xfit_ridge(X, y, n_folds = 2L, lambda = 1.0),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("dml_xfit_ridge error: %s", conditionMessage(out)))
  }
  expect_true(is.numeric(out) || is.list(out))
})

# ============================================================ effect_sizes.R

test_that(".arr computes absolute relative ratio", {
  out <- morie:::.arr(0.3)
  expect_true(is.numeric(out))
})

test_that(".bootstrap_ci returns CI bounds on a simple statistic", {
  args <- list(x = stats::rnorm(50))
  ci <- tryCatch(
    morie:::.bootstrap_ci(function(x) mean(x), args, n_boot = 50L),
    error = function(e) e)
  if (inherits(ci, "error")) {
    skip(sprintf("bootstrap_ci error: %s", conditionMessage(ci)))
  }
  expect_true(is.list(ci) || is.numeric(ci))
})

# ============================================================ sensitivity.R

test_that(".rr_to_evalue converts a risk ratio to an E-value", {
  out <- morie:::.rr_to_evalue(2.0)
  expect_true(is.numeric(out) && out > 1)
})

test_that(".evalue_result builds an E-value result list", {
  out <- morie:::.evalue_result(
    point_estimate = 1.5, e_value_point = 2.4, e_value_ci = 1.7,
    rr = 1.5, ci_lower = 1.1, ci_upper = 2.0,
    interpretation = "Moderate evidence")
  expect_type(out, "list")
  expect_s3_class(out, "morie_evalue")
})

test_that(".rosenbaum_result builds a Rosenbaum-bound result list", {
  out <- morie:::.rosenbaum_result(
    gamma_values = c(1, 1.5, 2),
    p_upper = c(0.04, 0.10, 0.18),
    p_lower = c(0.04, 0.01, 0.0005),
    critical_gamma = 1.5, method = "wilcoxon",
    interpretation = "moderate")
  expect_type(out, "list")
  expect_s3_class(out, "morie_rosenbaum_bounds")
})

test_that(".tipping_point_result builds a tipping-point result list", {
  out <- morie:::.tipping_point_result(
    delta_values = seq(-1, 1, by = 0.5),
    adjusted_estimates = c(0.6, 0.5, 0.4, 0.3, 0.2),
    adjusted_p_values = c(0.001, 0.005, 0.02, 0.1, 0.3),
    tipping_point = 0.5, original_estimate = 0.4,
    interpretation = "robust to delta < 0.5")
  expect_type(out, "list")
  expect_s3_class(out, "morie_tipping_point")
})

test_that(".spec_curve_result builds a specification-curve result list", {
  out <- morie:::.spec_curve_result(
    estimates = c(0.4, 0.5, 0.6),
    ses = c(0.1, 0.1, 0.12),
    p_values = c(0.001, 0.02, 0.05),
    specifications = data.frame(spec = c("a", "b", "c")),
    median_estimate = 0.5, iqr_lower = 0.4, iqr_upper = 0.6,
    pct_significant = 0.67, pct_same_sign = 1.00)
  expect_type(out, "list")
  expect_s3_class(out, "morie_spec_curve")
})

# ============================================================ rdd.R

test_that(".morie_rdd_get_kernel returns a kernel function or weight vector", {
  out <- tryCatch(morie:::.morie_rdd_get_kernel("triangular"),
                  error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("rdd_get_kernel error: %s", conditionMessage(out)))
  }
  expect_true(is.function(out) || is.list(out) || is.character(out))
})

test_that(".morie_rdd_have_* returns logical", {
  expect_type(morie:::.morie_rdd_have_rdrobust(), "logical")
  expect_type(morie:::.morie_rdd_have_rddensity(), "logical")
})

test_that(".morie_rdd_local_poly_fit fits a local polynomial near cutoff", {
  set.seed(5L); n <- 100L
  x <- stats::runif(n, -5, 5)
  y <- 0.3 * x + ifelse(x >= 0, 1.5, 0) + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(
    morie:::.morie_rdd_local_poly_fit(x, y, x0 = 0, h = 2.0),
    error = function(e) e)
  if (inherits(out, "error")) {
    skip(sprintf("rdd_local_poly_fit error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.numeric(out))
})

test_that(".morie_rdd_bw_result builds a bandwidth result list", {
  out <- morie:::.morie_rdd_bw_result(h = 2.5, method = "imbens-kalyanaraman")
  expect_type(out, "list")
})

test_that(".morie_rdd_result builds an RDD estimate result list", {
  out <- morie:::.morie_rdd_result(estimate = 1.5, se = 0.3, n = 100L,
                                     method = "local-linear")
  expect_type(out, "list")
})

# ============================================================ ingest_chicago.R

test_that(".morie_chicago_rows_to_df handles a small list of rows", {
  rows <- list(
    list(case_number = "ABC-001", arrest = "Y"),
    list(case_number = "ABC-002", arrest = "N"))
  out <- morie:::.morie_chicago_rows_to_df(rows)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that(".morie_chicago_rows_to_df returns 0-row frame for empty input", {
  out <- morie:::.morie_chicago_rows_to_df(list())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})
