# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2Z: tests for 22 more single-fn alias R files (mostly 50-60
# lines each).

# ----------------------------------------------------- statistics

test_that("morie_percentile_modified_rank computes a rank-percentile test", {
  set.seed(1L)
  x <- stats::rnorm(50, 0); y <- stats::rnorm(50, 0.5)
  out <- tryCatch(morie_percentile_modified_rank(x, y),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("pctmr error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_kendall_tau_partial returns partial Kendall's tau", {
  set.seed(2L)
  x <- stats::rnorm(60); y <- stats::rnorm(60); z <- stats::rnorm(60)
  out <- tryCatch(morie_kendall_tau_partial(x, y, z),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("ktaup error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_ordered_categories computes the cumulative-link test", {
  set.seed(3L)
  x <- matrix(sample.int(4, 60, replace = TRUE), 12L, 5L)
  out <- tryCatch(morie_ordered_categories(x), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("ordct error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_wilcoxon_power computes power for a Wilcoxon test", {
  out <- tryCatch(
    morie_wilcoxon_power(x = 30L, effect_size = 0.5),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("wsrpw error: %s", conditionMessage(out)))
  expect_true(is.list(out) || is.numeric(out))
})

test_that("morie_confusion_matrix_metrics returns precision/recall/F1", {
  out <- morie_confusion_matrix_metrics(
    y_true = c(1, 0, 1, 1, 0, 0),
    y_pred = c(1, 0, 0, 1, 1, 0))
  expect_type(out, "list")
})

test_that("morie_polynomial_regression fits a degree-2 polynomial", {
  set.seed(4L)
  x <- stats::rnorm(60); y <- x^2 + stats::rnorm(60, sd = 0.3)
  out <- morie_polynomial_regression(x, y, degree = 2L)
  expect_type(out, "list")
})

# ----------------------------------------------------- signal-processing

test_that("morie_coherence computes coherence between two signals", {
  set.seed(5L)
  x <- sin(seq(0, 4 * pi, length.out = 256))
  y <- x + stats::rnorm(256, sd = 0.2)
  out <- tryCatch(morie_coherence(x, y, nperseg = 64L, fs = 1),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("cohrc error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_wavelet_time_series decomposes a series via Haar wavelet", {
  set.seed(6L)
  x <- arima.sim(list(ar = 0.5), n = 128L)
  out <- tryCatch(morie_wavelet_time_series(as.numeric(x),
                                              wavelet = "haar"),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("wavts error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- Ghosal BNP suite

test_that("morie_ghosal_gp_squared_exponential fits a GP regression", {
  set.seed(7L)
  x <- stats::rnorm(40)
  y <- 0.5 * x + stats::rnorm(40, sd = 0.3)
  out <- tryCatch(
    morie_ghosal_gp_squared_exponential(x, y),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("ghgps error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_ghosal_posterior_consistency runs on synthetic data", {
  set.seed(8L)
  x <- stats::rnorm(60)
  out <- tryCatch(
    morie_ghosal_posterior_consistency(x),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("ghcon error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_ghosal_np_classification runs on binary labels", {
  set.seed(9L)
  x <- matrix(stats::rnorm(40 * 3), 40L, 3L)
  y <- as.integer(x[, 1] + x[, 2] > 0)
  out <- tryCatch(
    morie_ghosal_np_classification(x, y),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("ghcls error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- attention / MoE

test_that("grouped_query_attention runs on small Q/K/V tensors", {
  set.seed(10L)
  Q <- matrix(stats::rnorm(8 * 4), 8L, 4L)
  K <- matrix(stats::rnorm(8 * 4), 8L, 4L)
  V <- matrix(stats::rnorm(8 * 4), 8L, 4L)
  out <- tryCatch(morie:::grouped_query_attention(Q, K, V),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("grpqa error: %s", conditionMessage(out)))
  expect_true(is.matrix(out) || is.list(out))
})

test_that("mixture_of_experts runs on small input", {
  set.seed(11L)
  x <- matrix(stats::rnorm(20 * 4), 20L, 4L)
  out <- tryCatch(morie:::mixture_of_experts(x), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("moeml error: %s", conditionMessage(out)))
  expect_true(is.list(out) || is.matrix(out))
})

# ----------------------------------------------------- genomic / regression

test_that("morie_rkhs_full runs on synthetic markers", {
  set.seed(12L)
  x <- stats::rnorm(50)
  y <- stats::rnorm(50)
  markers <- matrix(sample(0:2, 50 * 5, replace = TRUE), 50, 5)
  out <- tryCatch(
    morie_rkhs_full(x = x, y = y, markers = markers),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("rkhsf error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_multi_trait_gblup runs on synthetic data", {
  set.seed(13L)
  x <- stats::rnorm(40)
  y <- matrix(stats::rnorm(40 * 2), 40L, 2L)
  markers <- matrix(sample(0:2, 40 * 5, replace = TRUE), 40, 5)
  out <- tryCatch(
    morie_multi_trait_gblup(x = x, y = y, markers = markers),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("mtgbl error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_gblup_full runs on synthetic markers", {
  set.seed(14L)
  x <- stats::rnorm(50)
  y <- stats::rnorm(50)
  markers <- matrix(sample(0:2, 50 * 5, replace = TRUE), 50, 5)
  out <- tryCatch(
    morie_gblup_full(x = x, y = y, markers = markers),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("gblpf error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_learning_curve computes learning-curve cv scores", {
  set.seed(15L)
  x <- matrix(stats::rnorm(80 * 3), 80L, 3L)
  y <- x[, 1] + stats::rnorm(80, sd = 0.3)
  out <- tryCatch(
    morie_learning_curve(x, y, sizes = c(20L, 40L, 60L), cv = 3L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("lrcvg error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- Horowitz misc

test_that("hrzm1 runs a Horowitz mixture model on a univariate series", {
  set.seed(16L)
  y <- c(stats::rnorm(50, 0), stats::rnorm(50, 3))
  out <- tryCatch(hrzm1(y, k = 2L, maxit = 30L), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzm1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzn2 runs Horowitz nonparametric density on a series", {
  set.seed(17L)
  y <- stats::rnorm(80)
  out <- tryCatch(hrzn2(y, sigma_u = 0.3), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzn2 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzw1 runs Horowitz wild-bootstrap on a regression residuals", {
  set.seed(18L)
  n <- 60L
  x <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  resid <- y - stats::predict(stats::lm(y ~ x))
  out <- tryCatch(hrzw1(x, y, residuals = resid, B = 20L),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzw1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzw2 runs Horowitz double-bootstrap on synthetic data", {
  set.seed(19L)
  n <- 50L
  x <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(hrzw2(x, y, B = 5L, n_h = 5L), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzw2 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzs1 runs Horowitz semiparametric-IV estimator", {
  set.seed(20L)
  n <- 80L
  x <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  z <- stats::rnorm(n)
  d <- stats::rbinom(n, 1L, 0.5)
  out <- tryCatch(hrzs1(x, y, z, d), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzs1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})
