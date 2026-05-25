# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2OO: tests for internal helpers in fairness_xai.R,
# fairness_simulation.R, fairness_gan.R, fairness_predpol.R, and
# mrm_uof.R.

# ========================================================== fairness_xai.R

test_that(".xai_result builds a morie_fairness_result list", {
  out <- morie:::.xai_result(title = "Test", call = "demo",
                               summary_lines = list(N = 5))
  expect_type(out, "list")
  expect_s3_class(out, "morie_fairness_result")
  expect_s3_class(out, "morie_rich_result")
})

test_that(".xai_names auto-generates x0..x{d-1} when feature_names is NULL", {
  expect_equal(morie:::.xai_names(NULL, 4L),
               c("x0", "x1", "x2", "x3"))
})

test_that(".xai_names returns supplied names when length matches", {
  expect_equal(morie:::.xai_names(c("age", "race", "score"), 3L),
               c("age", "race", "score"))
})

test_that(".xai_names errors on length mismatch", {
  expect_error(morie:::.xai_names(c("a", "b"), 3L),
               regexp = "feature_names has")
})

test_that(".xai_resolve maps a feature name to its 1-based index", {
  expect_equal(morie:::.xai_resolve("b", c("a", "b", "c")), 2L)
})

test_that(".xai_resolve passes an integer index through", {
  expect_equal(morie:::.xai_resolve(2L, c("a", "b", "c")), 2L)
})

test_that(".xai_resolve errors on unknown name", {
  expect_error(morie:::.xai_resolve("z", c("a", "b", "c")),
               regexp = "not in feature_names")
})

test_that(".xai_predict wraps a predict function and asserts length", {
  X <- matrix(stats::rnorm(20), 5L, 4L)
  fn <- function(x) rep(0.5, nrow(x))
  expect_equal(morie:::.xai_predict(fn, X), rep(0.5, 5L))
})

test_that(".xai_predict errors on length mismatch", {
  X <- matrix(stats::rnorm(20), 5L, 4L)
  fn <- function(x) c(0.5, 0.5)
  expect_error(morie:::.xai_predict(fn, X),
               regexp = "one prediction per row")
})

test_that(".xai_have_iml returns logical", {
  expect_type(morie:::.xai_have_iml(), "logical")
})

# ====================================================== fairness_simulation.R

test_that(".sim_result builds a morie_fairness_result list", {
  out <- morie:::.sim_result(title = "Sim", call = "demo")
  expect_type(out, "list")
  expect_s3_class(out, "morie_fairness_result")
})

# ========================================================== fairness_gan.R

test_that(".fairness_result builds a morie_fairness_result list", {
  out <- morie:::.fairness_result(title = "GAN", call = "demo")
  expect_type(out, "list")
  expect_s3_class(out, "morie_fairness_result")
})

test_that(".fairness_backend returns a backend list or NULL", {
  out <- morie:::.fairness_backend()
  expect_true(is.null(out) || is.list(out))
})

test_that(".fairness_no_backend_result builds a 'no backend' result", {
  out <- morie:::.fairness_no_backend_result(
    title = "GAN", call = "demo",
    note = "No backend available")
  expect_type(out, "list")
  expect_s3_class(out, "morie_fairness_result")
})

test_that(".fairness_he_init builds He-init MLP params with correct shapes", {
  set.seed(1L)
  params <- morie:::.fairness_he_init(c(4L, 8L, 2L))
  expect_length(params, 2L)
  expect_equal(dim(params[[1]]$W), c(4L, 8L))
  expect_equal(dim(params[[2]]$W), c(8L, 2L))
  expect_length(params[[1]]$b, 8L)
  expect_length(params[[2]]$b, 2L)
})

test_that(".fairness_mlp_forward runs a He-init MLP forward pass", {
  set.seed(2L)
  params <- morie:::.fairness_he_init(c(3L, 5L, 1L))
  x <- matrix(stats::rnorm(15), 5L, 3L)
  out <- morie:::.fairness_mlp_forward(params, x)
  expect_true(is.matrix(out))
  expect_equal(dim(out), c(5L, 1L))
})

# ========================================================== fairness_predpol.R

test_that(".predpol_result builds a morie_fairness_result list", {
  out <- morie:::.predpol_result(title = "PredPol", call = "demo")
  expect_type(out, "list")
  expect_s3_class(out, "morie_fairness_result")
})

test_that(".predpol_ordered_unique preserves first-occurrence order", {
  expect_equal(
    morie:::.predpol_ordered_unique(c("b", "a", "b", "c", "a")),
    c("b", "a", "c"))
})

test_that(".predpol_mode returns the most-frequent value as a string", {
  expect_equal(morie:::.predpol_mode(c("a", "b", "a", "c", "a")), "a")
})

# ========================================================== mrm_uof.R

test_that(".uof_gini equals 0 on a uniform vector", {
  expect_equal(morie:::.uof_gini(rep(5, 10L)), 0)
})

test_that(".uof_gini returns NA on empty or all-zero input", {
  expect_true(is.na(morie:::.uof_gini(numeric(0))))
  expect_true(is.na(morie:::.uof_gini(rep(0, 5L))))
})

test_that(".uof_gini approaches 1 on a strongly skewed vector", {
  x <- c(rep(0, 99L), 1000)
  out <- morie:::.uof_gini(x)
  expect_true(out > 0.9)
})

test_that(".uof_hill_alpha returns NA when not enough events above x_min", {
  expect_true(is.na(morie:::.uof_hill_alpha(numeric(0))))
  expect_true(is.na(morie:::.uof_hill_alpha(c(1.0))))
})

test_that(".uof_hill_alpha returns a positive scalar on Pareto-ish data", {
  set.seed(3L)
  x <- 1 + stats::rexp(200L, rate = 0.5)
  out <- morie:::.uof_hill_alpha(x, x_min = 1.0)
  expect_true(is.numeric(out) && is.finite(out) && out > 0)
})

test_that(".uof_topk_share returns 1 when k >= length(x)", {
  expect_equal(morie:::.uof_topk_share(c(1, 2, 3, 4), k = 4L), 1)
})

test_that(".uof_topk_share returns the top-1 share for k = 1", {
  expect_equal(morie:::.uof_topk_share(c(1, 2, 3, 4), k = 1L), 4 / 10)
})

test_that(".uof_topk_share returns NA on all-zero / empty input", {
  expect_true(is.na(morie:::.uof_topk_share(rep(0, 5L), k = 2L)))
  expect_true(is.na(morie:::.uof_topk_share(numeric(0), k = 1L)))
})

test_that(".uof_wilson_ci returns CI bounds inside [0, 1] for valid k/n", {
  out <- morie:::.uof_wilson_ci(k = 30L, n = 100L)
  expect_length(out, 2L)
  expect_true(all(out > 0 & out < 1))
  expect_true(out[1] < out[2])
})

test_that(".uof_wilson_ci returns NA bounds when n = 0", {
  out <- morie:::.uof_wilson_ci(k = 0L, n = 0L)
  expect_true(all(is.na(out)))
})

test_that(".uof_cramers_v returns NA when k <= 0 or n = 0", {
  expect_true(is.na(morie:::.uof_cramers_v(chi2 = 10, n = 0L,
                                              r = 2L, c = 2L)))
  expect_true(is.na(morie:::.uof_cramers_v(chi2 = 10, n = 100L,
                                              r = 1L, c = 5L)))
})

test_that(".uof_cramers_v computes the standard formula", {
  # V = sqrt(chi2 / (n * min(r-1, c-1)))
  out <- morie:::.uof_cramers_v(chi2 = 16, n = 100L, r = 3L, c = 3L)
  expect_equal(out, sqrt(16 / (100 * 2)), tolerance = 1e-10)
})

test_that(".uof_fmt_pct formats a finite proportion as 'NN.NN%'", {
  expect_equal(morie:::.uof_fmt_pct(0.1234), "12.34%")
})

test_that(".uof_fmt_pct returns 'n/a' on non-finite input", {
  expect_equal(morie:::.uof_fmt_pct(NA_real_), "n/a")
  expect_equal(morie:::.uof_fmt_pct(NaN), "n/a")
  expect_equal(morie:::.uof_fmt_pct(Inf), "n/a")
})

test_that(".uof_result builds a morie_uof rich-result list", {
  out <- morie:::.uof_result(title = "UoF", call = "demo",
                               summary_lines = list(N = 100))
  expect_type(out, "list")
})
