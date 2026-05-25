# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)

set.seed(1)

make_xai_inputs <- function(n = 100, d = 4, seed = 1) {
  set.seed(seed)
  X <- matrix(rnorm(n * d), n, d)
  colnames(X) <- paste0("f", seq_len(d))
  beta <- c(1.5, -0.7, 0.0, 0.3)
  predict_fn <- function(M) as.numeric(M %*% beta)
  list(X = X, predict_fn = predict_fn,
       feature_names = colnames(X), beta = beta)
}

# ---------------------------------------------------------------------------
# permutation_importance
# ---------------------------------------------------------------------------

test_that("morie_fairness_xai_permutation_importance basic", {
  set.seed(1); s <- make_xai_inputs()
  r <- morie_fairness_xai_permutation_importance(
    s$predict_fn, s$X, feature_names = s$feature_names,
    n_repeats = 3L, seed = 1L
  )
  expect_s3_class(r, "morie_fairness_result")
  expect_equal(length(r$importances), 4L)
  expect_true(is.finite(r$value))
  # f1 has largest |beta|, should rank top
  expect_equal(r$ranking[1L], "f1")
})

test_that("morie_fairness_xai_permutation_importance flags protected", {
  set.seed(2); s <- make_xai_inputs()
  r <- morie_fairness_xai_permutation_importance(
    s$predict_fn, s$X, feature_names = s$feature_names,
    n_repeats = 3L, protected = c("f1"), seed = 1L
  )
  expect_true(length(r$warnings) >= 1L)
})

test_that("morie_fairness_xai_permutation_importance errors on unknown protected", {
  set.seed(3); s <- make_xai_inputs()
  expect_error(
    morie_fairness_xai_permutation_importance(
      s$predict_fn, s$X, feature_names = s$feature_names,
      n_repeats = 2L, protected = "nope"),
    "not in features"
  )
})

test_that("morie_fairness_xai_permutation_importance errors on empty X", {
  expect_error(
    morie_fairness_xai_permutation_importance(
      function(M) numeric(nrow(M)), matrix(numeric(0), 0, 0)),
    "empty"
  )
})

# ---------------------------------------------------------------------------
# partial_dependence
# ---------------------------------------------------------------------------

test_that("morie_fairness_xai_partial_dependence returns PD curve", {
  set.seed(4); s <- make_xai_inputs()
  r <- morie_fairness_xai_partial_dependence(
    s$predict_fn, s$X, feature = "f1",
    feature_names = s$feature_names, grid_size = 10L
  )
  expect_s3_class(r, "morie_fairness_result")
  expect_length(r$pd, 10L)
  expect_true(is.finite(r$value))
  expect_gt(r$value, 0)
})

test_that("morie_fairness_xai_partial_dependence accepts integer index", {
  set.seed(5); s <- make_xai_inputs()
  r <- morie_fairness_xai_partial_dependence(
    s$predict_fn, s$X, feature = 1L, grid_size = 5L
  )
  expect_equal(r$feature, "x0")
})

test_that("morie_fairness_xai_partial_dependence errors on bad feature name", {
  set.seed(6); s <- make_xai_inputs()
  expect_error(
    morie_fairness_xai_partial_dependence(
      s$predict_fn, s$X, feature = "nope",
      feature_names = s$feature_names),
    "not in feature_names"
  )
})

# ---------------------------------------------------------------------------
# ale
# ---------------------------------------------------------------------------

test_that("morie_fairness_xai_ale returns ALE", {
  set.seed(7); s <- make_xai_inputs()
  r <- morie_fairness_xai_ale(
    s$predict_fn, s$X, feature = "f2",
    feature_names = s$feature_names, n_bins = 5L
  )
  expect_s3_class(r, "morie_fairness_result")
  expect_true(is.finite(r$value))
  expect_true(length(r$ale) >= 2L)
})

test_that("morie_fairness_xai_ale errors on constant feature", {
  set.seed(8); s <- make_xai_inputs()
  s$X[, 1L] <- 0
  expect_error(
    morie_fairness_xai_ale(s$predict_fn, s$X, feature = 1L, n_bins = 4L),
    "no spread"
  )
})

# ---------------------------------------------------------------------------
# ceteris_paribus
# ---------------------------------------------------------------------------

test_that("morie_fairness_xai_ceteris_paribus profiles one feature", {
  set.seed(9); s <- make_xai_inputs()
  inst <- s$X[1L, ]
  r <- morie_fairness_xai_ceteris_paribus(
    s$predict_fn, inst, feature = "f1",
    X_ref = s$X, feature_names = s$feature_names,
    grid_size = 8L
  )
  expect_s3_class(r, "morie_fairness_result")
  expect_length(r$profile, 8L)
  expect_true(is.finite(r$value))
  expect_true(is.finite(r$base))
})

test_that("morie_fairness_xai_ceteris_paribus errors on wrong x length", {
  set.seed(10); s <- make_xai_inputs()
  expect_error(
    morie_fairness_xai_ceteris_paribus(
      s$predict_fn, c(1, 2), feature = 1L, X_ref = s$X,
      feature_names = s$feature_names),
    "one value per feature"
  )
})

# ---------------------------------------------------------------------------
# shap_values
# ---------------------------------------------------------------------------

test_that("morie_fairness_xai_shap_values returns attributions", {
  set.seed(11); s <- make_xai_inputs(n = 30, d = 4)
  inst <- s$X[1L, ]
  r <- morie_fairness_xai_shap_values(
    s$predict_fn, inst, background = s$X,
    feature_names = colnames(s$X), n_samples = 10L, seed = 1L
  )
  expect_s3_class(r, "morie_fairness_result")
  expect_length(r$shap_values, 4L)
  expect_true(is.finite(r$prediction))
  expect_true(is.finite(r$background_mean))
})

test_that("morie_fairness_xai_shap_values errors on x length", {
  set.seed(12); s <- make_xai_inputs(n = 20, d = 4)
  expect_error(
    morie_fairness_xai_shap_values(
      s$predict_fn, c(1, 2), background = s$X, n_samples = 2L),
    "one value per background feature"
  )
})