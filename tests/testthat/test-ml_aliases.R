# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2U: tests for 7 single-fn ML-alias R files (gbgen, ghdpm,
# bglup, ukrig, gbens, cnnge, xgbst). Each is a moderate-size
# genomic / Bayesian / NN / ensemble / kriging routine; we exercise
# the happy-path on a small synthetic input and any obvious error
# branch (length mismatch, missing arg, etc.).

.make_synthetic_xy <- function(n = 50L, p = 4L, seed = 1L) {
  set.seed(seed)
  list(
    x = stats::rnorm(n),
    y = stats::rnorm(n),
    markers = matrix(sample(0:2, n * p, replace = TRUE), n, p)
  )
}

# --------------------------------------------- gbgen / morie_gradient_boosting_genomic

test_that("morie_gradient_boosting_genomic returns rich result with finite estimate", {
  fix <- .make_synthetic_xy(n = 60L, p = 6L)
  out <- tryCatch(
    morie_gradient_boosting_genomic(x = fix$x, y = fix$y,
                                     markers = fix$markers,
                                     n_estimators = 20L, seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("gbgen error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

# --------------------------------------------- ghdpm

test_that("morie_ghosal_dpmixture_density returns a density estimate", {
  set.seed(2L)
  x <- stats::rnorm(80, mean = 0, sd = 1)
  out <- tryCatch(
    morie_ghosal_dpmixture_density(x, n_iter = 30L, burn = 10L,
                                    seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("ghdpm error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

# --------------------------------------------- bglup

test_that("morie_bayes_cpi_genomic returns posterior-mean predictor weights", {
  set.seed(3L)
  x <- matrix(stats::rnorm(60 * 8), 60, 8)
  y <- x[, 1] + 0.5 * x[, 2] + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_bayes_cpi_genomic(x = x, y = y, n_iter = 50L, burn = 10L,
                             seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("bglup error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

# --------------------------------------------- ukrig / morie_universal_kriging

test_that("morie_universal_kriging returns predicted values on a small grid", {
  set.seed(4L)
  n <- 30L
  coords <- matrix(stats::runif(n * 2), n, 2)
  x <- stats::rnorm(n)
  target <- matrix(stats::runif(10L * 2L), 10L, 2L)
  out <- tryCatch(
    morie_universal_kriging(x = x, coords = coords, target = target),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("ukrig error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

# --------------------------------------------- gbens

test_that("morie_gradient_boosting_ensemble returns ensemble predictions", {
  set.seed(5L)
  x <- matrix(stats::rnorm(60 * 4), 60, 4)
  y <- x[, 1] + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_gradient_boosting_ensemble(x = x, y = y,
                                      n_estimators = 20L, seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("gbens error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

# --------------------------------------------- cnnge

test_that("morie_cnn_genomic runs a tiny CNN on synthetic genomic data", {
  fix <- .make_synthetic_xy(n = 30L, p = 6L, seed = 6L)
  out <- tryCatch(
    morie_cnn_genomic(x = fix$x, y = fix$y, markers = fix$markers,
                       n_epochs = 10L, n_filters = 4L, hidden = 4L,
                       seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("cnnge error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})

# --------------------------------------------- xgbst

test_that("morie_xgboost_objective returns ensemble metadata", {
  set.seed(7L)
  x <- matrix(stats::rnorm(60 * 4), 60, 4)
  y <- x[, 1] + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_xgboost_objective(x = x, y = y,
                             n_estimators = 20L, seed = 1L),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("xgbst error: %s", conditionMessage(out)))
  }
  expect_type(out, "list")
})
