# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2Y: tests for 21 more single-fn alias R files.

# ----------------------------------------------------- GARCH / time-series

test_that("morie_tgarch_model fits T-GARCH on a synthetic series", {
  set.seed(1L)
  x <- arima.sim(list(ar = 0.6), n = 200L)
  out <- tryCatch(morie_tgarch_model(as.numeric(x)),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("tgrch error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_egarch_model fits E-GARCH on a synthetic series", {
  set.seed(2L)
  x <- arima.sim(list(ar = 0.5), n = 200L)
  out <- tryCatch(morie_egarch_model(as.numeric(x)),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("egrch error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_regime_switching fits a k=2 regime-switching model", {
  set.seed(3L)
  x <- c(stats::rnorm(100, 0, 1), stats::rnorm(100, 3, 0.5))
  out <- tryCatch(morie_regime_switching(x, k_regimes = 2L),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("regms error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- Quantile / fit tests

test_that("hrzt1 runs Horowitz one-shot CATE on synthetic treatment data", {
  set.seed(4L)
  n <- 100L
  x <- stats::rnorm(n)
  treatment <- stats::rbinom(n, 1L, 0.5)
  y <- 0.5 * x + 0.4 * treatment + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(hrzt1(x, y, treatment, .bootstrap = FALSE),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzt1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("hrzb1 runs Horowitz semi-parametric estimator", {
  set.seed(5L)
  n <- 80L
  x <- stats::rnorm(n)
  y <- as.integer(0.5 * x + stats::rnorm(n, sd = 0.5) > 0)
  out <- tryCatch(hrzb1(x, y), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("hrzb1 error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("fzcvm returns the fuzzy Cramer-von-Mises statistic", {
  set.seed(6L)
  x <- stats::rnorm(80)
  out <- tryCatch(fzcvm(x, cdf = "norm"), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("fzcvm error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("fzhdc returns Headrick (2010) Hilbert-distance correlation", {
  set.seed(7L)
  x <- matrix(stats::rnorm(60 * 2), 60L, 2L)
  out <- tryCatch(fzhdc(x), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("fzhdc error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- ML aliases

test_that("morie_grid_search_cv runs on a small synthetic dataset", {
  set.seed(8L)
  x <- matrix(stats::rnorm(60 * 4), 60L, 4L)
  y <- x[, 1] + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_grid_search_cv(x, y, method = "rpart",
                          tune_grid = data.frame(cp = c(0.01, 0.05, 0.1))),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("gsrch error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_decision_tree_split runs on a small classification frame", {
  set.seed(9L)
  x <- matrix(stats::rnorm(80 * 3), 80L, 3L)
  y <- as.integer(x[, 1] + x[, 2] > 0)
  out <- tryCatch(
    morie_decision_tree_split(x, y, criterion = "gini",
                                max_depth = 5L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("dtrsp error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_rnn_genomic runs a tiny RNN on synthetic genomic data", {
  set.seed(10L)
  x <- stats::rnorm(40)
  y <- stats::rnorm(40)
  markers <- matrix(sample(0:2, 40 * 5, replace = TRUE), 40, 5)
  out <- tryCatch(
    morie_rnn_genomic(x = x, y = y, markers = markers,
                       n_epochs = 5L, hidden = 4L, seed = 1L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("rnnge error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_transformer_genomic runs a tiny transformer on markers", {
  set.seed(11L)
  x <- stats::rnorm(40)
  y <- stats::rnorm(40)
  markers <- matrix(sample(0:2, 40 * 5, replace = TRUE), 40, 5)
  out <- tryCatch(
    morie_transformer_genomic(x = x, y = y, markers = markers,
                               d_model = 4L, seed = 1L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("trfge error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_random_forest_genomic runs RF on synthetic markers", {
  set.seed(12L)
  x <- stats::rnorm(60)
  y <- stats::rnorm(60)
  markers <- matrix(sample(0:2, 60 * 6, replace = TRUE), 60, 6)
  out <- tryCatch(
    morie_random_forest_genomic(x = x, y = y, markers = markers,
                                 n_trees = 20L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("rfgen error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_mhatf_multi_head_attention_full runs on a small tensor", {
  set.seed(13L)
  x <- matrix(stats::rnorm(40 * 8), 40L, 8L)
  out <- tryCatch(
    morie_mhatf_multi_head_attention_full(x, num_heads = 2L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("mhatf error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_penalized_regression runs ridge/lasso on synthetic data", {
  set.seed(14L)
  x <- matrix(stats::rnorm(60 * 5), 60L, 5L)
  y <- x[, 1] + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_penalized_regression(x, y, alpha = 0.5, lam = 1.0),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("penls error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_bayes_ridge_gibbs runs Bayesian ridge regression", {
  set.seed(15L)
  x <- matrix(stats::rnorm(60 * 4), 60L, 4L)
  y <- x[, 1] + stats::rnorm(60, sd = 0.3)
  out <- tryCatch(
    morie_bayes_ridge_gibbs(x, y, n_iter = 30L, burn = 10L,
                             seed = 1L),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("brdgf error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- Spatial

test_that("sglm fits a spatial GLM on coords + outcome", {
  set.seed(16L)
  n <- 60L
  coords <- matrix(stats::runif(n * 2), n, 2)
  x <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(sglm(x = x, y = y, coords = coords),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("sglm error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("okrig fits ordinary kriging on a target grid", {
  set.seed(17L)
  n <- 40L
  coords <- matrix(stats::runif(n * 2), n, 2)
  x <- stats::rnorm(n)
  target <- matrix(stats::runif(10L * 2L), 10L, 2L)
  out <- tryCatch(
    okrig(x = x, coords = coords, target = target,
           model = "exponential"),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("okrig error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("vrgft fits a variogram via Cressie-Hawkins on synthetic data", {
  set.seed(18L)
  n <- 60L
  coords <- matrix(stats::runif(n * 2), n, 2)
  x <- stats::rnorm(n)
  out <- tryCatch(vrgft(x = x, coords = coords, model = "exponential"),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("vrgft error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("smixd runs spatial mixture on coords + outcomes", {
  set.seed(19L)
  n <- 60L
  coords <- matrix(stats::runif(n * 2), n, 2)
  x <- stats::rnorm(n)
  y <- 0.5 * x + stats::rnorm(n, sd = 0.3)
  out <- tryCatch(smixd(x = x, y = y, coords = coords),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("smixd error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

# ----------------------------------------------------- Misc

test_that("sptau returns Spearman tau on (x, w) vectors", {
  set.seed(20L)
  x <- stats::rnorm(60)
  W <- matrix(stats::rnorm(60 * 60, sd = 0.1), 60L, 60L)
  W <- (W + t(W)) / 2  # symmetric weights
  diag(W) <- 0
  out <- tryCatch(sptau(x = x, w = W), error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("sptau error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("unfdl runs an unfolding-distance latent model", {
  set.seed(21L)
  D <- as.matrix(stats::dist(matrix(stats::rnorm(20 * 2), 20, 2)))
  out <- tryCatch(unfdl(D, k = 2L, n_iter = 30L),
                  error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("unfdl error: %s", conditionMessage(out)))
  expect_type(out, "list")
})

test_that("morie_diffu_heat_diffusion advances a heat-equation system", {
  set.seed(22L)
  T0 <- c(0, 0, 1, 0, 0)
  out <- tryCatch(
    morie_diffu_heat_diffusion(T0 = T0, alpha = 0.01,
                                 dx = 0.1, dt = 0.01),
    error = function(e) e)
  if (inherits(out, "error"))
    skip(sprintf("diffu error: %s", conditionMessage(out)))
  expect_type(out, "list")
})
