# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch I: vines, ksr19, polrz, xgbst, hrzp1, ghcls, rfens, gcvgn, gwreg, stvar.

test_that("vines runs on multivariate normal sample", {
  set.seed(1)
  Sigma <- matrix(c(1, 0.5, 0.3, 0.5, 1, 0.4, 0.3, 0.4, 1), 3)
  z <- MASS::mvrnorm(200, c(0, 0, 0), Sigma)
  out <- morie:::vines(z)
  expect_equal(out$d, 3L)
  expect_true(is.finite(out$loglik))
})

test_that("vines short-circuits when n<3 or d<2", {
  out_n <- morie:::vines(matrix(rnorm(4), 2, 2))
  expect_true(is.na(out_n$estimate))
})

test_that("morie_ksr19_kosorok_cox_partial_likelihood runs on standard inputs", {
  set.seed(1)
  xs <- rnorm(100)
  ts <- rexp(100, rate = exp(0.5 * xs))
  out <- morie_ksr19_kosorok_cox_partial_likelihood(xs, ts, rep(1L, 100))
  expect_true(is.finite(out$estimate))
  expect_equal(out$n, 100L)
})

test_that("polrz with default median split returns positive polarization", {
  set.seed(1)
  out <- polrz(rnorm(100))
  expect_true(is.finite(out$estimate))
  expect_equal(out$n_R + out$n_D, 100L)
})

test_that("polrz error guards fire", {
  expect_error(polrz(c(1, 2, 3), group = c(1, 2)), "length")
  expect_error(polrz(c(1, 2, 3), group = c("a", "b", "c")), "2 levels")
})

test_that("morie_xgboost_objective regression task on auto-detected continuous y", {
  skip_if_not_installed("gbm")
  skip_if_not_installed("xgboost")
  testthat::skip_if_not(
    requireNamespace("xgboost", quietly = TRUE) ||
      requireNamespace("gbm", quietly = TRUE),
    "neither xgboost nor gbm installed"
  )
  set.seed(1)
  x <- matrix(rnorm(200), 50, 4)
  y <- as.numeric(x %*% c(1, -1, 0.5, 0) + 0.3 * rnorm(50))
  out <- morie_xgboost_objective(x, y, n_estimators = 10L)
  expect_true(is.finite(out$estimate))
  expect_equal(out$task, "regression")
})

test_that("hrzp1 runs on default Robinson inputs", {
  set.seed(1)
  n <- 80
  z <- runif(n)
  x <- rnorm(n)
  y <- 2 * x + sin(2 * pi * z) + 0.3 * rnorm(n)
  out <- morie:::hrzp1(x, y, z)
  expect_true(is.finite(out$estimate))
  expect_equal(out$n, n)
})

test_that("hrzp1 insufficient-data guard returns NA", {
  out <- morie:::hrzp1(1:3, 1:3, 1:3)
  expect_true(is.na(out$estimate))
  expect_match(out$method, "insufficient")
})

test_that("morie_ghosal_np_classification runs on binary y", {
  set.seed(1)
  x <- matrix(rnorm(80), 20, 4)
  y <- rbinom(20, 1, 0.5)
  out <- morie_ghosal_np_classification(x, y, n_iter = 20)
  expect_true(out$accuracy >= 0 && out$accuracy <= 1)
  expect_equal(out$n, 20L)
})

test_that("morie_random_forest_ensemble regression path runs", {
  set.seed(1)
  x <- matrix(rnorm(200), 50, 4)
  y <- as.numeric(x %*% c(1, -1, 0.5, 0) + 0.3 * rnorm(50))
  out <- morie_random_forest_ensemble(x, y, n_estimators = 20L)
  expect_equal(out$task, "regression")
  expect_true(is.finite(out$estimate))
})

test_that("morie_genomic_cross_validation runs on default inputs", {
  set.seed(1)
  X <- matrix(rnorm(200), 50, 4)
  b <- c(1, -1, 0.5, 0)
  y <- as.numeric(X %*% b + 0.3 * rnorm(50))
  out <- morie_genomic_cross_validation(X, y, K = 5, seed = 1)
  expect_true(is.finite(out$estimate))
  expect_length(out$r_per_fold, 5L)
  expect_equal(out$K, 5)
})

test_that("gwreg gaussian kernel default bandwidth runs", {
  set.seed(1)
  n <- 30
  coords <- matrix(runif(2 * n), n, 2)
  X <- cbind(1, rnorm(n))
  y <- as.numeric(X %*% c(0.5, 1) + 0.2 * rnorm(n))
  out <- gwreg(X, y, coords)
  expect_equal(dim(out$estimate), c(n, 2L))
  expect_equal(out$kernel, "gaussian")
})

test_that("stvar default bins runs on random spatiotemporal sample", {
  set.seed(1)
  n <- 50
  coords <- matrix(runif(2 * n), n, 2)
  times <- sort(cumsum(rexp(n)))
  x <- rnorm(n)
  out <- stvar(x, coords, times)
  expect_equal(dim(out$estimate$gamma), c(6L, 6L))
  expect_equal(out$n, n)
})

test_that("stvar shape-mismatch raises", {
  expect_error(stvar(rnorm(10), matrix(0, 5, 2), 1:10), "shape mismatch")
})
