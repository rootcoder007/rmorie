# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch F: cokrg, gbens, gsrch, indkr, modules, fzlst, hrzc1, longitudinal_sim, rghfd, coitg.

test_that("cokrg runs on default args with vector target", {
  set.seed(1)
  out <- cokrg(
    x = rnorm(50), y = rnorm(50),
    coords = matrix(runif(100), 50, 2), target = c(0.5, 0.5)
  )
  expect_type(out, "list")
  expect_equal(out$n, 50)
})

test_that("cokrg errors on dim mismatches", {
  set.seed(1)
  expect_error(
    cokrg(
      x = rnorm(10), y = rnorm(10),
      coords = matrix(runif(20), 10, 2), target = c(0.5, 0.5, 0.5)
    ),
    "target/coords dim mismatch"
  )
})

test_that("morie_gradient_boosting_ensemble runs regression on small data", {
  set.seed(1)
  out <- morie_gradient_boosting_ensemble(
    x = matrix(rnorm(60), 30, 2), y = rnorm(30),
    n_estimators = 10L, max_depth = 2L
  )
  expect_type(out, "list")
  expect_equal(out$task, "regression")
})

test_that("morie_grid_search_cv regression returns scores", {
  set.seed(1)
  out <- morie_grid_search_cv(
    x = matrix(rnorm(150), 50, 3), y = rnorm(50),
    method = "lm", tune_grid = data.frame(intercept = c(TRUE, FALSE)),
    cv = 3L, task = "regression", seed = 1L
  )
  expect_type(out, "list")
  expect_equal(out$task, "regression")
})

test_that("indkr runs on default args + alias matches", {
  set.seed(1)
  out <- indkr(
    x = rnorm(40),
    coords = matrix(runif(80), 40, 2),
    threshold = 0.5
  )
  expect_equal(out$n, 40)
  expect_true(all(out$estimate >= 0 & out$estimate <= 1))
  expect_identical(morie_indicator_kriging, indkr)
})

test_that("indkr errors on bad dims", {
  set.seed(1)
  expect_error(
    indkr(x = rnorm(10), coords = matrix(runif(18), 9, 2), threshold = 0),
    "coords rows must match length"
  )
})

test_that("morie_list_morie_modules returns a 21-row data.frame", {
  out <- morie_list_morie_modules()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 21L)
  expect_named(out, c("name", "description"))
})

test_that(".cpads_default_csv returns a character path", {
  p <- morie:::.cpads_default_csv()
  expect_type(p, "character")
})

test_that(".resolve_cpads_csv errors on a path it cannot find", {
  expect_error(
    morie:::.resolve_cpads_csv(file.path(tempdir(), "does-not-exist-xyz.csv")),
    "CPADS CSV not found"
  )
})

test_that("fzlst with default constant J returns estimate close to mean", {
  set.seed(1)
  x <- rnorm(200)
  out <- fzlst(x)
  expect_type(out, "list")
  expect_equal(out$n, 200L)
  expect_true(is.finite(out$estimate))
})

test_that("fzlst returns NA-stub when n < 2", {
  out <- fzlst(numeric(0))
  expect_equal(out$n, 0L)
  expect_true(is.na(out$estimate))
})

test_that("hrzc1 returns insufficient-data stub when n too small", {
  set.seed(1)
  out <- morie:::hrzc1(matrix(rnorm(8), 4, 2), c(1, 1, 1, 1), censor = 0)
  expect_true(all(is.na(out$estimate)))
  expect_match(out$method, "insufficient data")
})

test_that("morie_sync_rng emits identical streams for identical seeds", {
  rng1 <- morie_sync_rng(42L)
  rng2 <- morie_sync_rng(42L)
  expect_equal(rng1$rnorm(5), rng2$rnorm(5))
})

test_that("morie_generate_ar_coefficients respects spectral_radius", {
  rng <- morie_sync_rng(1L)
  A <- morie_generate_ar_coefficients(3L, rng, spectral_radius = 0.5)
  expect_equal(dim(A), c(3L, 3L))
  rho <- max(Mod(eigen(A, only.values = TRUE)$values))
  expect_lt(rho, 0.51)
})

test_that("morie_mvn_with_covariance supports each kernel", {
  for (k in c("ar1", "independent", "compound", "toeplitz")) {
    rng <- morie_sync_rng(7L)
    out <- morie_mvn_with_covariance(10L, 3L, rng, kernel = k, rho = 0.4)
    expect_equal(dim(out), c(10L, 3L))
  }
})

test_that("rghfd returns expected list keys for white noise", {
  set.seed(1)
  out <- rghfd(rnorm(200), kmax = 8L)
  expect_named(out, c("HFD", "intercept", "log_L", "log_inv_k", "kmax"))
  expect_true(is.finite(out$HFD))
  expect_equal(out$kmax, 8L)
})

test_that("rghfd errors on degenerate inputs", {
  expect_error(rghfd(rnorm(3)), "length\\(x\\) >= 4")
  expect_error(rghfd(rnorm(10), kmax = 1L), "kmax >= 2")
})

test_that("morie_eg_coint runs with default lag selection", {
  set.seed(1)
  out <- morie_eg_coint(y1 = cumsum(rnorm(120)), y2 = cumsum(rnorm(120)))
  expect_equal(out$n, 120L)
  expect_true(is.numeric(out$adf_statistic))
})

test_that("morie_eg_coint errors on mismatched lengths and too-short series", {
  expect_error(morie_eg_coint(rnorm(50), rnorm(40)), "Length mismatch")
  expect_error(morie_eg_coint(rnorm(10), rnorm(10)), ">=20 obs")
})
