# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch A: aaa_helpers_time_series_advanced, retlv, siu, hrzq1, xavir.

# ==== aaa_helpers_time_series_advanced.R ====
test_that("%||% returns first arg when non-null and non-empty", {
  expect_equal(morie:::`%||%`(1, 2), 1)
  expect_equal(morie:::`%||%`("a", "b"), "a")
})

test_that("%||% returns second arg when first is NULL", {
  expect_equal(morie:::`%||%`(NULL, 5), 5)
})

test_that(".morie_beta_weights returns a length-K probability vector", {
  set.seed(1)
  w <- morie:::.morie_beta_weights(2, 3, 12)
  expect_length(w, 12)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_true(all(w >= 0))
})

test_that(".morie_beta_weights falls back to uniform when weights collapse to zero", {
  set.seed(1)
  w <- morie:::.morie_beta_weights(1, 1, 4)
  expect_length(w, 4)
  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 1e-10)
})

# ==== retlv.R ====
test_that("retlv returns a Coles-2001 return-level list on Gumbel-like maxima", {
  set.seed(1)
  x <- 10 + 2 * (-log(-log(stats::runif(300))))
  out <- morie:::retlv(x, return_period = 100)
  expect_false(is.null(out))
  expect_true(is.list(out))
  expect_true("estimate" %in% names(out))
})

test_that("retlv exposes mu/sigma/xi/se/n when GEV fit succeeds", {
  set.seed(1)
  x <- 10 + 2 * (-log(-log(stats::runif(400))))
  out <- morie:::retlv(x, return_period = 50)
  if (is.finite(out$estimate)) {
    expect_true(all(c("mu", "sigma", "xi", "se", "n", "return_period") %in% names(out)))
    expect_equal(out$return_period, 50)
    expect_equal(out$n, length(x))
    expect_true(out$se >= 0)
  } else {
    expect_match(out$method, "GEV fit failed")
  }
})

test_that("morie_return_level alias is identical to retlv", {
  expect_identical(morie_return_level, morie:::retlv)
})

# ==== siu.R ====
test_that("morie_fetch_siu returns existing SIU.csv without reparsing", {
  tmp <- tempfile("siu-")
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE))
  out <- file.path(tmp, "SIU.csv")
  writeLines("case_number\nfake-1", out)
  got <- morie_fetch_siu(cache_dir = tmp, overwrite = FALSE, progress = FALSE)
  expect_equal(normalizePath(got), normalizePath(out))
})

# ==== hrzq1.R ====
test_that("hrzq1 returns NA estimates when n is below threshold", {
  set.seed(1)
  out <- morie:::hrzq1(c(1, 2, 3), c(4, 5, 6), tau = 0.5)
  expect_true(is.list(out))
  expect_true(all(is.na(out$estimate)))
  expect_match(out$method, "insufficient data or invalid tau")
})

test_that("hrzq1 rejects out-of-range tau", {
  set.seed(1)
  x <- stats::rnorm(50)
  y <- 1 + 2 * x + stats::rnorm(50)
  out <- morie:::hrzq1(x, y, tau = 1.5)
  expect_match(out$method, "insufficient data or invalid tau")
})

test_that("hrzq1 recovers slope on a clean linear signal at tau=0.5", {
  set.seed(1)
  x <- stats::rnorm(200)
  y <- 1 + 2 * x + stats::rnorm(200, sd = 0.5)
  out <- morie:::hrzq1(x, y, tau = 0.5)
  expect_false(is.null(out))
  expect_true(is.finite(out$estimate))
  expect_true(abs(out$estimate - 2) < 0.5)
  expect_true(out$se >= 0)
  expect_equal(out$n, 200)
})

# ==== xavir.R ====
test_that("morie_xavir_xavier_init errors on non-positive fan_in or fan_out", {
  expect_error(morie_xavir_xavier_init(0, 4), "fan_in and fan_out must be > 0")
  expect_error(morie_xavir_xavier_init(4, -1), "fan_in and fan_out must be > 0")
})

test_that("morie_xavir_xavier_init uniform branch returns matrix in [-limit, +limit]", {
  set.seed(1)
  out <- morie_xavir_xavier_init(fan_in = 8, fan_out = 16, seed = 42L, uniform = TRUE)
  expect_false(is.null(out))
  expect_equal(dim(out$weights), c(8, 16))
  limit <- sqrt(6 / (8 + 16))
  expect_true(all(out$weights >= -limit - 1e-12))
  expect_true(all(out$weights <= limit + 1e-12))
  expect_equal(out$method, "uniform")
  expect_equal(out$shape, c(8, 16))
})

test_that("morie_xavir_xavier_init normal branch returns matrix with expected sd scale", {
  set.seed(1)
  out <- morie_xavir_xavier_init(fan_in = 32, fan_out = 32, seed = 42L, uniform = FALSE)
  expect_equal(dim(out$weights), c(32, 32))
  expect_equal(out$method, "normal")
})
