# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage lift batch H: frns_temporal, hrzi1, paths, rgcoh, rgpsd, rkhsf, gbgen, spblk, data_access, sptrn.

test_that("morie_predpol_temporal_audit errors on misaligned input lengths", {
  expect_error(
    morie_predpol_temporal_audit(
      period = c("p1", "p2"),
      city = c("A"),
      y_pred = c(1, 0),
      group = c("X", "Y")
    ),
    "must all align"
  )
})

test_that("morie_predpol_temporal_audit errors on empty inputs", {
  expect_error(
    morie_predpol_temporal_audit(
      period = character(0),
      city = character(0),
      y_pred = integer(0),
      group = character(0)
    ),
    "inputs are empty"
  )
})

test_that(".hrzi1_obj returns big penalty for zero-norm beta", {
  set.seed(1)
  X <- matrix(rnorm(40), 20, 2)
  y <- rnorm(20)
  out <- morie:::.hrzi1_obj(c(0, 0), X, y, h0 = 1)
  expect_equal(out, 1e12)
})

test_that("hrzi1 returns NA estimate on insufficient data", {
  set.seed(1)
  X <- matrix(rnorm(8), 4, 2)
  y <- rnorm(4)
  res <- morie:::hrzi1(X, y)
  expect_true(all(is.na(res$estimate)))
  expect_match(res$method, "insufficient")
})

test_that("%||% returns rhs for NULL", {
  expect_equal(morie:::`%||%`(NULL, "fallback"), "fallback")
  expect_equal(morie:::`%||%`("value", "fallback"), "value")
})

test_that("is_absolute_path detects unix + windows roots", {
  expect_true(morie:::is_absolute_path("/etc/passwd"))
  expect_true(morie:::is_absolute_path("C:/Users/x"))
  expect_false(morie:::is_absolute_path("relative/path"))
})

test_that("morie_find_project_root errors when no markers found", {
  tmp <- tempfile()
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE))
  expect_error(
    morie_find_project_root(start = tmp, max_up = 2L),
    "Unable to detect project root"
  )
})

test_that("morie_paths returns named list when given explicit root", {
  tmp <- tempfile()
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE))
  paths <- morie_paths(project_root = tmp)
  expect_true(grepl("data$", paths$data_dir))
})

test_that("rgcoh errors on unequal-length inputs", {
  expect_error(rgcoh(rnorm(100), rnorm(50)), "equal length")
})

test_that("rgcoh recovers high coherence at a shared sinusoid frequency", {
  set.seed(1)
  fs <- 100
  t <- seq(0, 10, length.out = 1024)
  a <- sin(2 * pi * 10 * t)
  b <- a + 0.1 * rnorm(length(t))
  out <- rgcoh(a, b, fs = fs)
  expect_true(out$peak_coherence > 0.5)
})

test_that("rgpsd recovers a 10 Hz sinusoid peak", {
  set.seed(1)
  fs <- 100
  t <- seq(0, 10, length.out = 1000)
  x <- sin(2 * pi * 10 * t)
  r <- rgpsd(x, fs = fs, nperseg = 256)
  expect_true(abs(r$peak_freq - 10) < 1)
  expect_true(r$total_power > 0)
})

test_that("rgpsd supports hamming and boxcar windows", {
  set.seed(1)
  x <- rnorm(512)
  r_hamming <- rgpsd(x, fs = 50, nperseg = 128, window = "hamming")
  r_boxcar <- rgpsd(x, fs = 50, nperseg = 128, window = "boxcar")
  expect_true(is.finite(r_hamming$peak_freq))
})

test_that("morie_rkhs_full runs on default kernel bandwidth", {
  set.seed(1)
  M <- matrix(sample(0:2, 200, TRUE), 50, 4)
  out <- morie_rkhs_full(x = rnorm(50), y = rnorm(50), markers = M)
  expect_equal(length(out$alpha), 50L)
  expect_equal(out$n, 50L)
})

test_that("morie_gradient_boosting_genomic runs with default args", {
  set.seed(1)
  M <- matrix(sample(0:2, 200, TRUE), 50, 4)
  out <- morie_gradient_boosting_genomic(
    x = rnorm(50), y = rnorm(50), markers = M,
    n_estimators = 5, seed = 1
  )
  expect_equal(out$n, 50L)
  expect_true(is.finite(out$estimate))
})

test_that("spblk solves block kriging on 2D coords with explicit quadrature pts", {
  set.seed(1)
  coords <- cbind(runif(20), runif(20))
  x <- rnorm(20)
  block <- matrix(c(0.4, 0.4, 0.5, 0.5, 0.6, 0.6), ncol = 2, byrow = TRUE)
  out <- spblk(x, coords, blocks = list(block))
  expect_equal(out$n, 20L)
  expect_equal(length(out$estimate), 1L)
  expect_true(out$se[1] >= 0)
})

test_that(".morie_url_with_params handles NULL + existing query", {
  expect_equal(morie:::.morie_url_with_params("http://x", NULL), "http://x")
  out_existing <- morie:::.morie_url_with_params("http://x?a=1", list(b = "v"))
  expect_match(out_existing, "&b=v")
})

test_that(".morie_ckan_portal resolves known + errors on unknown", {
  expect_match(morie:::.morie_ckan_portal("open.canada.ca"), "^https://")
  expect_error(
    morie:::.morie_ckan_portal("not-a-portal"),
    "Unknown CKAN portal"
  )
})

test_that(".morie_detect_format falls back on URL extension", {
  expect_equal(morie:::.morie_detect_format("http://x.invalid/foo.csv"), "csv")
  expect_equal(morie:::.morie_detect_format("http://x.invalid/foo.json"), "json")
  expect_equal(morie:::.morie_detect_format("http://x.invalid/foo.bogus"), "csv")
})

test_that(".morie_parse_file errors on unsupported format", {
  tmp <- tempfile(fileext = ".dat")
  writeLines("x", tmp)
  withr::defer(unlink(tmp))
  expect_error(morie:::.morie_parse_file(tmp, "weirdfmt", TRUE), "Unsupported parse format")
})

test_that("morie_fetch requires zip_member for zip format", {
  expect_error(
    morie_fetch("http://x.invalid/x.zip", format = "zip"),
    "zip_member"
  )
})

test_that("sptrn 1D order-1 recovers intercept + slope", {
  set.seed(1)
  out <- sptrn(c(1, 2, 3, 4, 5), matrix(0:4, ncol = 1), order = 1)
  expect_equal(out$estimate, c(1, 1), tolerance = 1e-8)
  expect_equal(out$r2, 1, tolerance = 1e-8)
})

test_that("sptrn errors on order > 3", {
  coords <- matrix(runif(40), 20, 2)
  expect_error(sptrn(rnorm(20), coords, order = 4), "trend_order > 3")
})

test_that("sptrn 2D order-2 returns 6 coefficients", {
  set.seed(1)
  coords <- matrix(runif(40), 20, 2)
  out <- sptrn(rnorm(20), coords, order = 2)
  expect_equal(length(out$estimate), 6L)
})
