# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 12 -- long-tail sweep: small low-coverage files
# (bpblm, fzcvm, ghsrv, fast, det-rng helpers, beta weights, rgwav).

test_that("bits_per_byte computes BPB and validates input", {
  r <- morie:::bits_per_byte(c(1, 2, 3))
  expect_equal(r$nll_nats, 6)
  expect_equal(r$n_tokens, 3L)
  expect_equal(r$n_bytes, 3L)
  expect_equal(r$value, 6 / (3 * log(2)))
  expect_equal(morie:::bits_per_byte(c(1, 2), n_bytes = 10)$n_bytes, 10L)
  expect_error(morie:::bits_per_byte(numeric(0)), "at least one")
  expect_error(morie:::bits_per_byte(c(1, 2), n_bytes = 0), "must be > 0")
})

test_that(".morie_beta_weights and %||% behave", {
  w <- morie:::.morie_beta_weights(2, 5, 12)
  expect_length(w, 12L)
  expect_equal(sum(w), 1)
  nz <- morie:::`%||%`
  expect_equal(nz(NULL, 5), 5)
  expect_equal(nz(3, 5), 3)
})

test_that("morie_ghosal_survival_beta_process estimates posterior survival", {
  set.seed(1)
  res <- morie_ghosal_survival_beta_process(stats::rexp(60),
    event = stats::rbinom(60, 1, 0.7)
  )
  expect_true(is.list(res))
  expect_true(is.numeric(res$estimate))
  empty <- morie_ghosal_survival_beta_process(numeric(0))
  expect_equal(empty$n, 0)
})

test_that("fzcvm runs and .morie_cvm_pvalue spans its table", {
  expect_equal(fzcvm(1:3)$method, "fzcvm - too few obs")
  set.seed(2)
  r <- fzcvm(stats::rnorm(150), cdf = "norm", args = list(0, 1))
  expect_true(r$statistic >= 0)
  rf <- fzcvm(stats::rnorm(120), cdf = function(t) stats::pnorm(t))
  expect_true(is.numeric(rf$p_value))
  expect_error(fzcvm(stats::rnorm(20), cdf = "exp"), "function")
  expect_equal(morie:::.morie_cvm_pvalue(0), 1.0)
  expect_equal(morie:::.morie_cvm_pvalue(0.1), 0.5)
  expect_true(morie:::.morie_cvm_pvalue(2) > 0)
  expect_true(morie:::.morie_cvm_pvalue(0.5) > 0 &&
    morie:::.morie_cvm_pvalue(0.5) < 0.1)
})

test_that("det-rng helpers derive stable SHA-keyed seeds", {
  skip_if_not_installed("digest")
  skip_if_not_installed("openssl")
  skip_if(
    !requireNamespace("digest", quietly = TRUE) &&
      !requireNamespace("openssl", quietly = TRUE),
    "no SHA-256 backend"
  )
  expect_equal(
    morie:::.morie_sha256_hex("abc"),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  )
  s <- morie_det_rng("ksr07_bootstrap", 42L)
  expect_type(s, "integer")
  expect_identical(morie_det_rng("ksr07_bootstrap", 42L), s)
  hx <- morie_det_rng_sha_hex("ksr07_bootstrap", 42L)
  expect_equal(nchar(hx), 64L)
  expect_error(morie_det_rng(c("a", "b"), 1L))
})

test_that("fast.R kernels work on the C++ and the base-R fallback path", {
  expect_type(morie_fast_available(), "logical")
  expect_equal(morie:::morie_mean(1:10), 5.5)
  expect_equal(morie:::morie_cor_pearson(1:10, 1:10), 1)
  expect_equal(morie:::morie_normal_pdf(0), stats::dnorm(0))
  testthat::local_mocked_bindings(
    .cpp_available = function() FALSE, .package = "morie"
  )
  expect_equal(morie:::morie_mean(1:10), 5.5)
  expect_equal(morie:::morie_var(1:10), stats::var(1:10))
  expect_true(is.na(morie:::morie_var(5, ddof = 1)))
  expect_equal(morie:::morie_cor_pearson(1:5, 1:5), 1)
  expect_equal(morie:::morie_normal_pdf(0, 0, 1), stats::dnorm(0))
})

test_that("rgwav denoises via wavelets and via the MA fallback", {
  skip_if_not_installed("wavelets")
  set.seed(3)
  x <- sin(2 * pi * 3 * seq(0, 1, length.out = 256)) +
    0.3 * stats::rnorm(256)
  if (requireNamespace("wavelets", quietly = TRUE)) {
    r <- rgwav(x, level = 3)
    expect_length(r$signal, 256L)
    expect_equal(rgwav(x, level = 3, mode = "hard")$mode, "hard")
  }
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "wavelets")) FALSE else TRUE
    },
    .package = "base"
  )
  fb <- suppressWarnings(rgwav(x))
  expect_equal(fb$mode, "MA-fallback")
  expect_length(fb$signal, 256L)
})
