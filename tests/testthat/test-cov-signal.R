# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 5 -- R/signal.R: Butterworth / Savitzky-Golay filters,
# Hurst exponent, Higuchi fractal dimension, and the .morie_py_call
# Python-bridge fallback. The bridge is mocked; the signal-package
# paths run when 'signal' / 'pracma' are installed.

test_that("Butterworth + Savitzky-Golay filters run via the signal pkg", {
  set.seed(1)
  tt <- seq(0, 1, length.out = 600)
  x <- sin(2 * pi * 5 * tt) + 0.5 * sin(2 * pi * 60 * tt)
  expect_length(buttlp(x, 600, 20)$filtered, 600L)
  expect_length(butthp(x, 600, 1)$filtered, 600L)
  expect_length(buttbp(x, 600, 3, 30)$filtered, 600L)
  expect_equal(buttbs(x, 600)$name, "butter_bandstop")
  expect_length(morie_pcg_filter(stats::rnorm(2000))$filtered, 2000L)
  expect_equal(morie_sgolay_smooth(x, 11L, 3L)$name, "savitzky_golay")
})

test_that("morie_hurst_r estimates the Hurst exponent via pracma", {
  set.seed(2)
  r <- morie_hurst_r(cumsum(stats::rnorm(2048)))
  expect_true(r$interpretation %in%
    c("persistent", "anti-persistent", "random"))
})

test_that(".morie_py_call builds the bridge command and shells out", {
  captured <- NULL
  testthat::local_mocked_bindings(
    system2 = function(command, args, ...) {
      captured <<- args
      "bridge-result"
    }, .package = "base"
  )
  out <- morie:::.morie_py_call("hfd", c(1, 2, 3), 10L)
  expect_equal(out, "bridge-result")
  # a multi-element numeric arg is formatted as [1,2,3]
  expect_true(any(grepl("[1,2,3]", captured, fixed = TRUE)))
})

test_that("hfd returns a structured higuchi_fd list on a deterministic input", {
  set.seed(1L)
  out <- hfd(cumsum(stats::rnorm(50)), kmax = 5L)
  expect_type(out, "list")
  expect_equal(out$name, "higuchi_fd")
  expect_true(is.finite(out$value))
  expect_equal(out$extra$kmax, 5L)
  expect_length(out$extra$L_k, 5L)
})

test_that("filters fall back to the Python bridge without the signal pkg", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "signal")) FALSE else TRUE
    },
    .package = "base"
  )
  testthat::local_mocked_bindings(
    .morie_py_call = function(fn_name, ...) paste0("bridge:", fn_name),
    .package = "morie"
  )
  expect_equal(buttlp(1:10, 100, 10), "bridge:buttlp")
  expect_equal(butthp(1:10, 100, 10), "bridge:butthp")
  expect_equal(buttbp(1:10, 100, 5, 20), "bridge:buttbp")
  expect_equal(buttbs(1:10, 100), "bridge:buttbs")
  expect_equal(morie_sgolay_smooth(1:20), "bridge:sgolay")
})

test_that("morie_hurst_r falls back to the Python bridge without pracma", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "pracma")) FALSE else TRUE
    },
    .package = "base"
  )
  testthat::local_mocked_bindings(
    .morie_py_call = function(fn_name, ...) "bridge:hurst",
    .package = "morie"
  )
  expect_equal(morie_hurst_r(cumsum(stats::rnorm(64))), "bridge:hurst")
})
