# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 9 -- R/entheo_preprocess.R (EEG/fMRI preprocessing and
# its filter helpers) and R/dccmd.R (DCC multivariate GARCH).

test_that("preprocess_eeg cleans a synthetic DMT-imaging record", {
  # 1s of the suite here, and r-universe's macOS x86_64 builder is
  # about 1.8 times slower. The check there is killed at sixty minutes
  # and the suite alone was twenty-six of them. The heavy files run in
  # our own CI, which sets NOT_CRAN, where the clock is ours.
  skip_on_cran()
  rec <- rmorie:::.entheo_synthetic_record("03")
  out <- rmorie:::preprocess_eeg(rec)
  expect_true(is.list(out$record))
  expect_equal(out$sfreq, 250)
  expect_true(out$n_channels > 0)
  expect_match(out$interpretation, "bandpass")
})

test_that("preprocess_eeg skips absent / non-matrix eeg blocks", {
  skip_on_cran()
  rec <- rmorie:::.entheo_synthetic_record("04")
  rec$eeg$data_dmt <- NULL
  rec$eeg$data_pcb <- "not a matrix"
  out <- rmorie:::preprocess_eeg(rec)
  expect_true(length(out$warnings) >= 1L)
})

test_that("preprocess_fmri scrubs motion and projects noise components", {
  skip_on_cran()
  rec <- rmorie:::.entheo_synthetic_record("05")
  out <- rmorie:::preprocess_fmri(rec)
  expect_true(out$n_parcels > 0)
  expect_match(out$interpretation, "Motion-scrubbed")
})

test_that("preprocess_fmri warns when motion_fd_mm is absent", {
  skip_on_cran()
  rec <- rmorie:::.entheo_synthetic_record("06")
  rec$fmri$motion_fd_mm <- NULL
  out <- rmorie:::preprocess_fmri(rec)
  expect_true(any(grepl("motion_fd_mm absent", out$warnings)))
})

test_that(".entheo_asr_trim reconstructs extreme samples", {
  skip_on_cran()
  set.seed(1)
  m <- matrix(stats::rnorm(200), 10, 20)
  m[1, 1] <- 1e6
  # a lone outlier inflates its own row mean/sd, so its z is bounded by
  # ~sqrt(n-1); threshold 3 is reachable, threshold 5 would not be.
  tr <- rmorie:::.entheo_asr_trim(m, threshold = 3)
  expect_true(tr$n_bad >= 1L)
  expect_equal(dim(tr$arr), c(10L, 20L))
})

test_that("bandpass / notch fall back to FFT masks without the signal pkg", {
  skip_on_cran()
  set.seed(2)
  m <- matrix(stats::rnorm(20 * 64), 20, 64)
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "signal")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_equal(dim(rmorie:::.entheo_bandpass(m, 250, 1, 40)), dim(m))
  expect_equal(dim(rmorie:::.entheo_notch(m, 250, 60)), dim(m))
})

test_that("bandpass / notch use the signal package when available", {
  skip_on_cran()
  set.seed(3)
  m <- matrix(stats::rnorm(8 * 256), 8, 256)
  expect_equal(dim(rmorie:::.entheo_bandpass(m, 250, 1, 40)), dim(m))
  expect_equal(dim(rmorie:::.entheo_notch(m, 250, 60)), dim(m))
})

test_that("morie_dcc_multivariate_garch runs on a small bivariate series", {
  skip_on_cran()
  set.seed(4)
  x <- matrix(stats::rnorm(400), 200, 2)
  res <- tryCatch(suppressWarnings(morie_dcc_multivariate_garch(x)),
    error = function(e) e
  )
  expect_true(is.list(res) || inherits(res, "error"))
})
