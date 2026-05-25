# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/entheo_dmt.R

set.seed(2026L)

test_that("morie_entheo_available_subjects returns integer", {
  # Will likely be integer(0) on machines without the DMT_Imaging mirror;
  # that's the documented behaviour, not an error.
  subs <- morie_entheo_available_subjects()
  expect_true(is.integer(subs))
})

test_that("morie_entheo_load_fmri_subject errors when root not set", {
  # When DMT_Imaging root is missing, .morie_entheo_require_root errors
  res <- tryCatch(
    morie_entheo_load_fmri_subject(1L, "DMT"),
    error = function(e) e
  )
  expect_s3_class(res, "error")
})

test_that("morie_entheo_load_fmri_subject rejects invalid condition", {
  expect_error(morie_entheo_load_fmri_subject(1L, "PLACEBO_WRONG"))
})

test_that("morie_entheo_load_eeg_region errors when missing region/root", {
  res <- tryCatch(
    morie_entheo_load_eeg_region("Frontal"),
    error = function(e) e
  )
  expect_s3_class(res, "error")
})

test_that("morie_entheo_load_eeg_region rejects bad region", {
  expect_error(morie_entheo_load_eeg_region("BADREGION_XYZ"))
})

test_that("morie_entheo_dataset_overview errors without root", {
  res <- tryCatch(
    morie_entheo_dataset_overview(),
    error = function(e) e
  )
  expect_s3_class(res, "error")
})

test_that("morie_entheo_spectral_band_power on synthetic signal", {
  # Simulate 4s of 200Hz sine wave at 10Hz (alpha-band)
  fs <- 200
  t <- seq_len(4 * fs) / fs
  sig <- sin(2 * pi * 10 * t) + 0.1 * rnorm(length(t))
  res <- morie_entheo_spectral_band_power(sig, fs = fs)
  expect_true(is.list(res))
  expect_true("payload" %in% names(res))
  expect_true(length(res$payload$rows) >= 1L)
})

test_that("morie_entheo_spectral_band_power short signal warns", {
  res <- morie_entheo_spectral_band_power(c(1, 2, 3))
  expect_true(any(grepl("too short", res$warnings)))
})

test_that("morie_entheo_spectral_band_power with custom nperseg", {
  fs <- 100
  sig <- sin(2 * pi * 5 * seq_len(400) / fs) + 0.1 * rnorm(400)
  res <- morie_entheo_spectral_band_power(sig, fs = fs, nperseg = 128L)
  expect_true(is.list(res))
})

test_that("morie_entheo_dynamic_functional_connectivity happy path", {
  # 20 regions x 200 TRs
  bold <- matrix(rnorm(20 * 200), nrow = 20)
  res <- morie_entheo_dynamic_functional_connectivity(
    bold, window = 30L, step = 10L
  )
  expect_true(is.list(res))
  expect_true("payload" %in% names(res))
  expect_true(res$payload$n_windows > 0L)
})

test_that("morie_entheo_dynamic_functional_connectivity tiny BOLD warns", {
  bold <- matrix(rnorm(10), nrow = 2)
  res <- morie_entheo_dynamic_functional_connectivity(
    bold, window = 30L, step = 5L
  )
  expect_true(any(grepl("insufficient", res$warnings)))
})

test_that("morie_entheo_lz_complexity on alternating sequence", {
  sig <- c(rep(0, 20), rep(1, 20), rep(0, 20))
  res <- morie_entheo_lz_complexity(sig)
  expect_true("payload" %in% names(res))
  expect_true(is.numeric(res$payload$lz_raw))
  expect_true(is.numeric(res$payload$lz_normalised))
})

test_that("morie_entheo_lz_complexity custom threshold", {
  sig <- rnorm(100)
  res <- morie_entheo_lz_complexity(sig, threshold = 0)
  expect_equal(res$payload$threshold, 0)
})

test_that("morie_entheo_lz_complexity short signal warns", {
  res <- morie_entheo_lz_complexity(c(1, 2, 3, 4))
  expect_true(any(grepl("too short", res$warnings)))
})

test_that("morie_entheo_analyze_subject without dataset returns error rows", {
  res <- morie_entheo_analyze_subject(subject_id = 1L,
                                       conditions = c("DMT", "PCB"))
  expect_true("payload" %in% names(res))
  expect_true(length(res$payload$rows) == 2L)
  # Both rows should carry an error key (since no DMT_Imaging on disk)
  expect_true(all(vapply(res$payload$rows,
                          function(r) !is.null(r$error),
                          logical(1))))
})
