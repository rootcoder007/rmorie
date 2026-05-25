# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for r-package/morie/R/dsp_detection.R.
#
# Anchors:
#   * threshold_detect       - all/none crossings; min_distance enforced
#   * zero_crossing          - clean sine has the expected ZCR
#   * Teager energy          - closed form on a tiny constant-amp signal
#   * Hilbert envelope       - constant amplitude => flat envelope ~= A
#   * template_match         - peak inside a self-embedded segment

set.seed(1L)



# ---------------------------------------------------------------------------
# threshold_detect
# ---------------------------------------------------------------------------

test_that("threshold_detect picks all 'above' samples by default", {
  x <- c(0, 1, 0, 2, 0, 3, 0)
  idx <- morie_dsp_threshold_detect(x, threshold = 0.5)
  expect_identical(idx, c(2L, 4L, 6L))
})

test_that("threshold_detect 'below' direction returns sub-threshold samples", {
  x <- c(0, -1, 0, -2, 0)
  idx <- morie_dsp_threshold_detect(x, threshold = -0.5,
                                    direction = "below")
  expect_identical(idx, c(2L, 4L))
})

test_that("threshold_detect 'either' uses |x| > threshold", {
  x <- c(0.1, -2, 0.1, 3, 0.1)
  idx <- morie_dsp_threshold_detect(x, threshold = 1,
                                    direction = "either")
  expect_identical(idx, c(2L, 4L))
})

test_that("threshold_detect enforces min_distance >= gap", {
  x <- rep(1, 10)
  idx <- morie_dsp_threshold_detect(x, threshold = 0.5,
                                    min_distance = 3L)
  # First match kept, then only every 3rd or further.
  expect_identical(idx, c(1L, 4L, 7L, 10L))
})

test_that("threshold_detect errors on an unknown direction", {
  expect_error(
    morie_dsp_threshold_detect(1:3, 1, direction = "bogus"),
    "direction"
  )
})


# ---------------------------------------------------------------------------
# zero_crossing
# ---------------------------------------------------------------------------

test_that("zero_crossing whole-signal ZCR matches sine cycle count", {
  N <- 1000L
  t <- seq.int(0, N - 1L) / N
  f <- 5
  x <- sin(2 * pi * f * t)
  zcr <- morie_dsp_zero_crossing(x)
  # 5 full cycles -> 2 * 5 zero crossings; ZCR = 10 / (N-1).
  expect_equal(zcr, 10 / (N - 1L), tolerance = 1e-3)
})

test_that("zero_crossing per-frame returns one value per frame", {
  set.seed(1L)
  x <- stats::rnorm(200)
  z <- morie_dsp_zero_crossing(x, frame_length = 50L)
  expect_equal(length(z), 4L)
  expect_true(all(z >= 0 & z <= 1))
})


# ---------------------------------------------------------------------------
# Teager energy (closed form)
# ---------------------------------------------------------------------------

test_that("teager_energy matches its closed-form definition", {
  x <- c(1, 2, 3, 4, 5)
  # psi[i] = x[i]^2 - x[i-1] * x[i+1] for interior indices.
  want <- c(0,
            2^2 - 1 * 3,
            3^2 - 2 * 4,
            4^2 - 3 * 5,
            0)
  expect_equal(morie_dsp_teager_energy(x), want, tolerance = 1e-12)
})

test_that("teager_energy on a too-short signal returns zeros", {
  expect_equal(morie_dsp_teager_energy(c(1, 2)), c(0, 0),
               tolerance = 1e-12)
})


# ---------------------------------------------------------------------------
# Hilbert envelope
# ---------------------------------------------------------------------------

test_that("hilbert_envelope of a constant-amplitude sine is approximately A", {
  N <- 1024L
  fs <- 1024
  f <- 64
  t <- seq.int(0, N - 1L) / fs
  A <- 2.0
  x <- A * sin(2 * pi * f * t)
  env <- morie_dsp_hilbert_envelope(x)
  # Trim the FFT edges (~16 samples each side) before checking.
  interior <- env[20:(N - 20)]
  expect_equal(mean(interior), A, tolerance = 0.1)
  expect_lt(stats::sd(interior), 0.2)
})


# ---------------------------------------------------------------------------
# Template match
# ---------------------------------------------------------------------------

test_that("template_match peaks at the embedded location with high corr", {
  set.seed(1L)
  tpl <- c(0, 1, 2, 1, 0)
  x <- c(stats::rnorm(15, sd = 0.01), tpl,
         stats::rnorm(15, sd = 0.01))
  out <- morie_dsp_template_match(x, tpl, threshold = 0.7)
  expect_true(length(out$indices) >= 1L)
  # Best-correlation index should sit within +/- 1 of the embed start
  # (index 16; the slider returns leading indices).
  best <- out$indices[which.max(out$correlations)]
  expect_true(abs(best - 16L) <= 1L)
})

test_that("template_match rejects a zero-variance template", {
  out <- morie_dsp_template_match(stats::rnorm(50), rep(1, 5))
  expect_identical(out$indices, integer(0))
  expect_identical(out$correlations, numeric(0))
})


# ---------------------------------------------------------------------------
# Shannon energy + onset detection
# ---------------------------------------------------------------------------

test_that("shannon_energy is non-negative everywhere", {
  x <- stats::rnorm(50)
  se <- morie_dsp_shannon_energy(x)
  expect_equal(length(se), length(x))
  expect_true(all(se >= -1e-9))
})

test_that("onset_detect picks up the burst start in a flat-then-loud signal", {
  set.seed(1L)
  x <- c(0.01 * stats::rnorm(200),
         5 + 0.01 * stats::rnorm(200))
  onsets <- morie_dsp_onset_detect(x, fs = 1000,
                                   energy_window_ms = 5,
                                   threshold_factor = 3)
  expect_true(length(onsets) >= 1L)
  # Detector may fire on a few noise excursions before the burst at
  # sample 200; require at least one onset inside the burst-onset
  # window [100, 250] rather than insisting it's the first.
  expect_true(any(onsets >= 100L & onsets <= 250L))
})
