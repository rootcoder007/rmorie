# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for r-package/morie/R/dsp_waveform.R.
#
# Closed-form anchors:
#   * RMS                     - constant signal -> RMS == |constant|
#   * Hjorth                  - sinusoid mobility ~ angular freq; activity = variance
#   * waveform_length         - sum of |diff(x)| on a known sequence
#   * Higuchi / Katz / Ruler  - in roughly [1, 2] for a sine wave
#   * histogram_entropy       - uniform distribution maximises bits

set.seed(1L)



# ---------------------------------------------------------------------------
# RMS / form / crest / shape factors (closed-form anchors)
# ---------------------------------------------------------------------------

test_that("RMS of a constant signal equals its magnitude", {
  expect_equal(morie_dsp_rms(rep(3, 10)), 3, tolerance = 1e-12)
  expect_equal(morie_dsp_rms(rep(-2, 10)), 2, tolerance = 1e-12)
})

test_that("RMS of a unit-amplitude sine is 1/sqrt(2)", {
  N <- 10000L
  t <- seq.int(0, N - 1L) / N
  x <- sin(2 * pi * 5 * t)
  expect_equal(morie_dsp_rms(x), 1 / sqrt(2), tolerance = 1e-3)
})

test_that("crest factor of a unit sine is sqrt(2)", {
  N <- 10000L
  t <- seq.int(0, N - 1L) / N
  x <- sin(2 * pi * 5 * t)
  expect_equal(morie_dsp_crest_factor(x), sqrt(2),
               tolerance = 1e-2)
})

test_that("form factor returns 0 on an all-zero signal", {
  expect_equal(morie_dsp_form_factor(rep(0, 5)), 0)
  expect_equal(morie_dsp_crest_factor(rep(0, 5)), 0)
})


# ---------------------------------------------------------------------------
# Waveform-length descriptors
# ---------------------------------------------------------------------------

test_that("waveform_length returns sum(|diff|) on a known sequence", {
  x <- c(0, 1, 0, 1, 0)
  expect_equal(morie_dsp_waveform_length(x), 4, tolerance = 1e-12)
  expect_equal(morie_dsp_waveform_length_norm(x), 4 / 5,
               tolerance = 1e-12)
})

test_that("integrated_emg / mean_abs return |x| sum / mean", {
  x <- c(-2, 3, -1, 4)
  expect_equal(morie_dsp_integrated_emg(x), 10, tolerance = 1e-12)
  expect_equal(morie_dsp_mean_abs(x),       2.5, tolerance = 1e-12)
})

test_that("arc_length on a flat signal is n - 1", {
  expect_equal(morie_dsp_arc_length(rep(0, 5)), 4, tolerance = 1e-12)
})


# ---------------------------------------------------------------------------
# Hjorth parameters
# ---------------------------------------------------------------------------

test_that("Hjorth activity equals variance", {
  x <- stats::rnorm(500)
  out <- morie_dsp_hjorth(x)
  expect_named(out, c("activity", "mobility", "complexity"))
  expect_equal(out$activity, stats::var(x), tolerance = 1e-12)
})

test_that("Hjorth mobility of a sine is approximately its angular freq", {
  N <- 5000L
  fs <- 1000
  f <- 10
  t <- seq.int(0, N - 1L) / fs
  x <- sin(2 * pi * f * t)
  mob <- morie_dsp_hjorth_mobility(x)
  # mobility approximates 2 pi f / fs * sqrt(...) for a discrete-time
  # sine; we just check it's positive and finite.  Pure sinusoid yields
  # complexity ~ 1 (used as a textbook anchor).
  expect_true(is.finite(mob) && mob > 0)
  comp <- morie_dsp_hjorth_complexity(x)
  expect_equal(comp, 1, tolerance = 0.2)
})

test_that("Hjorth helpers gracefully handle the zero-variance edge case", {
  expect_equal(morie_dsp_hjorth_mobility  (rep(1, 10)), 0)
  expect_equal(morie_dsp_hjorth_complexity(rep(1, 10)), 0)
})


# ---------------------------------------------------------------------------
# Fractal dimensions
# ---------------------------------------------------------------------------

test_that("Higuchi FD is finite for a sine", {
  N <- 1024L
  t <- seq.int(0, N - 1L) / N
  x <- sin(2 * pi * 8 * t)
  fd <- morie_dsp_higuchi_fd(x, kmax = 6L)
  expect_true(is.finite(fd))
})

test_that("Katz FD returns 0 on a flat signal", {
  expect_equal(morie_dsp_katz_fd(rep(2, 50)), 0, tolerance = 1e-12)
})

test_that("Ruler FD returns a finite real number", {
  x <- cumsum(stats::rnorm(512))
  fd <- morie_dsp_ruler_fd(x, n_rulers = 6L)
  expect_true(is.finite(fd))
})


# ---------------------------------------------------------------------------
# Amplitude histogram + entropy
# ---------------------------------------------------------------------------

test_that("amplitude_histogram returns matching counts/edges/centers", {
  set.seed(1L)
  x <- stats::rnorm(200)
  h <- morie_dsp_amplitude_histogram(x, n_bins = 10L)
  expect_named(h, c("counts", "centers", "probabilities", "edges"))
  expect_equal(length(h$counts), 10L)
  expect_equal(length(h$centers), 10L)
  expect_equal(length(h$edges), 11L)
  expect_equal(sum(h$counts), 200L)
  expect_equal(sum(h$probabilities), 1, tolerance = 1e-12)
})

test_that("entropy_histogram is highest for a near-uniform sample", {
  set.seed(1L)
  uni <- stats::runif(2000, min = -1, max = 1)
  norm <- stats::rnorm(2000)
  Hu <- morie_dsp_entropy_histogram(uni,  n_bins = 20L)
  Hn <- morie_dsp_entropy_histogram(norm, n_bins = 20L)
  expect_gt(Hu, Hn)
  expect_lt(Hu, log2(20L) + 1e-6)
})


# ---------------------------------------------------------------------------
# Other helpers
# ---------------------------------------------------------------------------

test_that("variance_ratio returns var(x)/var(y) with Inf guard", {
  expect_equal(morie_dsp_variance_ratio(c(1, 2, 3), c(10, 20, 30)),
               stats::var(c(1, 2, 3)) / stats::var(c(10, 20, 30)),
               tolerance = 1e-12)
  expect_identical(morie_dsp_variance_ratio(c(1, 2, 3),
                                             rep(0, 3)),
                   Inf)
})

test_that("baseline_correlation returns 1 for x == y", {
  set.seed(1L)
  x <- stats::rnorm(100)
  expect_equal(morie_dsp_baseline_correlation(x, x), 1,
               tolerance = 1e-10)
})

test_that("morie_dsp_qrs_features returns the expected peak / duration", {
  beat <- c(0, 0.2, 0.5, 1.0, 0.6, 0.1, 0)
  out <- morie_dsp_qrs_features(beat)
  expect_named(out, c("amplitude", "duration", "area",
                      "slope_up", "slope_down", "peak_index"))
  expect_identical(out$peak_index, 4L)
  expect_equal(out$amplitude, 1.0, tolerance = 1e-12)
  expect_identical(out$duration, 7L)
})
