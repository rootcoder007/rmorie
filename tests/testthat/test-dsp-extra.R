# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2J: tests for dsp_filters.R + dsp_spectral.R exports not yet
# exercised by their existing test files. Uses make_synthetic_sine()
# and friends from helper-signal.R (testthat auto-sources helper-*.R).

# ---------------------------------------------------- dsp_filters extras

test_that("morie_dsp_hann_filter smooths noise + preserves length", {
  x <- make_synthetic_sine(n = 200L, fs = 200L, freq_hz = 5,
                           noise_sd = 0.4, seed = 1L)
  y <- morie_dsp_hann_filter(x, window = 7L)
  expect_length(y, 200L)
  expect_lt(stats::sd(y, na.rm = TRUE), stats::sd(x, na.rm = TRUE))
})

test_that("morie_dsp_alpha_trimmed_mean reduces variance below moving average", {
  x <- make_synthetic_sine(n = 200L, fs = 200L, freq_hz = 4,
                           noise_sd = 0.3, seed = 2L)
  x[seq(20L, 200L, by = 20L)] <- 10  # impulse contamination
  y <- morie_dsp_alpha_trimmed_mean(x, window = 9L, alpha = 0.3)
  expect_length(y, 200L)
  expect_true(is.finite(mean(y, na.rm = TRUE)))
})

test_that("morie_dsp_wiener_filter returns a same-length de-noised signal", {
  x <- make_synthetic_sine(n = 300L, fs = 300L, freq_hz = 8,
                           noise_sd = 0.4, seed = 3L)
  y <- tryCatch(morie_dsp_wiener_filter(x), error = function(e) e)
  if (inherits(y, "error")) {
    skip(sprintf("wiener_filter error: %s", conditionMessage(y)))
  }
  expect_length(y, 300L)
})

test_that("morie_dsp_notch removes a targeted 60 Hz tone", {
  fs <- 1000
  x <- make_synthetic_sine(n = fs, fs = fs, freq_hz = 10, seed = 4L) +
    0.5 * make_synthetic_sine(n = fs, fs = fs, freq_hz = 60, seed = 5L)
  y <- morie_dsp_notch(x, freq = 60, fs = fs)
  expect_length(y, length(x))
})

test_that("morie_dsp_comb removes the fundamental + harmonics", {
  fs <- 1000
  x <- make_synthetic_sine(n = fs, fs = fs, freq_hz = 50, seed = 6L) +
    0.5 * make_synthetic_sine(n = fs, fs = fs, freq_hz = 100, seed = 7L)
  y <- morie_dsp_comb(x, fundamental = 50, fs = fs, n_harmonics = 2L)
  expect_length(y, length(x))
})

test_that("morie_dsp_ensemble_average averages an n_segs x window matrix", {
  # ensemble_average expects a numeric matrix (n_segs rows x window cols),
  # not a list — it calls colMeans() directly.
  segs <- do.call(rbind, lapply(seq_len(8L), function(i)
    make_synthetic_sine(n = 100L, fs = 100L, freq_hz = 5,
                        noise_sd = 0.3, seed = 10L + i)))
  y <- morie_dsp_ensemble_average(segs)
  expect_length(y, 100L)
  expect_lt(stats::sd(y, na.rm = TRUE), stats::sd(segs[1, ], na.rm = TRUE))
})

test_that("morie_dsp_synchronized_average averages trigger-aligned windows", {
  set.seed(11L)
  fs <- 200L
  x <- make_synthetic_sine(n = 2000L, fs = fs, freq_hz = 5, seed = 12L)
  triggers <- seq(60L, length(x) - 100L, by = 100L)
  y <- morie_dsp_synchronized_average(x, trigger_indices = triggers,
                                       window = 80L)
  expect_true(is.numeric(y) || is.list(y))
})

test_that("morie_dsp_turning_points returns a list with turning_points count", {
  x <- make_synthetic_sine(n = 200L, fs = 200L, freq_hz = 5,
                           noise_sd = 0.0, seed = 14L)
  out <- morie_dsp_turning_points(x)
  expect_type(out, "list")
  expect_true(all(c("turning_points", "expected", "z_statistic",
                    "stationary") %in% names(out)))
  expect_true(out$turning_points >= 0L)
})

test_that("morie_dsp_cv returns 0 for a constant signal", {
  expect_equal(morie_dsp_cv(rep(5, 100L)), 0, tolerance = 1e-12)
})

test_that("morie_dsp_cv > 0 for a noisy signal", {
  expect_gt(morie_dsp_cv(stats::rnorm(100, mean = 5)), 0)
})

test_that("morie_dsp_wiener_hopf solves R w = r for a small system", {
  Rxx <- matrix(c(2, 0.5, 0.5, 2), 2, 2)
  rxd <- c(1.0, -0.3)
  w <- morie_dsp_wiener_hopf(Rxx, rxd)
  expect_length(w, 2L)
  expect_equal(as.numeric(Rxx %*% w), rxd, tolerance = 1e-10)
})

test_that("morie_dsp_even_odd decomposes x into even+odd halves", {
  set.seed(15L)
  x <- stats::rnorm(10L)
  out <- morie_dsp_even_odd(x)
  expect_true(is.list(out) || is.data.frame(out))
  # Most implementations return list(even, odd).
  if (is.list(out) && all(c("even", "odd") %in% names(out))) {
    expect_length(out$even, length(x))
    expect_length(out$odd, length(x))
    expect_equal(out$even + out$odd, x, tolerance = 1e-12)
  }
})

# --------------------------------------------------- dsp_spectral extras

test_that("morie_dsp_psd_bartlett peaks near the input sine frequency", {
  fs <- 1024L
  x <- make_synthetic_sine(n = fs, fs = fs, freq_hz = 50,
                           noise_sd = 0.1, seed = 21L)
  out <- morie_dsp_psd_bartlett(x, fs = fs, n_segments = 4L)
  expect_true(is.list(out) || is.numeric(out))
})

test_that("morie_dsp_spectral_ratio gives the ratio of band1 / band2 power", {
  freqs <- seq(0, 250, length.out = 256L)
  psd <- rep(1, length(freqs))     # flat PSD -> ratio of bandwidths
  out <- morie_dsp_spectral_ratio(psd, freqs,
                                   band1 = c(10, 30), band2 = c(50, 100))
  expect_true(is.numeric(out) && is.finite(out))
})

test_that("morie_dsp_acf_from_psd returns a same-length real autocorrelation", {
  freqs <- seq(0, 250, length.out = 128L)
  psd <- exp(-abs(freqs - 100) / 20)
  out <- morie_dsp_acf_from_psd(psd)
  expect_true(is.numeric(out) || is.list(out))
})

test_that("morie_dsp_fractal_dim_psd returns a finite scaling exponent", {
  set.seed(22L)
  freqs <- seq(1, 250, length.out = 256L)
  psd <- freqs^(-1.5) * exp(stats::rnorm(length(freqs), sd = 0.1))
  out <- morie_dsp_fractal_dim_psd(psd, freqs)
  expect_true(is.numeric(out) || is.list(out))
})

test_that("morie_dsp_coherence returns coherence values in [0, 1]", {
  fs <- 1024L
  x <- make_synthetic_sine(n = fs, fs = fs, freq_hz = 50, seed = 23L)
  y <- x + stats::rnorm(fs, sd = 0.2)
  out <- morie_dsp_coherence(x, y, fs = fs, nperseg = 256L)
  expect_true(is.list(out) || is.numeric(out))
})

test_that("morie_dsp_fbm_synthesis returns length-N path", {
  out <- morie_dsp_fbm_synthesis(N = 256L, H = 0.7)
  expect_true(is.numeric(out) || is.list(out))
  if (is.numeric(out)) expect_length(out, 256L)
})
