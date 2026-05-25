# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for r-package/morie/R/dsp_spectral.R.
#
# Closed-form / Parseval-anchored checks:
#   * periodogram         - peak at the sine frequency; total power
#                            equals A^2/2 for a unit-amplitude cosine
#   * Welch PSD           - same power identity, with a Hamming window
#   * spectral moments    - m0 sums to total power
#   * mean_frequency      - delta-peak at f -> mean_freq == f
#   * spectral_entropy    - white spectrum maximises entropy
#   * spectral_flatness   - flat (white) PSD => flatness ~= 1
#                            spike PSD => flatness ~= 0

set.seed(1L)



# ---------------------------------------------------------------------------
# Synthetic sinusoid fixture
# ---------------------------------------------------------------------------

.make_sine <- function(A = 1, f = 50, fs = 1024, N = 1024) {
  t <- seq.int(0, N - 1L) / fs
  list(x = A * cos(2 * pi * f * t), fs = fs, f = f, A = A)
}


# ---------------------------------------------------------------------------
# Periodogram
# ---------------------------------------------------------------------------

test_that("periodogram peaks near the sine frequency", {
  s <- .make_sine()
  out <- morie_dsp_psd_periodogram(s$x, fs = s$fs)
  expect_named(out, c("freqs", "psd"))
  expect_equal(length(out$freqs), length(s$x) %/% 2L + 1L)
  expect_equal(s$fs / 2, out$freqs[length(out$freqs)],
               tolerance = 1e-6)
  peak_f <- out$freqs[which.max(out$psd)]
  expect_equal(peak_f, s$f, tolerance = 1)  # 1 Hz resolution
})

test_that("periodogram total power == A^2/2 for a pure cosine (Parseval)", {
  # For x[n] = A cos(2 pi f n / fs) with f a bin frequency, integrating
  # the one-sided PSD over [0, fs/2] returns mean(x^2) = A^2 / 2.
  s <- .make_sine(A = 2.0, f = 64, fs = 1024, N = 1024)
  out <- morie_dsp_psd_periodogram(s$x, fs = s$fs)
  df <- out$freqs[2] - out$freqs[1]
  total <- sum(out$psd) * df
  expect_equal(total, mean(s$x^2), tolerance = 1e-3)
  expect_equal(total, s$A^2 / 2,   tolerance = 1e-2)
})


# ---------------------------------------------------------------------------
# Welch PSD
# ---------------------------------------------------------------------------

test_that("Welch PSD peaks near the sine frequency", {
  # Pure-R fallback path: independent of the optional `signal` package.
  s <- .make_sine(N = 2048)
  out <- morie_dsp_psd_welch(s$x, fs = s$fs, nperseg = 256L)
  expect_named(out, c("freqs", "psd"))
  peak_f <- out$freqs[which.max(out$psd)]
  expect_equal(peak_f, s$f, tolerance = s$fs / 256)
})

test_that("Welch integrated power recovers A^2/2 to within window loss", {
  s <- .make_sine(A = 1.0, f = 64, fs = 1024, N = 4096)
  out <- morie_dsp_psd_welch(s$x, fs = s$fs, nperseg = 256L)
  df <- out$freqs[2] - out$freqs[1]
  total <- sum(out$psd) * df
  expect_equal(total, 0.5, tolerance = 0.05)
})


# ---------------------------------------------------------------------------
# Moments / mean / median / edge frequency
# ---------------------------------------------------------------------------

test_that("spectral_moment order 0 equals integrated power", {
  s <- .make_sine()
  out <- morie_dsp_psd_periodogram(s$x, fs = s$fs)
  m0 <- morie_dsp_spectral_moment(out$psd, out$freqs, order = 0L)
  df <- out$freqs[2] - out$freqs[1]
  expect_equal(m0, sum(out$psd) * df, tolerance = 1e-12)
})

test_that("mean_frequency of a delta PSD equals the spike location", {
  freqs <- seq.int(0, 64) / 64
  psd <- numeric(65); psd[20L] <- 1
  expect_equal(morie_dsp_mean_frequency(psd, freqs),
               freqs[20L], tolerance = 1e-12)
})

test_that("median_frequency / spectral_edge return the expected bin", {
  # Uniform PSD: median frequency is the midpoint; SEF95 sits near
  # 95 % of fmax.
  freqs <- seq.int(0, 100) / 100
  psd <- rep(1, 101)
  mfreq <- morie_dsp_median_frequency(psd, freqs)
  expect_equal(mfreq, 0.5, tolerance = 0.05)
  expect_equal(morie_dsp_spectral_edge(psd, freqs, pct = 0.95),
               0.95, tolerance = 0.05)
})

test_that("mean / median frequency return 0 on a zero PSD", {
  expect_equal(morie_dsp_mean_frequency  (rep(0, 5), seq.int(0, 4)), 0)
  expect_equal(morie_dsp_median_frequency(rep(0, 5), seq.int(0, 4)), 0)
})


# ---------------------------------------------------------------------------
# Entropy / flatness
# ---------------------------------------------------------------------------

test_that("spectral_entropy is max for a uniform PSD, min for a spike", {
  N <- 64L
  uni <- rep(1, N)
  spike <- numeric(N); spike[10L] <- 1
  H_uni <- morie_dsp_spectral_entropy(uni)
  H_spk <- morie_dsp_spectral_entropy(spike)
  expect_equal(H_uni, log2(N), tolerance = 1e-10)
  expect_equal(H_spk, 0,        tolerance = 1e-10)
  expect_equal(morie_dsp_spectral_entropy(rep(0, N)), 0)
})

test_that("spectral_flatness ~ 1 for white PSD, ~ 0 for spike", {
  uni <- rep(1, 64)
  spk <- c(1, rep(1e-9, 63))
  expect_equal(morie_dsp_spectral_flatness(uni), 1, tolerance = 1e-12)
  expect_lt(morie_dsp_spectral_flatness(spk), 0.1)
})


# ---------------------------------------------------------------------------
# Other small helpers
# ---------------------------------------------------------------------------

test_that("psd_to_db is invertible up to the floor", {
  psd <- c(1, 0.1, 0.01)
  expect_equal(morie_dsp_psd_to_db(psd), c(0, -10, -20),
               tolerance = 1e-12)
})

test_that("morie_dsp_band_power integrates a uniform PSD correctly", {
  freqs <- seq.int(0, 100) / 100
  psd <- rep(1, 101)
  p <- morie_dsp_band_power(psd, freqs, 0.2, 0.5)
  expect_equal(p, 0.3, tolerance = 0.05)
})

test_that("morie_dsp_window returns a length-N vector with finite values", {
  for (w in c("hamming", "hann", "blackman", "bartlett",
              "rectangular")) {
    v <- morie_dsp_window(64L, w)
    expect_equal(length(v), 64L)
    expect_true(all(is.finite(v)))
  }
})

test_that("spectral_kurtosis is finite on a usable PSD", {
  s <- .make_sine()
  out <- morie_dsp_psd_periodogram(s$x, fs = s$fs)
  k <- morie_dsp_spectral_kurtosis(out$psd, out$freqs)
  expect_true(is.finite(k))
})
