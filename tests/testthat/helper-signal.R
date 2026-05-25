# SPDX-License-Identifier: AGPL-3.0-or-later
# Shared synthetic-signal fixtures for the signal.R test suite.
# testthat auto-sources helper-*.R before the test files.

# A deterministic 1-D sinusoid + noise. Anchored frequency lets spectral
# tests assert peak location.
make_synthetic_sine <- function(n = 512L, fs = 256L,
                                freq_hz = 10, noise_sd = 0.2,
                                seed = 1L) {
  set.seed(seed)
  t <- seq(0, (n - 1L) / fs, length.out = n)
  sin(2 * pi * freq_hz * t) + stats::rnorm(n, sd = noise_sd)
}

# Brownian motion (cumulative-sum of Gaussian increments). Hurst H -> 0.5,
# DFA alpha -> 1.5. Used for fractal / scaling tests with a known truth.
make_synthetic_brownian <- function(n = 2048L, seed = 1L) {
  set.seed(seed)
  cumsum(stats::rnorm(n))
}

# Pink (1/f) noise via cumulative integration of white noise; DFA alpha
# should land near ~1.0 with H around the persistent boundary.
make_synthetic_pink <- function(n = 2048L, seed = 1L) {
  set.seed(seed)
  white <- stats::rnorm(n)
  Re(stats::fft(stats::fft(white) /
                  sqrt(seq_len(n)), inverse = TRUE)) / n
}

# Crude ECG-like signal: 1.2 Hz dominant rate ~ 72 bpm, sampled at 250 Hz.
# Plenty of R-peaks for ecgdet to find but not so noisy that the test is
# flaky.
make_synthetic_ecg <- function(duration_s = 4, fs = 250L, seed = 1L) {
  set.seed(seed)
  t <- seq(0, duration_s, by = 1 / fs)
  sin(2 * pi * 1.2 * t) + 0.1 * stats::rnorm(length(t))
}

# RR-interval series in ms (typical resting adult HRV: mean ~800 ms,
# sd ~20 ms). Enough length (n=200) for the frequency-domain estimator.
make_synthetic_rr <- function(n = 200L, mean_ms = 800, sd_ms = 20,
                              seed = 1L) {
  set.seed(seed)
  mean_ms + cumsum(stats::rnorm(n, sd = sd_ms))
}

# PCG-like signal: white-noise carrier with two amplitude-modulated heart
# sounds per second; envelope methods should detect ~2 S1/S2 cycles per s.
make_synthetic_pcg <- function(duration_s = 2, fs = 2000L,
                               heart_rate_hz = 1.5, seed = 1L) {
  set.seed(seed)
  n <- as.integer(duration_s * fs)
  t <- seq(0, duration_s, length.out = n)
  carrier <- stats::rnorm(n, sd = 0.3)
  s1 <- exp(-((t %% (1 / heart_rate_hz) - 0.1)^2) / (2 * 0.02^2))
  s2 <- exp(-((t %% (1 / heart_rate_hz) - 0.4)^2) / (2 * 0.02^2))
  (s1 + 0.6 * s2) * carrier * 5
}
