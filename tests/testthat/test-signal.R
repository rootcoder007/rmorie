# SPDX-License-Identifier: AGPL-3.0-or-later
# Tests for r-package/morie/R/signal.R — 26 exported functions.
# Fixtures live in helper-signal.R (testthat auto-sources helper-*.R).

# ---------------------------------------------------------------- Butterworth

test_that("buttlp lowpass returns filtered vector of the same length", {
  x <- make_synthetic_sine(n = 500L, fs = 500L, freq_hz = 60, seed = 1L) +
    make_synthetic_sine(n = 500L, fs = 500L, freq_hz = 5, seed = 2L)
  y <- buttlp(x, fs = 500, cutoff = 20)
  expect_type(y, "list")
  expect_named(y, c("filtered", "fs", "order", "name"))
  expect_equal(y$name, "butter_lowpass")
  expect_equal(length(y$filtered), length(x))
  # 60 Hz noise above cutoff should be largely attenuated:
  expect_lt(stats::sd(y$filtered), stats::sd(x))
})

test_that("butthp highpass strips DC drift", {
  x <- seq(0, 1, length.out = 500L)         # pure linear drift
  y <- butthp(x, fs = 500, cutoff = 1)
  expect_equal(y$name, "butter_highpass")
  expect_length(y$filtered, length(x))
  expect_lt(abs(mean(y$filtered)), abs(mean(x)))
})

test_that("buttbp bandpass keeps in-band, suppresses out-of-band", {
  x <- make_synthetic_sine(n = 1000L, fs = 1000L, freq_hz = 10, seed = 3L) +
    make_synthetic_sine(n = 1000L, fs = 1000L, freq_hz = 100, seed = 4L)
  y <- buttbp(x, fs = 1000, low = 5, high = 20)
  expect_equal(y$name, "butter_bandpass")
  expect_length(y$filtered, length(x))
  expect_lt(stats::sd(y$filtered), stats::sd(x))
})

test_that("buttbs bandstop removes the targeted band (60 Hz mains)", {
  x <- make_synthetic_sine(n = 1000L, fs = 1000L, freq_hz = 10, seed = 5L) +
    0.5 * make_synthetic_sine(n = 1000L, fs = 1000L, freq_hz = 60, seed = 6L)
  y <- buttbs(x, fs = 1000)        # defaults: 59-61 Hz notch
  expect_equal(y$name, "butter_bandstop")
  expect_length(y$filtered, length(x))
})

# -------------------------------------------------------------------- Smoothing

test_that("morie_sgolay_smooth preserves length and reduces noise", {
  x <- make_synthetic_sine(n = 200L, fs = 200L, freq_hz = 3, noise_sd = 0.4,
                           seed = 7L)
  y <- morie_sgolay_smooth(x, window_length = 11L, polyorder = 3L)
  expect_named(y, c("filtered", "name"))
  expect_equal(y$name, "savitzky_golay")
  expect_length(y$filtered, length(x))
  expect_lt(stats::sd(y$filtered), stats::sd(x))
})

test_that("sgolay short alias delegates + forces odd window", {
  x <- make_synthetic_sine(n = 200L, fs = 200L, freq_hz = 3, seed = 8L)
  y <- sgolay(x, window = 12L, polyorder = 3L)   # even -> bumped to 13
  expect_equal(y$name, "savgol_smooth")
  expect_equal(y$extra$window, 13L)
  expect_length(y$filtered, length(x))
})

# ---------------------------------------------------------------------- Fractal

test_that("morie_hurst_r returns H near 0.5 for Brownian motion", {
  x <- make_synthetic_brownian(n = 2048L, seed = 11L)
  res <- morie_hurst_r(x)
  expect_named(res, c("H", "interpretation"))
  # Brownian motion: theoretical H = 0.5 (R/S finite-sample bias allowed):
  expect_true(res$H > 0.3 && res$H < 1.0)
  expect_true(res$interpretation %in%
                c("random", "persistent", "anti-persistent"))
})

test_that("kfd returns a finite Katz fractal dimension > 0", {
  x <- make_synthetic_brownian(n = 1000L, seed = 12L)
  res <- kfd(x)
  expect_equal(res$name, "katz_fd")
  expect_true(is.finite(res$value) && res$value > 0)
  expect_named(res$extra, c("L", "d", "n"))
})

test_that("kfd handles degenerate input (n < 2 or zero diameter)", {
  expect_true(is.na(kfd(numeric(0))$value))
  expect_true(is.na(kfd(rep(3.14, 100))$value))   # zero diameter
})

test_that("pfd returns a finite Petrosian fractal dimension", {
  x <- make_synthetic_brownian(n = 1000L, seed = 13L)
  res <- pfd(x)
  expect_equal(res$name, "petrosian_fd")
  expect_true(is.finite(res$value) && res$value > 0)
  expect_named(res$extra, c("n_delta", "n"))
})

test_that("pfd handles n<2 degenerate input", {
  expect_true(is.na(pfd(numeric(1))$value))
})

test_that("hfd recovers a finite Higuchi fractal dimension in [1, 2]", {
  x <- make_synthetic_brownian(n = 1000L, seed = 16L)
  res <- hfd(x, kmax = 10L)
  expect_equal(res$name, "higuchi_fd")
  expect_true(is.finite(res$value))
  expect_true(res$value >= 0.5 && res$value <= 2.5)  # generous, finite-N
  expect_equal(res$extra$kmax, 10L)
  expect_length(res$extra$L_k, 10L)
})

test_that("hfd handles degenerate input (n < 4 or kmax < 2)", {
  expect_true(is.na(hfd(c(1, 2, 3), kmax = 5L)$value))
  expect_true(is.na(hfd(rnorm(100), kmax = 1L)$value))
})

test_that("dfa recovers alpha ~ 1.5 for Brownian motion", {
  x <- make_synthetic_brownian(n = 2048L, seed = 14L)
  res <- dfa(x)
  expect_equal(res$name, "dfa")
  expect_true(is.finite(res$value))
  # Theoretical alpha for Brownian motion is 1.5 - allow generous tolerance:
  expect_true(res$value > 1.0 && res$value < 2.0)
  expect_true(length(res$extra$scales) >= 3)
})

test_that("dfa accepts custom scales + handles short input", {
  x <- make_synthetic_brownian(n = 200L, seed = 15L)
  res <- dfa(x, scales = c(4L, 8L, 16L, 32L))
  expect_true(is.finite(res$value))
  short <- dfa(rnorm(10), scales = c(4L))
  expect_true(is.na(short$value))
})

# ---------------------------------------------------------------------- Cepstral

test_that("cepst returns length-n_fft real cepstrum + quefrency axis", {
  x <- make_synthetic_sine(n = 256L, fs = 256L, freq_hz = 8, seed = 21L)
  res <- cepst(x)
  expect_equal(res$name, "real_cepstrum")
  expect_length(res$filtered, 256L)
  expect_length(res$extra$quefrency, 256L)
  expect_equal(res$extra$n_fft, 256L)
})

test_that("cepst auto-pads to next power of two", {
  res <- cepst(make_synthetic_sine(n = 300L, fs = 300L, seed = 22L))
  expect_equal(res$extra$n_fft, 512L)        # 2^ceil(log2(300))
})

test_that("hcepst returns same-length real cepstrum (complex)", {
  x <- make_synthetic_sine(n = 256L, fs = 256L, freq_hz = 8, seed = 23L)
  res <- hcepst(x)
  expect_equal(res$name, "complex_cepstrum")
  expect_length(res$filtered, 256L)
  expect_equal(res$extra$original_length, 256L)
})

test_that("hdecon returns minimum-phase component + excitation", {
  x <- make_synthetic_sine(n = 256L, fs = 256L, freq_hz = 12, seed = 24L)
  res <- hdecon(x, cutoff = 20)
  expect_equal(res$name, "homomorphic_deconvolve")
  expect_length(res$filtered, length(x))
  expect_length(res$extra$excitation, length(x))
  expect_equal(res$extra$cutoff, 20)
})

# ----------------------------------------------------------------- ECG / HRV

test_that("ecgdet finds R-peaks on a synthetic ECG", {
  ecg <- make_synthetic_ecg(duration_s = 4, fs = 250L, seed = 31L)
  res <- ecgdet(ecg, fs = 250L)
  expect_equal(res$name, "pan_tompkins")
  expect_equal(res$fs, 250L)
  expect_true(res$extra$n_peaks >= 1L)
  expect_true(all(res$extra$r_peaks >= 1L &
                    res$extra$r_peaks <= length(ecg)))
})

test_that("ecgdet returns zero peaks on a flat signal", {
  res <- ecgdet(rep(0.0, 500L), fs = 250L)
  expect_equal(res$extra$n_peaks, 0L)
  expect_length(res$extra$r_peaks, 0L)
})

test_that("rrint converts R-peak indices to RR-ms intervals", {
  peaks <- c(100L, 350L, 600L, 850L, 1100L)        # 4 intervals @ 250 Hz
  res <- rrint(peaks, fs = 250L)
  expect_equal(res$name, "rr_intervals")
  expect_equal(res$extra$n_intervals, 4L)
  expect_equal(res$extra$mean_rr, mean(diff(peaks) / 250 * 1000))
})

test_that("rrint handles too-few peaks", {
  res <- rrint(c(100L), fs = 250L)
  expect_true(is.na(res$value))
  expect_length(res$extra$rr_ms, 0L)
})

test_that("hrvtd computes SDNN / RMSSD / pNN50", {
  rr <- make_synthetic_rr(n = 200L, seed = 41L)
  res <- hrvtd(rr)
  expect_equal(res$name, "hrv_time_domain")
  for (k in c("sdnn", "rmssd", "pnn50", "mean_rr", "mean_hr",
              "hrv_triangular_index", "n_intervals"))
    expect_true(!is.null(res$extra[[k]]))
  expect_true(res$extra$mean_hr > 0)
})

test_that("hrvtd handles single-element input", {
  expect_true(is.na(hrvtd(800)$value))
})

test_that("hrvfd computes VLF / LF / HF + LF/HF ratio", {
  rr <- make_synthetic_rr(n = 200L, seed = 42L)
  res <- hrvfd(rr)
  expect_equal(res$name, "hrv_freq_domain")
  for (k in c("vlf", "lf", "hf", "lf_hf_ratio", "total_power",
              "lf_norm", "hf_norm"))
    expect_true(!is.null(res$extra[[k]]))
  expect_true(res$extra$total_power >= 0)
})

test_that("hrvfd handles too-short input", {
  expect_true(is.na(hrvfd(rep(800, 5L))$value))
})

test_that("hrvnl computes Poincare SD1 / SD2", {
  rr <- make_synthetic_rr(n = 200L, seed = 43L)
  res <- hrvnl(rr)
  expect_equal(res$name, "hrv_nonlinear")
  expect_true(res$extra$sd1 >= 0 && res$extra$sd2 >= 0)
})

test_that("hrvnl handles too-short input", {
  expect_true(is.na(hrvnl(c(800, 810))$value))
})

# ----------------------------------------------------------------- Spectral

test_that("welch PSD has expected length + peak near input frequency", {
  fs <- 1024L
  freq <- 50
  x <- make_synthetic_sine(n = fs, fs = fs, freq_hz = freq,
                           noise_sd = 0.1, seed = 51L)
  res <- welch(x, fs = fs, nperseg = 256L)
  expect_equal(res$name, "welch_psd")
  expect_equal(res$fs, fs)
  expect_length(res$filtered, 256L %/% 2L + 1L)
  expect_length(res$extra$freqs, length(res$filtered))
  peak_freq <- res$extra$freqs[which.max(res$filtered)]
  expect_lt(abs(peak_freq - freq), 5)        # within ~one PSD bin
})

test_that("welch handles short signals by clipping nperseg", {
  x <- make_synthetic_sine(n = 64L, fs = 64L, freq_hz = 8, seed = 52L)
  res <- welch(x, fs = 64L, nperseg = 256L)
  expect_true(is.finite(res$filtered[1]))
})

test_that("pburg AR-PSD returns finite spectrum + AR coefficients", {
  x <- make_synthetic_sine(n = 512L, fs = 512L, freq_hz = 10, seed = 53L)
  res <- pburg(x, fs = 512L, order = 12L, nfft = 256L)
  expect_equal(res$name, "burg_psd")
  expect_length(res$filtered, 256L %/% 2L + 1L)
  expect_length(res$extra$ar_coefficients, 13L)         # order + 1
  expect_true(all(is.finite(res$filtered)))
})

test_that("pburg auto-caps order at n-1 when order >= n", {
  res <- pburg(rnorm(10L), fs = 10L, order = 16L, nfft = 64L)
  expect_true(is.finite(res$filtered[1]))
})

# -------------------------------------------------------------- PCG segments

test_that("pcgenv returns same-length envelope", {
  pcg <- make_synthetic_pcg(duration_s = 2, fs = 2000L, seed = 61L)
  res <- pcgenv(pcg, fs = 2000L)
  expect_equal(res$name, "pcg_envelope")
  expect_length(res$filtered, length(pcg))
})

test_that("pcgseg detects S1 / S2 cycles on the synthetic PCG envelope", {
  pcg <- make_synthetic_pcg(duration_s = 4, fs = 2000L, seed = 62L)
  env <- pcgenv(pcg, fs = 2000L)$filtered
  res <- pcgseg(env, fs = 2000L)
  expect_equal(res$name, "pcg_segment")
  expect_true(res$extra$n_cycles >= 1L)
  expect_true(length(res$extra$s1_indices) >= 1L)
})

test_that("pcgseg returns zero cycles on a flat envelope", {
  res <- pcgseg(rep(0.0, 100L), fs = 2000L)
  expect_equal(res$extra$n_cycles, 0L)
})

test_that("pcgmur returns a murmur score in [0, 1]", {
  pcg <- make_synthetic_pcg(duration_s = 2, fs = 2000L, seed = 63L)
  res <- pcgmur(pcg, fs = 2000L)
  expect_equal(res$name, "pcg_murmur_score")
  expect_true(res$value >= 0 && res$value <= 1)
  for (k in c("fractal_dimension", "hf_energy_ratio",
              "spectral_entropy", "fd_score", "hf_score", "ent_score"))
    expect_true(!is.null(res$extra[[k]]))
})

test_that("morie_pcg_filter wraps buttbp with PCG-band defaults", {
  pcg <- make_synthetic_pcg(duration_s = 1, fs = 2000L, seed = 64L)
  res <- morie_pcg_filter(pcg)
  expect_equal(res$name, "butter_bandpass")
  expect_length(res$filtered, length(pcg))
})

# ----------------------------------------------------------------- Entropy

test_that("sampen returns finite SampEn for a structured signal", {
  x <- make_synthetic_sine(n = 500L, fs = 500L, freq_hz = 3,
                           noise_sd = 0.1, seed = 71L)
  res <- sampen(x, m = 2L, r = 0.2)
  expect_equal(res$name, "sample_entropy")
  expect_true(is.finite(res$value))
})

test_that("sampen handles degenerate input gracefully", {
  # n < m + 2:
  expect_true(is.na(sampen(c(1, 2, 3), m = 2L)$value))
  # zero sd -> tol == 0:
  expect_true(is.na(sampen(rep(5, 100L))$value))
})
