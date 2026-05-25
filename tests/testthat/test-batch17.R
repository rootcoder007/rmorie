# SPDX-License-Identifier: AGPL-3.0-or-later
# Batch 17 tests: rgapn rgarb rgcoh rgcrl rgdfa rgeeg rgemg rgenv rgfir rghfd rghrv rgiir rglyp rgpsd rgqrs

test_that("rgapn returns named list with documented fields", {
  set.seed(1)
  r <- rgapn(rnorm(80), m = 2L)
  expect_type(r, "list")
  expect_named(r, c("ApEn", "phi_m", "phi_m1", "m", "r", "n"))
  expect_true(is.finite(r$ApEn))
  expect_equal(r$n, 80L)
  expect_equal(r$m, 2L)
  expect_true(is.finite(r$r) && r$r > 0)
})

test_that("rgapn honours explicit tolerance r and template length m", {
  set.seed(2)
  x <- rnorm(60)
  r <- rgapn(x, m = 3L, r = 0.5)
  expect_equal(r$m, 3L)
  expect_equal(r$r, 0.5)
  expect_true(is.finite(r$phi_m) && is.finite(r$phi_m1))
})

test_that("rgapn errors on series too short for template", {
  expect_error(rgapn(c(1, 2, 3), m = 2L), "m \\+ 1")
})

test_that("morie_rangayyan_approximate_entropy alias is identical to rgapn", {
  expect_identical(morie_rangayyan_approximate_entropy, rgapn)
})

test_that("rgarb returns AR coefficients of requested order", {
  set.seed(3)
  r <- rgarb(rnorm(300), order = 4L)
  expect_type(r, "list")
  expect_named(r, c("ar_coeffs", "variance", "order", "reflection"))
  expect_length(r$ar_coeffs, 4L)
  expect_length(r$reflection, 4L)
  expect_equal(r$order, 4L)
  expect_true(is.finite(r$variance) && r$variance >= 0)
})

test_that("rgarb default order path runs", {
  set.seed(4)
  r <- rgarb(rnorm(200))
  expect_length(r$ar_coeffs, 10L)
  expect_true(all(is.finite(r$ar_coeffs)))
})

test_that("rgarb reflection coefficients are bounded for a stable model", {
  set.seed(5)
  r <- rgarb(rnorm(250), order = 6L)
  expect_true(all(abs(r$reflection) <= 1 + 1e-8))
})

test_that("rgarb errors on invalid order", {
  expect_error(rgarb(rnorm(20), order = 0L), "order")
  expect_error(rgarb(rnorm(20), order = 20L), "order")
})

test_that("morie_rangayyan_ar_burg alias is identical to rgarb", {
  expect_identical(morie_rangayyan_ar_burg, rgarb)
})

test_that("rgcoh returns morie_coherence bounded in [0, 1]", {
  set.seed(6)
  n <- 512
  tt <- seq(0, 5, length.out = n)
  a <- sin(2 * pi * 8 * tt)
  b <- a + 0.2 * rnorm(n)
  r <- rgcoh(a, b, fs = 100)
  expect_type(r, "list")
  expect_named(r, c(
    "freqs", "morie_coherence", "mean_coherence",
    "peak_freq", "peak_coherence"
  ))
  ok <- is.finite(r$morie_coherence)
  expect_true(all(r$morie_coherence[ok] >= -1e-8 & r$morie_coherence[ok] <= 1 + 1e-8))
  expect_length(r$freqs, length(r$morie_coherence))
})

test_that("rgcoh honours explicit nperseg", {
  set.seed(7)
  n <- 400
  x <- rnorm(n)
  y <- rnorm(n)
  r <- rgcoh(x, y, fs = 50, nperseg = 128L)
  expect_equal(length(r$freqs), 128L %/% 2L + 1L)
  expect_true(is.finite(r$mean_coherence))
})

test_that("rgcoh errors on unequal length inputs", {
  expect_error(rgcoh(1:10, 1:8), "equal length")
})

test_that("morie_rangayyan_coherence alias is identical to rgcoh", {
  expect_identical(morie_rangayyan_coherence, rgcoh)
})

test_that("rgcrl returns D2 and scaling vectors", {
  set.seed(8)
  r <- rgcrl(rnorm(200), m = 3L, tau = 1L, n_r = 15L)
  expect_type(r, "list")
  expect_named(r, c("D2", "log_r", "log_C", "m", "tau"))
  expect_equal(r$m, 3L)
  expect_equal(r$tau, 1L)
  expect_equal(length(r$log_r), length(r$log_C))
})

test_that("rgcrl default arguments path runs", {
  set.seed(9)
  r <- rgcrl(rnorm(150))
  expect_true(is.na(r$D2) || is.finite(r$D2))
})

test_that("rgcrl errors on series too short for embedding", {
  expect_error(rgcrl(rnorm(8), m = 3L, tau = 2L), "too short")
})

test_that("morie_rangayyan_correlation_dimension alias is identical to rgcrl", {
  expect_identical(morie_rangayyan_correlation_dimension, rgcrl)
})

test_that("rgdfa returns alpha exponent and scaling vectors", {
  set.seed(10)
  r <- rgdfa(rnorm(400))
  expect_type(r, "list")
  expect_named(r, c("alpha", "scales", "fluct", "log_scales", "log_F"))
  expect_true(is.finite(r$alpha))
  expect_equal(length(r$log_scales), length(r$log_F))
})

test_that("rgdfa honours explicit scales and order", {
  set.seed(11)
  r <- rgdfa(rnorm(300), scales = c(8L, 16L, 32L, 64L), order = 2L)
  expect_true(all(c(8L, 16L, 32L, 64L) %in% r$scales))
  expect_true(is.finite(r$alpha))
})

test_that("rgdfa errors on series shorter than 32 samples", {
  expect_error(rgdfa(rnorm(20)), "32 samples")
})

test_that("morie_rangayyan_dfa alias is identical to rgdfa", {
  expect_identical(morie_rangayyan_dfa, rgdfa)
})

test_that("rgeeg returns absolute and relative band power", {
  set.seed(12)
  fs <- 128
  tt <- seq(0, 8, length.out = 1024)
  x <- sin(2 * pi * 10 * tt) + 0.3 * rnorm(length(tt))
  r <- rgeeg(x, fs = fs)
  expect_type(r, "list")
  expect_named(r, c("absolute", "relative", "total_power", "freqs", "psd"))
  expect_named(r$absolute, c("delta", "theta", "alpha", "beta", "gamma"))
  expect_named(r$relative, c("delta", "theta", "alpha", "beta", "gamma"))
  expect_true(all(r$absolute >= 0))
  expect_true(sum(r$relative) <= 1 + 1e-6)
  expect_true(is.finite(r$total_power) && r$total_power >= 0)
})

test_that("rgeeg honours custom bands and nperseg", {
  set.seed(13)
  fs <- 100
  x <- rnorm(800)
  bands <- list(low = c(1, 10), high = c(10, 40))
  r <- rgeeg(x, fs = fs, bands = bands, nperseg = 128L)
  expect_named(r$absolute, c("low", "high"))
  expect_named(r$relative, c("low", "high"))
})

test_that("morie_rangayyan_eeg_bands alias is identical to rgeeg", {
  expect_identical(morie_rangayyan_eeg_bands, rgeeg)
})

test_that("rgemg returns RMS envelope of length(x)", {
  set.seed(14)
  x <- rnorm(300)
  r <- rgemg(x, window = 32L)
  expect_type(r, "list")
  expect_named(r, c("rms", "window", "fs", "mean_rms"))
  expect_length(r$rms, 300L)
  expect_equal(r$window, 32L)
  expect_true(is.finite(r$mean_rms) && r$mean_rms >= 0)
  expect_true(all(r$rms >= 0))
})

test_that("rgemg default window and fs reporting", {
  set.seed(15)
  r <- rgemg(rnorm(200))
  expect_equal(r$window, 64L)
  expect_equal(r$fs, 1.0)
})

test_that("rgemg errors on window < 1", {
  expect_error(rgemg(rnorm(50), window = 0L), "window")
})

test_that("morie_rangayyan_emg_rms alias is identical to rgemg", {
  expect_identical(morie_rangayyan_emg_rms, rgemg)
})

test_that("rgenv returns envelope and instantaneous quantities (even N)", {
  tt <- seq(0, 1, length.out = 200)
  x <- cos(2 * pi * 5 * tt) * (1 + 0.3 * cos(2 * pi * 0.5 * tt))
  r <- rgenv(x)
  expect_type(r, "list")
  expect_named(r, c(
    "envelope", "analytic", "instantaneous_phase",
    "instantaneous_freq"
  ))
  expect_length(r$envelope, 200L)
  expect_length(r$instantaneous_phase, 200L)
  expect_length(r$instantaneous_freq, 199L)
  expect_true(all(r$envelope >= 0))
  expect_true(is.complex(r$analytic))
})

test_that("rgenv handles odd-length input", {
  set.seed(16)
  x <- rnorm(101)
  r <- rgenv(x)
  expect_length(r$envelope, 101L)
  expect_true(all(is.finite(r$envelope)))
})

test_that("morie_rangayyan_envelope alias is identical to rgenv", {
  expect_identical(morie_rangayyan_envelope, rgenv)
})

test_that("rgfir filters a signal when 'signal' is available", {
  set.seed(17)
  tt <- seq(0, 1, length.out = 400)
  x <- sin(2 * pi * 5 * tt) + 0.5 * sin(2 * pi * 60 * tt)
  r <- rgfir(x, cutoff = 10, order = 51L, fs = 200)
  expect_type(r, "list")
  expect_named(r, c("signal", "taps", "order", "cutoff", "fs", "window"))
  expect_length(r$signal, length(x))
  expect_equal(r$order, 51L)
  expect_true(all(is.finite(r$signal)))
})

test_that("rgfir coerces even order to odd and clamps small orders", {
  set.seed(18)
  x <- sin(2 * pi * 5 * seq(0, 1, length.out = 300))
  r <- rgfir(x, cutoff = 10, order = 50L, fs = 200)
  expect_equal(r$order, 51L)
  r2 <- rgfir(x, cutoff = 10, order = 1L, fs = 200)
  expect_equal(r2$order, 3L)
})

test_that("rgfir supports alternative windows and short-signal path", {
  set.seed(19)
  x <- sin(2 * pi * 3 * seq(0, 1, length.out = 90))
  for (w in c("hann", "blackman", "rectangular", "unknown")) {
    r <- rgfir(x, cutoff = 8, order = 21L, fs = 100, window = w)
    expect_length(r$signal, length(x))
  }
})

test_that("morie_rangayyan_fir_filter alias is identical to rgfir", {
  expect_identical(morie_rangayyan_fir_filter, rgfir)
})

test_that("rghfd returns HFD and scaling vectors", {
  set.seed(20)
  r <- rghfd(rnorm(400), kmax = 8L)
  expect_type(r, "list")
  expect_named(r, c("HFD", "intercept", "log_L", "log_inv_k", "kmax"))
  expect_true(is.finite(r$HFD))
  expect_equal(length(r$log_L), length(r$log_inv_k))
})

test_that("rghfd default kmax path runs", {
  set.seed(21)
  r <- rghfd(rnorm(300))
  expect_true(is.finite(r$HFD))
})

test_that("rghfd errors on too-short input or tiny kmax", {
  expect_error(rghfd(c(1, 2, 3), kmax = 8L), "kmax")
  expect_error(rghfd(rnorm(50), kmax = 1L), "kmax")
})

test_that("morie_rangayyan_higuchi_fd alias is identical to rghfd", {
  expect_identical(morie_rangayyan_higuchi_fd, rghfd)
})

test_that("rghrv returns documented time-domain indices", {
  set.seed(22)
  rr <- 800 + rnorm(200, sd = 40)
  r <- rghrv(rr)
  expect_type(r, "list")
  expect_named(r, c(
    "meanNN", "SDNN", "RMSSD", "pNN50",
    "heart_rate_bpm", "n"
  ))
  expect_equal(r$n, 200L)
  expect_true(is.finite(r$meanNN) && r$meanNN > 0)
  expect_true(is.finite(r$SDNN) && r$SDNN >= 0)
  expect_true(is.finite(r$RMSSD) && r$RMSSD >= 0)
  expect_true(r$pNN50 >= 0 && r$pNN50 <= 100)
  expect_true(is.finite(r$heart_rate_bpm))
})

test_that("rghrv handles minimal length-2 input", {
  r <- rghrv(c(800, 850))
  expect_equal(r$n, 2L)
  expect_true(is.finite(r$RMSSD))
})

test_that("rghrv errors on fewer than 2 intervals", {
  expect_error(rghrv(800), "2 RR")
})

test_that("morie_rangayyan_hrv alias is identical to rghrv", {
  expect_identical(morie_rangayyan_hrv, rghrv)
})

test_that("rgiir lowpass filters a signal when 'signal' is available", {
  set.seed(23)
  tt <- seq(0, 1, length.out = 500)
  x <- sin(2 * pi * 5 * tt) + 0.5 * sin(2 * pi * 40 * tt)
  r <- rgiir(x, cutoff = 10, order = 4L, fs = 500, btype = "low")
  expect_type(r, "list")
  expect_named(r, c("signal", "order", "cutoff", "fs", "btype"))
  expect_length(r$signal, length(x))
  expect_equal(r$btype, "low")
  expect_true(all(is.finite(r$signal)))
})

test_that("rgiir supports highpass and bandpass btypes", {
  set.seed(24)
  tt <- seq(0, 1, length.out = 500)
  x <- sin(2 * pi * 5 * tt) + 0.5 * sin(2 * pi * 40 * tt)
  rh <- rgiir(x, cutoff = 20, order = 3L, fs = 500, btype = "high")
  expect_equal(rh$btype, "high")
  rb <- rgiir(x, cutoff = c(8, 30), order = 2L, fs = 500, btype = "pass")
  expect_equal(rb$btype, "pass")
  expect_length(rb$signal, length(x))
})

test_that("rgiir rejects an invalid btype", {
  expect_error(rgiir(rnorm(100), cutoff = 10, fs = 100, btype = "bogus"))
})

test_that("morie_rangayyan_iir_filter alias is identical to rgiir", {
  expect_identical(morie_rangayyan_iir_filter, rgiir)
})

test_that("rglyp returns lyapunov exponent and divergence curve", {
  set.seed(25)
  r <- rglyp(rnorm(200), m = 3L, tau = 1L, max_t = 20L)
  expect_type(r, "list")
  expect_named(r, c("lyapunov", "divergence_curve", "t"))
  expect_length(r$t, 20L)
  expect_length(r$divergence_curve, 20L)
  expect_true(is.na(r$lyapunov) || is.finite(r$lyapunov))
})

test_that("rglyp default max_t path runs", {
  set.seed(26)
  r <- rglyp(rnorm(150))
  expect_true(is.na(r$lyapunov) || is.finite(r$lyapunov))
  expect_equal(length(r$t), length(r$divergence_curve))
})

test_that("rglyp errors on series too short for embedding", {
  expect_error(rglyp(rnorm(8), m = 3L, tau = 2L), "too short")
})

test_that("morie_rangayyan_lyapunov alias is identical to rglyp", {
  expect_identical(morie_rangayyan_lyapunov, rglyp)
})

test_that("rgpsd returns one-sided PSD with documented fields", {
  set.seed(27)
  fs <- 100
  tt <- seq(0, 10, length.out = 1000)
  x <- sin(2 * pi * 10 * tt)
  r <- rgpsd(x, fs = fs, nperseg = 256L)
  expect_type(r, "list")
  expect_named(r, c(
    "freqs", "psd", "fs", "nperseg",
    "peak_freq", "total_power"
  ))
  expect_equal(r$nperseg, 256L)
  expect_equal(length(r$freqs), 256L %/% 2L + 1L)
  expect_equal(length(r$psd), length(r$freqs))
  expect_true(all(r$psd >= 0))
  expect_true(is.finite(r$total_power) && r$total_power >= 0)
})

test_that("rgpsd supports alternative windows and default nperseg", {
  set.seed(28)
  x <- rnorm(600)
  for (w in c("hann", "hamming", "boxcar", "unknown")) {
    r <- rgpsd(x, fs = 50, window = w)
    expect_true(all(is.finite(r$psd)))
    expect_true(is.finite(r$peak_freq))
  }
})

test_that("morie_rangayyan_psd alias is identical to rgpsd", {
  expect_identical(morie_rangayyan_psd, rgpsd)
})

test_that("rgqrs detects R-peaks on a synthetic ECG", {
  set.seed(29)
  fs <- 360
  tt <- seq(0, 5, length.out = 5 * fs)
  ecg <- rowSums(vapply(
    seq(0.5, 4.5, by = 1.0),
    function(tk) exp(-((tt - tk) * 30)^2), numeric(length(tt))
  ))
  r <- rgqrs(ecg, fs = fs)
  expect_type(r, "list")
  expect_named(r, c(
    "r_peaks", "rr_intervals_ms", "heart_rate_bpm",
    "integrated", "fs"
  ))
  expect_equal(r$fs, fs)
  expect_true(is.numeric(r$r_peaks))
  expect_length(r$integrated, length(ecg))
  expect_true(all(r$rr_intervals_ms >= 0))
})

test_that("rgqrs default fs argument path runs", {
  set.seed(30)
  tt <- seq(0, 3, length.out = 3 * 360)
  ecg <- rowSums(vapply(
    seq(0.5, 2.5, by = 1.0),
    function(tk) exp(-((tt - tk) * 30)^2), numeric(length(tt))
  ))
  r <- rgqrs(ecg)
  expect_equal(r$fs, 360.0)
  expect_true(is.na(r$heart_rate_bpm) || is.finite(r$heart_rate_bpm))
})

test_that("morie_rangayyan_qrs_detect alias is identical to rgqrs", {
  expect_identical(morie_rangayyan_qrs_detect, rgqrs)
})
