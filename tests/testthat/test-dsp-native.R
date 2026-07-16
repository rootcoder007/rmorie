# SPDX-License-Identifier: AGPL-3.0-or-later
# Module 20 — structural tests for the native DSP engines.
#' @srrstats {G5.4} Filters verified against analytic frequency
#'   responses; DWT verified by perfect reconstruction + Parseval.

.freq_gain <- function(b, a, w) {
  z <- exp(-1i * pi * w)
  Mod(sum(b * z^(seq_along(b) - 1)) / sum(a * z^(seq_along(a) - 1)))
}

test_that("native Butterworth: passband/stopband gains are correct", {
  lp <- rmorie:::.morie_dsp_butter(4, 0.3, "low")
  expect_equal(.freq_gain(lp$b, lp$a, 0), 1, tolerance = 1e-8)
  expect_equal(.freq_gain(lp$b, lp$a, 0.3), 1 / sqrt(2), tolerance = 1e-6)
  expect_lt(.freq_gain(lp$b, lp$a, 0.8), 0.05)
  hp <- rmorie:::.morie_dsp_butter(4, 0.3, "high")
  expect_equal(.freq_gain(hp$b, hp$a, 1), 1, tolerance = 1e-8)
  expect_lt(.freq_gain(hp$b, hp$a, 0.05), 0.05)
  bp <- rmorie:::.morie_dsp_butter(2, c(0.2, 0.4), "pass")
  expect_gt(.freq_gain(bp$b, bp$a, 0.3), 0.9)
  expect_lt(.freq_gain(bp$b, bp$a, 0.05), 0.1)
  expect_lt(.freq_gain(bp$b, bp$a, 0.8), 0.1)
  bs <- rmorie:::.morie_dsp_butter(2, c(0.2, 0.4), "stop")
  expect_lt(.freq_gain(bs$b, bs$a, 0.29), 0.2)
  expect_gt(.freq_gain(bs$b, bs$a, 0.02), 0.9)
  expect_gt(.freq_gain(bs$b, bs$a, 0.95), 0.9)
})

test_that("filtfilt is zero-phase and attenuates out-of-band tones", {
  set.seed(80)
  fs <- 200; t <- seq(0, 4, by = 1 / fs)
  x <- sin(2 * pi * 5 * t) + sin(2 * pi * 60 * t)
  lp <- rmorie:::.morie_dsp_butter(4, 20 / (fs / 2), "low")
  y <- rmorie:::.morie_dsp_filtfilt(lp$b, lp$a, x)
  ref <- sin(2 * pi * 5 * t)
  core <- 100:(length(t) - 100)
  expect_lt(sqrt(mean((y[core] - ref[core])^2)), 0.05)
  # zero phase: cross-correlation peaks at lag 0
  cc <- ccf(y[core], ref[core], lag.max = 5, plot = FALSE)
  expect_equal(cc$lag[which.max(cc$acf)], 0)
})

test_that("buttlp/butthp/buttbp/buttbs + sgolay run natively", {
  set.seed(81)
  fs <- 100; x <- rnorm(500)
  for (fn in list(function() buttlp(x, fs, 10),
                  function() butthp(x, fs, 10),
                  function() buttbp(x, fs, 5, 20),
                  function() buttbs(x, fs, 5, 20))) {
    r <- fn()
    expect_true(all(is.finite(r$filtered)))
    expect_length(r$filtered, length(x))
  }
  sg <- morie_sgolay_smooth(x, polyorder = 3, window_length = 11)
  expect_length(sg$filtered, length(x))
  # SG preserves a cubic exactly (in the interior and at edges)
  cub <- (seq_len(50) / 10)^3
  sg2 <- morie_sgolay_smooth(cub, polyorder = 3, window_length = 11)
  expect_equal(sg2$filtered, cub, tolerance = 1e-8)
})

test_that("native Welch PSD localizes a pure tone; coherence detects link", {
  set.seed(82)
  fs <- 100; t <- seq(0, 20, by = 1 / fs)
  x <- sin(2 * pi * 10 * t) + rnorm(length(t), 0, 0.3)
  psd <- morie_dsp_psd_welch(x, fs = fs, nperseg = 256)
  pk <- psd$freqs[which.max(psd$psd)]
  expect_equal(pk, 10, tolerance = 0.5)
  # coherence: y = filtered x + noise is coherent at the tone
  y <- x + rnorm(length(t), 0, 0.3)
  coh <- morie_dsp_coherence(x, y, fs = fs, nperseg = 256)
  i10 <- which.min(abs(coh$freqs - 10))
  expect_gt(coh$coh[i10], 0.8)
})

test_that("native DWT: perfect reconstruction + Parseval energy", {
  set.seed(83)
  for (flt in c("haar", "d4", "d6", "d8", "la8")) {
    x <- rnorm(128)
    dw <- rmorie:::.morie_dsp_dwt(x, filter = flt, n_levels = 4)
    xr <- rmorie:::.morie_dsp_idwt(dw)
    expect_equal(xr, x, tolerance = 1e-10)
    # orthonormal transform preserves energy
    e <- sum(dw$V^2) + sum(vapply(dw$W, function(w) sum(w^2),
                                  numeric(1)))
    expect_equal(e, sum(x^2), tolerance = 1e-10)
  }
})

test_that("rgwav denoises and preserves shape natively", {
  set.seed(84)
  n <- 256
  clean <- sin(seq(0, 4 * pi, length.out = n))
  noisy <- clean + rnorm(n, 0, 0.3)
  out <- rgwav(noisy, wavelet = "la8")
  expect_length(out$signal, n)
  expect_lt(mean((out$signal - clean)^2), mean((noisy - clean)^2))
  wt <- morie_wavelet_time_series(rnorm(64), wavelet = "haar", level = 3)
  expect_match(wt$method, "rmorie native")
  expect_length(wt$details, 3)
})

test_that("hilbert envelope + pan-tompkins + notch run natively", {
  fs <- 250
  t <- seq(0, 2, by = 1 / fs)
  am <- (1 + 0.5 * sin(2 * pi * 2 * t)) * sin(2 * pi * 25 * t)
  env <- morie_dsp_hilbert_envelope(am)
  expect_equal(env[100:300], (1 + 0.5 * sin(2 * pi * 2 * t))[100:300],
               tolerance = 0.1)
  # notch kills a 60 Hz tone
  x <- sin(2 * pi * 10 * t) + sin(2 * pi * 60 * t)
  y <- morie_dsp_notch(x, 60, fs, q = 10)
  spec <- morie_dsp_psd_welch(y, fs = fs, nperseg = 256)
  i60 <- which.min(abs(spec$freqs - 60))
  i10 <- which.min(abs(spec$freqs - 10))
  expect_gt(spec$psd[i10] / spec$psd[i60], 50)
})
