# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Tests for the 18 signal-processing functions added 2026-05-22 to
# R/signal.R (cepst / hcepst / hdecon / dfa / kfd / pfd / pburg /
# welch / sampen / sgolay, plus the HRV + PCG helpers).
#
# Generous tolerances on stochastic / fractal checks (1e-2 -- 0.2).

# ---------------------------------------------------------------------------
# 1. cepst -- real cepstrum picks up periodic low-quefrency structure.
# ---------------------------------------------------------------------------

test_that("cepst returns the documented list shape", {
  set.seed(1)
  x <- sin(2 * pi * 5 * seq(0, 1, length.out = 512L))
  res <- cepst(x)
  expect_named(res, c("name", "filtered", "fs", "n_samples", "extra"),
               ignore.order = TRUE)
  expect_equal(res$name, "real_cepstrum")
  expect_true(is.numeric(res$filtered))
  # n_fft is next power of 2 >= length(x) (here 512).
  expect_equal(res$extra$n_fft, 512L)
  expect_length(res$filtered, 512L)
  # Cepstrum is finite.
  expect_true(all(is.finite(res$filtered)))
})

test_that("cepst real signal has a non-trivial low-quefrency peak", {
  set.seed(1)
  # Periodic signal -> cepstrum should have visible structure at low quefrency.
  fs <- 1000L
  t  <- seq(0, 1, length.out = fs)
  x  <- sin(2 * pi * 50 * t)  # 50 Hz tone -> period 20 samples
  res <- cepst(x)
  # Largest absolute coefficient (excluding the DC bin at index 1)
  # should be substantially above the noise floor.
  c_abs <- abs(res$filtered[-1L])
  expect_gt(max(c_abs), 5 * stats::median(c_abs))
})

# ---------------------------------------------------------------------------
# 2. dfa -- white noise alpha ~ 0.5.
# ---------------------------------------------------------------------------

test_that("dfa returns ~0.5 for white noise", {
  set.seed(1)
  x <- stats::rnorm(2048)
  res <- dfa(x)
  expect_equal(res$name, "dfa")
  expect_true(is.finite(res$value))
  expect_lt(abs(res$value - 0.5), 0.2)  # generous tolerance
  expect_true(!is.null(res$extra$scales))
  expect_true(!is.null(res$extra$fluctuation))
})

test_that("dfa returns NA on too-short input", {
  set.seed(1)
  res <- dfa(stats::rnorm(8))
  expect_true(is.na(res$value))
})

# ---------------------------------------------------------------------------
# 3. kfd / pfd -- Katz + Petrosian fractal dimensions on cumsum(rnorm).
# ---------------------------------------------------------------------------

test_that("kfd lands in a reasonable range on a random-walk signal", {
  set.seed(1)
  x <- cumsum(stats::rnorm(1000))
  res <- kfd(x)
  expect_equal(res$name, "katz_fd")
  expect_true(is.finite(res$value))
  # Katz FD on a random walk is typically in [1, 2].
  expect_gt(res$value, 1.0 - 0.2)
  expect_lt(res$value, 2.0 + 0.2)
  expect_true(!is.null(res$extra$L))
  expect_true(!is.null(res$extra$d))
})

test_that("pfd lands in [1, 2] on a random-walk signal", {
  set.seed(1)
  x <- cumsum(stats::rnorm(1000))
  res <- pfd(x)
  expect_equal(res$name, "petrosian_fd")
  expect_true(is.finite(res$value))
  expect_gte(res$value, 1.0)
  # Petrosian FD is bounded above by 2 for any non-trivial sequence.
  expect_lte(res$value, 2.0 + 0.2)
})

test_that("kfd / pfd return NA on tiny input", {
  set.seed(1)
  expect_true(is.na(kfd(numeric(0))$value))
  expect_true(is.na(pfd(c(1.0))$value))
})

# ---------------------------------------------------------------------------
# 4. sampen -- low for regular signals, high for random signals.
# ---------------------------------------------------------------------------

test_that("sampen is lower for a clean sine than for white noise", {
  set.seed(1)
  n <- 400L
  sine  <- sin(seq(0, 20 * pi, length.out = n))
  noise <- stats::rnorm(n)
  res_s <- sampen(sine,  m = 2L, r = 0.2)
  res_n <- sampen(noise, m = 2L, r = 0.2)
  expect_equal(res_s$name, "sample_entropy")
  expect_true(is.finite(res_s$value))
  expect_true(is.finite(res_n$value))
  expect_lt(res_s$value, res_n$value)
})

test_that("sampen returns NA on a constant series (zero sd)", {
  set.seed(1)
  res <- sampen(rep(1.0, 100L))
  expect_true(is.na(res$value))
})

# ---------------------------------------------------------------------------
# 5. welch -- PSD on a pure sine peaks at the right frequency.
# ---------------------------------------------------------------------------

test_that("welch PSD peaks near the input sine's frequency", {
  set.seed(1)
  fs <- 1024L
  t  <- seq(0, 2, length.out = fs * 2L + 1L)
  f0 <- 50  # Hz
  x  <- sin(2 * pi * f0 * t)
  res <- welch(x, fs = fs, nperseg = 512L)
  expect_equal(res$name, "welch_psd")
  expect_equal(res$fs, fs)
  freqs <- res$extra$freqs
  expect_length(res$filtered, length(freqs))
  # Peak frequency within 2 Hz of f0.
  peak <- freqs[which.max(res$filtered)]
  expect_lt(abs(peak - f0), 2)
})

test_that("welch PSD is non-negative and finite", {
  set.seed(1)
  x <- stats::rnorm(1024L)
  res <- welch(x, fs = 1024L, nperseg = 256L)
  expect_true(all(is.finite(res$filtered)))
  expect_true(all(res$filtered >= 0))
})

# ---------------------------------------------------------------------------
# 6. pburg -- AR PSD also peaks at the right frequency.
# ---------------------------------------------------------------------------

test_that("pburg PSD peaks near the input sine's frequency", {
  set.seed(1)
  fs <- 512L
  t  <- seq(0, 1, length.out = fs)
  f0 <- 40
  x  <- sin(2 * pi * f0 * t) + 0.1 * stats::rnorm(length(t))
  res <- pburg(x, fs = fs, order = 16L, nfft = 256L)
  expect_equal(res$name, "burg_psd")
  expect_equal(res$fs, fs)
  freqs <- res$extra$freqs
  expect_length(res$filtered, length(freqs))
  peak <- freqs[which.max(res$filtered)]
  # AR-Burg with a tone-plus-noise input: peak within 3 Hz.
  expect_lt(abs(peak - f0), 3)
  # AR coefficient vector has length order+1.
  expect_length(res$extra$ar_coefficients, 16L + 1L)
})

test_that("pburg PSD values are non-negative and finite", {
  set.seed(1)
  x <- stats::rnorm(256L)
  res <- pburg(x, fs = 256L, order = 8L, nfft = 128L)
  expect_true(all(is.finite(res$filtered)))
  expect_true(all(res$filtered >= 0))
})

# ---------------------------------------------------------------------------
# 7. hcepst -- complex cepstrum smoke + shape.
# ---------------------------------------------------------------------------

test_that("hcepst returns a real-valued vector with documented shape", {
  set.seed(1)
  x <- sin(2 * pi * 5 * seq(0, 1, length.out = 256L))
  res <- hcepst(x)
  expect_equal(res$name, "complex_cepstrum")
  expect_true(is.numeric(res$filtered))
  expect_length(res$filtered, 256L)
  expect_true(all(is.finite(res$filtered)))
  expect_equal(res$extra$original_length, 256L)
})

# ---------------------------------------------------------------------------
# 8. sgolay -- delegates to morie_sgolay_smooth without error.
# ---------------------------------------------------------------------------

test_that("sgolay returns a smoothed signal of the input length", {
  set.seed(1)
  x <- sin(seq(0, 2 * pi, length.out = 200L)) + 0.1 * stats::rnorm(200L)
  res <- sgolay(x, window = 11L, polyorder = 3L)
  expect_equal(res$name, "savgol_smooth")
  expect_length(res$filtered, length(x))
  expect_equal(res$extra$window, 11L)
  expect_equal(res$extra$polyorder, 3L)
  # Smoothing reduces variance of the high-frequency residual.
  expect_lt(stats::sd(res$filtered - sin(seq(0, 2 * pi, length.out = 200L))),
            stats::sd(x - sin(seq(0, 2 * pi, length.out = 200L))))
})
