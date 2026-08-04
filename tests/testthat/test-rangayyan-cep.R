# Rangayyan cepstra and homomorphic filtering in R.  The anchor is eq
# (4.80): the complex cepstrum of a wavelet plus one echo carries
# impulses at the delay and its multiples with amplitudes
# (-1)^(k+1) a^k / k, computed here from the printed series.

N <- 64

echo_sig <- function(a = 0.5, n0 = 8, n = N) {
  x <- numeric(n); x[1] <- 1; x[n0 + 1L] <- a; x
}

wavelet <- function(n = N) {
  h <- numeric(n); h[1:4] <- c(1, 0.6, -0.3, 0.1); h
}

test_that("the complex cepstrum of an echo is the train of eq (4.80)", {
  a <- 0.5; n0 <- 8
  c0 <- CCepstrum(echo_sig(a, n0))$cepstrum
  for (k in 1:4) {
    expect_equal(c0[k * n0 + 1L], (-1)^(k + 1) * a^k / k, tolerance = 2e-3)
  }
  off <- abs(c0[-1][(seq_len(N - 1)) %% 8 != 0])
  expect_lt(max(off), abs(c0[9]) / 50)
})

test_that("a pure delay is removed as the z^r factor of eq (4.68)", {
  x <- numeric(N); x[6] <- 1
  expect_equal(CCepstrum(x)$delay_removed, -5L)
})

test_that("the complex log needs a nonzero spectrum", {
  x <- numeric(N); x[1] <- 1; x[2] <- -1
  expect_error(CCepstrum(x), "nonzero spectrum")
})

test_that("EchoSeries matches the printed series and the cepstrum", {
  r <- EchoSeries(0.5, 8, terms = 4)
  expect_equal(r$amplitudes, c(0.5, -0.125, 1 / 24, -0.015625))
  expect_equal(r$quefrencies, c(8, 16, 24, 32))
  c0 <- CCepstrum(echo_sig(0.4, 10))$cepstrum
  p <- EchoSeries(0.4, 10, terms = 3)
  for (i in seq_along(p$amplitudes)) {
    expect_equal(c0[p$quefrencies[i] + 1L], p$amplitudes[i], tolerance = 2e-3)
  }
  expect_error(EchoSeries(1, 8), "\\|a\\| < 1")
  expect_lt(EchoSeries(0.3, 8, terms = 40, omega = 0.7)$max_error, 1e-12)
})

test_that("MultModel, LogSep and ConvModel implement eqs (4.58)-(4.61)", {
  expect_equal(MultModel(c(2, 3, 4), c(5, 0.5, 2))$y, c(10, 1.5, 8))
  r <- LogSep(c(2, 3, 4), c(5, 0.5, 2))
  expect_true(r$additive)
  expect_error(LogSep(c(1, 0), c(1, 1)), "!= 0")
  expect_error(LogSep(c(1, -1), c(1, 1)), "!= 0")
  expect_equal(ConvModel(c(1, 2), c(3, 4))$y, c(3, 10, 8))
})

test_that("CCepSum leaves only a truncation residual (eq 4.66)", {
  expect_lt(CCepSum(wavelet(16), echo_sig(0.5, 4, 16))$relative_residual,
            0.05)
})

test_that("CCepClosed implements eq (4.72) and its phase properties", {
  r <- CCepClosed(2, zeros_in = 0.5, zeros_out = complex(0),
                  poles_in = 0.3, poles_out = complex(0), nmax = 6)
  expect_true(r$causal)
  expect_equal(r$c0, log(2))
  expect_equal(Re(r$positive[1]), -0.5 + 0.3)
  expect_equal(Re(r$positive[2]), -0.125 + 0.045)
  expect_true(all(Mod(r$negative) == 0))
  m <- CCepClosed(1, complex(0), 0.4, complex(0), complex(0), nmax = 4)
  expect_true(m$anticausal)
  expect_true(all(Mod(m$positive) == 0))
  expect_equal(Re(m$negative[length(m$negative)]), 0.4)
  expect_true(CCepClosed(1, 0.5, complex(0), complex(0),
                         complex(0))$infinite_duration)
})

test_that("CCepClosed agrees with the numerical cepstrum", {
  x <- numeric(N); x[1] <- 1; x[2] <- -0.5
  num <- CCepstrum(x)$cepstrum
  cl <- CCepClosed(1, 0.5, complex(0), complex(0), complex(0), nmax = 5)
  for (n in seq_along(cl$positive)) {
    expect_equal(num[n + 1L], Re(cl$positive[n]), tolerance = 1e-6)
  }
})

test_that("RatZ checks root membership and evaluates the product form", {
  expect_error(RatZ(1, 0, 1.5, complex(0), complex(0), complex(0)),
               "inside the unit circle")
  r <- RatZ(2, 0, 0.5, complex(0), 0.25, complex(0), z = 2)
  expect_equal(Re(r$X), 2 * 0.75 / 0.875)
  expect_true(r$minimum_phase)
})

test_that("CCepDecay bounds the numerical cepstrum (eq 4.73)", {
  x <- numeric(N); x[1] <- 1; x[2] <- -0.5
  c0 <- CCepstrum(x)$cepstrum
  b <- CCepDecay(0.5, complex(0), complex(0), complex(0), nmax = 8)
  expect_equal(b$alpha, 0.5)
  for (n in seq_along(b$bound)) {
    expect_lte(abs(c0[n + 1L]), b$bound[n] + 1e-9)
  }
  expect_true(CCepDecay(0.98, complex(0), complex(0), complex(0))$near_unit_circle)
  expect_false(CCepDecay(0.3, complex(0), complex(0), complex(0))$near_unit_circle)
})

test_that("the power cepstrum follows eqs (4.81)-(4.83)", {
  x <- echo_sig(0.5, 8)
  sq <- PCepstrum(x, square = TRUE)
  raw <- PCepstrum(x, square = FALSE)
  expect_true(sq$squared)
  expect_true(raw$additivity_exact)
  expect_equal(sq$cepstrum[9], raw$cepstrum[9]^2)
  ex <- PCepSum(wavelet(16), echo_sig(0.5, 4, 16), square = FALSE)
  expect_true(ex$exact)
  expect_lt(ex$relative_residual, 1e-9)
  sqm <- PCepSum(wavelet(16), echo_sig(0.5, 4, 16), square = TRUE)
  expect_gt(sqm$relative_residual, ex$relative_residual)
  rel <- PCepRel(echo_sig(0.5, 8))
  expect_lt(rel$relative_residual, 1e-9)
  expect_true(rel$phase_lost)
})

test_that("the real cepstrum is not invertible but shows the echo", {
  c0 <- Cepstrum(echo_sig(0.5, 8))
  expect_false(c0$invertible)
  rng <- 2:(N %/% 2L)
  expect_equal(rng[which.max(abs(c0$cepstrum[rng]))] - 1L, 8L)
})

test_that("Lifter is symmetric and its halves partition the cepstrum", {
  cc <- as.numeric(0:(N - 1))
  r <- Lifter(cc, high = 3, keep = "low")
  kept <- which(r$liftered != 0) - 1L
  expect_true(all(kept %in% c(0:3, N - 1, N - 2, N - 3)))
  expect_true(r$symmetric)
  v <- as.numeric(1:N)
  lo <- Lifter(v, high = 5, keep = "low")$liftered
  hi <- Lifter(v, low = 6, keep = "high")$liftered
  expect_equal(lo + hi, v)
  expect_error(Lifter(c(1, 2), low = 5, high = 1), "not be below")
  expect_error(Lifter(c(1, 2), keep = "middle"), "'low', 'high' or 'band'")
})

test_that("HomoFilt separates a slow-times-fast product", {
  n <- 128; i <- 0:(n - 1)
  slow <- 2 + sin(2 * pi * i / n)
  fast <- 1 + 0.3 * sin(2 * pi * 20 * i / n)
  low <- HomoFilt(slow * fast, cutoff = 3, keep = "low")$y
  ratio <- low / slow
  expect_lt(max(ratio) - min(ratio), 0.15 * mean(ratio))
  expect_error(HomoFilt(rep(c(1, -1), 8), cutoff = 2), "strictly positive")
})

test_that("HomDeconv suppresses the echo by low-time liftering", {
  h <- wavelet()
  y <- h + 0.5 * c(numeric(12), h[1:(N - 12)])
  est <- HomDeconv(y, cutoff = 6, keep = "low")$y
  before <- max(abs(y[13:16])) / max(abs(y[1:4]))
  after <- max(abs(est[13:16])) / max(abs(est[1:4]))
  expect_equal(before, 0.5, tolerance = 0.05)
  expect_lt(after, before / 2)
})

test_that("HomPred components convolve circularly back to the signal", {
  h <- wavelet()
  y <- h + 0.5 * c(numeric(12), h[1:(N - 12)])
  r <- HomPred(y, cutoff = 6)
  expect_lt(r$relative_error, 1e-6)
  expect_equal(length(r$low_time), N)
  expect_error(HomPred(wavelet(), cutoff = 0), "1..N/2-1")
})

test_that("VocalTract finds the pitch inside the plausible range", {
  fs <- 8000; n <- 512; period <- 64
  h <- exp(-(0:47) / 12) * sin(2 * pi * 700 * (0:47) / fs)
  y <- numeric(n)
  for (start in seq(0, n - 48 - 1, by = period)) {
    y[start + 1:48] <- y[start + 1:48] + h
  }
  r <- VocalTract(y, fs = fs)
  expect_equal(r$peak_quefrency, period, tolerance = 4)
  expect_equal(r$pitch_hz, fs / period, tolerance = 0.1 * fs / period)
  # searching the whole cepstrum would report a rahmonic instead
  whole <- VocalTract(y, fs = fs, pitch_range = c(0.002, 0.05))
  expect_gt(whole$peak_quefrency, 2 * period)
})

test_that("MinPhase preserves the magnitude spectrum", {
  x <- numeric(32); x[1] <- 1; x[6] <- -1.5; x[12] <- 0.4
  r <- MinPhase(x)
  expect_true(r$magnitude_preserved)
  expect_true(r$energy_front_loaded)
})

test_that("Mfcc warps the axis and separates gain from shape", {
  fs <- 8000; i <- 0:511
  x <- sin(2 * pi * 440 * i / fs)
  r <- Mfcc(x, fs = fs, n_filters = 20, n_coeffs = 13)
  expect_equal(length(r$mfcc), 13L)
  expect_equal(length(r$filterbank_energies), 20L)
  expect_true(r$c0_is_energy)
  e <- Mfcc(c(numeric(8), rep(1, 8)), fs = fs, n_filters = 8,
            n_coeffs = 4)$edges
  expect_gt(e[length(e)] - e[length(e) - 1L], 1.5 * (e[2] - e[1]))
  y <- sin(2 * pi * 300 * (0:255) / fs)
  a <- Mfcc(y, fs = fs, n_filters = 16, n_coeffs = 4)$mfcc
  b <- Mfcc(4 * y, fs = fs, n_filters = 16, n_coeffs = 4)$mfcc
  expect_gt(b[1], a[1])
  expect_equal(b[-1], a[-1], tolerance = 1e-6)
  expect_error(Mfcc(rep(1, 32), fs = fs, fmin = 5000, fmax = 1000),
               "fmin < fmax")
})

test_that("CCepX reports the unwrapping diagnostics", {
  r <- CCepX(echo_sig(0.5, 8))
  expect_true(r$well_conditioned)
  expect_equal(r$cepstrum, CCepstrum(echo_sig(0.5, 8))$cepstrum)
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_cepstrum(echo_sig(0.5, 8))$n, N)
  expect_equal(morie_ch4_complex_cepstrum(echo_sig(0.5, 8))$cepstrum[9],
               0.5, tolerance = 2e-3)
  expect_equal(morie_liftering(rep(1, 8), high = 2)$keep, "low")
})
