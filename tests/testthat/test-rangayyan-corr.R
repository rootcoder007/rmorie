# Rangayyan correlation, spectral density and the matched filter, in R.
# Same book equations as the Python arm; expected values hand-computed.

sine_c <- function(n, cycles, amp = 1, phase = 0)
  amp * sin(2 * pi * cycles * (0:(n - 1)) / n + phase)

test_that("DotProd implements eqs (4.24)-(4.25)", {
  r <- DotProd(c(1, 2, 3), c(4, 5, 6))
  expect_equal(r$dot_product, 32)
  expect_equal(r$gamma, 32 / sqrt(14 * 77))
  expect_equal(DotProd(c(1, 2, 3), c(2, 4, 6))$gamma, 1)
  raw <- DotProd(c(1, 2, 3), c(3, 2, 1))$gamma
  centred <- DotProd(c(1, 2, 3), c(3, 2, 1), subtract_mean = TRUE)$gamma
  expect_equal(raw, 10 / 14)
  expect_equal(centred, -1)
})

test_that("ContProj carries the dt of eq (4.26)", {
  r <- ContProj(rep(1, 5), rep(2, 5), dt = 0.5)
  expect_equal(r$discrete_sum, 10)
  expect_equal(r$theta, 4)
})

test_that("CcfOuter measures the Toeplitz structure of eq (4.29)", {
  flat <- CcfOuter(sine_c(600, 17), sine_c(600, 17), order = 4)
  expect_true(flat$toeplitz)
  expect_equal(dim(flat$theta), c(4L, 4L))
  ramp <- sine_c(600, 17) * (1 + 4 * (0:599) / 600)
  tilted <- CcfOuter(ramp, ramp, order = 4)
  expect_gt(tilted$relative_deviation, 2 * flat$relative_deviation)
  expect_false(tilted$toeplitz)
  expect_error(CcfOuter(c(1, 2), c(1, 2), order = 5), "at least 5 samples")
})

test_that("Csd computes eqs (4.30)-(4.31) two ways", {
  r <- Csd(sine_c(64, 5), sine_c(64, 5, phase = 0.4))
  expect_true(r$agrees)
  expect_equal(r$max_difference, 0, tolerance = 1e-6)
  expect_lt(max(abs(Im(Csd(sine_c(64, 5), sine_c(64, 5))$csd))), 1e-8)
})

test_that("Cohere refuses a single segment (eq 4.32 caveat)", {
  expect_error(Cohere(sine_c(64, 5), sine_c(64, 5, phase = 0.3),
                      nperseg = 64), "AVERAGED")
})

test_that("Cohere is 1 for a linearly related pair", {
  n <- 1024
  x <- sine_c(n, 13) + 0.2 * sine_c(n, 97)
  r <- Cohere(x, 2 * x, fs = 128, nperseg = 128)
  expect_equal(r$coherence[which.max(r$sxx)], 1, tolerance = 1e-6)
  expect_gte(r$n_segments, 2L)
})

test_that("Cohere reports the phase difference", {
  n <- 1024; fs <- 128; cyc <- 16
  r <- Cohere(sine_c(n, cyc), sine_c(n, cyc, phase = pi / 2), fs = fs,
              nperseg = 256)
  k <- which.min(abs(r$freqs - cyc * fs / n))
  expect_lt(abs(abs(r$phase[k]) - pi / 2), 0.3)
})

test_that("Msc is the square of the magnitude coherence", {
  n <- 1024
  x <- sine_c(n, 11) + 0.5 * sine_c(n, 53)
  y <- sine_c(n, 11) + 0.5 * sine_c(n, 71)
  r <- Msc(x, y, nperseg = 128)
  expect_equal(r$msc, r$magnitude_coherence^2)
  expect_true(all(r$msc >= 0 & r$msc <= 1 + 1e-9))
})

test_that("Template finds the planted copy and resists a plateau", {
  ref <- c(0, 1, 3, 1, 0)
  x <- numeric(20); x[9:13] <- ref
  r <- Template(x, ref)
  expect_equal(r$best_shift, 8L)
  expect_equal(r$best_gamma, 1, tolerance = 1e-9)
  y <- numeric(30); y[6:10] <- 0.1 * ref; y[16:25] <- 50
  expect_equal(Template(y, ref)$best_shift, 5L)
  z <- numeric(40); z[6:10] <- ref; z[26:30] <- ref
  d <- Template(z, ref, threshold = 0.95)
  expect_equal(d$detections, c(5L, 25L))
  expect_error(Template(rep(1, 10), rep(2, 3)), "zero energy")
})

REF <- c(3, 2, 1)

test_that("RefPattern implements eqs (4.53)-(4.54)", {
  r <- RefPattern()
  expect_equal(r$g, c(3, 2, 1))
  expect_equal(r$h, c(1, 2, 3))
  expect_equal(r$delay, 2L)
  expect_true(r$output_is_acf)
  expect_equal(r$y, c(3, 8, 14, 8, 3))
})

test_that("MfImpulse reverses, scales and delays (eq 4.49)", {
  r <- MfImpulse(REF)
  expect_equal(r$h[1:4], c(0, 1, 2, 3))
  expect_equal(r$shift_samples, 3L)
  expect_true(r$causal)
  expect_error(MfImpulse(REF, t0 = 1), "causal filter")
})

test_that("MfAcf output is the reference ACF", {
  r <- MfAcf(REF)
  expect_true(r$equals_acf)
  expect_equal(r$peak_value, r$expected_peak)
  expect_equal(r$energy, 14)
})

test_that("MfTf conjugates the spectrum (eq 4.48)", {
  X <- c(complex(real = 1, imaginary = 2), complex(real = -0.5, imaginary = 0.25))
  r <- MfTf(X, c(0, 1), t0 = 0)
  expect_equal(r$H[1], complex(real = 1, imaginary = -2))
  expect_true(r$conjugate_of_signal)
  d <- MfTf(c(complex(real = 1), complex(real = 1)), c(0, 0.25), t0 = 1)
  expect_equal(d$H[2], complex(real = cos(-pi / 2), imaginary = sin(-pi / 2)),
               tolerance = 1e-12)
})

test_that("the EEG forms delegate to the general ones", {
  X <- c(complex(real = 1, imaginary = 2), complex(real = -0.5, imaginary = 0.25))
  expect_equal(MfTfEeg(X, c(0, 1), t0 = 0.5)$H, MfTf(X, c(0, 1), t0 = 0.5)$H)
  expect_true(grepl("N-1", MfTfEeg(X, c(0, 1), t0 = 0.5)$dft_shift_caveat))
  expect_equal(MfImpEeg(REF)$h, MfImpulse(REF)$h)
  expect_true(MfImpEeg(REF)$equivalent_to_correlation)
})

test_that("the matched-filter chain eqs (4.33)-(4.39)", {
  expect_equal(Re(MfInput(c(1, 1), omega = 0, dt = 0.5)$X), 1)
  o <- MfOutput(REF, rev(REF))
  expect_equal(o$peak_index, 2L)
  expect_equal(o$peak_magnitude, 14)
  expect_equal(MfNoiseIn(4)$density, 2)
  no <- MfNoiseOut(4, rep(1, 4), df = 0.25)
  expect_equal(no$psd, rep(2, 4))
  expect_equal(no$power, 2)
  expect_equal(no$rms, sqrt(2))
  p <- MfPeak(rep(complex(real = 1), 3), rep(complex(real = 1), 3),
              c(0, 0.5, 1), t0 = 0)
  expect_equal(p$my, 1)
  s <- MfSnr(4, 2)
  expect_equal(s$snr, 8)
  expect_true(s$peak_to_mean)
})

test_that("SigEnergy integrates, and reports a mismatch", {
  r <- SigEnergy(c(4, 0, 0, 0), dt = 1)
  expect_equal(r$energy, 8)          # trapezoid: 0.5 * (16 + 0)
  f <- SigEnergy(NULL, X = rep(complex(real = sqrt(2)), 4),
                 freqs = c(0, 1, 2, 3))
  expect_equal(f$energy_freq, 6)
  b <- SigEnergy(rep(1, 4), X = rep(complex(real = 0), 4),
                 freqs = c(0, 1, 2, 3))
  expect_false(b$parseval_holds)
  expect_gt(b$max_difference, 0)
})

test_that("MfRatio reaches its bound only at the optimum (eq 4.41)", {
  freqs <- (0:8) / 8
  X <- complex(real = cos(0:8), imaginary = sin(2 * (0:8)))
  opt <- MfTf(X, freqs, t0 = 0)$H
  good <- MfRatio(X, opt, freqs, t0 = 0, noise_power = 2)
  bad <- MfRatio(X, rep(complex(real = 1), 9), freqs, t0 = 0,
                 noise_power = 2)
  expect_equal(good$optimality, 1, tolerance = 1e-9)
  expect_lt(bad$optimality, 1)
  expect_equal(good$bound, 1)
})

test_that("the Schwarz family, eqs (4.42)-(4.45)", {
  grid <- (0:8) / 8
  B <- complex(real = cos(0:8), imaginary = sin(0:8))
  r <- SchwarzC(3 * Conj(B), B, grid)
  expect_true(r$holds)
  expect_true(r$equality)
  expect_equal(r$k, complex(real = 3, imaginary = 0), tolerance = 1e-9)
  u <- SchwarzC(complex(real = cos(0:8)), complex(real = sin(3 * (0:8))),
                grid)
  expect_true(u$holds)
  expect_false(u$equality)
  a <- c(1, 2, 3, 4, 5)
  s <- SchwarzR(a, 2 * a)
  expect_true(s$equality)
  expect_equal(s$k, 0.5)
  expect_false(SchwarzR(a, c(5, 1, 4, 2, 3))$equality)
  c0 <- CauchySch(c(3, 4), c(6, 8))
  expect_equal(c0$lhs, 50)
  expect_equal(c0$rhs, 50)
  expect_true(c0$equality)
  expect_equal(c0$cosine, 1)
  t0 <- Triangle(c(3, 4), c(6, 8))
  expect_equal(t0$lhs, 15)
  expect_true(t0$equality)
  expect_false(Triangle(c(1, 0), c(0, 1))$equality)
})

test_that("MfPsd output is real and nonnegative (eq 4.57)", {
  r <- MfPsd(c(REF, numeric(5)))
  expect_true(r$is_psd)
  expect_lt(r$max_imaginary, 1e-9)
  expect_true(all(r$psd >= -1e-12))
})

test_that("MfMaxSnr is 2E/N0 and depends only on energy (eq 4.46)", {
  r <- MfMaxSnr(rep(1, 4), 2)
  expect_equal(r$snr, 2 * r$energy / 2)
  expect_true(r$depends_only_on_energy)
  a <- MfMaxSnr(c(0, 2, 2, 0), 1)
  b <- MfMaxSnr(c(0, sqrt(8), 0, 0), 1)
  expect_equal(a$energy, b$energy)
  expect_equal(a$snr, b$snr)
  x <- sine_c(64, 4)
  expect_equal(MfMaxSnr(3 * x, 1)$snr, 9 * MfMaxSnr(x, 1)$snr)
})

test_that("MatchedFilt designs, runs and whitens", {
  ref <- c(1, 2, 3)
  x <- numeric(20); x[8:10] <- ref
  r <- MatchedFilt(ref, x = x)
  expect_false(r$whitened)
  expect_equal(r$peak_index, 10L)
  expect_equal(r$h[1:4], c(0, 3, 2, 1))
  w <- MatchedFilt(c(1, 2, 3, 0), noise_psd = c(4, 1, 1, 1))
  expect_true(w$whitened)
  expect_equal(Mod(w$H[1]), Mod(MatchedFilt(c(1, 2, 3, 0))$H[1]) / 4)
  expect_error(MatchedFilt(c(1, 2), noise_psd = c(1, 0)), "positive")
})

test_that("Idft, Parseval and SyncSum, eqs (3.81), (3.91), (3.96)", {
  x <- c(1, 2, 3, 4)
  expect_equal(Idft(Dft(x)$X)$x, x, tolerance = 1e-12)
  p <- Parseval(x)
  expect_equal(p$energy_time, 30)
  expect_true(p$holds)
  s <- SyncSum(list(c(1, 2), c(3, 4)))
  expect_equal(s$sum, c(4, 6))
  expect_equal(s$average, c(2, 3))
})

test_that("SpecMoments implements eqs (6.32)-(6.43)", {
  n <- 512; fs <- 256; cyc <- 32
  x <- sine_c(n, cyc)
  f <- Dft(x - mean(x))
  half <- n %/% 2L + 1L
  p <- Mod(f$X[seq_len(half)])^2 / n
  r <- SpecMoments(p, fs = fs)
  want <- cyc * fs / n
  expect_equal(r$mean_frequency, want, tolerance = 1)
  expect_equal(r$median_frequency, want, tolerance = 1)
  expect_lt(r$bandwidth, 3)
  flat <- SpecMoments(rep(1, 65), fs = 128)
  expect_equal(flat$uniformity, 1)
  expect_equal(flat$mean_frequency, 32, tolerance = 1)
  expect_error(SpecMoments(c(1, -1)), "cannot be negative")
})

test_that("EmgFreq separates the mean from the median", {
  n <- 1024; fs <- 1000
  x <- sine_c(n, 40) + 0.25 * sine_c(n, 300)
  r <- EmgFreq(x, fs = fs)
  expect_gt(r$mean_frequency, r$median_frequency)
  fresh <- EmgFreq(sine_c(n, 200), fs = fs)
  tired <- EmgFreq(sine_c(n, 100), fs = fs)
  expect_lt(tired$mean_frequency, fresh$mean_frequency)
  expect_lt(tired$median_frequency, fresh$median_frequency)
})

test_that("SpecRes depends on record length and trades with the window", {
  a <- SpecRes(256, fs = 256)
  b <- SpecRes(512, fs = 256)
  expect_equal(a$delta_f, 1)
  expect_equal(b$delta_f, 0.5)
  expect_false(a$zero_padding_helps)
  black <- SpecRes(256, fs = 256, window = "blackman")
  expect_lt(black$sidelobe_db, a$sidelobe_db)
  expect_gt(black$main_lobe_bins, a$main_lobe_bins)
  expect_gt(black$resolution, a$resolution)
  expect_error(SpecRes(256, window = "kaiser"), "unknown window")
})

test_that("PsdHz band powers carry the bin width", {
  r <- PsdHz(rep(1, 9), fs = 16,
             bands = list(low = c(0, 4), high = c(4, 8)))
  expect_equal(r$bin_width, 1)
  expect_equal(r$band_power$low, 4)
  expect_equal(r$band_power$high, 4)
  expect_equal(r$band_fraction$low, 0.5)
  expect_error(PsdHz(rep(1, 5), fs = 8, bands = list(bad = c(3, 1))),
               "hi <= lo")
})

test_that("PcgSyncAvg keeps murmur power waveform averaging cancels", {
  n <- 128; m <- 12
  cycles <- lapply(0:(m - 1), function(k)
    sine_c(n, 3) + 0.8 * sin(2 * pi * 30 * (0:(n - 1)) / n + k * 1.7))
  r <- PcgSyncAvg(cycles)
  expect_lt(r$power_retained, 0.7)
  expect_gt(sum(r$average_psd), sum(r$psd_of_average))
})

test_that("ErpArtifact rejects the contaminated epochs", {
  good <- replicate(9, sine_c(32, 2), simplify = FALSE)
  bad <- 50 * sine_c(32, 2)
  r <- ErpArtifact(c(good, list(bad)), reject = 5)
  expect_equal(r$n_rejected, 1L)
  expect_equal(r$rejected, 9L)
  expect_equal(r$m_kept, 9L)
  expect_equal(r$snr_gain, 3)
  dirty <- ErpArtifact(c(good, list(bad)))$average
  expect_gt(max(abs(dirty)), 2 * max(abs(r$average)))
  expect_error(ErpArtifact(replicate(4, sine_c(32, 2), simplify = FALSE),
                           reject = 1e-6), "every epoch")
})

test_that("SeizCohere tracks bands over a moving window", {
  n <- 2048; fs <- 128
  a <- sine_c(n, 100) + 0.3 * sine_c(n, 13)
  b <- sine_c(n, 100) + 0.3 * sine_c(n, 191)
  r <- SeizCohere(list(a, b), fs = fs, window = 512, step = 256,
                  nperseg = 128)
  expect_gte(r$n_windows, 2L)
  expect_equal(sort(names(r$coherence)),
               sort(c("delta", "theta", "alpha", "beta")))
  expect_true(all(vapply(r$coherence,
                         function(v) all(v >= 0 & v <= 1 + 1e-9),
                         logical(1))))
  expect_error(SeizCohere(list(sine_c(256, 5)), fs = 64, window = 128),
               "two channels")
})

test_that("CardioResp separates PLV from coherence", {
  n <- 512; fs <- 8
  resp <- sin(2 * pi * 0.25 * (0:(n - 1)) / fs)
  ecg <- sin(2 * pi * 0.25 * (0:(n - 1)) / fs + 0.7)
  expect_equal(CardioResp(ecg, resp, fs = fs)$plv, 1, tolerance = 0.05)
  drift <- sin(2 * pi * 0.32 * (0:(n - 1)) / fs)
  expect_gt(CardioResp(resp, resp, fs = fs)$plv,
            CardioResp(drift, resp, fs = fs)$plv)
  expect_error(CardioResp(sine_c(64, 3), sine_c(64, 3), fs = 8,
                          band = c(0.1, 9)), "fs/2")
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_ch4_dot_product(c(1, 2), c(3, 4))$dot_product, 11)
  expect_true(morie_ch3_parseval(c(1, 2))$holds)
  expect_gt(morie_matched_filter_snr(c(1, 1), 2)$snr, 0)
})
