# Rangayyan signal-level features in R -- same book equations as the
# Python arm.  Expected values hand-computed or properties the book
# states (a sinusoid has complexity 1; averaging M realizations gains
# sqrt(M)).

sine <- function(n, cycles, amp = 1) amp * sin(2 * pi * cycles * (0:(n - 1)) / n)

test_that("Rms implements eq (3.9) and its short-time form", {
  r <- Rms(c(3, 4))
  expect_equal(r$ms, 12.5)
  expect_equal(r$rms, sqrt(12.5))
  w <- Rms(c(1, 1, 1, 4), window = 2)
  expect_equal(w$short_time, c(1, 1, 1, sqrt(17 / 2)))
  expect_error(Rms(c(1, 2), window = 0), "at least one sample")
})

test_that("FormFactor is Hjorth complexity, not RMS over mean-abs", {
  coarse <- FormFactor(sine(200, 5))$form_factor
  fine <- FormFactor(sine(2000, 5))$form_factor
  expect_lt(abs(fine - 1), abs(coarse - 1))
  expect_equal(fine, 1, tolerance = 2e-3)
  # the placeholder's ratio is pi/(2 sqrt 2) for a sinusoid, not 1
  x <- sine(2000, 5)
  ratio <- sqrt(mean(x^2)) / mean(abs(x))
  expect_equal(ratio, pi / (2 * sqrt(2)), tolerance = 1e-3)
  expect_gt(abs(FormFactor(x)$form_factor - ratio), 1e-2)
})

test_that("FormFactor grows with complexity and rejects a constant", {
  simple <- FormFactor(sine(2000, 5))$form_factor
  rough <- FormFactor(sine(2000, 5) + sine(2000, 37, 0.4))$form_factor
  expect_gt(rough, simple)
  x <- sine(1000, 3)
  expect_equal(FormFactor(x)$activity, mean((x - mean(x))^2))
  expect_error(FormFactor(rep(2, 10)), "zero activity")
})

test_that("TurnsCount implements Willison's rule (Section 5.6.3)", {
  expect_equal(TurnsCount(c(0, 5, 0, 5, 0), threshold = 1)$turns, 3L)
  # a small ripple on a flat baseline: turning points, but no turns
  ripple <- ifelse(seq_len(40) %% 2 == 0, 0.2, -0.2)
  expect_equal(TurnsCount(ripple, threshold = 1)$turns, 0L)
  expect_gt(TurnsCount(ripple, threshold = 0)$turns, 30L)
  # the swing is measured against the LAST COUNTED turn
  x <- c(0, 3, 0, 3, 0, 3)
  expect_equal(TurnsCount(x, threshold = 2)$turns, 4L)
  expect_equal(TurnsCount(x, threshold = 4)$turns, 0L)
  s <- TurnsCount(rep(c(0, 5, 0, 5, 0), 4), threshold = 1, window = 5)
  expect_equal(length(s$short_time), 20L)
  expect_error(TurnsCount(c(0, 1, 0), threshold = -1), "nonnegative")
})

test_that("Snr keeps the power and peak definitions apart", {
  x <- sine(1000, 5); e <- 0.1 * sine(1000, 97)
  r <- Snr(x, e)
  expect_equal(r$snr_db, r$snr_power_db)
  expect_equal(r$snr_peak_db, 20 * log10(2 / (0.1 / sqrt(2))),
               tolerance = 0.05)
  expect_gt(r$snr_peak_db, r$snr_power_db)
  expect_equal(Snr(c(1, -1, 1, -1), c(0.1, -0.1, 0.1, -0.1))$snr_db, 20)
  expect_equal(Snr(x, rep(0.05, 1000), definition = "peak")$snr_db,
               Snr(x, rep(0.05, 1000))$snr_peak_db)
  expect_error(Snr(x, e, definition = "whatever"), "power")
  expect_error(Snr(c(1, 2), c(0, 0)), "unbounded")
})

test_that("SnrFilt penalises distortion as well as noise", {
  clean <- sine(400, 3)
  expect_equal(SnrFilt(clean, clean)$snr_db, Inf)
  expect_equal(SnrFilt(clean, 0.5 * clean)$snr_db, 10 * log10(4))
  expect_error(SnrFilt(c(1, 2), 1), "same length")
})

test_that("SyncAvg implements eqs (3.95)-(3.96) with a sqrt(M) gain", {
  r <- SyncAvg(list(c(1, 2), c(3, 4), c(5, 6), c(7, 8)))
  expect_equal(r$average, c(4, 5))
  expect_equal(r$m, 4L)
  expect_equal(r$snr_gain, 2)
  expect_equal(r$snr_gain_db, 10 * log10(4))
  expect_error(SyncAvg(list(c(1, 2), 1)), "same length")
  n <- 64; m <- 100; base <- sine(n, 3); step <- 0.37
  recs <- lapply(0:(m - 1), function(k)
    base + 0.5 * sin(step * (k * n + (0:(n - 1)))))
  avg <- SyncAvg(recs)$average
  expect_lt(max(abs(avg - base)), max(abs(recs[[1]] - base)) / 3)
})

test_that("ObsReal builds the ensemble of eq (3.95)", {
  r <- ObsReal(c(1, 2), list(c(0.1, 0.1), c(-0.1, -0.1)))
  expect_equal(r$y[[1]], c(1.1, 2.1))
  expect_equal(r$y[[2]], c(0.9, 1.9))
  expect_true(r$identical_repetitions)
  expect_false(ObsReal(list(c(1, 2), c(1, 3)),
                       list(c(0, 0), c(0, 0)))$identical_repetitions)
  expect_equal(SyncAvg(r$y)$average, c(1, 2))
})

test_that("FdPsd implements eqs (6.50)-(6.52)", {
  beta <- 1.2
  f <- (1:199) / 10
  p <- f^(-beta)
  r <- FdPsd(p, f)
  expect_equal(r$beta, beta, tolerance = 1e-9)
  expect_equal(r$fd, (5 - beta) / 2, tolerance = 1e-9)
  expect_equal(r$hurst, (beta - 1) / 2, tolerance = 1e-9)
  expect_equal(r$r_squared, 1, tolerance = 1e-12)
  expect_true(r$in_range)
  expect_false(FdPsd(f^(-3), f)$in_range)
  # the DC bin is dropped
  d <- FdPsd(c(1e6, f^(-1)), c(0, f))
  expect_equal(d$n_bins, 199L)
  expect_equal(d$beta, 1, tolerance = 1e-9)
  expect_error(FdPsd(c(1, 2), c(1, 2)), "at least three")
})

test_that("FdVag fits inside the requested band", {
  fs <- 2000; n <- 1024
  x <- sin(2 * pi * 150 * (0:(n - 1)) / fs) +
    0.5 * sin(2 * pi * 320 * (0:(n - 1)) / fs)
  r <- FdVag(x, fs = fs, fmin = 100, fmax = 500)
  expect_gt(r$fd, 0)
  expect_lt(r$fd, 3)
  expect_gte(r$band[1], 100)
  expect_lte(r$band[2], 500)
})

test_that("KatzFd is 1 for a line and larger for a rough trace", {
  expect_equal(KatzFd(as.numeric(0:49))$fd, 1, tolerance = 1e-9)
  expect_gt(KatzFd(sine(500, 60))$fd, KatzFd(sine(500, 2))$fd)
  a <- KatzFd(sine(500, 5))$fd
  b <- KatzFd(10 * sine(500, 5))$fd
  expect_gt(abs(a - b), 1e-6)
  expect_true(KatzFd(sine(500, 5))$scale_sensitive)
})

test_that("SpecEntropy is log2(K) for a flat spectrum and 0 for a tone", {
  r <- SpecEntropy(rep(1, 8))
  expect_equal(r$entropy, 3)
  expect_equal(r$normalized, 1)
  expect_equal(SpecEntropy(c(0, 0, 5, 0))$entropy, 0)
  expect_equal(SpecEntropy(rep(1, 4), c(0, 10, 20, 30),
                           fmin = 10, fmax = 20)$n_bins, 2L)
  expect_error(SpecEntropy(c(1, -1)), "cannot be negative")
})

test_that("FiringRate is the reciprocal of the mean interval", {
  r <- FiringRate(c(0, 0.1, 0.2, 0.3))
  expect_equal(r$mean_idi, 0.1)
  expect_equal(r$mfr, 10)
  expect_equal(r$cv_idi, 0, tolerance = 1e-15)
  v <- FiringRate(c(0, 0.05, 0.35))
  expect_equal(v$mfr, 1 / 0.175)
  expect_equal(v$mean_instantaneous_rate, (1 / 0.05 + 1 / 0.30) / 2)
  expect_lt(v$mfr, v$mean_instantaneous_rate)
  expect_equal(FiringRate(c(0, 100, 200), fs = 1000)$mfr, 10)
  expect_error(FiringRate(c(0, 0.2, 0.1)), "strictly increasing")
})

test_that("SigFeatures agrees with the individual measures", {
  x <- sine(512, 7)
  r <- SigFeatures(x, fs = 256)
  expect_equal(r$rms, Rms(x)$rms)
  expect_equal(r$form_factor, FormFactor(x)$form_factor)
  expect_equal(r$turns, TurnsCount(x, threshold = 0)$turns)
  fs <- 256; n <- 512; f0 <- 16
  tone <- sin(2 * pi * f0 * (0:(n - 1)) / fs)
  expect_equal(SigFeatures(tone, fs = fs)$spectral_centroid, f0,
               tolerance = 0.5)
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_rms(c(3, 4))$rms, sqrt(12.5))
  expect_equal(morie_turns_count(c(0, 5, 0), threshold = 1)$turns, 1L)
  expect_equal(morie_form_factor(sine(2000, 5))$form_factor, 1,
               tolerance = 2e-3)
})
