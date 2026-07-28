# Cross-language anchors from the morie.fn rg*/rng* modules at full
# precision (testthat tolerance is relative).

test_that("autocorrelation matches Python for BOTH divisors", {
  x <- c(1, 2, 3, 4, 5, 4, 3, 2, 1, 0)
  out <- morie_acf_estimate(x, max_lag = 3)
  expect_equal(out$acf_unbiased,
    c(8.5, 8.88888888888889, 8.5, 7.42857142857143), tolerance = 1e-12)
  expect_equal(out$acf_biased, c(8.5, 8, 6.8, 5.2), tolerance = 1e-12)
  # R(0) is the mean square under both divisors
  expect_equal(out$acf_biased[1], mean(x^2), tolerance = 1e-12)
  expect_error(morie_acf_estimate(x, max_lag = 99))
})

test_that("Yule-Walker and Burg both recover a simulated AR(2)", {
  set.seed(11)
  n <- 3000
  e <- stats::rnorm(n)
  x <- numeric(n)
  for (t in 3:n) x[t] <- 0.75 * x[t - 1] - 0.5 * x[t - 2] + e[t]
  yw <- morie_yule_walker(x, order = 2)
  expect_equal(yw$a[1], -0.75, tolerance = 0.08)
  expect_equal(yw$a[2], 0.5, tolerance = 0.08)
  expect_true(yw$stable) # the biased ACF guarantees this
  bg <- morie_burg_method(x, order = 2)
  expect_equal(bg$a[1], -0.75, tolerance = 0.08)
  expect_true(bg$stable)
  expect_true(all(abs(bg$reflection) <= 1)) # what makes it automatic
  expect_error(morie_yule_walker(x[1:2], order = 5))
})

test_that("AR spectrum peaks at the pole angle", {
  set.seed(12)
  n <- 3000
  e <- stats::rnorm(n)
  x <- numeric(n)
  for (t in 3:n) x[t] <- 0.75 * x[t - 1] - 0.5 * x[t - 2] + e[t]
  out <- morie_ar_spectrum(x, order = 2, fs = 100)
  roots <- polyroot(c(0.5, -0.75, 1))
  f_exp <- abs(Arg(roots[1])) / (2 * pi) * 100
  expect_equal(out$freqs[which.max(out$psd)], f_exp, tolerance = 3)
  expect_error(morie_ar_spectrum(x, fs = -1))
})

test_that("moving average matches Python and nulls a tone at fs/M", {
  out <- morie_moving_average(0:9, M = 4)
  expect_equal(out$y[1:6], c(0, 0.25, 0.75, 1.5, 2.5, 3.5), tolerance = 1e-12)
  expect_equal(out$group_delay, 1.5)
  # an M-point boxcar has a zero at exactly fs/M
  n <- 0:399
  tone <- sin(2 * pi * 10 * n / 100)
  expect_lt(max(abs(morie_moving_average(tone, M = 10)$y[50:400])), 1e-9)
  expect_error(morie_moving_average(1:10, M = 0))
  # general FIR: symmetric taps are linear phase
  expect_true(morie_fir_filter(1:20, c(0.25, 0.5, 0.25))$linear_phase)
  expect_false(morie_fir_filter(1:20, c(0.1, 0.9))$linear_phase)
})

test_that("LMS gradient is exactly -2 e r and RLS solves its normal equations", {
  set.seed(13)
  R <- matrix(stats::rnorm(60), ncol = 2)
  x <- stats::rnorm(30)
  w <- c(0.3, -0.7)
  lms <- morie_lms_error(x, w, R)
  expect_equal(lms$error, x - as.numeric(R %*% w), tolerance = 1e-12)
  expect_equal(lms$gradient, -2 * lms$error * R, tolerance = 1e-12)
  # RLS: Phi matches Python and the weights solve Phi w = Theta
  r <- matrix(0:11, ncol = 2, byrow = TRUE)
  phi <- morie_rls_phi(r, lam = 0.9)
  expect_equal(as.numeric(phi$Phi),
    c(201.0484, 227.3366, 227.3366, 258.31039), tolerance = 1e-8)
  full <- morie_rls_phi(R, x = x, lam = 0.99)
  expect_equal(as.numeric(full$Phi %*% full$weights), full$Theta,
    tolerance = 1e-8
  )
  expect_equal(full$effective_memory, 100, tolerance = 1e-10)
  expect_true(is.infinite(morie_rls_phi(R, lam = 1)$effective_memory))
  expect_error(morie_rls_phi(R, lam = 1.5))
})

test_that("Pan-Tompkins uses the exact 1/8 coefficient", {
  out <- morie_pan_tompkins_update(2, SPKI = 1, NPKI = 0, is_signal = TRUE)
  expect_equal(out$SPKI, 1.125, tolerance = 1e-12) # 0.125*2 + 0.875*1
  noise <- morie_pan_tompkins_update(2, SPKI = 1, NPKI = 0.4,
                                     is_signal = FALSE)
  expect_equal(noise$NPKI, 0.125 * 2 + 0.875 * 0.4, tolerance = 1e-12)
  run <- morie_pan_tompkins_update(c(rep(1, 5), rep(0.05, 5)), SPKI = 1,
                                   NPKI = 0.05)
  expect_true(run$NPKI < run$threshold && run$threshold < run$SPKI)
  expect_error(morie_pan_tompkins_update(-1))
})

test_that("ensemble averaging, bandwidth and cepstral pitch behave", {
  set.seed(14)
  sig <- sin(2 * pi * seq_len(200) / 50)
  reps <- t(vapply(1:64, function(i) sig + stats::rnorm(200, sd = 2), numeric(200)))
  ea <- morie_ensemble_average(reps)
  expect_equal(ea$snr_gain, 8) # sqrt(64)
  expect_lt(stats::sd(ea$ensemble_mean - sig), stats::sd(reps[1, ] - sig) / 4)
  # bandwidth: the two criteria genuinely differ on a peaky spectrum
  f <- seq(0, 50, length.out = 501)
  psd <- exp(-(f - 10)^2 / (2 * 0.5^2)) + 0.001
  b3 <- morie_spectral_bandwidth(psd, f, "3dB")
  b99 <- morie_spectral_bandwidth(psd, f, "99")
  expect_equal(b3$f_peak, 10, tolerance = 0.2)
  expect_gt(b99$bandwidth, b3$bandwidth)
  expect_error(morie_spectral_bandwidth(psd, f, "half"))
  # cepstrum finds the pitch of a synthetic glottal train
  fs <- 8000
  f0 <- 120
  nn <- 0:4095
  train <- numeric(4096)
  train[seq(1, 4096, by = round(fs / f0))] <- 1
  envp <- exp(-nn / 800) * sin(2 * pi * 900 * nn / fs)
  xx <- stats::convolve(train, rev(envp), type = "open")[1:4096]
  expect_equal(morie_cepstrum_pitch(xx, fs, c(60, 400))$f0, f0, tolerance = 12)
})

test_that("transfer function refuses a single segment and tracks coherence", {
  set.seed(15)
  x <- stats::rnorm(8192)
  b <- c(0.5, 0.3, 0.2)
  y <- as.numeric(stats::filter(x, b, method = "convolution", sides = 1L))
  y[is.na(y)] <- 0
  out <- morie_transfer_function(x, y, fs = 100, nperseg = 512)
  expect_gt(stats::median(out$coherence), 0.99)
  noisy <- morie_transfer_function(x, y + stats::rnorm(8192, sd = 2),
                                   nperseg = 512)
  expect_lt(stats::median(noisy$coherence), stats::median(out$coherence))
  # a single segment gives coherence identically 1 and is refused
  expect_error(morie_transfer_function(x[1:512], y[1:512], nperseg = 512))
})
