# Coverage tests for R/dsp_detection.R: derivative peak detection, the
# simplified Pan-Tompkins detector, T waves, dicrotic notches,
# homomorphic filtering, the complex cepstrum and Welch cross-spectra.

test_that("derivative detector marks rising-to-falling slope changes", {
  x <- c(0, 1, 3, 2, 1, 1, 4, 6, 5, 0.5)
  dx <- diff(x) * 10
  thr <- 0.5 * max(abs(dx))
  ref <- which(vapply(2:(length(dx) - 1), function(i) dx[i - 1] > 0 && dx[i] <= 0 && abs(dx[i - 1]) > thr, TRUE)) + 1L
  expect_equal(morie_dsp_derivative_detect(x, fs = 10), ref)
  expect_equal(morie_dsp_derivative_detect(x, threshold_factor = 0), c(3L, 8L))
  expect_equal(morie_dsp_derivative_detect(c(1, 2, 3)), integer(0))
})

test_that("Pan-Tompkins style detector finds each beat once; T waves follow", {
  fs <- 360
  n <- fs * 5
  ecg <- numeric(n)
  beats <- round(seq(0.5, 4.5, by = 0.8) * fs)
  for (b in beats) ecg[b + (-4:4)] <- exp(-((-4:4) / 1.5)^2)
  q <- morie_dsp_pan_tompkins(ecg, fs)
  # the detector reports where the centred integrator first crosses half
  # its maximum, which leads the R peak by less than half the 150 ms window
  expect_length(q, length(beats))
  expect_true(all(beats - q >= 0 & beats - q <= 0.075 * fs))
  for (b in beats) ecg[b + 110 + (-20:20)] <- 0.3 * exp(-((-20:20) / 8)^2)
  expect_equal(morie_dsp_pan_tompkins(numeric(100), fs), integer(0))
  tw <- morie_dsp_t_wave(ecg, beats, fs)
  ref <- vapply(beats[beats + 0.5 * fs <= n], function(l) l + 72L + which.max(ecg[(l + 72):(l + 180)]) - 1, 0)
  expect_equal(tw, ref)
  expect_equal(tw, beats[seq_along(tw)] + 110L)
})

test_that("dicrotic notch candidates are second-difference minima after systole", {
  fs <- 125
  tv <- (0:(2 * fs - 1)) / fs
  ph <- (tv %% 1)
  pulse <- exp(-((ph - 0.2) / 0.08)^2) + 0.4 * exp(-((ph - 0.45) / 0.06)^2)
  r <- morie_dsp_dicrotic_notch(pulse, fs)
  d2 <- diff(pulse, differences = 2)
  expect_true(all(r > as.integer(0.3 * fs)))
  expect_true(all(-d2[r] > -d2[r - 1] & -d2[r] >= -d2[r + 1]))
  expect_equal(morie_dsp_dicrotic_notch(rep(1, 50), fs), integer(0))
})

test_that("homomorphic filtering and the complex cepstrum", {
  x <- c(2, 3, 1.5, 4, 2.5, 3.5, 1, 2)
  expect_equal(morie_dsp_homomorphic(x, cutoff = 0), abs(x) + 1e-10, tolerance = 1e-12)
  # removing only the DC term of log|x| divides by the geometric mean
  h <- morie_dsp_homomorphic(x, cutoff = 0.05)
  expect_equal(h, x / exp(mean(log(x))), tolerance = 1e-9)
  xo <- x[-8]
  expect_equal(morie_dsp_homomorphic(xo, cutoff = 0.05), xo / exp(mean(log(xo))), tolerance = 1e-9)
  cc <- morie_dsp_complex_cepstrum(x)
  X <- fft(x)
  ph <- Arg(X)
  d <- diff(ph)
  d <- ifelse(d > pi, d - 2 * pi, ifelse(d < -pi, d + 2 * pi, d))
  uph <- c(ph[1], ph[1] + cumsum(d))
  expect_equal(cc$cepstrum, Re(fft(complex(real = log(Mod(X) + 1e-10), imaginary = uph), inverse = TRUE)) / 8, tolerance = 1e-12)
  expect_equal(cc$quefrency, 0:7)
})

test_that("Welch cross-spectral density and coherence", {
  n <- 512
  t <- 0:(n - 1)
  x <- sin(2 * pi * t / 16) + 0.3 * cos(2 * pi * t / 5)
  y <- 0.5 * sin(2 * pi * t / 16 + 0.7) + 0.2 * sin(2 * pi * t / 7)
  r <- morie_dsp_csd(x, y, fs = 2, nperseg = 64)
  w <- 0.54 - 0.46 * cos(2 * pi * (0:63) / 63)
  starts <- seq(1, n - 63, by = 32)
  P <- Reduce(`+`, lapply(starts, function(s) {
    X <- fft(x[s:(s + 63)] * w)[1:33]
    Y <- fft(y[s:(s + 63)] * w)[1:33]
    X * Conj(Y) / (sum(w^2) * 2)
  })) / length(starts)
  expect_equal(r$csd, P, tolerance = 1e-12)
  expect_equal(r$freqs, (0:32) * 2 / 64)
  cxx <- morie_dsp_csd(x, x, nperseg = 64)$csd
  expect_equal(Im(cxx), rep(0, 33), tolerance = 1e-12)
  co <- morie_dsp_coherence_spectrum(x, y, nperseg = 64)
  cyy <- morie_dsp_csd(y, y, nperseg = 64)$csd
  expect_equal(co$coh, Mod(morie_dsp_csd(x, y, nperseg = 64)$csd)^2 / (Re(cxx) * Re(cyy)), tolerance = 1e-9)
  expect_equal(morie_dsp_coherence_spectrum(x, 3 * x, nperseg = 64)$coh, rep(1, 33), tolerance = 1e-12)
})
