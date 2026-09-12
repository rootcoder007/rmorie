# Physiological signal analysis (Rangayyan, Biomedical Signal Analysis).
#
# Anchors outside the module: base R's fft for the radix-2 transform and
# acf for the autocorrelation, the analytic limits of the Hodgkin-Huxley
# rate constants at their removable singularities, and signals whose
# spectra are known in closed form (a pure sinusoid, a flat band, an
# all-pole filter) for everything built on top.

test_that("the radix-2 transform is base R's fft", {
  set.seed(3)
  for (n in c(2, 4, 8, 16, 64)) {
    x <- rnorm(n)
    g <- .bsafft(x, rep(0, n))
    r <- fft(x)
    expect_equal(g$re, Re(r))
    expect_equal(g$im, Im(r))
  }
  # a complex input is transformed too, not just a real one
  set.seed(5)
  re <- rnorm(8)
  im <- rnorm(8)
  g <- .bsafft(re, im)
  r <- fft(complex(real = re, imaginary = im))
  expect_equal(g$re, Re(r))
  expect_equal(g$im, Im(r))
  # a constant signal puts all its energy in the zero bin
  c8 <- .bsafft(rep(2, 8), rep(0, 8))
  expect_equal(c8$re[1], 16)
  expect_equal(c8$re[-1], rep(0, 7))
  expect_equal(c8$im, rep(0, 8))
  # a real input has a conjugate-symmetric transform
  x <- rnorm(16)
  g <- .bsafft(x, rep(0, 16))
  expect_equal(g$re[2:8], rev(g$re[10:16]))
  expect_equal(g$im[2:8], -rev(g$im[10:16]))
})

test_that("the autocorrelation is base R's covariance acf", {
  set.seed(3)
  for (n in c(16, 64)) {
    x <- rnorm(n)
    for (ml in c(1, 5, 12)) {
      got <- .bsaacf(x, ml)
      ref <- as.numeric(acf(x, lag.max = ml, type = "covariance",
                            demean = TRUE, plot = FALSE)$acf)
      expect_equal(got, ref)
    }
  }
  # lag zero is the biased variance of the mean-removed signal
  x <- c(1, 2, 3, 4)
  expect_equal(.bsaacf(x, 0), mean((x - mean(x))^2))
  # the lag is capped at n - 1 rather than running off the end
  expect_length(.bsaacf(x, 99), 4L)
  # a constant signal has no variance left after mean removal
  expect_equal(.bsaacf(rep(5, 6), 2), rep(0, 3))
  expect_error(.bsaacf(1, 0), "at least 2 samples")
})

test_that("the periodogram finds a sinusoid and respects the Nyquist limit", {
  fs <- 100
  n <- 256
  t <- (seq_len(n) - 1) / fs
  p <- .bsapsd(sin(2 * pi * 10 * t), fs)
  # one-sided spectrum: nfft/2 + 1 bins spanning 0 to fs/2
  expect_length(p$freqs, n / 2 + 1)
  expect_equal(p$freqs[1], 0)
  expect_equal(max(p$freqs), fs / 2)
  expect_equal(p$freqs[2] - p$freqs[1], fs / n)
  # the peak sits in the bin containing 10 Hz
  expect_true(abs(p$freqs[which.max(p$power)] - 10) < fs / n)
  expect_true(all(p$power >= 0))
  # a higher tone moves the peak
  p2 <- .bsapsd(sin(2 * pi * 30 * t), fs)
  expect_true(abs(p2$freqs[which.max(p2$power)] - 30) < fs / n)
  # detrending removes the constant, so a pure offset carries no zero-bin
  # power, and leaving it in does
  off <- .bsapsd(rep(3, 64) + sin(2 * pi * 10 * (seq_len(64) - 1) / fs), fs)
  no <- .bsapsd(rep(3, 64) + sin(2 * pi * 10 * (seq_len(64) - 1) / fs), fs,
                detrend = FALSE)
  expect_true(no$power[1] > off$power[1])
  expect_error(.bsapsd(c(1, 2, 3), 100), "at least 4 samples")
  expect_error(.bsapsd(rnorm(8), 0), "fs must be positive")
  expect_error(.bsapsd(rnorm(8), -1), "fs must be positive")
})

test_that("band power sums the half-open band", {
  fr <- c(0, 1, 2, 3, 4, 5)
  pw <- c(10, 1, 2, 3, 4, 5)
  expect_equal(.bsabandpow(fr, pw, 1, 4), 1 + 2 + 3)
  # the lower edge is included and the upper edge excluded
  expect_equal(.bsabandpow(fr, pw, 2, 3), 2)
  expect_equal(.bsabandpow(fr, pw, 0, 6), sum(pw))
  # an empty band carries no power
  expect_equal(.bsabandpow(fr, pw, 10, 20), 0)
  expect_equal(.bsabandpow(fr, pw, 3, 3), 0)
  # adjacent bands partition the total
  expect_equal(.bsabandpow(fr, pw, 0, 3) + .bsabandpow(fr, pw, 3, 6),
               sum(pw))
})

test_that("peak picking returns the strongest maxima, separated", {
  fr <- seq(0, 10, by = 1)
  # local maxima at 2 (power 9), 5 (power 7) and 8 (power 5)
  pw <- c(1, 2, 9, 2, 1, 7, 1, 2, 5, 2, 1)
  pk <- .bsapeaks(fr, pw, count = 3L)
  expect_equal(as.numeric(pk[[1]]), c(2, 5, 8))
  expect_equal(as.numeric(pk[[2]]), c(9, 7, 5))
  # asking for fewer returns the strongest ones only
  expect_equal(as.numeric(.bsapeaks(fr, pw, count = 1L)[[1]]), 2)
  expect_equal(as.numeric(.bsapeaks(fr, pw, count = 2L)[[1]]), c(2, 5))
  # a minimum separation suppresses the neighbours of a stronger peak
  far <- .bsapeaks(fr, pw, count = 3L, minsep = 4)
  expect_true(all(diff(sort(as.numeric(far[[1]]))) >= 4))
  # a monotone spectrum has no interior maximum
  expect_length(as.numeric(.bsapeaks(fr, seq_along(fr), count = 3L)[[1]]), 0L)
  # and a spectrum too short to have an interior point has none either
  expect_length(as.numeric(.bsapeaks(c(0, 1), c(1, 2), count = 2L)[[1]]), 0L)
})

test_that("the all-pole spectrum is the inverse squared filter response", {
  fs <- 100
  npts <- 64
  a <- c(-0.5, 0.2)
  got <- .bsalpcspec(a, fs, npts)
  f <- 0.5 * fs * (seq_len(npts) - 1) / (npts - 1)
  expect_equal(got$freqs, f)
  expect_equal(got$freqs[1], 0)
  expect_equal(max(got$freqs), fs / 2)
  # written out as 1 / |1 + sum a_k exp(-i w k)|^2
  w <- 2 * pi * f / fs
  den <- 1 + a[1] * exp(-1i * w) + a[2] * exp(-2i * w)
  expect_equal(got$power, 1 / Mod(den)^2)
  expect_true(all(got$power > 0))
  # an empty coefficient vector is the flat all-pass response
  expect_equal(.bsalpcspec(numeric(0), fs, 8)$power, rep(1, 8))
  # a single pole near the unit circle makes a sharp low-frequency peak
  sharp <- .bsalpcspec(-0.95, fs, 128)
  expect_equal(which.max(sharp$power), 1L)
})

test_that("spectral moments treat the periodogram as a density", {
  fr <- c(0, 1, 2, 3, 4)
  pw <- c(0, 0, 1, 0, 0)
  m <- .bsapsdmom(fr, pw)
  # all the mass at 2 Hz: mean and median are 2 and there is no spread
  expect_equal(m$total_power, 1)
  expect_equal(m$mean_freq_hz, 2)
  expect_equal(m$median_freq_hz, 2)
  expect_equal(m$fm2_hz2, 0)
  expect_equal(m$spread_hz, 0)
  expect_equal(m$spectral_skewness, 0)
  expect_equal(m$spectral_kurtosis, 0)
  # a symmetric two-line spectrum has zero skewness and a known spread
  pw2 <- c(0, 1, 0, 1, 0)
  m2 <- .bsapsdmom(fr, pw2)
  expect_equal(m2$mean_freq_hz, 2)
  expect_equal(m2$fm2_hz2, 1)
  expect_equal(m2$spread_hz, 1)
  expect_equal(m2$spectral_skewness, 0)
  expect_equal(m2$spectral_kurtosis, 1)
  # the mean is the power-weighted average and the median the half-power point
  pw3 <- c(1, 1, 1, 1, 4)
  m3 <- .bsapsdmom(fr, pw3)
  expect_equal(m3$total_power, 8)
  expect_equal(m3$mean_freq_hz, sum(fr * pw3) / 8)
  # cumulative power reaches half the total at the fourth bin, 3 Hz
  expect_equal(m3$median_freq_hz, 3)
  expect_error(.bsapsdmom(fr, rep(0, 5)), "zero total power")
})

test_that("the quality factor is the peak frequency over its bandwidth", {
  # a triangular peak of height 2 centred at 5 Hz; the first bins at or
  # below half power are 3 Hz below and 7 Hz above
  fr <- 0:10
  pw <- c(0, 0, 0, 1, 1.5, 2, 1.5, 1, 0, 0, 0)
  q <- .bsaqfactor(fr, pw, 5)
  expect_equal(q$bandwidth_hz, 7 - 3)
  expect_equal(q$q, 5 / (7 - 3))
  # a sharper peak has the larger quality factor
  sharp <- c(0, 0, 0, 0, 0.5, 2, 0.5, 0, 0, 0, 0)
  expect_true(.bsaqfactor(fr, sharp, 5)$q > q$q)
  # an empty spectrum has neither
  e <- .bsaqfactor(numeric(0), numeric(0), 1)
  expect_null(e$bandwidth_hz)
  expect_null(e$q)
})

test_that("the envelope is the short-time root mean square", {
  fs <- 10
  x <- c(rep(1, 10), rep(3, 10))
  e <- .bsaenvelope(x, fs, 1)
  # two non-overlapping one-second windows of ten samples each
  expect_equal(e$env, c(1, 3))
  expect_equal(e$step_s, 1)
  # the root mean square of a window, not its mean
  y <- c(0, 4, 0, 4)
  expect_equal(.bsaenvelope(y, 4, 1)$env, sqrt(mean(c(0, 16, 0, 16))))
  # a signal shorter than one window yields nothing
  expect_length(.bsaenvelope(c(1, 2), 10, 1)$env, 0L)
  # the window length is at least one sample however short the request
  expect_equal(.bsaenvelope(c(1, 2, 3), 10, 1e-9)$step_s, 1 / 10)
  expect_equal(.bsarms(c(3, 4)), sqrt((9 + 16) / 2))
  expect_equal(.bsarms(c(-2, 2)), 2)
  expect_error(.bsarms(numeric(0)), "empty signal")
})

test_that("phase unwrapping removes the branch cuts", {
  # every successive difference is brought into (-pi, pi]
  ph <- c(0, 3, -3, 0.5, 2.9, -2.9)
  u <- .bsaunwrap(ph)
  expect_equal(u[1], ph[1])
  expect_true(all(abs(diff(u)) <= pi + 1e-12))
  # the unwrapped phase agrees with the wrapped one modulo 2 pi
  expect_equal(((u - ph) / (2 * pi)) %% 1, rep(0, length(ph)),
               tolerance = 1e-12)
  # a smooth ramp that never wraps is returned untouched
  smooth <- seq(0, 3, by = 0.5)
  expect_equal(.bsaunwrap(smooth), smooth)
  # a sampled linear phase is recovered as a straight line
  true_ph <- seq(0, 8 * pi, length.out = 40)
  wrapped <- ((true_ph + pi) %% (2 * pi)) - pi
  expect_equal(.bsaunwrap(wrapped) - .bsaunwrap(wrapped)[1],
               true_ph - true_ph[1], tolerance = 1e-9)
  expect_length(.bsaunwrap(1), 1L)
})

test_that("the Hodgkin-Huxley rates take their limits at the singularities", {
  # alpha_n = 0.01 (v + 55) / (1 - exp(-(v + 55) / 10)) is 0/0 at v = -55;
  # its limit is 0.01 * 10 = 0.1
  expect_equal(.bsahhrates(-55)[["an"]], 0.1)
  # alpha_m = 0.1 (v + 40) / (1 - exp(-(v + 40) / 10)) is 0/0 at v = -40;
  # its limit is 0.1 * 10 = 1
  expect_equal(.bsahhrates(-40)[["am"]], 1)
  # and the substituted value is continuous with its neighbourhood
  for (d in c(1e-4, 1e-2)) {
    expect_equal(.bsahhrates(-55 + d)[["an"]], 0.1, tolerance = 1e-2)
    expect_equal(.bsahhrates(-40 + d)[["am"]], 1, tolerance = 1e-2)
  }
  # the published closed forms away from the singularities
  v <- -65
  r <- .bsahhrates(v)
  expect_equal(r[["bn"]], 0.125 * exp(-(v + 65) / 80))
  expect_equal(r[["bm"]], 4 * exp(-(v + 65) / 18))
  expect_equal(r[["ah"]], 0.07 * exp(-(v + 65) / 20))
  expect_equal(r[["bh"]], 1 / (1 + exp(-(v + 35) / 10)))
  expect_equal(r[["an"]], 0.01 * (v + 55) / (1 - exp(-(v + 55) / 10)))
  expect_equal(r[["am"]], 0.1 * (v + 40) / (1 - exp(-(v + 40) / 10)))
  # at rest the gates are named in the documented order and all positive
  expect_named(r, c("am", "bm", "ah", "bh", "an", "bn"))
  expect_true(all(r > 0))
  # the steady-state activation m_inf = am / (am + bm) rises with voltage
  minf <- function(v) {
    q <- .bsahhrates(v)
    q[["am"]] / (q[["am"]] + q[["bm"]])
  }
  expect_true(minf(-20) > minf(-65))
  expect_true(minf(-65) >= 0 && minf(-20) <= 1)
  # and the inactivation h_inf falls with voltage
  hinf <- function(v) {
    q <- .bsahhrates(v)
    q[["ah"]] / (q[["ah"]] + q[["bh"]])
  }
  expect_true(hinf(-20) < hinf(-65))
})
