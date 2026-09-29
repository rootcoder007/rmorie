# Coverage tests for R/aaa_rangayyan_qrs.R (Rangayyan 2024, ch. 3-4, 8-10).
# Filters and operators are checked against their difference equations
# evaluated with stats::filter or closed forms; detectors are checked on
# noise-free synthetic signals whose event positions are known.

qrs_fir <- function(x, b) {
  # causal FIR with zero initial conditions
  k <- length(b) - 1L
  as.numeric(stats::filter(c(rep(0, k), x), b, sides = 1))[-seq_len(k)]
}

qrs_mavg <- function(x, m) {
  vapply(seq_along(x), function(i) mean(x[max(1L, i - m + 1L):i]), 0)
}

qrs_ecg <- function(fs, dur, beats, shape = c(0.1, 0.4, 1.2, -0.5, 0.1, 0.05, 0)) {
  x <- numeric(round(dur * fs))
  pos <- round(beats * fs)
  for (i in pos) x[i + seq_along(shape) - 1L] <- shape
  list(x = x, pos = pos)
}

qrs_pulse <- function(fs, dur, period, lag = 0) {
  tv <- seq(0, dur, by = 1 / fs)
  ph <- ((tv - lag) %% period) / period
  exp(-((ph - 0.25) / 0.12)^2) + 0.35 * exp(-((ph - 0.55) / 0.05)^2)
}

test_that("Balda derivative operators follow eqs 4.1-4.3", {
  x <- c(0.2, -0.1, 0.7, 1.9, 0.4, -0.6, 0.3, 0.1, 0.05)
  n <- length(x)
  y0 <- c(0, 0, abs(x[3:n] - x[1:(n - 2)]))
  y1 <- c(rep(0, 4), abs(x[5:n] - 2 * x[3:(n - 2)] + x[1:(n - 4)]))
  expect_equal(QrsDeriv1(x)$y0, y0, tolerance = 1e-12)
  expect_equal(QrsDeriv2(x)$y1, y1, tolerance = 1e-12)
  expect_equal(QrsDerivMx(y0, y1)$y2, 1.3 * y0 + 1.1 * y1, tolerance = 1e-12)
  expect_error(QrsDeriv1(1:2), "at least 3")
  expect_error(QrsDeriv2(1:4), "at least 5")
  expect_error(QrsDerivMx(1:3, 1:2), "same length")
})

test_that("QrsDeriv finds each beat of a clean ECG", {
  fs <- 200
  shape <- c(0, 0.1, 0.3, 0.6, 1, 1.4, 1.2, 0.6, 0, -0.4, -0.5, -0.3, -0.1, 0)
  e <- qrs_ecg(fs, 4, c(0.4, 1.2, 2.0, 2.8, 3.6), shape)
  r <- QrsDeriv(e$x, fs = fs, thresh = 0.5)
  xn <- e$x / max(abs(e$x))
  n <- length(xn)
  y0 <- c(0, 0, abs(xn[3:n] - xn[1:(n - 2)]))
  y1 <- c(rep(0, 4), abs(xn[5:n] - 2 * xn[3:(n - 2)] + xn[1:(n - 4)]))
  expect_equal(r$y2, 1.3 * y0 + 1.1 * y1, tolerance = 1e-12)
  expect_equal(r$y3, qrs_mavg(r$y2, 8), tolerance = 1e-12)
  expect_length(r$qrs, 5L)
  expect_true(all(abs(r$qrs - (e$pos - 1L)) <= 8))
  expect_equal(r$hr, 60 * 5 / (n / fs), tolerance = 1e-12)
  # a 14-sample complex never keeps 6 of 8 samples above the default 1.0
  expect_length(QrsDeriv(e$x, fs = fs)$qrs, 0L)
  expect_error(QrsDeriv(numeric(40), fs), "identically zero")
  expect_error(QrsDeriv(e$x, fs, thresh = 0), "thresh")
})

test_that("Murthy-Rangaraj weighted squared derivative and MA smoothing", {
  x <- c(0.3, 0.1, 0.8, 1.5, -0.2, 0.4, 0.9, 0.2, -0.1, 0.6, 0.5)
  N <- 4L
  g <- numeric(length(x))
  for (nn in N:(length(x) - 1L)) {
    i <- 1:N
    g[nn + 1L] <- sum((x[nn - i + 2L] - x[nn - i + 1L])^2 * (N - i + 1))
  }
  expect_equal(QrsWSqDrv(x, N)$g1, g, tolerance = 1e-12)
  expect_error(QrsWSqDrv(x[1:3], 4), "longer than")
  expect_equal(QrsDrvSmth(g, 3)$g, qrs_mavg(g, 3), tolerance = 1e-12)
  expect_error(QrsDrvSmth(g, 0), "mwin")
})

test_that("Pan-Tompkins transfer functions match B(z)/A(z) and closed forms", {
  f <- c(0.5, 5, 11, 23.7, 60, 99)
  w <- 2 * pi * f / 200
  z <- exp(-1i * w)
  lp <- QrsLPassTf(f)
  H <- vapply(z, function(zz) sum(lp$b * zz^(0:12)) / sum(lp$a * zz^(0:2)), 0i)
  expect_equal(lp$mag, Mod(H), tolerance = 1e-9)
  expect_equal(lp$phase, Arg((vapply(z, function(zz) sum(zz^(0:5)), 0i))^2 / 32), tolerance = 1e-12)
  expect_equal(QrsLPassTf(0)$mag, 36 / 32, tolerance = 1e-12)
  hl <- QrsHpLpTf(f)
  expect_equal(hl$mag, abs(sin(16 * w) / sin(w / 2)), tolerance = 1e-12)
  expect_equal(QrsHpLpTf(0)$mag, 32)
  hp <- QrsHPassTf(f)
  Hh <- vapply(z, function(zz) zz^16 - sum(zz^(0:31)) / 32, 0i)
  expect_equal(hp$mag, Mod(Hh), tolerance = 1e-12)
  expect_equal(QrsHPassTf(0)$mag, 0, tolerance = 1e-12)
  expect_error(QrsLPassTf(numeric(0)), "frequency")
  expect_error(QrsHPassTf(1, fs = -1), "fs")
})

test_that("Pan-Tompkins difference equations equal stats::filter", {
  x <- sin((1:120) / 3) + ((1:120) %% 7) / 10
  lp <- QrsLPassDf(x)$y
  fir <- qrs_fir(x, c(1, rep(0, 5), -2, rep(0, 5), 1) / 32)
  expect_equal(lp, as.numeric(stats::filter(fir, c(2, -1), method = "recursive")), tolerance = 1e-9)
  expect_equal(QrsHpLpDf(x)$y, qrs_fir(x, rep(1, 32)), tolerance = 1e-12)
  hd <- QrsHPassDf(x)
  lag16 <- c(rep(0, 16), x)[seq_along(x)]
  expect_equal(hd$p, lag16 - qrs_fir(x, rep(1, 32)) / 32, tolerance = 1e-12)
  expect_equal(QrsHPassIo(x)$p, hd$p, tolerance = 1e-12)
  d <- QrsDerivOp(x)
  expect_equal(d$y, qrs_fir(x, d$b), tolerance = 1e-12)
  # zero on a constant; on a ramp of slope s the taps give -s sum(k b_k) = 1.25 s
  expect_equal(QrsDerivOp(rep(3, 20))$y[5:20], rep(0, 16), tolerance = 1e-12)
  expect_equal(QrsDerivOp(3 + 2 * (1:20))$y[5:20], rep(2.5, 16), tolerance = 1e-12)
  expect_error(QrsLPassDf(numeric(0)), "at least 1")
})

test_that("moving-window integrator, thresholds, search-back, rate", {
  x <- c(1, 4, 2, 8, 5, 7, 3, 6, 9, 0)
  expect_equal(QrsMwInt(x, 3)$y, qrs_mavg(x, 3), tolerance = 1e-12)
  m <- QrsMwInt(x, fs = 40)
  expect_equal(m$nwin, 6L)
  expect_equal(m$widthsec, 6 / 40)
  expect_equal(m$y, qrs_mavg(x, 6), tolerance = 1e-12)
  expect_error(QrsMwInt(x, 0), "nwin")
  s <- QrsThresh(2, 1, 0.4, TRUE)
  expect_equal(s$spki, 0.125 * 2 + 0.875 * 1, tolerance = 1e-12)
  expect_equal(s$thresh1, 0.4 + 0.25 * (s$spki - 0.4), tolerance = 1e-12)
  expect_equal(s$thresh2, s$thresh1 / 2, tolerance = 1e-12)
  nz <- QrsThresh(0.2, 1, 0.4, FALSE)
  expect_equal(nz$npki, 0.125 * 0.2 + 0.875 * 0.4, tolerance = 1e-12)
  expect_equal(nz$spki, 1)
  expect_equal(QrsSpkiUpd(3, 1)$spki, 1.5, tolerance = 1e-12)
  expect_equal(HrFromCnt(18, 15)$hr, 72, tolerance = 1e-12)
  expect_error(HrFromCnt(-1, 10), "non-negative")
  expect_error(HrFromCnt(3, 0), "duration")
})

test_that("QrsDetect chains the named equations and finds every beat", {
  fs <- 200
  e <- qrs_ecg(fs, 6, seq(0.5, 5.7, by = 0.8))
  r <- QrsDetect(e$x, fs)
  bp <- QrsHPassIo(QrsLPassDf(e$x)$y)$p
  expect_equal(r$bandpass, bp, tolerance = 1e-12)
  sq <- QrsDerivOp(bp)$y^2
  expect_equal(r$integrated, QrsMwInt(sq, fs = fs)$y, tolerance = 1e-12)
  expect_equal(r$delay, 23L + 30L %/% 2L)
  expect_length(r$qrs, length(e$pos))
  expect_true(all(abs(r$qrs - (e$pos - 1L)) <= round(0.05 * fs)))
  expect_equal(r$rr, diff(r$qrs) / fs, tolerance = 1e-12)
  expect_equal(r$hr, 60 * length(r$qrs) / 6, tolerance = 1e-12)
  expect_error(QrsDetect(numeric(20), fs), "at least 40")
})

test_that("baseline-wander filter is the eq 3.132 recursion", {
  x <- 1 + sin((1:50) / 4) + (1:50) / 25
  r <- BlWander(x, fs = 100, pole = 0.9)
  y <- as.numeric(stats::filter(100 * c(0, diff(x)), 0.9, method = "recursive"))
  expect_equal(r$ecg_detrended, y, tolerance = 1e-12)
  H <- function(f) {
    z <- exp(-1i * 2 * pi * f / 100)
    100 * Mod((1 - z) / (1 - 0.9 * z))
  }
  expect_equal(r$gain_at_half_hz, H(0.5), tolerance = 1e-12)
  expect_equal(r$gain_at_nyquist, H(50), tolerance = 1e-12)
  expect_equal(r$gain_dc, 0)
  expect_true(r$dc_is_rejected)
  expect_error(BlWander(x, 100, pole = 1), "unit circle")
  expect_error(BlWander(1, 100), "two samples")
})

test_that("power-line notch is the normalised eq 3.150 FIR", {
  fs <- 360
  tv <- (0:359) / fs
  x <- 0.3 + sin(2 * pi * 60 * tv) + 0.5 * sin(2 * pi * 120 * tv)
  r <- PLineNotch(x, fs, f0 = 60, harmonics = 2)
  b1 <- c(1, -2 * cos(2 * pi * 60 / fs), 1) / (2 - 2 * cos(2 * pi * 60 / fs))
  b2 <- c(1, -2 * cos(2 * pi * 120 / fs), 1) / (2 - 2 * cos(2 * pi * 120 / fs))
  expect_equal(r$coeffs, list(b1, b2), tolerance = 1e-12)
  expect_equal(r$y, qrs_fir(qrs_fir(x, b1), b2), tolerance = 1e-12)
  # both zeros lie on the unit circle: after the 4-sample transient only
  # the DC term survives, at unit gain
  expect_equal(r$y[5:360], rep(0.3, 356), tolerance = 1e-9)
  expect_equal(PLineNotch(x, fs, 150, harmonics = 3)$notched, 150)
  expect_error(PLineNotch(x, fs, 200), "Nyquist")
  expect_error(PLineNotch(x, fs, 60, 0), "harmonics")
})

test_that("time-domain HRV equals the Task Force definitions", {
  rr <- c(0.81, 0.79, 0.86, 0.92, 0.84, 0.78, 0.88, 0.95)
  r <- HrvTime(rr)
  d <- diff(rr * 1000)
  expect_equal(r$sdnn, sd(rr * 1000), tolerance = 1e-12)
  expect_equal(r$rmssd, sqrt(mean(d^2)), tolerance = 1e-12)
  expect_equal(r$nn50, sum(abs(d) > 50))
  expect_equal(r$pnn50, 100 * sum(abs(d) > 50) / 7, tolerance = 1e-12)
  expect_equal(r$meanhr, 60000 / mean(rr * 1000), tolerance = 1e-12)
  expect_error(HrvTime(c(0.8, -0.1)), "positive")
})

test_that("frequency-domain HRV integrates the interpolated periodogram", {
  k <- 0:99
  rr <- 0.8 + 0.05 * sin(2 * pi * 0.1 * k * 0.8) + 0.03 * sin(2 * pi * 0.3 * k * 0.8)
  spec <- function(bands) {
    tt <- cumsum(rr)
    m <- trunc(tt[100] * 4)
    g <- stats::approx(tt, rr, xout = (0:(m - 1)) / 4, rule = 2)$y
    g <- g - mean(g)
    P <- Mod(fft(g))^2 / (4 * m)
    kk <- 0:(m %/% 2)
    P <- P[kk + 1] * ifelse(kk > 0 & kk < m - kk, 2, 1)
    fr <- 4 * kk / m
    vapply(bands, function(b) sum(P[fr > b[1] & fr <= b[2]]) * (4 / m), 0)
  }
  r <- HrvFreq(rr)
  # direct O(n^2) DFT vs FFT: rounding differs at ~1e-13, compared at 1e-9
  ref <- spec(list(c(0, 0.04), c(0.04, 0.15), c(0.15, 0.4)))
  expect_equal(c(r$vlf, r$lf, r$hf), ref, tolerance = 1e-9)
  expect_equal(r$lfpct + r$hfpct + r$vlfpct, 100, tolerance = 1e-12)
  expect_equal(r$lfhf, r$lf / r$hf, tolerance = 1e-12)
  rb <- HrvFreq(rr, bands = "bianchi")
  expect_equal(c(rb$vlf, rb$lf, rb$hf), spec(list(c(0, 0.03), c(0.03, 0.15), c(0.18, 0.4))), tolerance = 1e-9)
  expect_error(HrvFreq(rr, bands = "x"), "taskforce")
  lh <- LfHfRatio(rr)
  expect_equal(lh$lfhf, r$lfhf, tolerance = 1e-12)
  expect_equal(lh$rrvar, var(rr), tolerance = 1e-12)
  expect_error(LfHfRatio(rr[1:4]), "at least 8")
})

test_that("ECG-EMG coupling correlates per-cycle HR with EMG RMS and mean frequency", {
  fs <- 200
  q <- c(40L, 200L, 340L, 470L, 590L, 700L)
  n <- 760
  ecg <- numeric(n)
  ecg[q + 1L] <- 1
  tv <- 0:(n - 1)
  emg <- sin(2 * pi * 13 * tv / fs) * (1 + tv / n) + 0.5 * cos(2 * pi * 37 * tv / fs)
  r <- EcgEmgCpl(ecg, emg, q, fs)
  hr <- rms <- mnf <- numeric(0)
  for (k in 2:6) {
    seg <- emg[(q[k - 1] + 1):q[k]]
    seg <- seg - mean(seg)
    L <- length(seg)
    P <- Mod(fft(seg))^2
    kk <- 0:(L %/% 2)
    P <- P[kk + 1] * ifelse(kk > 0 & kk < L - kk, 2, 1)
    hr <- c(hr, 60 * fs / (q[k] - q[k - 1]))
    rms <- c(rms, sqrt(mean(seg^2)))
    mnf <- c(mnf, sum(fs * kk / L * P) / sum(P))
  }
  expect_equal(r$hr, hr, tolerance = 1e-12)
  expect_equal(r$rms, rms, tolerance = 1e-12)
  expect_equal(r$meanfreq, mnf, tolerance = 1e-9)
  expect_equal(r$rrms, cor(hr, rms), tolerance = 1e-9)
  expect_equal(r$rmnf, cor(hr, mnf), tolerance = 1e-9)
  expect_error(EcgEmgCpl(ecg, emg[-1], q, fs), "same length")
  expect_error(EcgEmgCpl(ecg, emg, q[1:2], fs), "three QRS")
})

test_that("ECG features measure amplitudes against the PQ reference", {
  fs <- 200
  n <- 800
  x <- rep(0.1, n)
  q <- c(200L, 600L)
  for (p in q) {
    x[p + 1:5 - 3L] <- 0.1 + c(-0.2, 0.3, 1.5, -0.4, 0)
    x[p - 30L + (-6:6)] <- 0.1 + 0.2 * exp(-((-6:6) / 3)^2)
    x[p + 50L + (-10:10)] <- 0.1 + 0.4 * exp(-((-10:10) / 5)^2)
  }
  r <- EcgFeat(x, q, fs)
  expect_equal(r$ramp, c(1.5, 1.5), tolerance = 1e-12)
  expect_equal(r$qamp, c(-0.2, -0.2), tolerance = 1e-12)
  expect_equal(r$samp, c(-0.4, -0.4), tolerance = 1e-12)
  expect_equal(r$tamp, c(0.4, 0.4), tolerance = 1e-12)
  expect_equal(r$pamp, c(0.2, 0.2), tolerance = 1e-12)
  expect_equal(r$rampmean, 1.5, tolerance = 1e-12)
  expect_equal(r$nbeats, 2L)
  expect_true(all(r$qrsdur > 0 & r$qtdur > r$qrsdur))
  expect_error(EcgFeat(x, integer(0), fs), "QRS")
})

test_that("ECG waveshape duration and ST rules", {
  a <- EcgWaveShp(0.110, 0.15, rdur = 0.07, sdur = 0.03, qpresent = FALSE)
  expect_equal(a$qrsdurms, 110, tolerance = 1e-12)
  expect_true(a$lbbbdur && a$rbbbdur)
  expect_false(a$qrswide)
  expect_identical(a$stfinding, "elevated")
  expect_true(a$rdurok)
  expect_false(a$sdurok)
  expect_true(a$qabsent)
  expect_length(a$required, 2L)
  b <- EcgWaveShp(0.095, -0.2)
  expect_false(b$lbbbdur)
  expect_true(b$rbbbdur)
  expect_identical(b$stfinding, "depressed")
  expect_null(b$rdurok)
  expect_identical(EcgWaveShp(0.13, 0)$stfinding, "isoelectric")
  expect_true(EcgWaveShp(0.13, 0)$qrswide)
  expect_error(EcgWaveShp(0, 0), "positive")
})

test_that("exercise ST level and slope after the J point", {
  fs <- 250
  n <- 1000
  x <- numeric(n)
  q <- c(200L, 600L)
  jpt <- round(0.04 * fs)
  off <- round(0.06 * fs)
  span <- round(0.04 * fs)
  for (p in q) x[p + 1:120] <- 0.15 - 0.002 * (1:120)
  r <- ExerEcgSt(x, q, fs)
  m <- q + jpt + off
  expect_equal(r$stdev, x[m + 1], tolerance = 1e-12)
  expect_equal(r$stslope, (x[m + span + 1] - x[m + 1]) * fs / span, tolerance = 1e-12)
  expect_equal(r$pattern, c("downsloping", "downsloping"))
  expect_equal(r$flagged, sum(abs(x[m + 1]) >= 0.1))
  expect_error(ExerEcgSt(x, 990L, fs), "J point")
  expect_error(ExerEcgSt(x, q, fs, jofs = 0), "jofs")
})

test_that("dicrotic notch operator is eqs 4.22-4.23", {
  fs <- 250
  cp <- qrs_pulse(fs, 2.4, 0.8)
  n <- length(cp)
  r <- DicNotch(cp, fs)
  p <- numeric(n)
  i <- 3:(n - 2)
  p[i] <- 2 * cp[i - 2] - cp[i - 1] - 2 * cp[i] - cp[i + 1] + 2 * cp[i + 2]
  expect_equal(r$p, p, tolerance = 1e-12)
  expect_equal(r$s, qrs_fir(p^2, 16:1), tolerance = 1e-12)
  expect_equal(DNotchSmth(p, 16)$s, r$s, tolerance = 1e-12)
  expect_equal(DNotchSmth(p, 16)$weights, 16:1)
  # notch rule: peaks of s above a quarter of its maximum, strictly larger
  # than the 50 ms guard on each side, alternate upstroke / notch; each
  # notch is the minimum of cp within +-20 ms of its s peak
  guard <- round(0.05 * fs)
  tol <- round(0.02 * fs)
  pk <- which(vapply(seq_len(n), function(i) {
    nb <- setdiff(max(1, i - guard):min(n, i + guard), i)
    r$s[i] > 0.25 * max(r$s) && all(r$s[i] > r$s[nb])
  }, TRUE)) - 1
  expect_equal(r$upstroke, pk[c(TRUE, FALSE)])
  notch <- vapply(pk[c(FALSE, TRUE)], function(c0) {
    lo <- max(0, c0 - tol)
    lo + which.min(cp[(lo + 1):min(n, c0 + tol + 1)]) - 1
  }, 0)
  expect_equal(r$notch, notch)
  expect_error(DicNotch(cp[1:5], fs), "at least 9")
  expect_error(DNotchSmth(p, 0), "mwin")
})

test_that("carotid pulse features: intervals and rate corrections", {
  fs <- 250
  cp <- qrs_pulse(fs, 2.4, 0.8)
  q <- as.integer(round(c(0.02, 0.82, 1.62) * fs))
  r <- CPulseFeat(cp, fs, q)
  hr <- 60 * fs / mean(diff(q))
  expect_equal(r$hr, hr, tolerance = 1e-12)
  dn <- DicNotch(cp, fs, qrs = q)
  expect_equal(r$notch, dn$notch)
  k <- seq_len(min(length(dn$notch), length(dn$upstroke)))
  expect_equal(r$et, 1000 * (dn$notch[k] - dn$upstroke[k]) / fs, tolerance = 1e-12)
  expect_equal(r$etc, r$et + 1.6 * hr, tolerance = 1e-12)
  expect_equal(r$pepc, r$pep + 0.4 * hr, tolerance = 1e-12)
  expect_equal(r$etmean, mean(r$et), tolerance = 1e-12)
  expect_equal(CPulseFeat(cp, fs, q[1], hr = 70)$hr, 70)
  expect_error(CPulseFeat(cp, fs, q[1]), "hr must be given")
})

test_that("heart sounds and PCG segmentation compose the QRS and notch detectors", {
  fs <- 250
  beats <- seq(0.4, 2.8, by = 0.8)
  e <- qrs_ecg(fs, 3 + 1 / fs, beats, c(0.2, 1.2, -0.5, 0.1, 0.05, 0))
  cp <- qrs_pulse(fs, 3, 0.8, lag = 0.45)
  ecg <- e$x[seq_along(cp)]
  h <- HSoundId(ecg, cp, fs)
  q <- QrsDetect(ecg, fs)$qrs
  expect_equal(h$s1, q)
  expect_equal(h$notch, DicNotch(cp, fs, qrs = q)$notch)
  expect_equal(h$s2, pmax(0L, h$notch - round(0.0526 * fs)))
  tv <- (seq_along(cp) - 1) / fs
  pcg <- sin(2 * pi * 60 * tv) * exp(-((tv %% 0.8) / 0.06))
  pp <- PcgParts(pcg, ecg, cp, fs)
  expect_equal(pp$s1, h$s1)
  for (k in seq_along(pp$systole)) {
    ab <- pp$systole[[k]]
    expect_equal(ab[2], min(h$s2[h$s2 > ab[1]]))
    expect_equal(pp$systolerms[k], sqrt(mean(pcg[(ab[1] + 1):ab[2]]^2)), tolerance = 1e-12)
  }
  for (k in seq_along(pp$diastole)) {
    ab <- pp$diastole[[k]]
    expect_equal(pp$diastolerms[k], sqrt(mean(pcg[(ab[1] + 1):ab[2]]^2)), tolerance = 1e-12)
  }
  expect_error(HSoundId(ecg, cp[-1], fs), "same length")
  expect_error(PcgParts(pcg[-1], ecg, cp, fs), "same length")
})

test_that("maternal ECG cancellation is normalised LMS from zero weights", {
  n <- 120
  thor <- sin(2 * pi * (1:n) / 17) + 0.3 * cos(2 * pi * (1:n) / 5)
  abd <- 0.6 * thor + 0.2 * sin(2 * pi * (1:n) / 7)
  r <- MEcgFilt(abd, thor, order = 4, mu = 0.1)
  w <- numeric(4)
  err <- est <- numeric(n)
  eps <- 1e-3 * 4 * mean(thor^2)
  for (i in 1:n) {
    xv <- vapply(0:3, function(k) if (i - k >= 1) thor[i - k] else 0, 0)
    est[i] <- sum(w * xv)
    err[i] <- abd[i] - est[i]
    w <- w + 0.1 / (sum(xv^2) + eps) * err[i] * xv
  }
  expect_equal(r$fetal, err, tolerance = 1e-12)
  expect_equal(r$maternal, est, tolerance = 1e-12)
  expect_equal(r$weights, w, tolerance = 1e-12)
  expect_error(MEcgFilt(abd, thor, mu = 2), "mu")
  expect_error(MEcgFilt(abd[1:4], thor[1:4], order = 4), "longer")
})

test_that("motion artifact: window flagged and bridged linearly", {
  fs <- 20
  x <- rep(c(0.1, -0.1), 100)
  x[81:100] <- x[81:100] * 30
  r <- MotionArt(x, fs, win = 1, factor = 4)
  expect_equal(r$nsegments, 1L)
  expect_equal(r$artifact[[1]], c(80, 100))
  t <- (1:20) / 21
  expect_equal(r$clean[81:100], x[80] * (1 - t) + x[101] * t, tolerance = 1e-12)
  expect_equal(r$clean[-(81:100)], x[-(81:100)])
  expect_equal(r$fraction, 20 / 200, tolerance = 1e-12)
  expect_error(MotionArt(x, fs, factor = 1), "factor")
})

test_that("PPG features: systolic peak, foot, amplitude, perfusion index", {
  fs <- 100
  ppg <- 2 + qrs_pulse(fs, 4, 0.8)
  r <- PpgFeat(ppg, fs)
  expect_equal(r$amplitude, ppg[r$systolic + 1] - ppg[r$onset + 1], tolerance = 1e-12)
  expect_equal(r$ac, mean(r$amplitude), tolerance = 1e-12)
  expect_equal(r$dc, mean(ppg), tolerance = 1e-12)
  expect_equal(r$pi, 100 * r$ac / r$dc, tolerance = 1e-12)
  expect_equal(r$rate, 60 * length(r$systolic) / (length(ppg) / fs), tolerance = 1e-12)
  tv <- (seq_along(ppg) - 1) / fs
  expect_true(all(abs(((tv[r$systolic + 1] %% 0.8) / 0.8) - 0.25) < 0.05))
  expect_error(PpgFeat(ppg[1:20], fs), "at least 32")
})

test_that("P-wave detector locates P waves between beats", {
  fs <- 250
  n <- fs * 4
  x <- numeric(n)
  q <- as.integer(round(seq(0.5, 3.5, by = 0.75) * fs))
  pc <- q - round(0.15 * fs)
  for (k in seq_along(q)) {
    x[pc[k] + (-6:6)] <- 0.15 * exp(-((-6:6) / 2.5)^2)
    x[q[k] + 0:5] <- c(0.2, 1.2, -0.5, 0.1, 0.05, 0)
  }
  r <- PWaveDet(x, q, fs)
  found <- unlist(r$p)
  expect_length(found, length(q) - 1L)
  expect_true(all(abs(found - (pc[-1] - 1)) <= round(0.03 * fs)))
  expect_error(PWaveDet(x, q[1], fs), "two QRS")
  expect_error(PWaveDet(x, q, 20), "22 Hz")
})

test_that("length transformation is the windowed multichannel arc length", {
  a <- c(0, 0.5, 1.5, 1, 0.2, 0, 0.4, 0.9)
  b <- c(1, 1.2, 0.8, 0.3, 0.3, 0.6, 0.1, 0)
  r <- LengthXfm(list(a, b), wwin = 0.3, fs = 10)
  st <- sqrt(diff(a)^2 + diff(b)^2)
  w <- 3
  ref <- numeric(8)
  for (j in 0:(7 - w)) ref[j + 1] <- sum(st[(j + 1):(j + w)])
  ref[1] <- sum(st[1:w])
  expect_equal(r$length, ref, tolerance = 1e-12)
  expect_equal(r$wsamp, 3L)
  expect_error(LengthXfm(list(a, b[-1]), 0.3, 10), "same length")
  expect_error(LengthXfm(list(a), 0, 10), "wwin")
})

test_that("T-wave detection with the length transform", {
  fs <- 250
  n <- fs * 3
  x <- numeric(n)
  q <- as.integer(round(seq(0.5, 2.3, by = 0.6) * fs))
  tc <- q + round(0.25 * fs)
  for (k in seq_along(q)) {
    x[q[k] + 0:5] <- c(0.2, 1.2, -0.5, 0.1, 0.05, 0)
    x[tc[k] + (-12:12)] <- 0.3 * exp(-((-12:12) / 5)^2)
  }
  r <- TWaveDet(list(x, 0.5 * x), q, fs)
  expect_equal(r$nchan, 2L)
  expect_true(all(abs(unlist(r$t) - (tc - 1)) <= 2))
  expect_true(all(unlist(r$onset) < unlist(r$t)))
  expect_error(TWaveDet(list(x), integer(0), fs), "QRS")
  expect_error(TWaveDet(list(x), q, fs, tdur = 0), "tdur")
})

test_that("ECG-derived respiration recovers the modulation rate", {
  fs <- 100
  bt <- seq(0.5, 29.5, by = 0.5)
  q <- as.integer(round(bt * fs))
  amp <- 1 + 0.2 * sin(2 * pi * 0.25 * bt)
  x <- numeric(31 * fs)
  for (k in seq_along(q)) x[q[k] + 0:2] <- amp[k] * c(0.5, 1, 0.3)
  r <- EdrSignal(x, q, fs)
  expect_equal(r$amp, amp, tolerance = 1e-12)
  expect_equal(r$times, q / fs, tolerance = 1e-12)
  m <- trunc((r$times[length(r$times)] - r$times[1]) * 4)
  g <- stats::approx(r$times, amp, xout = r$times[1] + (0:(m - 1)) / 4)$y
  expect_equal(r$edr, g - mean(g), tolerance = 1e-12)
  expect_equal(r$resprate, 15, tolerance = 60 * 4 / m)
  expect_error(EdrSignal(x, q[1:5], fs), "eight beats")
})

test_that("sleep apnea scoring needs a pause and a desaturation", {
  fs <- 4
  n <- fs * 600
  edr <- sin(2 * pi * 0.25 * (1:n) / fs)
  edr[800:1000] <- 0
  spo2 <- rep(97, n)
  spo2[850:1100] <- 92
  r <- ApneaEdr(edr, spo2, fs)
  expect_equal(r$nevents, 1L)
  expect_equal(r$desatdepth, 5)
  expect_equal(r$hours, 600 / 3600, tolerance = 1e-12)
  expect_equal(r$ahi, 1 / r$hours, tolerance = 1e-12)
  s2 <- spo2
  s2[] <- 97
  expect_equal(ApneaEdr(edr, s2, fs)$nevents, 0L)
  expect_error(ApneaEdr(edr, spo2[-1], fs), "same length")
  expect_error(ApneaEdr(edr, spo2, fs, desat = 0), "desat")
})

test_that("spectral T-wave alternans from the beat-series periodogram", {
  base <- exp(-((1:20 - 10) / 4)^2)
  jit <- c(0.013, -0.021, 0.008, 0.017, -0.011, 0.004, -0.019, 0.015, -0.006, 0.02, -0.003, 0.009)
  tw <- lapply(1:12, function(k) base * (1 + 0.05 * (-1)^k) + jit[k] * cos(1:20))
  r <- TwaSpectr(tw, 0.1, 0.45)
  m <- 12
  acc <- numeric(m / 2 + 1)
  for (j in 1:20) {
    s <- vapply(tw, function(b) b[j], 0)
    P <- Mod(fft(s - mean(s)))^2 / m^2
    kk <- 0:(m / 2)
    acc <- acc + P[kk + 1] * ifelse(kk > 0 & kk < m - kk, 2, 1)
  }
  acc <- acc / 20
  cyc <- (0:(m / 2)) / m
  band <- which(cyc >= 0.1 & cyc <= 0.45)
  expect_equal(r$altpower, acc[m / 2 + 1], tolerance = 1e-9)
  expect_equal(r$noisemean, mean(acc[band]), tolerance = 1e-9)
  expect_equal(r$noisesd, sd(acc[band]), tolerance = 1e-9)
  expect_equal(r$valt, sqrt(acc[m / 2 + 1] - mean(acc[band])), tolerance = 1e-9)
  expect_equal(TwaSpectr(c(tw, list(base)), 0.1, 0.45)$nbeats, 12L)
  expect_error(TwaSpectr(tw[1:6]), "eight beats")
  expect_error(TwaSpectr(tw, 0.4, 0.3), "noise band")
})

test_that("VF heuristic flags a quasi-sinusoid and not a paced ECG", {
  fs <- 250
  tv <- (0:1999) / fs
  vf <- VfDetect(sin(2 * pi * 5 * tv), fs)
  expect_equal(vf$nwin, 2L)
  expect_equal(vf$domfreq, c(5, 5))
  expect_true(all(vf$flag))
  expect_equal(vf$fraction, 1)
  e <- qrs_ecg(fs, 8, seq(0.3, 7.5, by = 0.8), c(0.2, 1.2, -0.5, 0.1, 0.05, 0))
  ne <- VfDetect(e$x, fs)
  expect_false(any(ne$flag))
  expect_error(VfDetect(sin(tv[1:100]), fs), "shorter than one")
  expect_error(VfDetect(sin(2 * pi * 5 * tv), fs, conc = 0), "conc")
})
