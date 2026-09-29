# Coverage for the Rangayyan time-frequency family. Every expectation is
# rebuilt in the test from its defining formula: the analytic signal and
# spectra from stats::fft, Haar filter banks and a-trous steps from their
# closed forms, the natural spline from stats::splinefun, and the echo
# family from eqs (4.74)-(4.85).

.an <- function(x) {
  n <- length(x)
  h <- numeric(n)
  h[1L] <- 1
  if (n %% 2L == 0L) {
    h[n / 2 + 1L] <- 1
    if (n / 2 >= 2L) h[2:(n / 2)] <- 2
  } else {
    h[2:((n + 1) / 2)] <- 2
  }
  stats::fft(stats::fft(x) * h, inverse = TRUE) / n
}

.wvd <- function(x, nf = length(x)) {
  z <- .an(x)
  n <- length(x)
  out <- matrix(0, n, nf)
  for (i in seq_len(n)) {
    m <- min(i - 1L, n - i)
    tau <- (-m):m
    ker <- z[i + tau] * Conj(z[i - tau])
    for (k in seq_len(nf)) out[i, k] <- 2 * Re(sum(ker * exp(-2i * pi * (k - 1) * tau / nf)))
  }
  out
}

# one Haar analysis step: pairwise sums and differences over sqrt(2)
.haar_step <- function(a) {
  if (length(a) %% 2L == 1L) a <- c(a, a[length(a)])
  o <- a[c(TRUE, FALSE)]
  e <- a[c(FALSE, TRUE)]
  list(lo = (o + e) / sqrt(2), hi = (o - e) / sqrt(2))
}

# Haar a-trous level: filter taps two^(lev-1) apart, periodic
.haar_swt <- function(x, levels) {
  n <- length(x)
  a <- x
  det <- vector("list", levels)
  for (lev in seq_len(levels)) {
    s <- 2^(lev - 1)
    nb <- a[((seq_len(n) - 1 + s) %% n) + 1]
    det[[lev]] <- (a - nb) / sqrt(2)
    a <- (a + nb) / sqrt(2)
  }
  list(approx = a, details = det)
}

.mother <- function(name, t, w0 = 5) {
  switch(name,
    mexh = as.complex((1 - t^2) * exp(-t^2 / 2)),
    morlet = (exp(1i * w0 * t) - exp(-w0^2 / 2)) * exp(-t^2 / 2) / pi^0.25,
    haar = as.complex(ifelse(t >= 0 & t < 0.5, 1, ifelse(t >= 0.5 & t < 1, -1, 0)))
  )
}

# eq (8.107), truncated at the documented support radius of each wavelet
.cwt <- function(x, s, name, w0 = 5) {
  rad <- c(morlet = 4, mexh = 5, haar = 1)[[name]]
  n <- length(x)
  half <- trunc(rad * s) + 1
  vapply(seq_len(n), function(tau) {
    idx <- max(1, tau - half):min(n, tau + half)
    sum(x[idx] * Conj(.mother(name, (idx - tau) / s, w0))) / sqrt(s)
  }, complex(1))
}

.popcor <- function(u, w) {
  mu <- mean(u)
  mw <- mean(w)
  mean((u - mu) * (w - mw)) / sqrt(mean((u - mu)^2) * mean((w - mw)^2))
}

.x64 <- function(seed = 3L) {
  set.seed(seed)
  i <- 0:63
  sin(2 * pi * 3 * i / 64) + 0.6 * cos(2 * pi * 11 * i / 64 + 0.4) + 0.3 * stats::rnorm(64)
}

test_that("CDemod recovers the amplitude and phase of an on-bin cosine", {
  i <- 0:63
  x <- 1.5 * cos(2 * pi * 8 * i / 64 + 0.7)
  r <- CDemod(x, fs = 64)
  expect_equal(r$f0, 8, tolerance = 1e-12)
  expect_equal(r$bandwidth, 64 / 16, tolerance = 1e-12)
  expect_equal(r$amplitude, rep(1.5, 64), tolerance = 1e-12)
  expect_equal(r$phase, rep(0.7, 64), tolerance = 1e-12)
  expect_equal(r$mean_amplitude, 1.5, tolerance = 1e-12)
  r2 <- CDemod(x, fs = 64, f0 = 8, bandwidth = 2)
  expect_equal(r2$demodulated, rep(1.5 * exp(0.7i), 64), tolerance = 1e-12)
  expect_error(CDemod(x, fs = 64, f0 = 40), "Nyquist")
  expect_error(CDemod(x, fs = 64, f0 = 8, bandwidth = 9), "not smaller than f0")
  expect_error(CDemod(1:3), "at least 4 samples")
})

test_that("BiorDwt is the CDF 5/3 lifting scheme and reconstructs exactly", {
  x <- c(2, -1, 4, 3, 0.5, 7, -2, 1, 5)
  lift <- function(a) {
    if (length(a) %% 2L == 1L) a <- c(a, a[length(a)])
    h <- length(a) / 2
    s <- a[2 * seq_len(h) - 1]
    d <- a[2 * seq_len(h)]
    d0 <- d
    for (k in seq_len(h)) d0[k] <- d[k] - 0.5 * (s[k] + s[min(k + 1, h)])
    s0 <- s
    for (k in seq_len(h)) s0[k] <- s[k] + 0.25 * (d0[max(k - 1, 1)] + d0[k])
    list(s = s0, d = d0)
  }
  r1 <- BiorDwt(x, levels = 1)
  l1 <- lift(x)
  expect_equal(r1$approx, l1$s, tolerance = 1e-12)
  expect_equal(r1$details[[1L]], l1$d, tolerance = 1e-12)
  r3 <- BiorDwt(x, wavelet = "cdf53", levels = 3)
  l2 <- lift(l1$s)
  l3 <- lift(l2$s)
  expect_equal(r3$approx, l3$s, tolerance = 1e-12)
  expect_equal(r3$details, list(l3$d, l2$d, l1$d), tolerance = 1e-12)
  expect_equal(r3$lengths, c(3L, 5L, 9L))
  expect_equal(r3$reconstructed, x, tolerance = 1e-12)
  expect_lt(r3$max_reconstruction_error, 1e-12)
  expect_error(BiorDwt(x, wavelet = "bior4.4"), "only the 5/3")
  expect_error(BiorDwt(x, levels = 0), "levels must be >= 1")
})

test_that("ExpKerTfd tends to the pseudo-WVD as sigma grows", {
  x <- .x64()[1:16]
  r <- ExpKerTfd(x, fs = 2, sigma = 1e6)
  z <- .an(x)
  n <- 16L
  ml <- n %/% 4L
  ref <- matrix(0, n, n)
  for (i in seq_len(n)) {
    tau <- (-ml):ml
    tau <- tau[i + tau >= 1 & i + tau <= n & i - tau >= 1 & i - tau <= n]
    ker <- z[i + tau] * Conj(z[i - tau])
    for (k in seq_len(n)) ref[i, k] <- 2 * Re(sum(ker * exp(-2i * pi * (k - 1) * tau / n)))
  }
  expect_equal(r$tfd, ref, tolerance = 1e-12)
  expect_identical(r$maxlag, ml)
  expect_equal(r$freqs, (0:15) * 2 / 32, tolerance = 1e-12)
  expect_equal(r$times, (0:15) / 2, tolerance = 1e-12)
  expect_equal(r$peak_freq, r$freqs[which.max(colSums(ref))])
  expect_equal(r$crossterm_ratio, sum(-ref[ref < 0]) / sum(abs(ref)), tolerance = 1e-12)
  expect_error(ExpKerTfd(x, sigma = 0), "sigma must be positive")
  expect_error(ExpKerTfd(1:7), "at least 8 samples")
})

test_that("CprWt: band-limited Morlet scale energies and their FWHM", {
  fs <- 100
  i <- 0:199
  ecg <- sin(2 * pi * 8 * i / fs) + 0.5 * sin(2 * pi * 30 * i / fs)
  r <- CprWt(ecg, fs = fs, band = c(3, 21))
  n <- length(ecg)
  k <- 0:(n - 1)
  f <- ifelse(k <= n %/% 2, k, k - n) * fs / n
  X <- stats::fft(ecg)
  X[!(abs(f) >= 3 & abs(f) <= 21)] <- 0
  filt <- Re(stats::fft(X, inverse = TRUE) / n)
  sc <- numeric(0)
  s <- max(1, 5 * fs / (2 * pi * 21))
  while (s <= 5 * fs / (2 * pi * 3) && length(sc) < 32) {
    sc <- c(sc, s)
    s <- s * 2^0.25
  }
  expect_equal(r$scales, sc, tolerance = 1e-12)
  en <- vapply(sc, function(s) sum(Mod(.cwt(filt, s, "morlet"))^2), 1)
  p <- en / sum(en)
  expect_equal(r$scale_energy, p, tolerance = 1e-10)
  pk <- which.max(p)
  hm <- p[pk] / 2
  lo <- pk
  while (lo > 1 && p[lo - 1] >= hm) lo <- lo - 1
  hi <- pk
  while (hi < length(p) && p[hi + 1] >= hm) hi <- hi + 1
  left <- if (lo > 1 && p[lo] > p[lo - 1]) lo - (p[lo] - hm) / (p[lo] - p[lo - 1]) else lo
  right <- if (hi < length(p) && p[hi] > p[hi + 1]) hi + (p[hi] - hm) / (p[hi] - p[hi + 1]) else hi
  expect_equal(r$sdw, right - left, tolerance = 1e-9)
  expect_equal(r$peak_freq, 5 * fs / (2 * pi * sc[pk]), tolerance = 1e-12)
  expect_identical(r$organised, (right - left) < length(sc) / 2)
  expect_error(CprWt(ecg, fs = fs, band = c(21, 3)), "0 < low < high")
  expect_error(CprWt(ecg, fs = 30, band = c(3, 21)), "exceeds Nyquist")
})

test_that("Cwt and Scalogram follow eq (8.107) for each mother wavelet", {
  x <- .x64()[1:40]
  for (w in c("morlet", "mexh", "haar")) {
    r <- Cwt(x, fs = 4, wavelet = w, scales = c(1, 2.5, 4))
    for (j in 1:3) expect_equal(r$coeffs[[j]], .cwt(x, c(1, 2.5, 4)[j], w), tolerance = 1e-12)
    fc <- c(morlet = 5 / (2 * pi), mexh = 0.25, haar = 0.5)[[w]]
    expect_equal(r$freqs, fc * 4 / c(1, 2.5, 4), tolerance = 1e-12)
    ep <- vapply(r$coeffs, function(v) sum(Mod(v)^2), 1)
    expect_equal(r$energy_per_scale, ep, tolerance = 1e-12)
    expect_equal(r$peak_scale, c(1, 2.5, 4)[which.max(ep)])
  }
  expect_equal(Cwt(x)$scales, c(1, 2, 4))
  sg <- Scalogram(x, fs = 4, scales = c(1, 2, 3), wavelet = "mexh")
  ref <- t(vapply(c(1, 2, 3), function(s) Mod(.cwt(x, s, "mexh"))^2, numeric(40)))
  expect_equal(sg$scalogram, ref, tolerance = 1e-12)
  expect_identical(sg$ridge, apply(ref, 2, which.max))
  expect_equal(sg$total_energy, sum(ref), tolerance = 1e-12)
  expect_equal(sg$energy_per_scale, rowSums(ref), tolerance = 1e-12)
  expect_error(Cwt(x, scales = c(1, -1)), "positive")
  expect_error(Cwt(x, wavelet = "gaus"), "unknown wavelet")
})

test_that("WvDist and Gtfd reproduce the Wigner-Ville distribution and its smoothing", {
  x <- .x64()[1:24]
  ref <- .wvd(x)
  wv <- WvDist(x, fs = 3)
  expect_equal(wv$tfd, ref, tolerance = 1e-12)
  expect_equal(wv$freqs, (0:23) * 3 / 48, tolerance = 1e-12)
  expect_equal(wv$total_energy, sum(ref), tolerance = 1e-12)
  # a row of the WVD sums to 2 N |z(n)|^2 (only tau = 0 survives the frequency sum)
  expect_equal(rowSums(wv$tfd), 2 * 24 * Mod(.an(x))^2, tolerance = 1e-12)
  expect_equal(Gtfd(x, fs = 3, kernel = "wvd")$tfd, ref, tolerance = 1e-12)
  g <- function(L) {
    w <- exp(-0.5 * (((0:(L - 1)) - (L - 1) / 2) / (L / 6))^2)
    w / sum(w)
  }
  sw <- Gtfd(x, kernel = "swvd", tsmooth = 5)
  gt <- g(5)
  refs <- ref
  for (i in 1:24) for (k in 1:24) refs[i, k] <- sum(gt * ref[pmin(24, pmax(1, i + (0:4) - 2)), k])
  expect_equal(sw$tfd, refs, tolerance = 1e-12)
  expect_identical(c(sw$tsmooth, sw$fsmooth), c(5L, 1L))
  pw <- Gtfd(x, kernel = "pwvd", fsmooth = 3)
  gf <- g(3)
  refp <- ref
  for (i in 1:24) for (k in 1:24) refp[i, k] <- sum(gf * ref[i, pmin(24, pmax(1, k + (0:2) - 1))])
  expect_equal(pw$tfd, refp, tolerance = 1e-12)
  sp <- Gtfd(x)
  expect_identical(c(sp$tsmooth, sp$fsmooth), c(3L, 3L))
  expect_equal(sp$crossterm_ratio, sum(-sp$tfd[sp$tfd < 0]) / sum(abs(sp$tfd)), tolerance = 1e-12)
  expect_error(Gtfd(x, kernel = "born-jordan"), "unknown kernel")
  expect_error(WvDist(x, nfreq = 1), "nfreq must be >= 2")
})

test_that("OrthFilt returns the Daubechies taps with their QMF identities", {
  r2 <- OrthFilt(2)
  h <- c(1 + sqrt(3), 3 + sqrt(3), 3 - sqrt(3), 1 - sqrt(3)) / (4 * sqrt(2))
  expect_equal(r2$rec_lo, h, tolerance = 1e-12)
  expect_equal(r2$dec_lo, rev(h), tolerance = 1e-12)
  expect_equal(r2$rec_hi, (-1)^(0:3) * rev(h), tolerance = 1e-12)
  expect_equal(r2$dec_hi, rev(r2$rec_hi), tolerance = 1e-12)
  expect_equal(r2$sum_lo, sqrt(2), tolerance = 1e-12)
  expect_equal(r2$norm_lo, 1, tolerance = 1e-12)
  r4 <- OrthFilt(4)
  expect_identical(r4$length, 8L)
  hh <- r4$rec_lo
  expect_lt(abs(sum(hh[1:6] * hh[3:8])), 1e-12)
  expect_lt(abs(sum(hh[1:4] * hh[5:8])), 1e-12)
  expect_lt(abs(sum(hh[1:2] * hh[7:8])), 1e-12)
  expect_equal(r4$max_shift_inner_product, max(abs(c(sum(hh[1:6] * hh[3:8]), sum(hh[1:4] * hh[5:8]), sum(hh[1:2] * hh[7:8])))), tolerance = 1e-12)
  expect_error(OrthFilt(11), "1..10")
  skip_if_not_installed("wavelets")
  # wavelets tabulates the scaling filter in the same (extremal-phase) order
  expect_equal(r2$rec_lo, wavelets::wt.filter("d4")@g, tolerance = 1e-12)
})

test_that("AtomTfd: a Fourier atom captures an on-bin tone and MP energy bookkeeping holds", {
  i <- 0:15
  r <- AtomTfd(cos(2 * pi * 3 * i / 16), dictionary = "fourier", max_atoms = 1)
  expect_identical(r$n_atoms, 1L)
  expect_equal(r$atoms[[1L]]$freq, 3 / 16, tolerance = 1e-12)
  expect_equal(r$atoms[[1L]]$coeff, 4, tolerance = 1e-12)
  expect_equal(r$explained, 1, tolerance = 1e-12)
  x <- .x64()[1:16]
  g <- AtomTfd(x, max_atoms = 4, min_decay = 0)
  e0 <- sum(Mod(.an(x))^2)
  cf <- vapply(g$atoms, function(a) a$coeff, 1)
  # projecting out a unit-norm atom removes |<r, g>|^2 of residual energy
  expect_equal(g$residual_energy, e0 - sum(cf^2), tolerance = 1e-10)
  en <- e0 - cumsum(c(0, cf^2))
  expect_equal(g$decay, sqrt(1 - en[-1] / en[-length(en)]), tolerance = 1e-9)
  expect_equal(g$explained, 1 - g$residual_energy / e0, tolerance = 1e-12)
  expect_error(AtomTfd(x, dictionary = "wavelet"), "unknown dictionary")
  expect_error(AtomTfd(x, max_atoms = 0), "max_atoms must be >= 1")
})

test_that("Dwt, Dwt2Tap, WtEnergy, WtEntropy and WtMoment on the Haar and db4 banks", {
  x <- c(4, 2, 5, 5, -1, 3, 0, 8)
  s1 <- .haar_step(x)
  s2 <- .haar_step(s1$lo)
  d <- Dwt(x, wavelet = "haar", levels = 2)
  expect_equal(d$approx, s2$lo, tolerance = 1e-12)
  expect_equal(d$details, list(s2$hi, s1$hi), tolerance = 1e-12)
  expect_equal(d$lengths, c(4L, 8L))
  expect_equal(d$energy, sum(x^2), tolerance = 1e-12)
  h <- Dwt2Tap(x, levels = 2)
  expect_equal(h$coeffs, d$coeffs, tolerance = 1e-12)
  expect_equal(h$input_energy, sum(x^2), tolerance = 1e-12)
  y <- .x64()[1:32]
  d4 <- Dwt(y, "db4", 3)
  expect_equal(d4$energy, sum(y^2), tolerance = 1e-12)
  expect_error(Dwt(y, "db4", 4), "exceeds the maximum")
  we <- WtEnergy(y, "db4", 3)
  en <- vapply(d4$coeffs, function(v) sum(v^2), 1)
  expect_equal(we$energies, en, tolerance = 1e-12)
  expect_identical(we$labels, c("A3", "D3", "D2", "D1"))
  expect_equal(we$relative, en / sum(en), tolerance = 1e-12)
  expect_lt(we$energy_balance, 1e-12)
  expect_identical(we$dominant_band, we$labels[which.max(en)])
  p <- en / sum(en)
  ee <- WtEntropy(y, "db4", 3)
  expect_equal(ee$entropy, -sum(p * log(p)), tolerance = 1e-12)
  expect_equal(ee$normalized_entropy, -sum(p * log(p)) / log(4), tolerance = 1e-12)
  expect_equal(WtEntropy(y, "db4", 3, base = "2")$entropy, -sum(p * log2(p)), tolerance = 1e-12)
  expect_error(WtEntropy(y, base = "10"), "base must be")
  expect_error(WtEntropy(rep(0, 32)), "zero energy")
  wm <- WtMoment(y, "db4", 3)
  for (q in 1:4) {
    v <- d4$coeffs[[q]]
    mu <- mean(v)
    sdv <- sqrt(mean((v - mu)^2))
    m <- wm$moments[[q]]
    expect_equal(c(m$mean, m$variance, m$energy), c(mu, mean((v - mu)^2), sum(v^2)), tolerance = 1e-12)
    expect_equal(c(m$skewness, m$kurtosis), c(mean(((v - mu) / sdv)^3), mean(((v - mu) / sdv)^4)), tolerance = 1e-12)
  }
})

test_that("Mra bands are the Haar pair averages and sum back to the signal", {
  x <- c(4, 2, 5, 5, -1, 3, 0, 8)
  m1 <- Mra(x, "haar", 1)
  avg <- rep((x[c(TRUE, FALSE)] + x[c(FALSE, TRUE)]) / 2, each = 2)
  expect_equal(m1$approximation, avg, tolerance = 1e-12)
  expect_equal(m1$details[[1L]], x - avg, tolerance = 1e-12)
  y <- .x64()[1:32]
  m3 <- Mra(y, "db4", 3)
  expect_equal(m3$approximation + Reduce(`+`, m3$details), y, tolerance = 1e-12)
  expect_lt(m3$reconstruction_error, 1e-12)
  expect_equal(m3$energy_per_band, vapply(m3$bands, function(b) sum(b^2), 1), tolerance = 1e-12)
})

test_that("Wpt leaves are the natural-order Haar packets", {
  x <- c(4, 2, 5, 5, -1, 3, 0, 8)
  r <- Wpt(x, "haar", 2)
  a <- .haar_step(x)
  leaves <- c(.haar_step(a$lo)[c("lo", "hi")], .haar_step(a$hi)[c("lo", "hi")])
  expect_equal(r$leaves, unname(leaves), tolerance = 1e-12)
  en <- vapply(leaves, function(v) sum(v^2), 1)
  expect_equal(r$energy_per_leaf, unname(en), tolerance = 1e-12)
  expect_equal(sum(r$energy_per_leaf), sum(x^2), tolerance = 1e-12)
  p <- en / sum(en)
  expect_equal(r$entropy, -sum(p[p > 0] * log(p[p > 0])), tolerance = 1e-12)
  expect_identical(r$dominant_leaf, unname(which.max(en)))
  expect_error(Wpt(x, "db4", 2), "too short")
})

test_that("Swt, WtVar and WtXcor follow the Haar a-trous transform", {
  y <- .x64()[1:32]
  s <- Swt(y, "haar", 3)
  ref <- .haar_swt(y, 3)
  expect_equal(s$details, ref$details, tolerance = 1e-12)
  expect_equal(s$approx, ref$approx, tolerance = 1e-12)
  expect_equal(s$energy_per_level, vapply(ref$details, function(v) sum(v^2), 1), tolerance = 1e-12)
  expect_identical(s$redundancy, 4L)
  expect_error(Swt(y[1:6], "haar", 3), "needs a signal of at least 8")
  wv <- WtVar(y, "haar", 3)
  vv <- vapply(1:3, function(j) {
    keep <- ref$details[[j]][(2^(j - 1) + 1):32]
    sum(keep^2) / (2^j * length(keep))
  }, 1)
  expect_equal(wv$variances, vv, tolerance = 1e-12)
  expect_equal(wv$n_used, 32L - c(1L, 2L, 4L))
  expect_true(wv$is_allan)
  expect_equal(wv$dominant_scale, 2^(which.max(vv) - 1))
  expect_equal(wv$sample_variance, mean((y - mean(y))^2), tolerance = 1e-12)
  expect_error(WtVar(y[1:4], "haar", 3), "needs at least 8")
  z <- .x64(9L)[1:32]
  xc <- WtXcor(y, z, "haar", 2)
  rz <- .haar_swt(z, 2)
  expect_equal(xc$correlations, c(.popcor(ref$details[[1]], rz$details[[1]]), .popcor(ref$details[[2]], rz$details[[2]])), tolerance = 1e-12)
  expect_equal(xc$overall_correlation, stats::cor(y, z), tolerance = 1e-12)
  xl <- WtXcor(y, z, "haar", 1, max_lag = 2)
  cands <- vapply(-2:2, function(lag) {
    idx <- seq_len(32)
    idx <- idx[idx + lag >= 1 & idx + lag <= 32]
    .popcor(ref$details[[1]][idx], rz$details[[1]][idx + lag])
  }, 1)
  expect_equal(xl$correlations, cands[which.max(abs(cands))], tolerance = 1e-12)
  expect_identical(xl$best_lags, (-2:2)[which.max(abs(cands))])
  expect_error(WtXcor(y, z[-1]), "same length")
  expect_error(WtXcor(y, z, max_lag = 32), "max_lag must satisfy")
})

test_that("WtThresh, SwtDen and PpgWtDen apply the universal threshold", {
  y <- .x64()[1:32]
  d <- Dwt(y, "db4", 2)
  fin <- sort(abs(d$details[[2L]]))
  sig <- fin[length(fin) %/% 2 + 1] / 0.6745
  r <- WtThresh(y, "db4", 2)
  expect_equal(r$sigma, sig, tolerance = 1e-12)
  expect_equal(r$threshold, sig * sqrt(2 * log(32)), tolerance = 1e-12)
  allc <- unlist(d$details)
  expect_identical(r$n_zeroed, sum(abs(allc) < r$threshold))
  expect_equal(r$sparsity, mean(abs(allc) < r$threshold), tolerance = 1e-12)
  expect_equal(WtThresh(y, "db4", 2, threshold = 0)$denoised, y, tolerance = 1e-12)
  expect_equal(WtThresh(y, "db4", 2, "hard", threshold = 1e9)$denoised, Mra(y, "db4", 2)$approximation, tolerance = 1e-12)
  hd <- WtThresh(y, "db4", 2, "hard")
  expect_equal(hd$noise_removed, sum((y - hd$denoised)^2), tolerance = 1e-12)
  expect_error(WtThresh(y, threshold_type = "garrote"), "soft' or 'hard'")
  expect_error(WtThresh(y, threshold = -1), "non-negative")
  sw <- SwtDen(y, "haar", 1, threshold = 1e9, threshold_type = "hard")
  # averaging the two Haar shifts leaves the (1, 2, 1) / 4 circular smoother
  expect_equal(sw$denoised, (c(y[32], y[-32]) + 2 * y + c(y[-1], y[1])) / 4, tolerance = 1e-12)
  expect_identical(sw$n_shifts, 2L)
  expect_equal(SwtDen(y, "db4", 2, threshold = 0)$denoised, y, tolerance = 1e-12)
  sd2 <- SwtDen(y, "db4", 2)
  expect_equal(sd2$threshold, sig * sqrt(2 * log(32)), tolerance = 1e-12)
  expect_error(SwtDen(y, threshold_type = "x"), "soft' or 'hard'")
  expect_error(SwtDen(y[1:4], levels = 3), "needs at least 8")
  pp <- PpgWtDen(.x64(), fs = 50, levels = 3)
  ref <- WtThresh(.x64(), "db4", 3)
  expect_equal(pp$denoised, ref$denoised, tolerance = 1e-12)
  expect_equal(pp$artifact, .x64() - ref$denoised, tolerance = 1e-12)
  expect_equal(pp$snr_improvement_db, 10 * log10(sum(.x64()^2) / sum((.x64() - ref$denoised)^2)), tolerance = 1e-12)
  expect_equal(pp$approx_energy, sum(Dwt(.x64(), "db4", 3)$approx^2), tolerance = 1e-12)
})

test_that("SeizWt fluctuation intensity, eq (8.132)", {
  y <- .x64()
  r <- SeizWt(y, fs = 256, wavelet = "haar", levels = 5, threshold = 0.1)
  d <- Dwt(y, "haar", 5)$details
  fine <- rev(d)
  fi <- vapply(3:5, function(s) sum(abs(diff(fine[[s]]))) / length(fine[[s]]), 1)
  expect_equal(r$fi, fi, tolerance = 1e-12)
  expect_equal(r$fi_total, sum(fi), tolerance = 1e-12)
  expect_equal(r$bands[[1L]], c(256 / 16, 256 / 8))
  expect_identical(r$seizure_detected, sum(fi) > 0.1)
  expect_null(SeizWt(y, wavelet = "haar", levels = 5)$seizure_detected)
  expect_error(SeizWt(y, wavelet = "haar", levels = 5, scales = 6), "outside 1..levels")
})

test_that("Spectrogram, StftParam and IStft: STFT and weighted overlap-add", {
  y <- .x64()
  sp <- Spectrogram(y, fs = 8, nperseg = 16, window = "hamming")
  w <- 0.54 - 0.46 * cos(2 * pi * (0:15) / 15)
  starts <- seq(1, 64 - 15, by = 8)
  ref <- t(vapply(starts, function(s) Mod(stats::fft(y[s:(s + 15)] * w))[1:9]^2, numeric(9)))
  expect_equal(sp$spectrogram, ref, tolerance = 1e-12)
  expect_equal(sp$times, ((starts - 1) + 7.5) / 8, tolerance = 1e-12)
  expect_equal(sp$freqs, (0:8) * 8 / 16, tolerance = 1e-12)
  expect_equal(sp$peak_freq, sp$freqs[which.max(colSums(ref))])
  expect_equal(sp$total_energy, sum(ref), tolerance = 1e-12)
  expect_error(Spectrogram(y, nperseg = 65), "exceeds the signal length")
  expect_error(Spectrogram(y, nperseg = 16, noverlap = 16), "noverlap must satisfy")
  expect_error(Spectrogram(y, nperseg = 16, window = "kaiser"), "unknown window")
  hs <- Spectrogram(y, nperseg = 16)
  inv <- IStft(hs$stft)
  wh <- 0.5 - 0.5 * cos(2 * pi * (0:15) / 15)
  den <- numeric(64)
  for (f in seq_along(hs$stft)) den[(f - 1) * 8 + 1:16] <- den[(f - 1) * 8 + 1:16] + wh^2
  ok <- den > 1e-10 * max(den)
  expect_equal(inv$signal[ok], y[ok], tolerance = 1e-12)
  expect_identical(c(inv$valid_start, inv$valid_end), range(which(ok)))
  expect_error(IStft(hs$stft, hop = 16), "gap between analysis windows")
  expect_error(IStft(hs$stft, hop = 17), "hop must satisfy")
  st <- StftParam(100, 0.5, 4)
  expect_identical(c(st$nperseg_time, st$nperseg_freq, st$nperseg), c(50L, 25L, 50L))
  expect_true(st$feasible)
  st2 <- StftParam(100, 0.05, 4)
  expect_false(st2$feasible)
  expect_identical(st2$nperseg, 25L)
  expect_equal(c(st2$achieved_t_res, st2$achieved_f_res, st2$heisenberg_bound), c(0.25, 4, 1 / (4 * pi)), tolerance = 1e-12)
  expect_error(StftParam(0, 1, 1), "fs must be positive")
})

test_that("HrvTv band powers come from the resampled tachogram's spectrogram", {
  rr <- 0.8 + 0.05 * sin((1:200) / 3) + 0.02 * cos((1:200) / 1.3)
  r <- HrvTv(rr, fs_resamp = 4, window_len = 64)
  bt <- cumsum(rr)
  m <- trunc(bt[200] * 4)
  g <- (0:(m - 1)) / 4
  rs <- stats::approx(bt, rr, g, rule = 2)$y
  rs <- rs - mean(rs)
  expect_equal(r$resampled, rs, tolerance = 1e-12)
  wh <- 0.5 - 0.5 * cos(2 * pi * (0:63) / 63)
  st <- seq(1, m - 63, by = 32)
  S <- t(vapply(st, function(s) Mod(stats::fft(rs[s:(s + 63)] * wh))[1:33]^2, numeric(33)))
  fr <- (0:32) * 4 / 64
  band <- function(lo, hi) rowSums(S[, fr >= lo & fr < hi, drop = FALSE])
  # the module's O(N^2) DFT and stats::fft agree to rounding in each bin
  expect_equal(r$lf, band(0.04, 0.15), tolerance = 1e-10)
  expect_equal(r$hf, band(0.15, 0.40), tolerance = 1e-10)
  expect_equal(r$vlf, band(0, 0.04), tolerance = 1e-10)
  expect_equal(r$lf_hf_ratio, r$lf / r$hf, tolerance = 1e-12)
  expect_equal(r$mean_hr, 60 / mean(rr), tolerance = 1e-12)
  rb <- HrvTv(rr, fs_resamp = 4, window_len = 64, standard = "bianchi")
  expect_equal(rb$hf, band(0.18, 0.40), tolerance = 1e-10)
  expect_error(HrvTv(c(rr, -1)), "must be positive")
  expect_error(HrvTv(rr, standard = "esc"), "taskforce' or 'bianchi")
  expect_error(HrvTv(rr[1:10], window_len = 64), "exceeds the resampled length")
})

test_that("Sift, Imf, EmdEns and VfEmd: EMD identities and IMF diagnostics", {
  x <- .x64()
  s <- Sift(x, max_imfs = 4)
  expect_equal(Reduce(`+`, s$imfs) + s$residual, x, tolerance = 1e-12)
  expect_equal(s$energy_per_imf, vapply(s$imfs, function(v) sum(v^2), 1), tolerance = 1e-12)
  im <- Imf(x)
  expect_equal(im$imf, s$imfs[[1L]], tolerance = 1e-12)
  expect_equal(im$residual, x - im$imf, tolerance = 1e-12)
  cc <- im$imf
  n <- length(cc)
  mx <- which(cc[2:(n - 1)] > cc[1:(n - 2)] & cc[2:(n - 1)] > cc[3:n]) + 1L
  mn <- which(cc[2:(n - 1)] < cc[1:(n - 2)] & cc[2:(n - 1)] < cc[3:n]) + 1L
  expect_identical(im$n_extrema, length(mx) + length(mn))
  a <- cc[-n]
  b <- cc[-1]
  expect_identical(im$n_zero_crossings, sum((a < 0 & b >= 0) | (a > 0 & b <= 0)))
  up <- stats::splinefun(c(1, mx, n), c(cc[1], cc[mx], cc[n]), method = "natural")(1:n)
  lo <- stats::splinefun(c(1, mn, n), c(cc[1], cc[mn], cc[n]), method = "natural")(1:n)
  expect_equal(im$mean_envelope, (up + lo) / 2, tolerance = 1e-10)
  expect_equal(im$amplitude, Mod(.an(cc)), tolerance = 1e-12)
  expect_identical(im$is_imf, abs(length(mx) + length(mn) - im$n_zero_crossings) <= 1 && max(abs((up + lo) / 2)) <= 0.05 * max(abs(cc)))
  expect_error(Imf(1:16), "maxima")
  expect_error(Sift(x, tol = 0), "tol must be positive")
  e0 <- EmdEns(x, n_ensembles = 3, noise_std = 0, max_imfs = 4)
  expect_equal(e0$imfs, s$imfs, tolerance = 1e-12)
  expect_lt(e0$reconstruction_error, 1e-12)
  e1 <- EmdEns(x, n_ensembles = 2, seed = 5)
  expect_identical(e1, EmdEns(x, n_ensembles = 2, seed = 5))
  expect_false(identical(e1$imfs, EmdEns(x, n_ensembles = 2, seed = 6)$imfs))
  expect_error(EmdEns(x, n_ensembles = 0), "n_ensembles must be >= 1")
  vf <- VfEmd(x, fs = 50, n_imfs = 3)
  ref <- Sift(x, max_imfs = 3)$imfs
  expect_equal(vf$imfs, ref, tolerance = 1e-12)
  tot <- sum(vapply(ref, function(v) sum(v^2), 1))
  for (q in seq_along(ref)) {
    ph <- Arg(.an(ref[[q]]))
    d <- diff(ph)
    d <- (d + pi) %% (2 * pi) - pi
    fi <- abs(d) * 50 / (2 * pi)
    ft <- vf$features[[q]]
    expect_equal(c(ft$energy, ft$relative_energy), c(sum(ref[[q]]^2), sum(ref[[q]]^2) / tot), tolerance = 1e-12)
    expect_equal(c(ft$mean_freq, ft$freq_std), c(mean(fi), sqrt(mean((fi - mean(fi))^2))), tolerance = 1e-10)
    expect_equal(ft$mean_amplitude, mean(Mod(.an(ref[[q]]))), tolerance = 1e-12)
  }
  expect_identical(vf$dominant_imf, which.max(vapply(ref, function(v) sum(v^2), 1)))
  expect_error(VfEmd(x, n_imfs = 0), "n_imfs must be >= 1")
})

test_that("EmdSpec bins the instantaneous amplitude^2 at the central-difference frequency", {
  x <- .x64()
  r <- EmdSpec(x, fs = 10, max_imfs = 3, nfreq = 16)
  ref <- Sift(x, max_imfs = 3)$imfs
  expect_equal(r$imfs, ref, tolerance = 1e-12)
  spec <- matrix(0, 64, 16)
  for (q in seq_along(ref)) {
    z <- .an(ref[[q]])
    ph <- Arg(z)
    d <- (diff(ph) + pi) %% (2 * pi) - pi
    un <- cumsum(c(ph[1], d))
    dth <- c(un[2] - un[1], (un[3:64] - un[1:62]) / 2, un[64] - un[63])
    fi <- abs(dth) * 10 / (2 * pi)
    expect_equal(r$inst_freq[[q]], fi, tolerance = 1e-10)
    k <- trunc(fi / (10 / 32))
    for (i in which(k < 16)) spec[i, k[i] + 1] <- spec[i, k[i] + 1] + Mod(z[i])^2
  }
  expect_equal(r$spectrum, spec, tolerance = 1e-12)
  expect_equal(r$marginal, colSums(spec), tolerance = 1e-12)
  expect_equal(r$peak_freq, r$freqs[which.max(colSums(spec))])
  expect_error(EmdSpec(x, nfreq = 1), "nfreq must be >= 2")
})

test_that("TwaEmd averages odd and even detrended T windows", {
  n <- 400
  fs <- 100
  ecg <- 0.1 * sin(2 * pi * (0:(n - 1)) / 170)
  rp <- seq(10, 360, by = 50)
  ecg[rp] <- 2
  tw <- rep(c(0.3, 0.2), length.out = length(rp))
  for (b in seq_along(rp)) ecg[rp[b] + 20:30] <- ecg[rp[b] + 20:30] + tw[b]
  r <- TwaEmd(ecg, fs = fs, r_peaks = rp, max_imfs = 6)
  s <- Sift(ecg, max_imfs = 6)
  det <- ecg - s$residual
  segs <- lapply(rp, function(p) det[(p + 15):(p + 39)])
  ev <- segs[seq(1, length(rp), 2)]
  od <- segs[seq(2, length(rp), 2)]
  om <- Reduce(`+`, od) / length(od)
  em <- Reduce(`+`, ev) / length(ev)
  expect_equal(r$odd_mean, om, tolerance = 1e-12)
  expect_equal(r$even_mean, em, tolerance = 1e-12)
  expect_equal(r$twa_amplitude, max(abs(om - em)), tolerance = 1e-12)
  expect_equal(r$twa_rms, sqrt(mean((om - em)^2)), tolerance = 1e-12)
  auto <- TwaEmd(ecg, fs = fs)
  expect_identical(auto$r_peaks, as.integer(rp))
  expect_false(auto$rpeaks_supplied)
  expect_error(TwaEmd(ecg, fs = fs, r_peaks = rp[1:3]), "at least 4")
  expect_error(TwaEmd(ecg, twa_window = c(0.4, 0.1)), "0 <= start < end")
})

test_that("PcgEnvAvg: ECG-triggered averaging of the smoothed analytic envelope", {
  fs <- 200
  n <- 800
  ecg <- numeric(n)
  trig <- c(50, 250, 450, 650)
  ecg[trig] <- 1
  i <- 0:(n - 1)
  pcg <- 0.01 * sin(2 * pi * 13 * i / fs)
  for (t0 in trig) {
    pcg[t0 + 5:25] <- pcg[t0 + 5:25] + sin(2 * pi * 40 * (5:25) / fs)
    pcg[t0 + 90:105] <- pcg[t0 + 90:105] + 0.5 * sin(2 * pi * 50 * (90:105) / fs)
  }
  r <- PcgEnvAvg(pcg, ecg, fs = fs)
  env <- Mod(.an(pcg))
  sm <- vapply(seq_len(n), function(k) mean(env[max(1, k - 2):min(n, k + 2)]), 1)
  expect_identical(r$triggers, as.integer(trig))
  expect_identical(r$cycle_len, 200L)
  cyc <- lapply(trig[1:3], function(t0) sm[t0:(t0 + 199)])
  avg <- Reduce(`+`, cyc) / 3
  expect_equal(r$average_envelope, avg, tolerance = 1e-12)
  expect_identical(r$n_cycles, 3L)
  s1 <- which.max(avg[1:66])
  s2 <- 66L + which.max(avg[67:200])
  expect_identical(c(r$s1_index, r$s2_index), c(s1, s2))
  expect_equal(r$s2_s1_ratio, avg[s2] / avg[s1], tolerance = 1e-12)
  expect_equal(r$snr_gain_db, 10 * log10(3), tolerance = 1e-12)
  expect_error(PcgEnvAvg(pcg, ecg[-1], fs = fs), "same length")
  one <- numeric(n)
  one[50] <- 1
  expect_error(PcgEnvAvg(pcg, one, fs = fs), "only 1 QRS triggers")
})

test_that("VModes: one mode reproduces a pure tone at its centre frequency", {
  x <- cos(2 * pi * 5 * (0:63) / 64)
  for (ini in c("uniform", "zero")) {
    r <- VModes(x, K = 1, init = ini, tol = 1e-20, max_iter = 50)
    expect_equal(r$modes[[1L]], x, tolerance = 1e-12)
    expect_equal(r$center_freqs, 5 / 64, tolerance = 1e-12)
    expect_lt(r$reconstruction_error, 1e-12)
  }
  # dual ascent: the multiplier decays by (1 - tau / 2) per sweep, and the
  # stopping rule (squared relative update < tol = 1e-20, so a relative
  # step of 1e-10) halts it about 1e-10 / (tau / 2) = 2e-9 from the fixed point
  rt <- VModes(x, K = 1, tau = 0.1, tol = 1e-20, max_iter = 1000)
  expect_true(rt$converged)
  expect_equal(rt$modes[[1L]], x, tolerance = 1e-8)
  expect_false(isTRUE(all.equal(VModes(x, K = 1, tau = 0.1, tol = 1e-20, max_iter = 20)$modes[[1L]], x, tolerance = 1e-6)))
  expect_error(VModes(x, K = 0), "K must be >= 1")
  expect_error(VModes(x, init = "random"), "uniform' or 'zero")
  expect_error(VModes(x, alpha = 0), "alpha must be positive")
})

test_that("CwtRidge keeps prominent scale-and-time local maxima of the scalogram", {
  i <- 1:64
  x <- exp(-((i - 32) / 3)^2) + 0.4 * exp(-((i - 12) / 1.5)^2)
  r <- CwtRidge(x, scales = 1:6, min_prominence = 0.05)
  sg <- Scalogram(x, scales = 1:6, wavelet = "mexh")$scalogram
  expect_equal(r$scalogram, sg, tolerance = 1e-12)
  found <- list()
  for (si in 1:6) {
    for (k in 2:63) {
      v <- sg[si, k]
      if (v > sg[si, k - 1] && v >= sg[si, k + 1] && v >= 0.05 * max(sg) &&
          !(si > 1 && sg[si - 1, k] > v) && !(si < 6 && sg[si + 1, k] > v)) {
        found[[length(found) + 1L]] <- c(k, si, v)
      }
    }
  }
  en <- vapply(found, function(f) f[3], 1)
  found <- found[order(-en)]
  expect_identical(r$n_structures, length(found))
  expect_equal(vapply(r$structures, function(s) s$sample, 1), vapply(found, function(f) f[1], 1))
  expect_equal(vapply(r$structures, function(s) s$scale, 1), vapply(found, function(f) f[2], 1))
  expect_identical(r$structures[[1L]]$sample, 32L)
  expect_error(CwtRidge(x, min_prominence = 0), "\\(0, 1\\]")
  expect_error(CwtRidge(rep(0, 16)), "no wavelet energy")
})

test_that("the echo family evaluates eqs (4.74)-(4.85)", {
  ei <- EchoImp(a = 0.6, n_0 = 3, n = 6)
  expect_equal(ei$x, c(1, 0, 0, 0.6, 0, 0))
  expect_equal(ei$n, 0:5)
  expect_equal(EchoImp(0.6, 3, c(3, 7, 0))$x, c(0.6, 0, 1))
  expect_error(EchoImp(0.6, 0, 5), "positive delay")
  h <- c(1, -0.5, 0.25)
  es <- EchoSig(h, a = 0.4, n_0 = 4)
  expect_equal(es$y, c(1, -0.5, 0.25, 0, 0.4, -0.2, 0.1))
  expect_true(es$echo_visible)
  expect_false(EchoSig(h, 0.4, 2)$echo_visible)
  expect_equal(EchoSig(h, 0.4, 2)$y, c(1, -0.5, 0.65, -0.2, 0.1))
  z <- c(1.2 + 0.3i, -0.7i, 2)
  ez <- EchoZ(0.5, 2, z, H = c(1, 2, 3))
  expect_equal(ez$Y, (1 + 0.5 * z^-2) * c(1, 2, 3), tolerance = 1e-12)
  expect_error(EchoZ(0.5, 2, c(1, 0)), "region of convergence")
  w <- seq(0.1, 3, length.out = 7)
  sp <- EchoSpec(0.5, 3, w, H = 2)
  Y <- 2 * (1 + 0.5 * exp(-3i * w))
  expect_equal(sp$Y, Y, tolerance = 1e-12)
  expect_equal(sp$magnitude, Mod(Y), tolerance = 1e-12)
  expect_equal(sp$phase, Arg(Y), tolerance = 1e-12)
  expect_equal(sp$ripple_period, 2 * pi / 3)
  ls <- EchoLogSp(0.5, 3, w, n_terms = 60)
  expect_equal(ls$Y_hat, log(1 + 0.5 * exp(-3i * w)), tolerance = 1e-12)
  k <- 1:60
  ser <- vapply(w, function(om) sum((-1)^(k + 1) * 0.5^k / k * exp(-1i * k * om * 3)), complex(1))
  expect_equal(ls$series, ser, tolerance = 1e-12)
  expect_lt(ls$series_error, 1e-12)
  expect_null(EchoLogSp(1.5, 3, w)$series)
  expect_error(EchoLogSp(1, 1, pi), "vanishes")
  ec <- EchoCep(c(0.5, 0.2), a = 0.5, n_0 = 3, n = 10)
  ref <- numeric(10)
  ref[1:2] <- c(0.5, 0.2)
  ref[c(4, 7, 10)] <- ref[c(4, 7, 10)] + (-1)^(0:2) * 0.5^(1:3) / (1:3)
  expect_equal(ec$y_hat, ref, tolerance = 1e-12)
  expect_identical(ec$n_impulses, 3L)
  expect_error(EchoCep(1, a = 1, n_0 = 2), "requires a < 1")
  zz <- exp(1i * w)
  ps <- EchoPsd(H = 1.5, a = 0.5, n_0 = 3, z = zz)
  expect_equal(ps$power, 2.25 * Mod(1 + 0.5 * zz^-3)^2, tolerance = 1e-12)
  expect_equal(ps$power, 2.25 * (1.25 + cos(3 * w)), tolerance = 1e-12)
  lp <- EchoLogPsd(H = 1.5, a = 0.5, n_0 = 3, omega = w)
  expect_equal(lp$log_power, log(2.25) + log(1.25 + cos(3 * w)), tolerance = 1e-12)
  expect_equal(lp$dc_term, log(1.25), tolerance = 1e-12)
  expect_equal(lp$modulation_index, 0.8, tolerance = 1e-12)
  expect_equal(lp$ripple, log(1 + 0.8 * cos(3 * w)), tolerance = 1e-12)
  expect_lt(lp$decomposition_error, 1e-12)
  expect_error(EchoLogPsd(H = 0, a = 0.5, n_0 = 3, omega = w), "undefined")
  expect_error(EchoLogPsd(H = 1, a = 1, n_0 = 1, omega = pi), "cancel exactly")
})
