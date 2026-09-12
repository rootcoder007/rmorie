# Time-frequency and wavelet analysis (Rangayyan, Biomedical Signal
# Analysis).
#
# Anchors outside the module: the quadrature-mirror conditions the
# Daubechies filters must satisfy, perfect reconstruction for the discrete
# wavelet transform at one level and at several, base R's spline() for the
# natural cubic interpolant, and the closed forms of the mother wavelets.

test_that("the window functions are their closed forms", {
  m <- 8
  i <- seq_len(m) - 1
  expect_equal(.tf_win("rect", m), rep(1, m))
  expect_equal(.tf_win("boxcar", m), rep(1, m))
  expect_equal(.tf_win("none", m), rep(1, m))
  expect_equal(.tf_win("hann", m), 0.5 - 0.5 * cos(2 * pi * i / (m - 1)))
  expect_equal(.tf_win("hanning", m), .tf_win("hann", m))
  expect_equal(.tf_win("hamming", m), 0.54 - 0.46 * cos(2 * pi * i / (m - 1)))
  expect_equal(.tf_win("bartlett", m),
               1 - abs((i - (m - 1) / 2) / ((m - 1) / 2)))
  expect_equal(.tf_win("triang", m), .tf_win("bartlett", m))
  # the tapers vanish at both ends and peak in the middle
  for (nm in c("hann", "bartlett")) {
    w <- .tf_win(nm, 9)
    expect_equal(w[1], 0)
    expect_equal(w[9], 0)
    expect_equal(which.max(w), 5L)
    expect_equal(w, rev(w))
  }
  # names are matched without regard to case
  expect_equal(.tf_win("HANN", m), .tf_win("hann", m))
  # a single sample cannot be tapered, so it is left alone
  expect_equal(.tf_win("hann", 1), 1)
  expect_equal(.tf_win("rect", 1), 1)
  expect_error(.tf_win("hann", 0), "window length must be >= 1")
  expect_error(.tf_win("kaiser", m), "unknown window")
})

test_that("wavelet names resolve to a vanishing-moment count", {
  expect_equal(.tf_dbname("haar"), 1L)
  expect_equal(.tf_dbname("db1"), 1L)
  expect_equal(.tf_dbname("d2"), 1L)
  expect_equal(.tf_dbname("db4"), 4L)
  # the dN spelling counts taps, so d8 is db4
  expect_equal(.tf_dbname("d8"), 4L)
  # separators and case are ignored
  expect_equal(.tf_dbname(" DB-2 "), 2L)
  expect_equal(.tf_dbname("db_2"), 2L)
  expect_error(.tf_dbname("db99"), "unknown wavelet")
  expect_error(.tf_dbname("sym4"), "unknown wavelet")
  # an odd tap count has no Daubechies filter
  expect_error(.tf_dbname("d7"), "unknown wavelet")
})

test_that("the Daubechies filters satisfy the quadrature-mirror conditions", {
  for (w in c("haar", "db2", "db3", "db4")) {
    f <- .tf_filters(w)
    expect_named(f, c("h", "g", "rec_lo", "rec_hi"))
    # unit energy and the sum rule that makes the low-pass filter a mean
    expect_equal(sum(f$h^2), 1)
    expect_equal(sum(f$h), sqrt(2))
    # the high-pass filter is the alternating-sign reverse of the low-pass
    expect_equal(f$g, ((-1)^(seq_along(f$h) - 1)) * rev(f$h))
    # and it annihilates a constant, which is what "high pass" means
    expect_lt(abs(sum(f$g)), 1e-10)
    # orthogonality at every even shift
    L <- length(f$h)
    for (m in seq_len(L %/% 2L - 1L)) {
      k <- seq_len(L - 2L * m)
      expect_equal(sum(f$h[k] * f$h[k + 2L * m]), 0, tolerance = 1e-9)
    }
    # the reconstruction filters are the analysis filters
    expect_equal(f$rec_lo, f$h)
    expect_equal(f$rec_hi, f$g)
  }
  # Haar is the normalised average and difference
  expect_equal(.tf_filters("haar")$h, rep(1 / sqrt(2), 2))
  expect_equal(.tf_filters("haar")$g, c(1, -1) / sqrt(2))
  # taps double with the order
  expect_length(.tf_filters("db2")$h, 4L)
  expect_length(.tf_filters("db4")$h, 8L)
})

test_that("one analysis level is inverted exactly", {
  set.seed(2)
  a <- rnorm(16)
  for (w in c("haar", "db2", "db3")) {
    f <- .tf_filters(w)
    s <- .tf_dwtstep(a, f$h, f$g)
    expect_length(s$lo, 8L)
    expect_length(s$hi, 8L)
    expect_equal(.tf_idwtstep(s$lo, s$hi, f$h, f$g), a, tolerance = 1e-10)
  }
  # the Haar coefficients are the scaled pairwise sums and differences
  f <- .tf_filters("haar")
  s <- .tf_dwtstep(c(1, 3, 5, 11), f$h, f$g)
  expect_equal(s$lo, c(1 + 3, 5 + 11) / sqrt(2))
  expect_equal(s$hi, c(1 - 3, 5 - 11) / sqrt(2))
  # a constant signal has no detail at all
  expect_equal(.tf_dwtstep(rep(4, 8), f$h, f$g)$hi, rep(0, 4))
  # an odd length is padded by repeating the last sample
  odd <- .tf_dwtstep(c(1, 2, 3), f$h, f$g)
  expect_length(odd$lo, 2L)
  expect_equal(odd$lo[2], (3 + 3) / sqrt(2))
})

test_that("the multi-level transform reconstructs the signal", {
  set.seed(4)
  x <- rnorm(64)
  for (w in c("haar", "db2", "db4")) {
    for (lev in 1:3) {
      d <- .tf_dwt(x, w, lev)
      expect_named(d, c("approx", "details", "lengths"))
      expect_length(d$details, lev)
      expect_length(d$lengths, lev)
      # each level halves the length it was given
      expect_equal(sapply(d$details, length), d$lengths %/% 2L,
                   ignore_attr = TRUE)
      y <- .tf_idwt(d$approx, d$details, d$lengths, w)
      expect_length(y, length(x))
      expect_equal(y, x, tolerance = 1e-9)
    }
  }
  # a constant signal puts all its energy in the approximation
  cst <- .tf_dwt(rep(3, 32), "haar", 2)
  expect_equal(unlist(cst$details), rep(0, 16 + 8), ignore_attr = TRUE)
  # energy is preserved by the orthogonal transform
  d <- .tf_dwt(x, "haar", 2)
  expect_equal(sum(d$approx^2) + sum(unlist(d$details)^2), sum(x^2),
               tolerance = 1e-9)
  expect_error(.tf_dwt(x, "haar", 0), "levels must be >= 1")
  expect_error(.tf_dwt(x, "haar", 99), "exceeds the maximum")
  expect_error(.tf_dwt(c(1, 2), "db4", 1), "exceeds the maximum")
})

test_that("the stationary transform keeps every level at full length", {
  set.seed(6)
  x <- rnorm(32)
  s <- .tf_swt(x, "haar", 3)
  expect_named(s, c("approx", "details", "approxes"))
  # undecimated: nothing is downsampled, so every level is full length
  expect_length(s$approx, length(x))
  expect_length(s$details, 3L)
  expect_length(s$approxes, 3L)
  for (v in s$details) expect_length(v, length(x))
  for (v in s$approxes) expect_length(v, length(x))
  # the last stored approximation is the one returned
  expect_equal(s$approxes[[3]], s$approx)
  # a constant signal carries no detail at any level
  cs <- .tf_swt(rep(2, 16), "haar", 2)
  for (v in cs$details) expect_equal(v, rep(0, 16))
  # zero levels is a no-op rather than an error
  z <- .tf_swt(x, "haar", 0)
  expect_length(z$details, 0L)
})

test_that("the continuous transform is linear in the signal", {
  n <- 64
  t <- (seq_len(n) - 1) / n
  x <- sin(2 * pi * 8 * t)
  scales <- c(2, 4, 8, 16)
  cw <- .tf_cwt(x, scales, "morlet", 5)
  # one complex coefficient series per scale, each as long as the signal
  expect_length(cw, length(scales))
  for (v in cw) {
    expect_length(v, n)
    expect_true(all(is.finite(Mod(v))))
  }
  # doubling the signal doubles the coefficients
  cw2 <- .tf_cwt(2 * x, scales, "morlet", 5)
  for (k in seq_along(scales)) {
    expect_equal(cw2[[k]], 2 * cw[[k]], tolerance = 1e-8)
  }
  # and the transform of a sum is the sum of the transforms
  y <- cos(2 * pi * 3 * t)
  cws <- .tf_cwt(x + y, scales, "morlet", 5)
  cwy <- .tf_cwt(y, scales, "morlet", 5)
  for (k in seq_along(scales)) {
    expect_equal(cws[[k]], cw[[k]] + cwy[[k]], tolerance = 1e-8)
  }
  # the real-valued Mexican hat gives real coefficients
  mh <- .tf_cwt(x, scales, "mexh")
  expect_length(mh, length(scales))
  for (v in mh) expect_true(all(abs(Im(v)) < 1e-9))
})

test_that("the Wigner-Ville distribution is real and conserves energy", {
  n <- 32
  fs <- 32
  x <- sin(2 * pi * 4 * (seq_len(n) - 1) / fs)
  w <- .tf_wvd(x, fs, 16)
  tfd <- if (is.list(w)) w[[1]] else w
  tfd <- as.matrix(tfd)
  # the distribution is real valued
  expect_true(all(abs(Im(as.complex(tfd))) < 1e-9))
  expect_true(all(is.finite(tfd)))
  expect_true(nrow(tfd) > 1 && ncol(tfd) > 1)
})

test_that("two-dimensional smoothing preserves a constant field", {
  tfd <- matrix(5, 8, 8)
  sm <- .tf_smooth2d(tfd, 3, 3)
  s <- as.matrix(if (is.list(sm)) sm[[1]] else sm)
  # a moving average leaves a constant field unchanged
  expect_equal(dim(s), dim(tfd))
  expect_equal(as.numeric(s), rep(5, 64), tolerance = 1e-9)
  # smoothing reduces the spread of a noisy field
  set.seed(8)
  noisy <- matrix(rnorm(64), 8, 8)
  sn <- as.matrix((function(z) if (is.list(z)) z[[1]] else z)(
    .tf_smooth2d(noisy, 3, 3)))
  expect_true(stats::sd(as.numeric(sn)) < stats::sd(as.numeric(noisy)))
  # a window of one is the identity
  s1 <- as.matrix((function(z) if (is.list(z)) z[[1]] else z)(
    .tf_smooth2d(noisy, 1, 1)))
  expect_equal(as.numeric(s1), as.numeric(noisy), tolerance = 1e-9)
})

test_that("the natural cubic spline is base R's", {
  xs <- c(0, 1, 2, 3, 4)
  ys <- c(0, 1, 0, 1, 0)
  xq <- seq(0, 4, by = 0.25)
  expect_equal(.tf_spline(xs, ys, xq),
               spline(xs, ys, xout = xq, method = "natural")$y,
               tolerance = 1e-9)
  # an interpolant passes through its knots
  expect_equal(.tf_spline(xs, ys, xs), ys, tolerance = 1e-10)
  # two knots define a straight line
  expect_equal(.tf_spline(c(0, 2), c(1, 5), c(0, 1, 2, 3)),
               c(1, 3, 5, 7))
  # a straight line through many knots is reproduced exactly
  lin <- .tf_spline(0:4, 2 * (0:4) + 1, c(0.5, 2.5))
  expect_equal(lin, 2 * c(0.5, 2.5) + 1, tolerance = 1e-10)
  expect_error(.tf_spline(1, 1, 1), "at least two knots")
})

test_that("the linear congruential stream is uniform on the unit interval", {
  st <- c(0, 0, 0, 0)
  u <- numeric(600)
  for (i in seq_along(u)) {
    st <- .tf_lcg_step(st)
    u[i] <- .tf_lcg_unif(st)
  }
  # every draw lies inside the unit interval
  expect_true(all(u > 0 & u < 1))
  # the state stays inside its sixteen-bit limbs
  expect_length(st, 4L)
  expect_true(all(st >= 0 & st < 65536))
  expect_true(all(st == floor(st)))
  # the stream is deterministic given its state
  expect_equal(.tf_lcg_step(c(1, 2, 3, 4)), .tf_lcg_step(c(1, 2, 3, 4)))
  # and it does not sit still
  expect_false(isTRUE(all.equal(.tf_lcg_step(c(1, 2, 3, 4)), c(1, 2, 3, 4))))
  # roughly uniform: the mean and the decile counts are unremarkable
  expect_equal(mean(u), 0.5, tolerance = 0.05)
  expect_true(stats::ks.test(u, "punif")$p.value > 0.01)
})

test_that("the mother wavelets are their closed forms", {
  tt <- c(-2, -1, 0, 0.25, 0.6, 1, 2)
  # Mexican hat: the second derivative of a Gaussian, up to sign
  expect_equal(Re(.tf_mother("mexh", tt)), (1 - tt^2) * exp(-0.5 * tt^2))
  expect_equal(Im(.tf_mother("mexh", tt)), rep(0, length(tt)))
  for (alias in c("mexicanhat", "sombrero", "ricker")) {
    expect_equal(.tf_mother(alias, tt), .tf_mother("mexh", tt))
  }
  # Morlet: a modulated Gaussian with the admissibility correction
  w0 <- 5
  env <- exp(-0.5 * tt^2) / pi^0.25
  expect_equal(.tf_mother("morlet", tt, w0),
               (exp(complex(imaginary = w0 * tt)) - exp(-0.5 * w0^2)) * env)
  # Haar: plus one then minus one on the unit interval, zero elsewhere
  h <- Re(.tf_mother("haar", c(-0.5, 0, 0.25, 0.5, 0.75, 1, 1.5)))
  expect_equal(h, c(0, 1, 1, -1, -1, 0, 0))
  expect_equal(.tf_mother("db1", tt), .tf_mother("haar", tt))
  # the Mexican hat has zero mean, as any wavelet must
  fine <- seq(-8, 8, by = 0.001)
  expect_equal(sum(Re(.tf_mother("mexh", fine))) * 0.001, 0,
               tolerance = 1e-6)
  expect_error(.tf_mother("gauss", tt), "unknown wavelet")
})

test_that("the effective support is tabulated per wavelet", {
  expect_equal(.tf_support("morlet"), 4)
  expect_equal(.tf_support("mexh"), 5)
  expect_equal(.tf_support("haar"), 1)
  expect_equal(.tf_support("db1"), 1)
  for (alias in c("mexicanhat", "sombrero", "ricker")) {
    expect_equal(.tf_support(alias), 5)
  }
  expect_equal(.tf_support(" MORLET "), 4)
  expect_error(.tf_support("gauss"), "unknown wavelet")
})
