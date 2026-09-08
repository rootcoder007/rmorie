# Rangayyan parametric modelling in R -- same book equations as the
# Python arm.  Expected values hand-computed or exact properties of a
# synthetic AR process.

ar1 <- function(n, a1 = 0.8, seed = 7) {
  x <- 0
  s <- seed
  out <- numeric(n)
  for (i in seq_len(n)) {
    s <- (1103515245 * s + 12345) %% 2147483648
    e <- s / 2147483648 - 0.5
    x <- a1 * x + e
    out[i] <- x
  }
  out
}

test_that("Levinson implements eqs (7.37)-(7.39)", {
  r <- Levinson(c(1, 0.5))
  expect_equal(r$reflection[1], -0.5)
  expect_equal(r$a, -0.5)
  expect_equal(r$error, 0.75)
  phi <- c(1, 0.5, 0.2)
  g1 <- -phi[2] / phi[1]
  e1 <- (1 - g1^2) * phi[1]
  g2 <- -(phi[3] + g1 * phi[2]) / e1
  s <- Levinson(phi)
  expect_equal(s$reflection, c(g1, g2))
  expect_equal(s$a, c(g1 + g2 * g1, g2))
  expect_equal(s$error, (1 - g2^2) * e1)
})

test_that("Levinson reports monotone error and stability", {
  r <- Levinson(c(1, 0.6, 0.3, 0.1, 0.05))
  expect_true(r$monotone)
  expect_true(r$stable)
  expect_false(Levinson(c(1, 2))$stable)
  expect_error(Levinson(c(1, 0.5), order = 3), "ACF lags")
})

test_that("Lpc recovers a known AR(1) with the book's sign", {
  r <- Lpc(ar1(4000, 0.8), 1)
  expect_equal(r$a[1], -0.8, tolerance = 0.05)
  expect_true(startsWith(r$sign_convention, "A(z) = 1 + sum a_k"))
  expect_true(r$stable)
})

test_that("Lpc gain squared is the prediction error of eq (7.35)", {
  r <- Lpc(ar1(2000, 0.6), 4)
  want <- r$acf[1] + sum(r$a * r$acf[-1])
  expect_equal(r$error, want, tolerance = 1e-9)
  expect_equal(r$gain^2, r$error, tolerance = 1e-9)
})

test_that("the Lpc residual whitens a known AR process", {
  r <- Lpc(ar1(4000, 0.85), 2)
  resid <- r$residual[-(1:2)]
  n <- length(resid)
  mu <- mean(resid)
  lag1 <- sum((resid[-n] - mu) * (resid[-1] - mu)) / n
  expect_lt(abs(lag1 / (sum((resid - mu)^2) / n)), 0.1)
})

test_that("Lpc refuses the covariance method rather than faking it", {
  expect_error(Lpc(ar1(100), 2, method = "covariance"), "autocorrelation")
  expect_error(Lpc(c(1, 2, 3), 5), "more samples")
})

test_that("LpcSynth inverts Lpc and flags divergence", {
  x <- ar1(500, 0.7)
  fit <- Lpc(x, 3)
  back <- LpcSynth(fit$a, fit$residual)
  expect_equal(back$y[1:400], x[1:400], tolerance = 1e-9)
  expect_false(back$diverged)
  expect_true(LpcSynth(-2.5, c(1, numeric(2000)))$diverged)
  expect_error(LpcSynth(c(0.5, 0.2), 1, initial = 0), "2 samples")
})

test_that("flipping the sign convention moves the poles", {
  a <- Lpc(ar1(4000, 0.8), 1)$a
  right <- PoleZero(1, a)$poles[1]
  flipped <- PoleZero(1, -a)$poles[1]
  expect_equal(Re(right), -a[1], tolerance = 1e-9)
  expect_equal(Re(flipped), a[1], tolerance = 1e-9)
  expect_gt(Mod(right - flipped), 1)
})

test_that("ArFit's PSD peaks at the AR resonance", {
  fs <- 1000
  r0 <- 0.95
  w0 <- 2 * pi * 100 / fs
  a1 <- -2 * r0 * cos(w0)
  a2 <- r0^2
  s <- 3
  h <- c(0, 0)
  x <- numeric(4000)
  for (i in seq_len(4000)) {
    s <- (1103515245 * s + 12345) %% 2147483648
    e <- s / 2147483648 - 0.5
    v <- e - a1 * h[1] - a2 * h[2]
    h <- c(v, h[1])
    x[i] <- v
  }
  r <- ArFit(x, 6, fs = fs, nfreq = 512)
  expect_equal(r$freqs[which.max(r$psd)], 100, tolerance = 8)
  expect_equal(r$max_peaks, 3L)
  expect_error(ArFit(ar1(200), 4, fs = 0), "positive")
})

test_that("FpeOrder and MdlOrder penalise order", {
  errs <- c(1, 0.5, 0.499, 0.4989, 0.49889)
  expect_equal(FpeOrder(errs, 200)$order, 2L)
  f <- FpeOrder(c(1, 0.5), 100)
  expect_equal(f$criterion[1], 1 * 102 / 98)
  expect_equal(f$criterion[2], 0.5 * 103 / 97)
  m <- MdlOrder(errs, 200)
  expect_lte(m$order, m$aic_order)
  expect_true(m$stricter_than_aic)
  expect_equal(m$penalty_per_parameter, log(200))
  d <- MdlOrder(c(1, 0.5), 64)
  expect_equal(d$criterion[1], 64 * log(1) + log(64))
  expect_equal(d$criterion[2], 64 * log(0.5) + 2 * log(64))
  expect_error(FpeOrder(c(1, 0), 100), "positive")
  expect_error(MdlOrder(c(1, -1), 100), "positive")
})

test_that("PzForm and PzFormZ agree (eqs 3.69, 3.70)", {
  r <- PzForm(0.5, 0.8, z = 2)
  expect_equal(Re(r$H), (1 - 0.25) / (1 - 0.4))
  expect_true(r$stable)
  expect_false(PzForm(complex(0), 1.2)$stable)
  expect_error(PzForm(0.5, 0.2, z = 0), "undefined at z = 0")
  a <- PzFormZ(c(0.5, -0.3), 0.8, z = complex(real = 0.6, imaginary = 0.7))
  expect_true(a$agrees_with_eq369)
  expect_equal(a$max_difference, 0, tolerance = 1e-12)
  expect_equal(PzFormZ(c(0.1, 0.2, 0.3), 0.4)$exponent, -2L)
})

test_that("PzResp reads magnitude and phase off the pole-zero plot", {
  r <- PzResp(0.5, 0.8, omega = 0.9)
  z0 <- complex(real = cos(0.9), imaginary = sin(0.9))
  expect_equal(r$magnitude, Mod(z0 - 0.5) / Mod(z0 - 0.8))
  expect_true(r$magnitude_matches_product)
  w0 <- 1.1
  zc <- complex(real = cos(w0), imaginary = sin(w0))
  expect_equal(PzResp(zc, complex(real = 0), omega = w0)$magnitude, 0,
               tolerance = 1e-12)
  near <- 0.99 * zc
  on <- PzResp(complex(0), near, omega = w0)$magnitude
  off <- PzResp(complex(0), near, omega = w0 + 0.5)$magnitude
  expect_gt(on, 5 * off)
})

test_that("PoleZero finds the roots of H", {
  r <- PoleZero(c(1, -0.5), -0.8)
  expect_equal(Re(r$zeros[1]), 0.5, tolerance = 1e-9)
  expect_equal(Re(r$poles[1]), 0.8, tolerance = 1e-9)
  expect_true(r$stable)
  expect_true(r$minimum_phase)
  p <- PoleZero(1, c(-1.2, 0.85))
  expect_equal(sort(Mod(p$poles)), rep(sqrt(0.85), 2), tolerance = 1e-6)
  u <- PoleZero(c(1, -1), NULL)
  expect_equal(length(u$zeros_on_unit_circle), 1L)
  expect_false(u$minimum_phase)
})

test_that("ArmaFit returns both polynomials", {
  r <- ArmaFit(ar1(1000, 0.7), p = 2, q = 1)
  expect_equal(length(r$a), 2L)
  expect_equal(length(r$b), 2L)
  expect_true(r$two_stage)
  expect_error(ArmaFit(ar1(200), p = 2, q = -1), "cannot be negative")
})

test_that("PcgAr reports one resonance per conjugate pair", {
  fs <- 1000
  n <- 2000
  t <- 0:(n - 1)
  x <- sin(2 * pi * 60 * t / fs) + 0.4 * sin(2 * pi * 180 * t / fs)
  r <- PcgAr(x, fs = fs, order = 8)
  freqs <- vapply(r$resonances, function(d) d$frequency, numeric(1))
  expect_true(all(freqs > 0 & freqs <= fs / 2))
  expect_lte(length(r$resonances), 4L)
  expect_true(any(abs(freqs - 60) < 15))
  expect_lt(PcgAr(ar1(600, 0.7), fs = 1000)$order,
            PcgAr(ar1(600, 0.7), fs = 8000)$order)
})

rr_series <- function(n = 300, mean_rr = 0.8, lf = 0.10, hf = 0.25,
                      amp = 0.02) {
  rr <- numeric(n)
  t <- 0
  for (i in seq_len(n)) {
    v <- mean_rr + amp * sin(2 * pi * lf * t) + amp * sin(2 * pi * hf * t)
    rr[i] <- v
    t <- t + v
  }
  rr
}

test_that("HrvAr uses the Task Force bands and removes the mean", {
  r <- HrvAr(rr_series(), order = 12)
  expect_equal(r$bands$lf, c(0.04, 0.15))
  expect_equal(r$bands$hf, c(0.15, 0.40))
  expect_equal(r$mean_rr, 0.8, tolerance = 0.02)
  expect_lt(abs(mean(r$resampled)), 1e-9)
  expect_error(HrvAr(rep(c(0.8, -0.1), 8)), "must be positive")
})

test_that("HrvRatio reports components, not only the ratio", {
  r <- HrvRatio(rr_series(), order = 12)
  expect_gt(r$lf, 0)
  expect_gt(r$hf, 0)
  expect_equal(r$lf_hf_ratio, r$lf / r$hf)
  expect_equal(r$lf_nu + r$hf_nu, 100)
  expect_true(grepl("sympathovagal", r$interpretation_caveat))
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_levinson_durbin(c(1, 0.5))$a, -0.5)
  expect_true(morie_ar_order_mdl(c(1, 0.5), 64)$order %in% c(1L, 2L))
  expect_equal(Re(morie_pole_zero_plot(c(1, -0.5))$zeros[1]), 0.5,
               tolerance = 1e-9)
})
