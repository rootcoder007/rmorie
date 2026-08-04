# Rangayyan signals and systems in R -- same book equations as the
# Python arm.  Expected values hand-computed from the printed equations.

test_that("LinConv implements eqs (3.36)-(3.39)", {
  r <- LinConv(c(1, 2), c(3, 4))
  expect_equal(r$y, c(3, 10, 8))
  expect_equal(r$n, 3L)
  expect_true(r$commutes)
  # eq (3.39): the contributions sum to the output
  s <- LinConv(c(1, -2, 0.5), c(2, 1))
  expect_equal(colSums(s$contributions), s$y)
  expect_equal(LinConv(1, c(5, -1, 2))$y, c(5, -1, 2))
  expect_error(LinConv(numeric(0), 1), "at least one")
})

test_that("LsiSer implements eqs (3.43)-(3.45)", {
  r <- LsiSer(c(1, 2, 3), c(1, 1), c(1, -1))
  expect_equal(r$s, c(1, 3, 5, 3))
  expect_equal(r$h, c(1, 0, -1))
  expect_equal(r$y, c(1, 2, 2, -2, -3))
  expect_true(r$equivalent)
  b <- LsiSerY(c(1, 2, 3), c(1, 1), c(1, -1))
  expect_equal(b$y, r$y)
  expect_equal(b$h, r$h)
})

test_that("LsiPar implements eqs (3.46)-(3.49)", {
  r <- LsiPar(c(1, 2), c(1, 1), 2)
  expect_equal(r$s1, c(1, 3, 2))
  expect_equal(r$s2, c(2, 4))
  expect_equal(r$h, c(3, 1))
  expect_equal(r$y, c(3, 7, 2))
  expect_true(r$equivalent)
  expect_equal(LsiPar(1, c(1, 2, 3), 1)$h, c(2, 2, 3))
  expect_equal(LsiPar2(c(1, 2), 2)$s2, c(2, 4))
  expect_equal(LsiParY(c(1, 2), c(1, 1), 2)$y, r$y)
})

test_that("a cascade convolves where a parallel pair adds", {
  expect_equal(LsiSer(1, c(1, 1), c(1, -1))$h, c(1, 0, -1))
  expect_equal(LsiPar(1, c(1, 1), c(1, -1))$h, c(2, 0))
})

test_that("LtiProd implements eq (3.53) in s and omega", {
  r <- LtiProd(c(1, 2, 1), c(1, -1), s = complex(real = 0.3, imaginary = 1.1),
               dt = 0.5)
  expect_true(r$holds)
  a <- LtiProd(c(1, 0.5, -0.25), c(1, 1), omega = 1.7, dt = 0.25)$Y
  b <- LtiProd(c(1, 0.5, -0.25), c(1, 1),
               s = complex(real = 0, imaginary = 1.7), dt = 0.25)$Y
  expect_equal(a, b, tolerance = 1e-14)
  expect_error(LtiProd(1, 1), "exactly one")
})

test_that("PerConv is eq (3.90)", {
  expect_equal(PerConv(c(1, 2), c(3, 4))$y, c(11, 10))
  expect_equal(PerConv(c(1, 2), c(3, 4), npoints = 3)$y, c(3, 10, 8))
})

test_that("AmSig uses the book's suppressed-carrier model", {
  r <- AmSig(rep(1, 8), fc = 1, fs = 8)
  expect_true(r$suppressed_carrier)
  expect_equal(r$y, r$carrier)
  expect_equal(AmSig(rep(0, 8), fc = 1, fs = 8)$y, rep(0, 8))
  z <- AmSig(rep(0, 8), fc = 1, fs = 8, conventional = TRUE)
  expect_equal(z$y, z$carrier)
  n <- 4096
  d <- AmSig(rep(1, n), fc = 100, fs = 1000)
  expect_equal(sum(d$demodulated) / n, 0.5, tolerance = 1e-3)
  expect_equal(d$image_frequency, 200)
  expect_error(AmSig(c(1, 2), fc = 600, fs = 1000), "fs/2")
})

test_that("FmSig tracks the instantaneous frequency", {
  r <- FmSig(c(rep(0, 10), rep(50, 10)), fc = 100, fs = 1000, kf = 1)
  expect_equal(r$instantaneous_frequency[1], 100)
  expect_equal(r$instantaneous_frequency[20], 150)
  expect_false(r$aliases)
  n <- 64
  z <- FmSig(rep(0, n), fc = 100, fs = 1000)
  expect_equal(z$y, cos(2 * pi * 100 * (0:(n - 1)) / 1000), tolerance = 1e-12)
  # trapezoidal phase: a constant modulator steps by 2 pi (fc + kf m)/fs
  p <- FmSig(rep(2, 5), fc = 10, fs = 100, kf = 1)
  expect_equal(p$phase[3] - p$phase[2], 2 * pi * 12 / 100)
  expect_true(FmSig(rep(400, 4), fc = 100, fs = 1000)$aliases)
})

test_that("TvLsi reduces to LSI for a constant kernel", {
  r <- TvLsi(c(1, 2, 3, 4), c(1, 0.5))
  expect_true(r$shift_invariant)
  expect_equal(r$y, c(1, 2.5, 4, 5.5))
  v <- TvLsi(c(1, 1, 1), list(1, 2, 3))
  expect_equal(v$y, c(1, 2, 3))
  expect_false(v$shift_invariant)
  expect_error(TvLsi(c(1, 2, 3), list(1, 1)), "one impulse response")
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_linear_convolution(c(1, 2), c(3, 4))$y, c(3, 10, 8))
  expect_equal(morie_ch3_lsi_parallel_total(1, 1, 2)$h, 3)
})
