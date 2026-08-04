# Rangayyan transforms in R, against the same book equations as the
# Python arm.  Expected values hand-computed from the printed equations.

FOUR <- c(1, 2, 3, 4)
FOUR_DFT <- c(complex(real = 10, imaginary = 0),
              complex(real = -2, imaginary = 2),
              complex(real = -2, imaginary = 0),
              complex(real = -2, imaginary = -2))

test_that("Ztrans implements eqs (3.54)-(3.55)", {
  expect_equal(Re(Ztrans(c(1, 2, 3), z = 2)$X), 1 + 1 + 0.75)
  expect_equal(Re(Ztrans(c(1, -2, 4), z = 1)$X), 3)
  expect_equal(Re(Ztrans(c(5, 1, 2), z = 2, n0 = -1)$X), 10 + 1 + 1)
  expect_false(Ztrans(c(5, 1, 2), z = 2, n0 = -1)$causal)
  expect_error(Ztrans(c(1, 2), z = 0), "pole")
})

test_that("ZtConv implements eq (3.56)", {
  r <- ZtConv(c(1, 2), c(3, 4), z = 1.7)
  expect_equal(r$y, c(3, 10, 8))
  expect_true(r$holds)
  expect_equal(r$max_difference, 0, tolerance = 1e-12)
})

test_that("DtftZ evaluates on the unit circle (eq 3.66)", {
  r <- DtftZ(c(1, 2, 3), omega = 0.7)
  expect_true(r$on_unit_circle)
  want <- sum(c(1, 2, 3) * exp(-1i * 0.7 * (0:2)))
  expect_equal(r$X, want, tolerance = 1e-12)
  rt <- DtftZ(c(1, 2), omega = 2, fs = 4)
  expect_equal(rt$T, 0.25)
  expect_equal(rt$X, 1 + 2 * exp(-1i * 2 * 0.25), tolerance = 1e-12)
})

test_that("Euler implements eq (3.74)", {
  r <- Euler(pi, 1)
  expect_equal(Re(r$value), -1, tolerance = 1e-15)
  expect_equal(Im(r$value), 0, tolerance = 1e-15)
  expect_true(r$unit_modulus)
})

test_that("Ctft implements eqs (3.75)-(3.76) in either variable", {
  x <- rep(1, 201); t <- seq(0, 2, length.out = 201)
  expect_equal(Re(Ctft(x, t = t, omega = 0)$X), 2, tolerance = 1e-12)
  xs <- c(1, 0.5, -0.25, 0.75, 0)
  ts <- c(0, 0.25, 0.5, 0.75, 1)
  f0 <- 0.7
  a <- Ctft(xs, t = ts, omega = 2 * pi * f0)$X
  b <- Ctft(xs, t = ts, f = f0)$X
  expect_equal(a, b, tolerance = 1e-14)
  expect_equal(CtftF(xs, f0, t = ts)$X, b, tolerance = 1e-14)
  expect_equal(Fourier(xs, t = ts, f = f0)$X, b, tolerance = 1e-14)
  expect_equal(Ctft(rep(1, 3), omega = 2 * pi, dt = 0.5)$f, 1)
  expect_error(Ctft(c(1, 2)), "exactly one")
  expect_error(Ctft(c(1, 2), omega = 1, f = 1), "exactly one")
})

test_that("Ictft carries 1/(2 pi) only in the omega form (eq 3.77)", {
  grid <- seq(-1, 1, length.out = 401)
  X <- rep(complex(real = 1, imaginary = 0), 401)
  om <- Ictft(X, t = 0, omega = grid)
  hz <- Ictft(X, t = 0, f = grid)
  expect_equal(om$scale, 1 / (2 * pi))
  expect_equal(hz$scale, 1)
  expect_equal(Re(om$x), 2 / (2 * pi), tolerance = 1e-12)
  expect_equal(Re(hz$x), 2, tolerance = 1e-12)
  expect_error(Ictft(c(1, 2), t = 0, omega = c(0, 1, 2)), "equal length")
})

test_that("Dtft implements eq (3.78) and is 2-pi periodic", {
  expect_equal(Re(Dtft(c(1, -2, 3), 0)$X), 2)
  expect_equal(Dtft(c(1, -2, 3, 0.5), 0.9)$X,
               Dtft(c(1, -2, 3, 0.5), 0.9 + 2 * pi)$X, tolerance = 1e-11)
})

test_that("Dft implements eq (3.80)", {
  r <- Dft(FOUR)
  expect_equal(r$X, FOUR_DFT, tolerance = 1e-12)
  expect_true(r$conjugate_symmetric)
  expect_equal(Dft(c(1, 0, 0, 0))$X, rep(complex(real = 1), 4),
               tolerance = 1e-15)
})

test_that("DftK implements eq (3.79) at any K", {
  expect_equal(DftK(FOUR, 4)$X, FOUR_DFT, tolerance = 1e-12)
  expect_false(DftK(FOUR, 4)$aliased)
  r <- DftK(FOUR, 8)
  for (k in 0:7) {
    expect_equal(r$X[k + 1], Dtft(FOUR, 2 * pi * k / 8)$X, tolerance = 1e-10)
  }
  expect_true(DftK(FOUR, 2)$aliased)
})

test_that("DftX puts the frequency axis in Hz", {
  r <- DftX(FOUR, fs = 8)
  expect_equal(r$freqs, c(0, 2, 4, 6))
  expect_equal(r$folding_frequency, 4)
  expect_equal(r$unique_bins, 3L)
})

test_that("Twiddle and its two FFT properties (eqs 3.82, 3.88, 3.89)", {
  expect_equal(Twiddle(4)$W, complex(real = 0, imaginary = -1),
               tolerance = 1e-15)
  expect_equal(Re(Twiddle(4, 2)$W), -1, tolerance = 1e-15)
  expect_true(Twiddle(4)$root_of_unity)
  expect_true(TwidConj(8, 3, 2)$holds)
  expect_true(TwidPer(8, 3, 2)$holds)
})

test_that("DftTw agrees with the definition (eq 3.83)", {
  r <- DftTw(FOUR)
  expect_equal(r$X, FOUR_DFT, tolerance = 1e-10)
  expect_true(r$agrees_with_definition)
})

test_that("TwidCS has the minus sign on the sine (eq 3.84)", {
  r <- TwidCS(8, 1, 1)
  ang <- 2 * pi / 8
  expect_equal(r$cos, cos(ang))
  expect_equal(r$sin, sin(ang))
  expect_equal(Im(r$W), -sin(ang))
})

test_that("DftRI splits the DFT into projections (eq 3.85)", {
  r <- DftRI(FOUR)
  expect_equal(r$X, FOUR_DFT, tolerance = 1e-12)
  expect_equal(r$imag, -r$sin_projection)
})

test_that("IdftRI inverts the DFT (eq 3.86)", {
  r <- IdftRI(FOUR_DFT)
  expect_equal(r$x, FOUR, tolerance = 1e-12)
  expect_equal(r$max_imaginary, 0, tolerance = 1e-12)
  bad <- IdftRI(c(complex(real = 1), complex(real = 1, imaginary = 1),
                  complex(real = 0), complex(real = 0)))
  expect_gt(bad$max_imaginary, 1e-6)
})

test_that("DftConv needs padding for linear convolution (eq 3.87)", {
  r <- DftConv(c(1, 2), c(3, 4))
  expect_equal(r$linear, c(3, 10, 8))
  expect_equal(r$padded_length, 3L)
  expect_equal(r$from_dft, c(3, 10, 8), tolerance = 1e-12)
  expect_true(r$holds)
  expect_equal(r$circular, c(11, 10))
  expect_true(r$wraps_if_unpadded)
})

test_that("CircConv implements eq (3.90) both ways", {
  r <- CircConv(c(1, 2), c(3, 4))
  expect_equal(r$y, c(11, 10))
  expect_equal(r$via_dft, c(11, 10), tolerance = 1e-12)
  expect_true(r$agrees)
  expect_false(r$equals_linear)
  p <- CircConv(c(1, 2), c(3, 4), npoints = 3)
  expect_equal(p$y, c(3, 10, 8))
  expect_true(p$equals_linear)
  expect_error(CircConv(c(1, 2, 3), 1, npoints = 2), "at least the length")
})

test_that("EvenOdd implements eqs (3.92)-(3.94)", {
  r <- EvenOdd(c(1, 2, 3), c(-1, 0, 1))
  expect_equal(r$even, c(2, 2, 2))
  expect_equal(r$odd, c(-1, 0, 1))
  expect_equal(r$reconstruction_error, 0, tolerance = 1e-15)
  expect_equal(OddPart(c(4, 7, -1, 0, 2), n = -2:2)$odd[3], 0)
  e <- EvenPart(c(3, 5, 3), n = c(-1, 0, 1))
  expect_equal(e$even, c(3, 5, 3))
  expect_equal(e$odd, c(0, 0, 0))
  expect_equal(EvenOdd(c(1, 2, 3))$n, c(-1L, 0L, 1L))
  expect_error(EvenOdd(c(1, 2)), "odd length")
  expect_error(EvenOdd(c(1, 2, 3), n = c(0, 1, 2)), "not symmetric")
})

test_that("LogFT shows log-spectra add (eqs 4.58-4.60)", {
  r <- LogFT(c(1, 2, 3, 2, 1), c(2, 2, 4, 4, 8), omega = 1.3, dt = 0.5)
  expect_equal(r$y, c(2, 4, 12, 8, 8))
  expect_true(r$additive)
  expect_error(LogFT(c(1, 0), c(1, 1), omega = 0), "!= 0")
})

test_that("FtConv implements eqs (4.61)-(4.62)", {
  r <- FtConv(c(1, 2, 1), c(1, -1), omega = 0.9, dt = 0.25)
  expect_equal(r$y, c(0.25, 0.25, -0.25, -0.25))
  expect_true(r$holds)
})

test_that("ClogSum adds complex logs up to a branch (eq 4.65)", {
  r <- ClogSum(c(1, 0.5), c(1, -0.3), z = 1.4)
  expect_equal(r$magnitude_difference, 0, tolerance = 1e-12)
  expect_true(r$holds_up_to_branch)
  w <- ClogSum(c(1, -0.9, 0.8), c(1, -0.95, 0.9),
               z = complex(real = 0.2, imaginary = 0.98))
  expect_equal(w$branch_offset, round(w$branch_offset), tolerance = 1e-9)
  expect_error(ClogSum(c(1, -1), 1, z = 1), "!= 0")
})

test_that("LogSeries implements eq (4.69) inside its radius", {
  r <- LogSeries(0.5, terms = 60)
  expect_equal(Re(r$value), log(1.5), tolerance = 1e-12)
  expect_lte(LogSeries(0.5, terms = 5)$error,
             LogSeries(0.5, terms = 5)$error_bound)
  expect_error(LogSeries(1.5), "converges only")
  expect_error(LogSeries(-1), "converges only")
})

test_that("LogMinPh is causal, LogMaxPh anticausal (eqs 4.70-4.71)", {
  a <- LogMinPh(0.5, terms = 80, z = 2)
  expect_true(a$causal)
  expect_equal(a$quefrency[1], 1L)
  expect_equal(Re(a$coefficients[1]), -0.5)
  expect_equal(Re(a$coefficients[2]), -0.125)
  expect_lt(a$error, 1e-12)
  expect_error(LogMinPh(0.5, z = 0.25), "\\|z\\| > \\|alpha\\|")
  b <- LogMaxPh(0.5, terms = 80, z = 0.5)
  expect_false(b$causal)
  expect_equal(b$quefrency[1], -1L)
  expect_lt(b$error, 1e-12)
  expect_error(LogMaxPh(0.5, z = 2.5), "1/\\|beta\\|")
})

test_that("pre-policy spellings still resolve", {
  expect_equal(morie_ch3_dft(FOUR)$X, FOUR_DFT, tolerance = 1e-12)
  expect_equal(morie_circular_conv_dft(c(1, 2), c(3, 4))$y, c(11, 10))
})
