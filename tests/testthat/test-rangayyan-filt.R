# Rangayyan filter design in R: LSI combination, the Laplace route, the
# generic IIR and pole-zero responses, the moving average, the Hann
# filter, and the order-statistic filters of Section 3.8.
# Same book equations and the same expected values as the Python arm.

SPIKY_F <- c(1, 1, 9, 1, 1, 1, -9, 1, 1)

test_that("LsiSerH eq (3.45) convolves and lengthens, and commutes", {
  expect_equal(LsiSerH(c(1, 1), c(1, 1))$h, c(1, 2, 1))
  expect_equal(LsiSerH(c(1, 1), c(1, 1))$n_taps, 3L)
  expect_equal(LsiSerH(c(1, 2, 3), 1)$h, c(1, 2, 3))
  expect_equal(LsiSerH(c(1, 2, 3), c(0.5, -1))$h,
               LsiSerH(c(0.5, -1), c(1, 2, 3))$h)
  expect_equal(LsiSerH(c(1, 1), c(1, 1), n = 1)$value, 2)
  expect_error(LsiSerH(c(1, 1), c(1, 1), n = -1), "nonnegative")
})

test_that("LsiParH eq (3.49) adds and keeps the longer length", {
  r <- LsiParH(c(1, 2), c(10, 20, 30))
  expect_equal(r$h, c(11, 22, 30))
  expect_equal(r$n_taps, 3L)
  expect_true(r$length_is_the_longer_branch)
  expect_equal(LsiParH(c(1, 2), 3, n = 5)$value, 0)
  expect_error(LsiParH(numeric(0), 1), "at least one tap")
})

test_that("Laplace eq (3.50) of a unit pulse", {
  t <- (0:200) / 200
  h <- rep(1, 201)
  expect_equal(Re(Laplace(h, t, 0)$H), 1, tolerance = 1e-12)
  expect_equal(Re(Laplace(h, t, 1)$H), (1 - exp(-1)), tolerance = 1e-5)
  r <- Laplace(h, t, 0)
  expect_equal(c(r$t_min, r$t_max), c(0, 1))
  expect_true(r$over_the_sampled_interval_only)
  expect_error(Laplace(c(1, 1), c(1, 0), 0), "increasing")
  expect_error(Laplace(1, 0, 0), "two samples")
})

test_that("LaplaceFr eq (3.52) is Laplace on the imaginary axis", {
  t <- (0:100) / 100
  h <- exp(-2 * t)
  a <- LaplaceFr(h, 3, t = t)$H
  b <- Laplace(h, t, complex(real = 0, imaginary = 3))$H
  expect_equal(Re(a), Re(b), tolerance = 1e-12)
  expect_equal(Im(a), Im(b), tolerance = 1e-12)
  expect_equal(Re(LaplaceFr(rep(1, 101), 0, T = 1)$H), 1, tolerance = 1e-12)
  expect_error(LaplaceFr(rep(1, 101), 0), "either the sample times")
  expect_error(LaplaceFr(rep(1, 101), 0, T = -1), "positive")
})

test_that("IirTf eq (3.67) treats the leading one as implicit", {
  r <- IirTf(1, -0.5, 1)
  expect_equal(Re(r$H), 2)
  expect_equal(r$denominator, c(1, -0.5))
  expect_true(r$leading_one_is_implicit)
  expect_error(IirTf(1, -1, 1), "pole")
  expect_error(IirTf(c(1, 2), NULL, 1, N = 5), "N must be")
  expect_error(IirTf(1, 0.5, 1, M = 3), "M must be")
})

test_that("IirDiff eq (3.68) subtracts the feedback", {
  expect_equal(IirDiff(c(1, 0, 0, 0), 1, -0.5)$y, c(1, 0.5, 0.25, 0.125))
  expect_true(IirDiff(c(1, 0, 0, 0), 1, -0.5)$recursive)
  r <- IirDiff(c(1, 2, 3), c(0.5, 0.5))
  expect_equal(r$y, c(0.5, 1.5, 2.5))
  expect_false(r$recursive)
  # the settled step response must equal the DC gain of the transfer fn
  y <- IirDiff(rep(1, 60), c(1, 0.5), -0.25)$y
  expect_equal(y[60], Re(IirTf(c(1, 0.5), -0.25, 1)$H), tolerance = 1e-9)
})

test_that("PzMag eq (3.72) is the ratio of distance products", {
  r <- PzMag(c(2, 3), c(1, 2))
  expect_equal(r$magnitude, 3)
  expect_equal(r$zero_product, 6)
  expect_equal(PzMag(0, 1)$magnitude, 0)
  expect_true(PzMag(0, 1)$on_a_zero)
  expect_error(PzMag(1, 0), "unbounded")
  expect_error(PzMag(-1, 1), "negative")
})

test_that("PzPhase eq (3.73) keeps the origin term", {
  r <- PzPhase(complex(real = 0, imaginary = 1), 0.2, c(0.1, 0.3))
  expect_equal(r$origin_term, pi / 2)
  expect_equal(r$phase, pi / 2 + 0.2 - 0.4)
  expect_false(r$origin_term_vanishes_when_orders_match)
  m <- PzPhase(complex(real = 0, imaginary = 1), c(0.2, 0.1), c(0.1, 0.3))
  expect_equal(m$origin_term, 0)
  expect_true(m$origin_term_vanishes_when_orders_match)
  expect_error(PzPhase(0, 0.1, 0.2), "no defined angle")
})

test_that("MaFir defaults to the equal-weight boxcar", {
  r <- MaFir(c(1, 2, 3, 4), N = 1)
  expect_equal(r$b, c(0.5, 0.5))
  expect_equal(r$y, c(0.5, 1.5, 2.5, 3.5))
  expect_true(r$equal_weights)
  expect_equal(r$delay_samples, 0.5)
  expect_equal(MaFir(rep(5, 20), N = 4)$y[5:20], rep(5, 16))
  expect_error(MaFir(c(1, 2)), "either the coefficients")
  expect_error(MaFir(c(1, 2), b_k = 1, N = 7), "N must be")
})

test_that("MaTf eq (3.99) is a polynomial with no poles", {
  r <- MaTf(c(0.25, 0.5, 0.25), 1)
  expect_equal(Re(r$H), 1)
  expect_equal(r$dc_gain, 1)
  expect_true(r$always_stable)
  expect_error(MaTf(1, 0), "pole")
})

test_that("HannFilt eq (3.100) is the one-two-one smoother", {
  r <- HannFilt(c(0, 0, 4, 0, 0))
  expect_equal(r$taps, c(0.25, 0.5, 0.25))
  expect_equal(r$y, c(0, 0, 1, 2, 1))
  expect_equal(r$delay_samples, 1)
  expect_equal(HannFilt(rep(3, 8))$y[3:8], rep(3, 6))
  expect_error(HannFilt(1, n = 9), "outside")
})

test_that("HannImp eq (3.101) is finite and sums to one", {
  r <- HannImp()
  expect_equal(r$h, c(0.25, 0.5, 0.25))
  expect_equal(r$sum, 1)
  expect_true(r$finite)
  expect_equal(HannImp(n = 5)$value, 0)
  # it is the response to a unit impulse
  expect_equal(HannFilt(c(1, 0, 0, 0))$y[1:3], r$h)
})

test_that("HannZ eq (3.102) factors the input out", {
  a <- HannZ(2, 1)
  b <- HannZ(5, 1)
  expect_equal(a$H, b$H)
  expect_equal(Re(a$Y), 2 * Re(a$H))
  expect_true(a$transfer_function_is_input_independent)
})

test_that("HannTf eq (3.103) has a double zero at Nyquist", {
  expect_equal(Mod(HannTf(-1)$H), 0, tolerance = 1e-15)
  expect_equal(HannTf(-1)$zero_multiplicity, 2L)
  expect_equal(Re(HannTf(1)$H), 1)
  expect_error(HannTf(0), "pole")
})

test_that("HannFr eq (3.104) agrees with the transfer function", {
  for (w in c(0, 0.3, 1, pi)) {
    z <- complex(real = cos(w), imaginary = sin(w))
    expect_equal(HannFr(w)$H, HannTf(z)$H, tolerance = 1e-12)
  }
})

test_that("HannFrs eq (3.105) agrees with the raw form", {
  r <- HannFrs(c(0, 0.4, 1.2, pi))
  expect_true(r$agrees_with_raw_form)
  expect_lt(r$max_difference_from_eq_3_104, 1e-12)
  expect_true(r$linear_phase)
})

test_that("HannMag eq (3.106) is lowpass and matches the modulus", {
  expect_equal(HannMag(0)$magnitude, 1)
  expect_equal(HannMag(pi)$magnitude, 0, tolerance = 1e-15)
  v <- HannMag(c(0, 0.5, 1, 2, pi))$magnitude
  expect_true(all(diff(v) <= 1e-12))
  for (w in c(0, 0.7, 2, pi))
    expect_equal(HannMag(w)$magnitude, Mod(HannFr(w)$H), tolerance = 1e-12)
})

test_that("HannPh eq (3.107) is exactly linear", {
  r <- HannPh(c(0, 0.5, 1))
  expect_equal(r$phase, c(0, -0.5, -1))
  expect_equal(r$group_delay, 1)
  expect_true(r$constant_group_delay)
  for (w in c(0.2, 1, 2.5))
    expect_equal(Arg(HannFr(w)$H), HannPh(w)$phase, tolerance = 1e-12)
})

test_that("the median filter removes impulses of both signs", {
  r <- OsFilt(SPIKY_F, 3)
  expect_equal(r$y, rep(1, 9))
  expect_true(r$nonlinear)
  expect_true(r$no_frequency_response)
})

test_that("min removes high-valued and max low-valued impulses", {
  lo <- OsFilt(SPIKY_F, 3, kind = "min")$y
  hi <- OsFilt(SPIKY_F, 3, kind = "max")$y
  expect_false(any(lo == 9))          # the book's stated use of Min
  expect_true(any(lo == -9))
  expect_false(any(hi == -9))         # and of Max
  expect_true(any(hi == 9))
  expect_equal(OsFilt(SPIKY_F, 3, kind = "minmax")$y,
               OsFilt(OsFilt(SPIKY_F, 3, kind = "min")$y, 3,
                      kind = "max")$y)
})

test_that("order 1 is the min, order w the max, and the middle the median", {
  expect_equal(OsFilt(SPIKY_F, 3, kind = "order", order = 1)$y,
               OsFilt(SPIKY_F, 3, kind = "min")$y)
  expect_equal(OsFilt(SPIKY_F, 3, kind = "order", order = 3)$y,
               OsFilt(SPIKY_F, 3, kind = "max")$y)
  expect_equal(OsFilt(SPIKY_F, 5, kind = "order", order = 3)$y,
               OsFilt(SPIKY_F, 5)$y)
})

test_that("alpha near one half approaches the median, alpha zero the mean", {
  expect_equal(OsFilt(SPIKY_F, 5, kind = "trimmed", alpha = 0.4)$y,
               OsFilt(SPIKY_F, 5)$y)
  expect_equal(OsFilt(SPIKY_F, 5, kind = "trimmed",
                      alpha = 0.4)$trimmed_each_end, 2L)
  x <- c(1, 2, 3, 4, 5)
  expect_equal(OsFilt(x, 3, kind = "trimmed", alpha = 0)$y,
               OsFilt(x, 3, kind = "l", weights = c(1, 1, 1))$y)
})

test_that("the L-filter can reproduce the median", {
  expect_equal(OsFilt(SPIKY_F, 3, kind = "l", weights = c(0, 1, 0))$y,
               OsFilt(SPIKY_F, 3)$y)
})

test_that("the edges are reflected so a ramp survives untouched", {
  r <- OsFilt(c(1, 2, 3, 4, 5), 3)
  expect_equal(r$n, 5L)
  expect_equal(r$y, c(1, 2, 3, 4, 5))
  expect_equal(r$edges, "symmetric reflection")
})

test_that("bad order-statistic arguments are refused", {
  expect_error(OsFilt(SPIKY_F, 4), "odd")
  expect_error(OsFilt(SPIKY_F, 99), "longer than the record")
  expect_error(OsFilt(SPIKY_F, 3, kind = "bogus"), "kind must be")
  expect_error(OsFilt(SPIKY_F, 3, kind = "trimmed", alpha = 0.5), "alpha")
  expect_error(OsFilt(SPIKY_F, 3, kind = "l"), "one weight per rank")
  expect_error(OsFilt(SPIKY_F, 3, kind = "l", weights = c(1, 1)), "weights")
  expect_error(OsFilt(SPIKY_F, 3, kind = "order"), "rank to take")
  expect_error(OsFilt(SPIKY_F, 3, kind = "order", order = 4), "1\\.\\.3")
})
