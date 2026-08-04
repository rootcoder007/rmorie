# The native distribution layer against R's own d/p/q/r.
#
# stats:: appears here ONLY as the verification reference -- the
# implementations under test never call it.  Anchors are computed by this
# R at run time, so no expectation is transcribed by hand.

test_that("native normal matches R to near machine precision", {
  for (x in c(-3, -1.5, 0, 0.7, 2.4)) {
    expect_equal(Dnorm(x, 2, 3), dnorm(x, 2, 3), tolerance = 1e-14)
    expect_equal(Pnorm(x), pnorm(x), tolerance = 1e-14)
  }
  for (p in c(0.001, 0.025, 0.5, 0.9, 0.999)) {
    expect_equal(Qnorm(p), qnorm(p), tolerance = 1e-12)
  }
  # far tail survives because it comes from erfc, not 1 - cdf
  expect_equal(Pnorm(-9), pnorm(-9), tolerance = 1e-9)
  expect_gt(Pnorm(9, lower_tail = FALSE), 0)
})

test_that("native exponential and gamma match R", {
  expect_equal(Dexp(1, 2), dexp(1, 2), tolerance = 1e-14)
  expect_equal(Pexp(1, 2), pexp(1, 2), tolerance = 1e-14)
  expect_equal(Qexp(0.5, 2), qexp(0.5, 2), tolerance = 1e-12)
  expect_equal(Dgamma(2, 3, 1), dgamma(2, 3, 1), tolerance = 1e-14)
  expect_equal(Pgamma(2, 3, 1), pgamma(2, 3, 1), tolerance = 1e-13)
  expect_equal(Qgamma(0.5, 3, 1), qgamma(0.5, 3, 1), tolerance = 1e-9)
  expect_equal(Pchisq(3.84, 1), pchisq(3.84, 1), tolerance = 1e-13)
  expect_equal(Qchisq(0.95, 1), qchisq(0.95, 1), tolerance = 1e-9)
})

test_that("native discrete laws match R, including the quantile convention", {
  expect_equal(Dpois(3, 2.5), dpois(3, 2.5), tolerance = 1e-13)
  expect_equal(Ppois(3, 2.5), ppois(3, 2.5), tolerance = 1e-13)
  expect_equal(Qpois(0.5, 2.5), qpois(0.5, 2.5))
  expect_equal(Dbinom(3, 10, 0.3), dbinom(3, 10, 0.3),
               tolerance = 1e-13)
  expect_equal(Pbinom(3, 10, 0.3), pbinom(3, 10, 0.3),
               tolerance = 1e-13)
  expect_equal(Qbinom(pbinom(3, 10, 0.3), 10, 0.3), 3)
  expect_equal(Dbinom(0, 10, 0), 1)
  expect_equal(Dbinom(10, 10, 1), 1)
})

test_that("native beta, t and F match R", {
  expect_equal(Dbeta(0.5, 2, 3), dbeta(0.5, 2, 3), tolerance = 1e-13)
  expect_equal(Pbeta(0.5, 2, 3), pbeta(0.5, 2, 3), tolerance = 1e-13)
  expect_equal(Qbeta(0.6875, 2, 3), qbeta(0.6875, 2, 3),
               tolerance = 1e-9)
  expect_equal(Dt(0, 5), dt(0, 5), tolerance = 1e-13)
  expect_equal(Pt(2.015, 5), pt(2.015, 5), tolerance = 1e-13)
  expect_equal(Qt(0.975, 10), qt(0.975, 10), tolerance = 1e-9)
  expect_equal(Qt(0.5, 7), 0)
  expect_equal(Pf(4.26, 3, 10), pf(4.26, 3, 10), tolerance = 1e-13)
  expect_equal(Qf(0.95, 3, 10), qf(0.95, 3, 10), tolerance = 1e-8)
})

test_that("native draws are reproducible and stream-stable", {
  a <- Rnorm(6, seed = 42)
  b <- Rnorm(3, seed = 42)
  # inversion: draw k depends only on uniform k, so a prefix matches
  expect_equal(a[1:3], b, tolerance = 1e-15)
  e <- Rexp(50, rate = 2, seed = 7)
  expect_true(all(e > 0))
})

test_that("parameters are validated", {
  expect_error(Dnorm(0, sd = 0), "positive")
  expect_error(Qnorm(0), "inside")
  expect_error(Dgamma(1, -1), "positive")
  expect_error(Dbinom(1, 10, 1.5), "\\[0, 1\\]")
  expect_error(Dexp(1, 0), "positive")
})

test_that("pre-policy underscore spellings still work", {
  expect_equal(morie_dnorm(0), Dnorm(0))
  expect_equal(morie_qnorm(0.975), Qnorm(0.975))
})
