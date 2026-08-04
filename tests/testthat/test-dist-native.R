# The native distribution layer against R's own d/p/q/r.
#
# stats:: appears here ONLY as the verification reference -- the
# implementations under test never call it.  Anchors are computed by this
# R at run time, so no expectation is transcribed by hand.

test_that("native normal matches R to near machine precision", {
  for (x in c(-3, -1.5, 0, 0.7, 2.4)) {
    expect_equal(morie_dnorm(x, 2, 3), dnorm(x, 2, 3), tolerance = 1e-14)
    expect_equal(morie_pnorm(x), pnorm(x), tolerance = 1e-14)
  }
  for (p in c(0.001, 0.025, 0.5, 0.9, 0.999)) {
    expect_equal(morie_qnorm(p), qnorm(p), tolerance = 1e-12)
  }
  # far tail survives because it comes from erfc, not 1 - cdf
  expect_equal(morie_pnorm(-9), pnorm(-9), tolerance = 1e-9)
  expect_gt(morie_pnorm(9, lower_tail = FALSE), 0)
})

test_that("native exponential and gamma match R", {
  expect_equal(morie_dexp(1, 2), dexp(1, 2), tolerance = 1e-14)
  expect_equal(morie_pexp(1, 2), pexp(1, 2), tolerance = 1e-14)
  expect_equal(morie_qexp(0.5, 2), qexp(0.5, 2), tolerance = 1e-12)
  expect_equal(morie_dgamma(2, 3, 1), dgamma(2, 3, 1), tolerance = 1e-14)
  expect_equal(morie_pgamma(2, 3, 1), pgamma(2, 3, 1), tolerance = 1e-13)
  expect_equal(morie_qgamma(0.5, 3, 1), qgamma(0.5, 3, 1), tolerance = 1e-9)
  expect_equal(morie_pchisq(3.84, 1), pchisq(3.84, 1), tolerance = 1e-13)
  expect_equal(morie_qchisq(0.95, 1), qchisq(0.95, 1), tolerance = 1e-9)
})

test_that("native discrete laws match R, including the quantile convention", {
  expect_equal(morie_dpois(3, 2.5), dpois(3, 2.5), tolerance = 1e-13)
  expect_equal(morie_ppois(3, 2.5), ppois(3, 2.5), tolerance = 1e-13)
  expect_equal(morie_qpois(0.5, 2.5), qpois(0.5, 2.5))
  expect_equal(morie_dbinom(3, 10, 0.3), dbinom(3, 10, 0.3),
               tolerance = 1e-13)
  expect_equal(morie_pbinom(3, 10, 0.3), pbinom(3, 10, 0.3),
               tolerance = 1e-13)
  expect_equal(morie_qbinom(pbinom(3, 10, 0.3), 10, 0.3), 3)
  expect_equal(morie_dbinom(0, 10, 0), 1)
  expect_equal(morie_dbinom(10, 10, 1), 1)
})

test_that("native beta, t and F match R", {
  expect_equal(morie_dbeta(0.5, 2, 3), dbeta(0.5, 2, 3), tolerance = 1e-13)
  expect_equal(morie_pbeta(0.5, 2, 3), pbeta(0.5, 2, 3), tolerance = 1e-13)
  expect_equal(morie_qbeta(0.6875, 2, 3), qbeta(0.6875, 2, 3),
               tolerance = 1e-9)
  expect_equal(morie_dt(0, 5), dt(0, 5), tolerance = 1e-13)
  expect_equal(morie_pt(2.015, 5), pt(2.015, 5), tolerance = 1e-13)
  expect_equal(morie_qt(0.975, 10), qt(0.975, 10), tolerance = 1e-9)
  expect_equal(morie_qt(0.5, 7), 0)
  expect_equal(morie_pf(4.26, 3, 10), pf(4.26, 3, 10), tolerance = 1e-13)
  expect_equal(morie_qf(0.95, 3, 10), qf(0.95, 3, 10), tolerance = 1e-8)
})

test_that("native draws are reproducible and stream-stable", {
  a <- morie_rnorm(6, seed = 42)
  b <- morie_rnorm(3, seed = 42)
  # inversion: draw k depends only on uniform k, so a prefix matches
  expect_equal(a[1:3], b, tolerance = 1e-15)
  e <- morie_rexp(50, rate = 2, seed = 7)
  expect_true(all(e > 0))
})

test_that("parameters are validated", {
  expect_error(morie_dnorm(0, sd = 0), "positive")
  expect_error(morie_qnorm(0), "inside")
  expect_error(morie_dgamma(1, -1), "positive")
  expect_error(morie_dbinom(1, 10, 1.5), "\\[0, 1\\]")
  expect_error(morie_dexp(1, 0), "positive")
})
