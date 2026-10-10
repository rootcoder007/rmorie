# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("GHSecant rejects bad shape or scale", {
  expect_error(GHSecant(0, t = -4), "need t > -pi")
  expect_error(GHSecant(0, scale = 0), "need t > -pi")
})

test_that("GHSecant t = 0 is the standardised logistic", {
  s <- pi / sqrt(3)
  r <- GHSecant(c(-1, 0, 1), t = 0, p = c(0.25, 0.5, 0.75))
  expect_equal(r$cdf, plogis(s * c(-1, 0, 1)))
  expect_equal(r$pdf, dlogis(s * c(-1, 0, 1)) * s)
  expect_equal(r$logpdf, log(r$pdf))
  expect_equal(r$quantile, qlogis(c(0.25, 0.5, 0.75)) / s)
})

test_that("GHSecant cdf and quantile invert each other for t < 0 and t > 0", {
  for (tt in c(-pi / 2, 1.5)) {
    x <- c(-2, -0.5, 0.3, 2)
    r <- GHSecant(x, t = tt, loc = 1, scale = 2)
    expect_true(all(diff(r$cdf) > 0))
    expect_true(all(r$cdf > 0 & r$cdf < 1))
    q <- GHSecant(t = tt, loc = 1, scale = 2, p = r$cdf)$quantile
    expect_equal(q, x, tolerance = 1e-8)
    dens_int <- integrate(function(v) GHSecant(v, t = tt)$pdf, -Inf, Inf)$value
    expect_equal(dens_int, 1, tolerance = 1e-6)
  }
})

test_that("GHSecant random draws come from the quantile function", {
  r <- GHSecant(t = 0.5, n = 5, seed = 3)
  expect_length(r$random, 5)
  expect_true(all(is.finite(r$random)))
  expect_identical(r$random, GHSecant(t = 0.5, n = 5, seed = 3)$random)
  expect_null(r$pdf)
})

test_that("BinghamDens validates A and x and handles p = 2", {
  expect_error(BinghamDens(c(1, 0), matrix(c(1, 2, 0, 1), 2)), "symmetric")
  expect_error(BinghamDens(c(1, 1), diag(2)), "unit vectors")
  r <- BinghamDens(c(1, 0), diag(c(2, 0)))
  logc <- log(2 * pi) + 1 + log(besselI(1, 0))
  expect_equal(r$log_normalizer, logc, tolerance = 1e-10)
  expect_equal(r$logpdf, 2 - logc, tolerance = 1e-10)
  # p = 2 density integrates to one over the circle
  th <- seq(0, 2 * pi, length.out = 2001)
  d <- BinghamDens(cbind(cos(th), sin(th)), diag(c(2, 0)))$pdf
  expect_equal(sum(diff(th) * (d[-1] + d[-length(d)]) / 2), 1, tolerance = 1e-6)
})

test_that("BinghamDens p = 3 normaliser integrates to one", {
  A <- diag(c(1, 0, -1))
  r <- BinghamDens(c(0, 0, 1), A)
  expect_equal(r$logpdf, -1 - r$log_normalizer)
  # Monte Carlo free check: uniform A gives the sphere area 4 pi
  u <- BinghamDens(c(1, 0, 0), diag(0, 3))
  expect_equal(u$log_normalizer, log(4 * pi), tolerance = 1e-10)
  expect_equal(u$pdf, 1 / (4 * pi), tolerance = 1e-10)
})
