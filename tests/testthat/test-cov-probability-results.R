# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/ProbabilityResults.R exports: the spectral density of a
# stationary series from its autocorrelations, the bivariate-normal
# dependence measures, Mills' conditional mean and the telegraph
# process. Expected values come from the defining formulas, with the
# spectral densities checked to integrate to one.

test_that("AcfSpectralDensity is (1 + 2 sum rho_n cos(n lam)) / 2 pi", {
  rho <- c(0.5, 0.25, 0.1)
  for (lam in c(0, 0.3, pi / 2, pi)) {
    expect_equal(AcfSpectralDensity(lam, rho),
                 (1 + 2 * sum(rho * cos(seq_along(rho) * lam))) / (2 * pi), tolerance = 1e-12)
  }
  # white noise: flat at 1 / 2 pi, and it integrates to one over (-pi, pi)
  expect_equal(AcfSpectralDensity(1.1, numeric(0)), 1 / (2 * pi), tolerance = 1e-12)
  expect_equal(integrate(function(v) vapply(v, AcfSpectralDensity, 0, rho = rho), -pi, pi,
                         rel.tol = 1e-12)$value, 1, tolerance = 1e-10)
  # even in lambda
  expect_equal(AcfSpectralDensity(0.7, rho), AcfSpectralDensity(-0.7, rho), tolerance = 1e-12)
})

test_that("BivNormDependence gives phi^2 and the mutual information", {
  for (rho in c(0, 0.3, -0.8)) {
    r <- BivNormDependence(rho)
    expect_equal(r$phi2, rho^2 / (1 - rho^2), tolerance = 1e-12)
    expect_equal(r$mutual_information, -0.5 * log(1 - rho^2), tolerance = 1e-12)
  }
  expect_equal(BivNormDependence(0)$mutual_information, 0)
  # both measures increase with |rho|
  expect_gt(BivNormDependence(0.9)$phi2, BivNormDependence(0.5)$phi2)
  expect_error(BivNormDependence(1), "rho must lie in \\(-1, 1\\)")
  expect_error(BivNormDependence(-1), "rho must lie in \\(-1, 1\\)")
})

test_that("MillsConditionalMean is rho phi(x) / Phi(-x)", {
  for (x in c(-1, 0, 2.5)) {
    expect_equal(MillsConditionalMean(x, 0.6), 0.6 * dnorm(x) / pnorm(-x), tolerance = 1e-12)
  }
  expect_equal(MillsConditionalMean(0, 1), sqrt(2 / pi), tolerance = 1e-12)
  expect_equal(MillsConditionalMean(1.5, 0), 0)
  # E[Y | X > x] for a standard bivariate normal, by numerical integration
  rho <- 0.6
  x0 <- 0.4
  num <- integrate(function(v) rho * v * dnorm(v), x0, 30, rel.tol = 1e-12)$value
  expect_equal(MillsConditionalMean(x0, rho), num / pnorm(-x0), tolerance = 1e-9)
  expect_error(MillsConditionalMean(0, 1.5), "rho must lie in")
})

test_that("TelegraphProcess has correlation exp(-|t|(a+b)) and a Cauchy spectrum", {
  r <- TelegraphProcess(0.5, 1.2, 0.3, 0.7)
  s <- 1
  expect_equal(r$rho, exp(-0.5 * s), tolerance = 1e-12)
  expect_equal(r$f, s / (pi * (s^2 + 1.2^2)), tolerance = 1e-12)
  expect_equal(TelegraphProcess(0, 0, 0.3, 0.7)$rho, 1)
  # the spectral density integrates to one over the whole line
  expect_equal(integrate(function(v) vapply(v, function(l) TelegraphProcess(0, l, 0.3, 0.7)$f, 0),
                         -Inf, Inf, rel.tol = 1e-10)$value, 1, tolerance = 1e-8)
  # symmetric in t
  expect_equal(TelegraphProcess(-2, 1, 0.5, 0.5)$rho, TelegraphProcess(2, 1, 0.5, 0.5)$rho)
  expect_error(TelegraphProcess(1, 1, 0, 1), "rates must be positive")
  expect_error(TelegraphProcess(1, 1, 1, -1), "rates must be positive")
})
