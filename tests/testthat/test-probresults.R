test_that("probability results recompute", {
  lam <- 1.7
  tt <- 0.9
  expect_equal(ErlangRenewal(tt, lam), lam * tt / 2 - (1 - exp(-2 * lam * tt)) / 4, tolerance = 1e-13)
  expect_equal(ErlangRenewal(80, 1, 4), 80 / 4 - 3 / 8, tolerance = 1e-12)
  g <- 0.13
  a <- 0.21
  cc <- 0.47
  p <- a + cc - a * cc
  px <- a + g * cc - a * g * cc
  expect_equal(BerksonCorrelation(g, a, cc), g * p * (1 - px) / sqrt(px * (1 - px) * g * p * (1 - g * p)), tolerance = 1e-12)
  tab <- matrix(c(0.3, 0.1, 0.2, 0.4), 2)
  expect_equal(MaximalCorrelation(tab)$m, abs(0.3 * 0.4 - 0.2 * 0.1) / sqrt(0.5 * 0.5 * 0.4 * 0.6), tolerance = 1e-12)
  expect_equal(sum(vapply(0:9, function(k) BetaBinomialPmf(k, 9, 1.7, 2.6), 0)), 1, tolerance = 1e-12)
  th <- c(0.5, -0.4, 0.2)
  expect_equal(MaAutocorrelation(th, 5), unname(stats::ARMAacf(ma = th, lag.max = 5)), tolerance = 1e-12)
  al <- 0.6
  expect_equal(ArSpectralDensity(1.3, al), (1 - al^2) / (2 * pi * (1 - 2 * al * cos(1.3) + al^2)), tolerance = 1e-12)
  expect_equal(StirlingGammaRatio(7), gamma(7) / (7^6.5 * exp(-7) * sqrt(2 * pi)), tolerance = 1e-12)
  expect_equal(SimpleEpidemicDuration(3, 1)$mean, 1 / 3 + 1 / 4 + 1 / 3, tolerance = 1e-14)
  expect_equal(WienerConditionalCorrelation(0, 1, 2, 4), sqrt(2 * 1 / (3 * 2)), tolerance = 1e-14)
})
