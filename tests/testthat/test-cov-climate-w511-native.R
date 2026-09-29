# Coverage tests for R/climate_w511_native.R: FAO-56 Penman-Monteith
# reference evapotranspiration, empirical quantile mapping and the Planck
# spectral radiance with the Wien and Stefan-Boltzmann constants.

test_that("FAO-56 Penman-Monteith ET0", {
  r <- Basevap(T = 16.9, R_n = 13.28, u2 = 2.078, VPD = 0.589, G = 0.14, P = 100.1)
  es <- 0.6108 * exp(17.27 * 16.9 / (16.9 + 237.3))
  delta <- 4098 * es / (16.9 + 237.3)^2
  gam <- 0.665e-3 * 100.1
  den <- delta + gam * (1 + 0.34 * 2.078)
  expect_equal(r$estimate, (0.408 * delta * (13.28 - 0.14) + gam * 900 / (16.9 + 273) * 2.078 * 0.589) / den, tolerance = 1e-12)
  expect_equal(r$radiative_term + r$aerodynamic_term, r$estimate)
  expect_equal(Basevap(20, 10, 2, 0)$aerodynamic_term, 0)
  expect_error(Basevap(20, 10, 2, -1), "VPD must be non-negative")
  expect_error(Basevap(20, 10, -2, 1), "wind speed")
  expect_error(Basevap(20, 10, 2, 1, P = 0), "pressure must be positive")
})

test_that("empirical quantile mapping", {
  obs <- c(3.1, 0.5, 2.2, 5.0, 1.7, 4.4)
  mod <- c(1, 2.5, 0.2, 3.3, 1.8)
  x <- c(0.1, 1.4, 2.5, 3.0, 9)
  r <- Qmds(x, obs, mod)
  p <- approx(sort(mod), (0:4) / 4, x, rule = 2)$y
  expect_equal(r$probs, p, tolerance = 1e-15)
  expect_equal(r$estimate, unname(quantile(obs, p, type = 7)), tolerance = 1e-12)
  expect_equal(Qmds(2, obs, 7)$probs, 0.5)
  expect_equal(Qmds(2, 4, mod)$estimate, 4)
  expect_error(Qmds(1, numeric(0), mod), "must be non-empty")
})

test_that("Planck radiance, Wien peak and total power", {
  lam <- c(500e-9, 1e-6, 10e-6)
  r <- Plncf(lam, 5778)
  h <- 6.62607015e-34
  c0 <- 299792458
  k <- 1.380649e-23
  expect_equal(r$estimate, 2 * h * c0^2 / lam^5 / expm1(h * c0 / (lam * k * 5778)), tolerance = 1e-12)
  expect_equal(r$wien_constant, 2.897771955e-3, tolerance = 1e-9)
  expect_equal(r$peak_wavelength, 2.897771955e-3 / 5778, tolerance = 1e-9)
  expect_equal(r$total_power, 5.670374419e-8 * 5778^4, tolerance = 1e-9)
  pk <- optimize(function(l) Plncf(l, 5778)$estimate, c(2e-7, 2e-6), maximum = TRUE, tol = 1e-15)$maximum
  expect_equal(pk, r$peak_wavelength, tolerance = 1e-6)
  expect_error(Plncf(lam, 0), "temperature must be > 0")
  expect_error(Plncf(c(1e-6, 0), 300), "wavelengths must be > 0")
})
