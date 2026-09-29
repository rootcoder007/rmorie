test_that("space-time variograms and covariances match their formulas", {
  s <- list(psill = 2, model = "Exp", range = 100)
  tm <- list(psill = 3, model = "Sph", range = 5)
  expect_equal(round(StModelVariogram(c(0, 50, 120), c(1, 3, 10), "productSum", space = s, time = tm, k = 0.1), 6),
               c(1.0656, 3.687244, 4.997612))
  j <- list(psill = 1.5, model = "Gau", range = 40)
  d <- sqrt(30^2 + 20^2)
  expect_equal(StModelVariogram(30, 2, "metric", joint = j, stani = 10), 1.5 * (1 - exp(-(d / 40)^2)))
  expect_equal(round(StCovarianceFamily(1, 2, "gneiting", a = 1, alpha = 0.5, beta = 1, gamma = 0.5, c = 1, tau = 1), 6),
               0.187128)
  expect_equal(StCovarianceFamily(c(1, 1), c(6, 12), "periodic", range = 2, period = 12), c(-exp(-0.5), exp(-0.5)))
  r <- StLinearCombination(1, 2, list(list("separable_exp", list(range_s = 2, range_t = 4)),
                                      list("cressie_huang", list(a = 1, b = 1))), c(0.25, 0.75))
  expect_equal(r$covariance, 0.25 * exp(-1) + 0.75 * exp(-1 / 5) / 5)
  expect_error(StModelVariogram(1, 1, "bogus"), "model must be")
  expect_error(StCovarianceFamily(1, 1, "bogus"), "unknown")
})

test_that("the Porcu quasi-arithmetic family matches its formula", {
  a1 <- 1 + (1.7 / 2)^1.5
  a2 <- 1 + (2.5 / 3)^0.8
  got <- StCovarianceFamily(1.7, -2.5, "porcu", sigma2 = 2, sep = 0.4, power_s = 1.5, power_t = 0.8, scale_s = 2,
                            scale_t = 3)
  expect_equal(got, 2 * (0.5 * a1^0.4 + 0.5 * a2^0.4)^(-1 / 0.4), tolerance = 1e-14)
  expect_equal(StCovarianceFamily(1.7, 2.5, "porcu", sep = 0, power_s = 1.5, power_t = 0.8, scale_s = 2, scale_t = 3),
               1 / sqrt(a1 * a2), tolerance = 1e-15)
  expect_equal(StCovarianceFamily(1.7, 2.5, "porcu", sep = 1e-7, power_s = 1.5, power_t = 0.8, scale_s = 2,
                                  scale_t = 3), 1 / sqrt(a1 * a2), tolerance = 1e-9)
  expect_equal(StCovarianceFamily(1.7, 2.5, "porcu", sep = 0, method = "GeoModels", power_s = 1.5, power_t = 0.8,
                                  scale_s = 2, scale_t = 3), 1 / (a1 * a2), tolerance = 1e-15)
  expect_error(StCovarianceFamily(1, 1, "porcu", power_s = 2.5, power_t = 1, scale_s = 1, scale_t = 1), "porcu needs")
})
