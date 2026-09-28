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
