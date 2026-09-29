# Cross test: the Porcu quasi-arithmetic space-time covariance against GeoModels::GeoCorrFct.
test_that("StCovarianceFamily porcu matches GeoModels", {
  skip_if_not_installed("GeoModels")
  h <- c(0, 0.3, 0.8, 1.1, 1.7, 2.2, 2.9, 3.4, 4.0, 5.5, 0.05, 1.3, 2.6, 3.8, 6.1, 0.9, 1.9, 2.4, 4.7, 7.3)
  u <- c(0, 2.5, 0.4, 1.0, 3.3, 0.1, 5.0, 2.2, 0.7, 1.9, 4.4, 0, 3.1, 0.6, 2.8, 1.4, 6.2, 0.2, 3.9, 1.1)
  pars <- list(c(1.5, 0.8, 2, 3, 0.4), c(1, 1, 1, 1, 1), c(2, 0.5, 0.7, 4, 0.9), c(0.6, 1.7, 3, 0.5, 0.05),
               c(1.2, 1.9, 1.5, 2, 0))
  for (p in pars) {
    ref <- vapply(seq_along(h), function(i) {
      GeoModels::GeoCorrFct(x = h[i], t = u[i], corrmodel = "porcu", covariance = TRUE,
                            param = list(power_s = p[1], power_t = p[2], scale_s = p[3], scale_t = p[4],
                                         sep = p[5], sill = 2.5, nugget = 0, mean = 0))$corr
    }, numeric(1))
    got <- StCovarianceFamily(h, u, "porcu", sigma2 = 2.5, power_s = p[1], power_t = p[2], scale_s = p[3],
                              scale_t = p[4], sep = p[5], method = "GeoModels")
    expect_equal(got, ref, tolerance = 1e-12)
  }
})
