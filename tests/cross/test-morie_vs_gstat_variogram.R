test_that("variography equals gstat variogram/variogramLine and geoR loglik.GRF", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  skip_if_not_installed("geoR")
  u <- .morie_random_uniform(90, seed = 31, stream = 0)
  P <- cbind(8 * u[1:30], 8 * u[31:60])
  z <- CholeskySim(P, list(list(model = "Nug", psill = 0.2), list(model = "Exp", psill = 1, range = 2)), seed = 3)$simulations[1, ]
  d <- data.frame(x = P[, 1], y = P[, 2], z = z)
  sp::coordinates(d) <- ~ x + y
  for (cr in c(FALSE, TRUE)) {
    g <- gstat::variogram(z ~ 1, d, cressie = cr)
    s <- SampleVariogram(z, P, estimator = if (cr) "cressie" else "classical")
    expect_equal(s$np, g$np)
    expect_equal(s$gamma, g$gamma, tolerance = 1e-12)
  }
  h <- c(0.2, 1.1, 2.9)
  for (m in c("Sph", "Pen", "Cir", "Wav", "Hol", "Bes")) {
    expect_equal(VgmSemivariance(h, list(model = m, psill = 0.7, range = 1.8)),
                 gstat::variogramLine(gstat::vgm(0.7, m, 1.8), dist_vector = h)$gamma, tolerance = 1e-13)
  }
  geo <- geoR::as.geodata(cbind(P, z))
  for (meth in c("ML", "REML")) {
    expect_equal(VariogramLoglik(z, P, list(model = "Exp", psill = 0.8, range = 1.7), nugget = 0.25, method = meth),
                 geoR::loglik.GRF(geo, cov.model = "exponential", cov.pars = c(0.8, 1.7), nugget = 0.25,
                                  method.lik = meth), tolerance = 1e-11)
  }
})
