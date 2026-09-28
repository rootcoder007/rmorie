test_that("EmpiricalVariogramBins and FitVariogramWls equal gstat", {
  skip_if_not_installed("gstat")
  u <- .morie_random_uniform(400, seed = 17, stream = 0)
  n <- 120
  P <- cbind(10 * u[1:n], 10 * u[(n + 1):(2 * n)])
  C <- KrigingCovariance(as.matrix(dist(P)), list(model = "Exp", psill = 1, range = 2, nugget = 0.2))
  z <- as.vector(t(chol(C)) %*% .morie_random_normal(n, seed = 17, stream = 1))
  d <- data.frame(x = P[, 1], y = P[, 2], z = z)
  v <- gstat::variogram(z ~ 1, ~x + y, d)
  ours <- EmpiricalVariogramBins(z, P)
  expect_equal(ours$np, v$np)
  expect_equal(ours$dist, v$dist, tolerance = 1e-12)
  expect_equal(ours$gamma, v$gamma, tolerance = 1e-12)
  for (mod in c("Exp", "Sph")) {
    f <- suppressWarnings(gstat::fit.variogram(v, gstat::vgm(1, mod, 2, 0.2), fit.method = 7))
    o <- FitVariogramWls(ours, mod)
    expect_equal(c(o$nugget, o$psill, o$range), c(f$psill[1], f$psill[2], f$range[2]), tolerance = 2e-3)
    # a flat optimum: gstat's Levenberg-Marquardt stops early, ours is at least as good
    expect_lte(o$sse, .ks_sse(ours, f$psill[1], f$psill[2], f$range[2], mod) * (1 + 1e-9))
  }
})
