test_that("space-time cokriging honours data and the diffusion covariance its formula", {
  u <- .morie_random_uniform(120, seed = 9)
  P <- cbind(10 * u[1:20], 10 * u[21:40])
  tt <- floor(3 * u[41:60])
  v <- (0:19) %% 2
  z <- cos(P[, 1] / 3) + 0.1 * tt + u[61:80]
  r <- StCokriging(P, tt, v, z, P[1, , drop = FALSE], tt[1], rbind(c(1, 0.5), c(0.5, 0.9)), 3, 2)
  expect_equal(r$estimate, z[1], tolerance = 1e-9)
  expect_equal(r$variance, 0, tolerance = 1e-9)
  s <- 1.1^2 + 4 * 0.4 * 0.8
  expect_equal(DiffusionStCovariance(1.3, 0.8, 1, 1.1, 0.4), 1.1^2 / s * exp(-1.69 / s), tolerance = 1e-14)
})
