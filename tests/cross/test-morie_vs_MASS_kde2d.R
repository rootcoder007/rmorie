test_that("SpatialKde matches MASS::kde2d", {
  skip_if_not_installed("MASS")
  u <- .morie_random_uniform(80, seed = 7)
  x <- qnorm(u[1:40]) * 1.3 + 2
  y <- u[41:80]^2 * 5
  k <- MASS::kde2d(x, y, n = 30)
  r <- SpatialKde(cbind(x, y), n = 30)
  expect_equal(r$bandwidth * 4, c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y)), tolerance = 1e-13)
  expect_equal(r$x, k$x, tolerance = 1e-14)
  expect_equal(r$z, k$z, tolerance = 1e-12)
  k2 <- MASS::kde2d(x, y, h = c(1, 2), n = c(12, 9), lims = c(-1, 5, 0, 6))
  r2 <- SpatialKde(cbind(x, y), h = c(0.25, 0.5), n = c(12, 9), lims = c(-1, 5, 0, 6))
  expect_equal(r2$z, k2$z, tolerance = 1e-12)
})
