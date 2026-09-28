skip_if_not_installed("gstat")

test_that("3-D classical sample variogram equals gstat::variogram", {
  set.seed(9)
  d <- data.frame(x = stats::runif(40), y = stats::runif(40), z = stats::runif(40), v = stats::rnorm(40))
  B <- c(0, 0.2, 0.4, 0.6, 0.8)
  ref <- gstat::variogram(v ~ 1, ~ x + y + z, data = d, boundaries = B)
  mine <- SampleVariogramNd(d$v, as.matrix(d[, 1:3]), B)
  expect_equal(mine$np, ref$np)
  expect_equal(mine$gamma, ref$gamma, tolerance = 1e-12)
  expect_equal(mine$dist, ref$dist, tolerance = 1e-12)
})
