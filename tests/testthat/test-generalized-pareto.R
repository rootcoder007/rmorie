test_that("GpdDistribution closed forms", {
  r <- GpdDistribution(2, 0.25, x = c(1, 3), p = c(0.5, 0.9))
  z <- 1 + 0.25 * c(1, 3) / 2
  expect_equal(r$cdf, 1 - z^(-4))
  expect_equal(1 - (1 + 0.25 * r$quantile / 2)^(-4), c(0.5, 0.9), tolerance = 1e-14)
  expect_equal(GpdDistribution(1, -0.5)$upper_endpoint, 2)
})

test_that("Gpfit maximises the GPD likelihood", {
  x <- c(0.2, 1.4, 0.7, 2.9, 0.3, 5.1, 1.1, 0.9, 3.8, 0.5, 2.2, 7.4, 1.8, 0.6, 4.3, 0.4, 2.6, 9.9, 1.3, 0.8)
  r <- Gpfit(x, 0.5)
  y <- x[x > 0.5] - 0.5
  ll <- function(s, k) -length(y) * log(s) - (1 + 1 / k) * sum(log(1 + k * y / s))
  expect_equal((ll(r$scale + 1e-4, r$shape) - ll(r$scale - 1e-4, r$shape)) / 2e-4, 0, tolerance = 1e-6)
  expect_equal((ll(r$scale, r$shape + 1e-4) - ll(r$scale, r$shape - 1e-4)) / 2e-4, 0, tolerance = 1e-6)
  expect_equal(r$loglik, ll(r$scale, r$shape), tolerance = 1e-12)
})
