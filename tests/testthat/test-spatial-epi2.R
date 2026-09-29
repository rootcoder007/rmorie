# Tests for SpatialEpi2: spatial epidemiology.

test_that("gamma sampler moments and ZIP posterior", {
  g <- vapply(0:1999, function(k) .se_rgamma(2.5, 11, k), 0)
  expect_lt(abs(mean(g) - 2.5), 4 * sqrt(2.5 / 2000))
  y <- c(rep(0, 12), 3, 4, 5, 2, 6, 3, 4, 5)
  r <- ZipGibbs(y, rep(1, 20), 600, burn = 100, seed = 5)
  expect_true(all(abs(r$theta[13:20] - (1 + y[13:20]) / 2) < 0.5))
})

test_that("Oden, scan, ecological regression and wombling", {
  W <- rbind(c(0, 1, 0, 0), c(1, 0, 1, 0), c(0, 1, 0, 1), c(0, 0, 1, 0))
  expect_equal(OdenIpop(c(10, 20, 30, 40), c(100, 200, 300, 400), W)$statistic, 0, tolerance = 1e-12)
  s <- ProspectiveScan(rbind(1, 1, c(1, 1, 1, 9)), matrix(1, 3, 4), cbind(c(0, 1, 2, 9), 0), max_window = 2, nsim = 19, seed = 1)
  expect_equal(s$cluster, 3)
  expect_equal(PoissonEcological(c(2, 4, 8), c(1, 1, 1), matrix(0:2))$beta[2], log(2), tolerance = 1e-12)
  expect_equal(ArealWombling(c(0, 0.1, 5, 5.2), W, quantile = 0.9)$barriers, matrix(c(1, 2), 1))
  expect_equal(NeedBasedAllocation(100, c(1, 1), c(1, 3), floor_share = 0.5), c(37.5, 62.5))
})
