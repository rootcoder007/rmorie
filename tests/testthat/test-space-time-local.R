test_that("space-time local statistics recompute", {
  W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
  expect_equal(BivariateMoran(1:4, 1:4, W, nsim = 9)$statistic, 0.4, tolerance = 1e-12)
  u <- .morie_random_uniform(200, seed = 21, stream = 0)
  P <- cbind(5 * u[1:20], 5 * u[21:40])
  tt <- (0:19) %% 3
  z <- 2 + 0.5 * (P[, 1] - 2.5) - 0.3 * (P[, 2] - 2.5)^2 + 0.7 * tt
  expect_gt(StTrendSurface(z, P, tt)$r2, 1 - 1e-12)
  Z <- matrix(u[41:120], 4, byrow = TRUE)
  g <- StGetisOrd(Z, P, 1.5, 1)
  expect_equal(dim(g$z), c(4L, 20L))
})
