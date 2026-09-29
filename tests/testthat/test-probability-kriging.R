test_that("probability kriging satisfies its constraints", {
  u <- .morie_random_uniform(100, seed = 5)
  P <- cbind(10 * u[1:20], 10 * u[21:40])
  z <- cos(P[, 1] / 2) + P[, 2] / 4 + u[41:60]
  r <- ProbabilityKriging(P, z, rbind(c(3, 4), P[8, ]), sort(z)[10], c(0, 0.2, 2.5), c(0, 0.08, 2.5), c(0, 0.1, 2.5))
  expect_equal(sum(r$weights[[1]][1:20]), 1, tolerance = 1e-10)
  expect_equal(sum(r$weights[[1]][21:40]), 0, tolerance = 1e-10)
  expect_equal(r$estimate[2], r$indicator[8], tolerance = 1e-9)
})
