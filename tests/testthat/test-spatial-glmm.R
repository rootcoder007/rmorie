test_that("latent fields, simulation and scoring", {
  Q <- CarPrecision(rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0)), 0.4, 2)
  expect_equal(Q[2, ], c(-0.8, 4, -0.8))
  r <- GmrfSimulate(Q, nsim = 2, seed = 3)
  expect_equal(as.vector(chol(Q) %*% r$samples[[2]]),
               .morie_random_normal(3, seed = 3, stream = 1), tolerance = 1e-12)
  s <- SpatialGlmmSimulate(matrix(1, 10, 1), 0.3, 0.1 * (0:9), family = "negbin", size = 2, seed = 4)
  expect_true(all(s$y == round(s$y) & s$y >= 0))
  expect_equal(CrpsGaussian(0, 0, 1), 2 * dnorm(0) - 1 / sqrt(pi), tolerance = 1e-15)
  expect_equal(CrpsSample(1, list(c(0, 2))), 0.5)
  expect_equal(GlmmResiduals(c(2, 0), c(2, 1))$pearson, c(0, -1))
})
