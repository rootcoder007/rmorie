test_that("WeightInit, Xavir and Xvrig are scaled Philox draws", {
  u <- .morie_random_uniform(12, seed = 9)
  a <- 2 * sqrt(6 / 4)
  expect_equal(WeightInit(4, 3, "he_uniform", gain = 2, seed = 9)$value, matrix(-a + 2 * a * u, 4, 3, byrow = TRUE))
  z <- .morie_random_normal(6, seed = 4)
  expect_equal(Xavir(2, 3, seed = 4, uniform = FALSE)$weights, matrix(sqrt(2 / 5) * z, 2, 3, byrow = TRUE))
  expect_equal(dim(Xvrig(3, 2, seed = 5)$weights), c(2, 3))
  Q <- WeightInit(6, 3, "orthogonal", gain = 1.5, seed = 3)$value
  expect_equal(crossprod(Q), diag(2.25, 3), tolerance = 1e-12)
})

test_that("Expkern and Rbfkern are the exponential and Gaussian kernels", {
  X <- rbind(c(0.1, 1.2, -0.3), c(0.8, 0.4, 0.5), c(-1, 0, 2))
  D <- as.matrix(dist(X))
  expect_equal(Expkern(X)$K, exp(-D / 3), ignore_attr = TRUE, tolerance = 1e-15)
  expect_equal(Rbfkern(X, gamma = 0.25)$K, exp(-0.25 * D^2), ignore_attr = TRUE, tolerance = 1e-15)
})
