X <- matrix(c(0, 0.4, 1.1, 1.7, 2.5))
Y <- c(0.2, 0.7, 0.9, 0.3, -0.4)

test_that("GP regression, LOO, fit", {
  expect_equal(GpCovariance(matrix(0), matrix(c(0, 1)), "matern32")[1, 2], (1 + sqrt(3)) * exp(-sqrt(3)))
  r <- GpPredict(matrix(c(0, 1)), c(0, 1), matrix(0.5), noise = 0.01)
  expect_equal(round(c(r$mean, r$variance), 6), c(0.54592, 0.036454))
  loo <- GpLoo(X, Y, noise = 0.05, lengthscale = 0.7)
  p <- GpPredict(X[-2, , drop = FALSE], Y[-2], X[2, , drop = FALSE], noise = 0.05, lengthscale = 0.7)
  expect_equal(loo$mean[2], p$mean, tolerance = 1e-10)
  f <- GpFit(X, Y, kernels = c("se", "matern52"))
  expect_true(f$best %in% c("se", "matern52"))
  expect_error(GpCovariance(matrix(0), matrix(1), "bogus"), "kernel must be")
})

test_that("classification, sparse, warping", {
  c <- GpClassify(matrix(c(-2, -1, -0.3, 0.4, 1.2, 2.1)), c(-1, -1, -1, 1, 1, 1), matrix(c(-1.5, 1.5)), variance = 4)
  expect_true(c$probability[1] < 0.5 && c$probability[2] > 0.5)
  full <- GpPredict(X, Y, matrix(0.8), noise = 0.1)
  sp <- GpSparse(X, Y, X, matrix(0.8), method = "dtc", noise = 0.1)
  expect_equal(sp$mean, full$mean, tolerance = 1e-7)
  expect_equal(KumaraswamyWarp(c(0, 0.5, 1), 2, 1), c(0, 0.25, 1))
  expect_length(DeepGpSample(matrix(c(0, 1)), list(list("se", list()), list("se", list(lengthscale = 0.5))))$layers, 2)
})
