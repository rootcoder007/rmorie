skip_if_not_installed("fields")

test_that("thin-plate RBF interpolation equals unscaled fields::Tps with lambda = 0", {
  set.seed(5)
  X <- cbind(stats::runif(15), stats::runif(15))
  y <- sin(4 * X[, 1]) + cos(3 * X[, 2])
  new <- cbind(c(0.2, 0.55, 0.9), c(0.3, 0.5, 0.75))
  fit <- fields::Tps(X, y, lambda = 0, scale.type = "unscaled")
  ref <- as.vector(stats::predict(fit, new))
  expect_equal(RbfInterpolate(X, y, new, kernel = "thin_plate", degree = 1)$prediction, ref, tolerance = 1e-8)
})
