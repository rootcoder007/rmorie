X <- rbind(c(0.1, 0.2), c(0.9, 0.1), c(0.5, 0.6), c(0.2, 0.9), c(0.8, 0.8), c(0.4, 0.3), c(0.6, 0.35))
Y <- sin(3 * X[, 1]) + X[, 2]^2

test_that("RBF interpolation", {
  expect_equal(RadialBasis(c(0, 0.5, 1), "wendland"), c(1, 0.1875, 0))
  expect_equal(RbfInterpolate(matrix(0:2), c(0, 1, 0), matrix(0.5), kernel = "cubic")$prediction, 0.6875)
  for (k in c("thin_plate", "cubic")) expect_equal(RbfInterpolate(X, Y, X, kernel = k)$prediction, Y, tolerance = 1e-8)
  lin <- 2 + 3 * X[, 1] - X[, 2]
  expect_equal(RbfInterpolate(X, lin, rbind(c(0.3, 0.7)), kernel = "thin_plate")$prediction, 2.2, tolerance = 1e-9)
  expect_equal(RbfMultiscale(X, Y, X, c(2, 1, 0.5))$prediction, Y, tolerance = 1e-8)
  expect_true(RbfLoocv(X, Y, c(1, 3))$best %in% c(1, 3))
  expect_error(RadialBasis(1, "bogus"), "unknown")
})
