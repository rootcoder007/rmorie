test_that("LP regression quantiles are optimal", {
  z <- .morie_random_normal(90, seed = 7)
  X <- cbind(1, z[1:30])
  y <- 0.5 + 2 * z[1:30] + z[31:60]
  obj <- function(b, tau) {
    e <- y - X %*% b
    sum(ifelse(e >= 0, tau * e, (tau - 1) * e))
  }
  r <- QuantileRegressionLp(y, X, 0.3)
  expect_equal(r$objective, obj(r$coefficients, 0.3), tolerance = 1e-12)
  expect_gte(obj(r$coefficients + c(0.01, 0), 0.3), r$objective - 1e-12)
  expect_equal(QuantileRegressionLp(c(3, 1, 7, 5, 4), matrix(1, 5, 1))$coefficients, 4)
})
