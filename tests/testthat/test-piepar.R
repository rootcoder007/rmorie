test_that("Piepar is b1 (xbar* - xbar) with its OLS standard error", {
  y <- c(2.1, 3.4, 1.9, 5.6, 2.8, 3.1, 6.2, 2.5, 3.3, 4.7, 5.1, 2.2)
  X <- cbind(c(1, 2, 0.5, 3.5, 1.5, 2.2, 4.1, 1.2, 2, 3, 3.8, 0.9),
             c(0.3, 1.1, 0.2, 0.9, 1.4, 0.1, 0.6, 0.8, 0.5, 1.2, 0.4, 1))
  xs <- c(1, 1.5, 2)
  fit <- stats::lm(y ~ X)
  dx <- mean(xs) - mean(X[, 1])
  r <- Piepar(y, X, xs)
  expect_equal(r$estimate, unname(stats::coef(fit)[2]) * dx, tolerance = 1e-10)
  expect_equal(r$se, abs(dx) * unname(sqrt(diag(stats::vcov(fit)))[2]), tolerance = 1e-10)
  expect_equal(Piepar(y, X, mean(X[, 1]))$se, 0)
})
