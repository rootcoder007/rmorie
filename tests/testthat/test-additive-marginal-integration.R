test_that("admod averages the full Nadaraya-Watson pilot over the other covariate", {
  X <- cbind(c(0, 1, 2, 3, 4, 5, 1.5), c(1, 0, 2, 1, 3, 2, 2.5))
  y <- c(1, 1.5, 3.2, 3.9, 6.1, 6, 2.2)
  h <- 1.3
  g <- function(t1, t2) {
    w <- exp(-0.5 * ((t1 - X[, 1]) / h)^2 - 0.5 * ((t2 - X[, 2]) / h)^2)
    sum(w * y) / sum(w)
  }
  r <- admod(y, X, bandwidth = h, grid_size = 4)
  grid <- r$components[[1]]$x_grid
  expect_equal(grid, c(0, 5 / 3, 10 / 3, 5))
  want <- vapply(grid, function(t) mean(vapply(X[, 2], function(v) g(t, v), 0)) - mean(y), 0)
  expect_equal(r$components[[1]]$m_hat, want, tolerance = 1e-12)
  expect_equal(r$intercept, mean(y))
  fit <- mean(y) + approx(grid, want, X[, 1], rule = 2)$y +
    approx(r$components[[2]]$x_grid, r$components[[2]]$m_hat, X[, 2], rule = 2)$y
  expect_equal(r$residuals, y - fit, tolerance = 1e-12)
})
