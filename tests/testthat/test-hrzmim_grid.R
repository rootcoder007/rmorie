test_that("Multindex smoother and grid recompute", {
  n <- 60
  i <- 0:(n - 1)
  X <- cbind(-2 + 4 * i / (n - 1), cos(0.6 * i))
  z <- X[, 1] + 0.7 * X[, 2]
  y <- z + 0.3 * z^2
  r <- Multindex(X, y, list(c(1, 2)), h = 0.6, hg = 0.3, ngrid = 5)
  b <- r$estimate[[1]]
  expect_equal(b[1], 1)
  idx <- as.vector(X %*% b)
  expect_equal(r$indices[, 1], idx, tolerance = 1e-12)
  w <- stats::dnorm((idx[4] - idx) / 0.3)
  expect_equal(r$ghat[4], sum(w * y) / sum(w), tolerance = 1e-12)
  w <- stats::dnorm((r$grid[3] - idx) / 0.3)
  expect_equal(r$ggrid[3], sum(w * y) / sum(w), tolerance = 1e-12)
  expect_error(Multindex(X, y, list(1, 2), ngrid = 5))
})
