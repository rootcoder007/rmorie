test_that("swiglu recomputes", {
  x <- rbind(c(0.5, -1, 2))
  W1 <- rbind(c(0.1, 0.2), c(0.3, -0.4), c(0.5, 0.6))
  W3 <- rbind(c(-0.2, 0.7), c(0.1, 0.1), c(0.4, -0.3))
  W2 <- rbind(c(1, -1, 0.5), c(0.25, 0.5, 2))
  g <- x %*% W3
  expect_equal(swiglu(x, W1, W2, W3)$value, ((x %*% W1) * g / (1 + exp(-g))) %*% W2, tolerance = 1e-14)
  expect_equal(swiglu(rbind(c(1, -2)))$value[1, 1], 1 / (1 + exp(-1)), tolerance = 1e-15)
})
