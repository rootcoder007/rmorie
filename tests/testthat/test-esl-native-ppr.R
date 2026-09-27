test_that("projection pursuit recovers a single index and matches the Python arm", {
  X <- cbind((0:20) / 5 - 2, ((7 * (0:20)) %% 11) / 5 - 1)
  w <- morie_esl_projection_pursuit(X, rowSums(X)^2, M = 1, penalty = 0.01)$omega[[1]]
  expect_lt(abs(abs(w[1]) - 1 / sqrt(2)), 5e-3)
  expect_lt(abs(w[1] - w[2]), 5e-3)
  i <- 0:59
  X <- cbind(sin(1.3 * i), cos(0.7 * i + 1), sin(0.37 * i * i))
  y <- X[, 1] * X[, 2]
  r <- morie_esl_projection_pursuit(X, y, M = 2, penalty = 0.01, newdata = X)
  expect_equal(r$predicted, r$fitted, tolerance = 1e-10)
  expect_equal(r$rss_path, c(2.14943580526022, 0.36087256473054985), tolerance = 1e-9)
  expect_lt(r$rss, sum((y - mean(y))^2) / 20)
})
