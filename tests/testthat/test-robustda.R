X <- rbind(c(0, 0), c(1, 0), c(0, 1), c(1, 1), c(0.5, 0.4), c(9, 0), c(0.2, 0.8),
           c(5, 5), c(6, 5), c(5, 6), c(6, 6), c(5.5, 5.4), c(5.2, 5.8), c(-9, 9))
y <- rep(0:1, each = 7)

test_that("RobustLda pooled scatter recomputes from Fastm", {
  r <- RobustLda(X, y)
  f0 <- Fastm(X[1:7, ])
  f1 <- Fastm(X[8:14, ])
  c0 <- matrix(unlist(f0$cov), 2, 2, byrow = is.list(f0$cov))
  c1 <- matrix(unlist(f1$cov), 2, 2, byrow = is.list(f1$cov))
  expect_equal(r$pooled_cov, (f0$h * c0 + f1$h * c1) / (f0$h + f1$h - 2), tolerance = 1e-12)
})

test_that("RobustLda scores follow the linear rule", {
  r <- RobustLda(X, y, newdata = rbind(c(2, 3)))
  Wi <- solve(r$pooled_cov)
  for (k in 1:2) {
    m <- r$centers[[k]]
    expect_equal(r$scores[1, k], sum((Wi %*% m) * c(2, 3)) - 0.5 * sum((Wi %*% m) * m) + log(0.5), tolerance = 1e-9)
  }
  expect_error(RobustLda(X, rep(0, 14)))
})
