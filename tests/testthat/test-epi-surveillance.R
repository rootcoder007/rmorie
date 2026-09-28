test_that("surveillance and area models follow their definitions", {
  u <- .morie_random_uniform(100, seed = 31)
  obs <- as.integer(5 + 5 * u[1:30])
  r <- BayesOutbreak(obs, w = 5)
  for (q in seq_along(r$time_points)) {
    t <- r$time_points[q]
    expect_equal(r$upperbound[q], stats::qnbinom(0.95, sum(obs[(t - 5):(t - 1)]) + 0.5, 5 / 6))
  }
  x <- as.integer(3 + 4 * u[31:70])
  cs <- CusumSurveillance(x, start = 20, k = 0.5, h = 3)
  m <- mean(x[1:19])
  expect_equal(cs$cusum[1], max(0, (x[20] - m) / sqrt(m) - 0.5), tolerance = 1e-13)
  A <- rbind(c(0, 1, 1, 0), c(1, 0, 1, 0), c(1, 1, 0, 1), c(0, 0, 1, 0))
  expect_equal(rowSums(LerouxPrecision(A, 1)), rep(0, 4), tolerance = 1e-15)
  b <- Bym2Structure(A, 2, 0.4)
  Q <- diag(rowSums(A)) - A
  expect_equal(Q %*% b$generalized_inverse %*% Q, Q, tolerance = 1e-12)
  expect_equal(exp(mean(log(diag(b$generalized_inverse) / b$scaling_factor))), 1, tolerance = 1e-12)
  k <- KernelExposure(rbind(c(0.2, 0.3)), rbind(c(0.25, 0.35), c(0.6, 0.8)), 0.5, kernel = "quartic")
  d <- sqrt(c(0.05^2 + 0.05^2, 0.4^2 + 0.5^2)) / 0.5
  expect_equal(k, sum(ifelse(d < 1, 3 / pi * (1 - d^2)^2, 0)) / 0.25, tolerance = 1e-13)
})
