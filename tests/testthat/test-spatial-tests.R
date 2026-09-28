test_that("Breusch-Pagan formulas and SDM impacts", {
  e <- c(1, -1, 2, -2, 3, -3)
  Z <- cbind(1, c(0, 0, 1, 1, 0, 0), c(0, 0, 0, 0, 1, 1))
  s2 <- 28 / 6
  fit <- c(1, 1, 4, 4, 9, 9) - s2
  expect_equal(BreuschPagan(e, Z)$statistic, 6 * sum(fit^2) / sum((e^2 - s2)^2), tolerance = 1e-12)
  expect_equal(BreuschPagan(e, Z, FALSE)$statistic, 0.5 * sum((fit / s2)^2), tolerance = 1e-12)
  u <- .morie_random_uniform(160, seed = 61, stream = 0)
  z <- .morie_random_normal(80, seed = 61, stream = 1)
  P <- cbind(u[1:40], u[41:80])
  D <- as.matrix(dist(P))
  W <- t(apply(D, 1, function(d) {
    w <- numeric(40)
    w[order(d)[2:5]] <- 0.25
    w
  }))
  x <- z[1:40]
  y <- as.vector(solve(diag(40) - 0.4 * W, 1 + 2 * x - 0.5 * W %*% x + z[41:80] * (0.5 + u[81:120])))
  r <- SdmML(y, cbind(1, x), W)
  expect_equal(r$total, (r$beta[2] + r$theta) / (1 - r$rho), tolerance = 1e-10)
  expect_equal(r$spillover_index, r$indirect / r$total, tolerance = 1e-15)
})
