fy <- c(10.2, 12.8, 11.3, 15.0, 13.2, 9.7, 16.5, 8.9, 14.1, 12.0)
fx <- c(3.1, 2.2, 4.0, 2.9, 4.4, 1.9, 3.6, 2.7, 4.8, 3.3)
fd <- c(1.2, 0.8, 2.0, 1.5, 0.9, 1.1, 1.7, 0.6, 1.3, 1.0)

test_that("Fay-Herriot estimating equations, EBLUP and closed forms", {
  X <- cbind(1, fx)
  for (m in c("REML", "ML", "FH")) {
    r <- FayHerriot(fy, X, fd, method = m, tol = 1e-13, maxiter = 1000L)
    w <- 1 / (r$A + fd)
    b <- as.vector(solve(crossprod(X, w * X), crossprod(X, w * fy)))
    expect_equal(r$beta, b, tolerance = 1e-10)
    res <- fy - as.vector(X %*% b)
    expect_equal(r$eblup, as.vector(X %*% b) + r$A * w * res, tolerance = 1e-10)
    if (m == "FH") expect_equal(sum(res^2 * w), 8, tolerance = 1e-9)
  }
  expect_equal(FayHerriot(c(1, 2, 3, 5), matrix(1, 4, 1), rep(1, 4))$A, 8.75 / 3 - 1, tolerance = 1e-12)
  expect_equal(FayHerriot(c(1, 2, 3, 5), matrix(1, 4, 1), rep(1, 4), method = "ML")$A, 8.75 / 4 - 1,
               tolerance = 1e-12)
})

test_that("BHF balanced ANOVA and EBLUP formula", {
  r <- BhfEblup(c(1, 2, 2, 3, 5, 6), matrix(1, 6, 1), c(0, 0, 1, 1, 2, 2), matrix(1, 3, 1))
  expect_equal(r$sigma2_e, 0.5, tolerance = 1e-12)
  expect_equal(r$sigma2_u, (var(c(1.5, 2.5, 5.5)) * 2 - 0.5) / 2, tolerance = 1e-10)
  expect_equal(r$eblup, c(1.5, 2.5, 5.5) + (1 - r$sigma2_u / (r$sigma2_u + 0.25)) * (19 / 6 - c(1.5, 2.5, 5.5)),
               tolerance = 1e-10)
})

test_that("empirical-Bayes rates and Potthoff-Whittinghill", {
  expect_equal(MarshallEb(c(2, 8), c(100, 100))$estimate, c(0.11, 0.19) / 3, tolerance = 1e-14)
  Y <- c(25, 2, 40, 8, 1, 30, 4, 60, 3, 18)
  E <- c(10.2, 6.1, 14.5, 9.0, 4.8, 11.9, 10.5, 18.2, 7.7, 9.4)
  cv <- c(0.3, -0.2, 0.8, 0.1, -0.5, 0.4, 0.0, 1.1, -0.3, 0.2)
  r <- PoissonGammaEb(Y, E, cv)
  mu <- E * r$mu
  expect_lt(max(abs(crossprod(cbind(1, cv), (Y - mu) / (1 + mu / r$alpha)))), 1e-8)
  w <- mu / (r$alpha + mu)
  expect_equal(r$RR, w * Y / E + (1 - w) * r$mu, tolerance = 1e-14)
  p <- PotthoffWhittinghill(c(3, 3, 3, 3), rep(3, 4))
  expect_equal(c(p$T, p$mean, p$variance), c(96, 132, 792))
})
