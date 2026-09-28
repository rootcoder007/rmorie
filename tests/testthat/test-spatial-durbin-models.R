n <- 16
ii <- 0:(n - 1)
B <- outer(ii, ii, function(a, b) as.numeric(abs(a %/% 4 - b %/% 4) + abs(a %% 4 - b %% 4) == 1))
W <- B / rowSums(B)
X <- cbind(1, (ii * 7 %% 11) / 5, ((ii * 5) %% 7) / 3 - 1)
y <- as.vector(1 + 2 * X[, 2] - X[, 3] + 0.3 * W %*% X[, 2] + ((ii * 3) %% 5 - 2) / 4)

test_that("SdemML and GnsML are the error and SAC models on the Durbin design", {
  s <- SdemML(y, X, W)
  e <- SpatialRegressionML(y, cbind(X, W %*% X[, 2:3]), W, "error")
  expect_equal(s$loglik, e$loglik, tolerance = 1e-12)
  expect_equal(s$impacts$total, s$coefficients[2:3] + s$coefficients[4:5])
  g <- GnsML(y, X, W)
  expect_equal(g$impacts$direct, SpatialImpacts(g$rho, g$coefficients[2:3], W, g$coefficients[4:5])$direct)
})

test_that("CarML, tests, ResidualMoran and LogJacobian", {
  cc <- CarML(y, X, B)
  f0 <- lm.fit(X, y)
  ll0 <- -n / 2 * (log(2 * pi * sum(f0$residuals^2) / n) + 1)
  expect_equal(cc$lr_test$statistic, 2 * (cc$loglik - ll0), tolerance = 1e-9)
  expect_equal(SpatialLrTest(-10, -12.5, 1)$statistic, 5)
  expect_equal(SpatialWaldTest(c(0.3, 0.2), diag(0.01, 2))$statistic, 13)
  expect_equal(LogJacobian(matrix(c(0, 1, 1, 0), 2), 0.5, 0.5), 2 * log(0.75))
  M <- diag(n) - X %*% solve(crossprod(X), t(X))
  expect_equal(ResidualMoran(y, X, W)$expected, sum(diag(M %*% W)) / (n - 3), tolerance = 1e-14)
})
