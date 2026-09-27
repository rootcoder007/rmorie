test_that("SpatialRegressionML matches spatialreg values", {
  U <- .morie_random_uniform(200, seed = 51, stream = 0)
  Z <- .morie_random_normal(200, seed = 51, stream = 1)
  P <- cbind(U[seq(1, 79, 2)], U[seq(2, 80, 2)])
  D <- as.matrix(stats::dist(P))
  W <- t(vapply(1:40, function(i) { w <- numeric(40); w[order(D[i, ], seq_len(40))[2:5]] <- 0.25; w }, numeric(40)))
  x <- P[, 1] * 3 + Z[1:40]
  y <- 1 + 2 * x + Z[101:140]
  r <- SpatialRegressionML(y, cbind(1, x), W, "lag")
  expect_lt(abs(r$rho + 0.1184623507166), 1e-7)
  expect_lt(max(abs(r$se[1:3] - c(0.3772750619259, 0.1327597567211, 0.0991681511581))), 1e-8)
  e <- SpatialRegressionML(y, cbind(1, x), W, "error")
  expect_lt(abs(e$loglik + 52.299911639139), 1e-9)
  s <- SpatialRegressionML(y, cbind(1, x), W, "sac")
  expect_lt(abs(s$lambda + 0.2015844059734), 1e-6)
})
