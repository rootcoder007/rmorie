test_that("SpatialTwoStageLS matches stsls", {
  U <- .morie_random_uniform(200, seed = 51, stream = 0)
  Z <- .morie_random_normal(200, seed = 51, stream = 1)
  P <- cbind(U[seq(1, 79, 2)], U[seq(2, 80, 2)])
  D <- as.matrix(stats::dist(P))
  W <- t(vapply(1:40, function(i) { w <- numeric(40); w[order(D[i, ], seq_len(40))[2:5]] <- 0.25; w }, numeric(40)))
  x <- P[, 1] * 3 + Z[1:40]
  y <- 1 + 2 * x + Z[101:140]
  r <- SpatialTwoStageLS(y, cbind(1, x, x^2 / 10), W)
  expect_lt(max(abs(r$coefficients - c(-0.0857735512, 1.213554624, 2.0070365699, -0.0037460142))), 1e-9)
  Wd <- (D <= 0.35) * 1
  diag(Wd) <- 0
  b <- SpatialTwoStageLS(y, cbind(1, x, x^2 / 10), Wd, robust = "HC1")
  expect_lt(max(abs(b$se - c(0.006309989, 0.3176631185, 0.2467729784, 0.6422550447))), 1e-9)
})
