test_that("SpatialRegressionML equals spatialreg with method LU", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  U <- .morie_random_uniform(300, seed = 52, stream = 0)
  Z <- .morie_random_normal(300, seed = 52, stream = 1)
  n <- 50
  P <- cbind(U[1:n], U[n + 1:n])
  nb <- spdep::knn2nb(spdep::knearneigh(P, 5))
  lw <- spdep::nb2listw(nb, style = "W")
  W <- spdep::listw2mat(lw)
  x1 <- Z[1:n]
  x2 <- U[101:150]
  y <- as.vector(solve(diag(n) - 0.4 * W, 1 + x1 - 2 * x2 + Z[201:250]))
  X <- cbind(1, x1, x2)
  l <- spatialreg::lagsarlm(y ~ x1 + x2, listw = lw, method = "LU")
  r <- SpatialRegressionML(y, X, W, "lag")
  expect_lt(abs(r$rho - l$rho), 1e-6)
  expect_lt(abs(r$loglik - as.numeric(l$LL)), 1e-7)
  expect_lt(max(abs(r$se[1:3] - l$rest.se)), 1e-6)
  e <- spatialreg::errorsarlm(y ~ x1 + x2, listw = lw, method = "LU")
  m <- SpatialRegressionML(y, X, W, "error")
  expect_lt(abs(m$lambda - e$lambda), 1e-6)
  expect_lt(abs(m$se[4] - e$lambda.se), 1e-6)
})

test_that("SpatialTwoStageLS equals spatialreg::stsls", {
  skip_if_not_installed("spatialreg")
  U <- .morie_random_uniform(300, seed = 53, stream = 0)
  Z <- .morie_random_normal(300, seed = 53, stream = 1)
  n <- 60
  P <- cbind(U[1:n], U[n + 1:n])
  lw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(P, 6)), style = "W")
  W <- spdep::listw2mat(lw)
  x1 <- Z[1:n]
  y <- as.vector(solve(diag(n) - 0.3 * W, 2 + 1.5 * x1 + Z[101:160]))
  f <- spatialreg::stsls(y ~ x1, listw = lw)
  r <- SpatialTwoStageLS(y, cbind(1, x1), W)
  expect_lt(max(abs(r$coefficients - stats::coef(f))), 1e-9)
  expect_lt(max(abs(r$se - sqrt(diag(f$var)))), 1e-9)
})
