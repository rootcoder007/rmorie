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

test_that("GMErrorSAR and GS2SLSSAC match spatialreg GMerrorsar and gstsls", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  nb <- spdep::cell2nb(6, 7, type = "rook")
  lw <- spdep::nb2listw(nb, style = "W")
  W <- spdep::listw2mat(lw)
  n <- nrow(W)
  z <- .morie_normal_quantile(.morie_random_uniform(3 * n, seed = 11, stream = 0))
  X <- cbind(1, z[1:n], z[n + 1:n])
  e <- z[2 * n + 1:n]
  y <- as.vector(1 + 2 * X[, 2] - X[, 3] + e + 0.5 * W %*% e)
  df <- data.frame(y = y, x1 = X[, 2], x2 = X[, 3])
  ctl <- list(rel.tol = 1e-14, x.tol = 1e-14)
  g <- suppressWarnings(spatialreg::GMerrorsar(y ~ x1 + x2, df, lw, control = ctl))
  ours <- GMErrorSAR(y, X, W)
  expect_equal(ours$lambda, unname(g$lambda), tolerance = 1e-7)
  expect_equal(ours$coefficients, unname(g$coefficients), tolerance = 1e-7)
  expect_equal(ours$se, unname(g$rest.se), tolerance = 1e-7)
  expect_equal(ours$s2, g$s2, tolerance = 1e-7)
  expect_equal(GMErrorSAR(y, X, W, lambda_se_method = "spatialreg")$lambda_se, as.numeric(g$lambda.se),
               tolerance = 1e-6)
  for (rb in c(FALSE, TRUE)) {
    s <- suppressWarnings(spatialreg::gstsls(y ~ x1 + x2, df, lw, robust = rb, control = ctl))
    o <- GS2SLSSAC(y, X, W, robust = if (rb) "HC0" else NULL)
    expect_equal(o$lambda, unname(s$lambda), tolerance = 1e-7)
    expect_equal(o$coefficients, unname(s$coefficients), tolerance = 1e-7)
    expect_equal(o$se, unname(s$rest.se), tolerance = 1e-7)
  }
})

test_that("MoranEigenvectorFilter matches spatialreg::SpatialFiltering", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("spdep")
  n <- 40
  u <- .morie_random_uniform(4 * n, seed = 5, stream = 0)
  pts <- cbind(u[1:n], u[n + 1:n])
  D <- as.matrix(stats::dist(pts))
  A <- matrix(0, n, n)
  for (i in 1:n) A[i, order(D[i, ])[2:5]] <- 1
  A <- 1 * ((A + t(A)) > 0)
  x <- u[2 * n + 1:n]
  y <- 2 + x + 3 * (pts[, 1] - 0.5)^2 + 2 * pts[, 2] + 0.3 * u[3 * n + 1:n]
  nb <- spdep::mat2listw(A, style = "B")$neighbours
  for (a in list(NULL, 0.2)) {
    # SpatialFiltering cat()s a note when it stops on an inversion
    s <- spatialreg::SpatialFiltering(y ~ x, data = data.frame(y, x), nb = nb, alpha = a)
    r <- MoranEigenvectorFilter(y, cbind(1, x), A, alpha = a)
    expect_equal(r$selection$evec, unname(s$selection[, "SelEvec"]))
    expect_equal(r$selection$moran, unname(s$selection[, "MinMi"]), tolerance = 1e-10)
    expect_equal(r$selection$z, unname(s$selection[, "ZMinMi"]), tolerance = 1e-10)
    expect_equal(r$selection$r2, unname(s$selection[, "R2"]), tolerance = 1e-10)
    expect_equal(r$fitted, unname(stats::fitted(stats::lm(y ~ x + stats::fitted(s)))), tolerance = 1e-10)
  }
})

test_that("SLXRegression equals lmSLX and SpatialImpacts equals impacts", {
  skip_if_not_installed("spatialreg")
  nb <- spdep::cell2nb(6, 7, type = "rook")
  lw <- spdep::nb2listw(nb, style = "W")
  W <- spdep::listw2mat(lw)
  n <- nrow(W)
  u <- .morie_random_uniform(3 * n, seed = 13, stream = 0)
  df <- data.frame(y = 1 + 2 * u[1:n] - u[n + 1:n] + u[2 * n + 1:n], x1 = u[1:n], x2 = u[n + 1:n])
  X <- cbind(1, df$x1, df$x2)
  ref <- spatialreg::lmSLX(y ~ x1 + x2, df, lw)
  s <- SLXRegression(df$y, X, W)
  expect_equal(s$coefficients, unname(stats::coef(ref)), tolerance = 1e-12)
  expect_equal(s$se, unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-12)
  fd <- spatialreg::lagsarlm(y ~ x1 + x2, df, lw, Durbin = TRUE)
  id <- spatialreg::impacts(fd, listw = lw)
  cf <- stats::coef(fd)
  m <- SpatialImpacts(fd$rho, cf[c("x1", "x2")], W, theta = cf[c("lag.x1", "lag.x2")])
  expect_equal(unname(m$direct), unname(id$direct), tolerance = 1e-10)
  expect_equal(unname(m$total), unname(id$total), tolerance = 1e-10)
})
