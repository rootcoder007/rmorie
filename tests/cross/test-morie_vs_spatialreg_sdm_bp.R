test_that("SdmML, BreuschPagan and SpatialBPTest equal spatialreg and lmtest", {
  skip_if_not_installed("spatialreg")
  skip_if_not_installed("lmtest")
  u <- .morie_random_uniform(300, seed = 71, stream = 0)
  z <- .morie_random_normal(200, seed = 71, stream = 1)
  n <- 60
  P <- cbind(u[1:n], u[n + 1:n])
  lw <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(P, 5)), style = "W")
  W <- spdep::listw2mat(lw)
  x1 <- z[1:n]
  x2 <- u[121:180]
  y <- as.vector(solve(diag(n) - 0.35 * W, 1 + x1 - 2 * x2 + 0.6 * W %*% x1 + z[61:120] * (0.3 + u[181:240])))
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  X <- cbind(1, x1, x2)
  ref <- spatialreg::lagsarlm(y ~ x1 + x2, df, lw, Durbin = TRUE, method = "LU")
  r <- SdmML(y, X, W)
  expect_equal(r$rho, unname(ref$rho), tolerance = 1e-6)
  expect_equal(c(r$beta, r$theta), unname(coef(ref)[-1]), tolerance = 1e-6)
  expect_equal(r$loglik, as.numeric(ref$LL), tolerance = 1e-9)
  imp <- spatialreg::impacts(ref, listw = lw)
  expect_equal(r$indirect, unname(imp$indirect), tolerance = 1e-6)
  ols <- lm(y ~ x1 + x2, df)
  for (st in c(TRUE, FALSE)) {
    expect_equal(BreuschPagan(residuals(ols), X, st)$statistic, unname(lmtest::bptest(ols, studentize = st)$statistic),
                 tolerance = 1e-12)
  }
  for (m in c("lag", "error")) {
    fit <- if (m == "lag") spatialreg::lagsarlm(y ~ x1 + x2, df, lw, method = "LU") else
      spatialreg::errorsarlm(y ~ x1 + x2, df, lw, method = "LU")
    for (st in c(TRUE, FALSE)) {
      expect_equal(SpatialBPTest(y, X, W, m, st)$statistic, unname(spatialreg::bptest.Sarlm(fit, studentize = st)$statistic),
                   tolerance = 1e-6)
    }
  }
})
