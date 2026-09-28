test_that("GWRBasic equals GWmodel::gwr.basic", {
  skip_if_not_installed("GWmodel")
  skip_if_not_installed("sp")
  n <- 45
  u <- .morie_random_uniform(5 * n, seed = 17, stream = 0)
  P <- cbind(10 * u[1:n], 10 * u[n + 1:n])
  X <- cbind(1, u[2 * n + 1:n], u[3 * n + 1:n])
  y <- 1 + (1 + 0.2 * P[, 1]) * X[, 2] - (0.5 + 0.1 * P[, 2]) * X[, 3] + 0.3 * (u[4 * n + 1:n] - 0.5)
  df <- data.frame(y = y, x1 = X[, 2], x2 = X[, 3])
  sp::coordinates(df) <- P
  for (cfg in list(list(4, "bisquare", FALSE), list(12, "gaussian", TRUE), list(20, "tricube", TRUE))) {
    g <- GWmodel::gwr.basic(y ~ x1 + x2, data = df, bw = cfg[[1]], kernel = cfg[[2]], adaptive = cfg[[3]])
    S <- as.data.frame(g$SDF)
    r <- GWRBasic(y, X, P, cfg[[1]], cfg[[2]], cfg[[3]])
    expect_equal(unname(r$betas), unname(as.matrix(S[, c("Intercept", "x1", "x2")])), tolerance = 1e-10)
    expect_equal(unname(r$se), unname(as.matrix(S[, c("Intercept_SE", "x1_SE", "x2_SE")])), tolerance = 1e-10)
    expect_equal(r$local_r2, S$Local_R2, tolerance = 1e-10)
    expect_equal(r$diagnostics$AICc, g$GW.diagnostic$AICc, tolerance = 1e-10)
  }
})
