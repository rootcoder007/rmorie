test_that("WeightsStyle and WeightsSummary equal spdep and spatialreg", {
  skip_if_not_installed("spdep")
  skip_if_not_installed("spatialreg")
  u <- .morie_random_uniform(150, seed = 7, stream = 0)
  n <- 10
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) for (j in (i + 1):n) if (u[(i - 1) * n + j] < 0.4) W[i, j] <- W[j, i] <- 0.5 + u[100 + i + j]
  nb <- spdep::mat2listw(W, style = "B", zero.policy = TRUE)$neighbours
  gl <- lapply(seq_len(n), function(i) W[i, W[i, ] != 0])
  for (st in c("W", "C", "U", "minmax", "S")) {
    l <- spdep::nb2listw(nb, glist = gl, style = st, zero.policy = TRUE)
    expect_equal(WeightsStyle(W, st), spdep::listw2mat(l), tolerance = 1e-14, ignore_attr = TRUE, info = st)
  }
  lw <- spdep::nb2listw(nb, glist = gl, style = "W", zero.policy = TRUE)
  k <- spdep::spweights.constants(lw, zero.policy = TRUE)
  s <- WeightsSummary(WeightsStyle(W, "W"))
  expect_equal(c(s$S0, s$S1, s$S2), c(k$S0, k$S1, k$S2), tolerance = 1e-12)
  expect_equal(sort(Re(s$eigenvalues)), sort(Re(spatialreg::eigenw(lw))), tolerance = 1e-10)
})
