test_that("spatial model entry points return the underlying estimators", {
  W <- matrix(0, 10, 10)
  W[abs(row(W) - col(W)) == 1] <- 1
  W <- W / rowSums(W)
  X <- cbind(1, c(2, -1, 0.1, 1.5, 0.6, -0.4, 0.9, -1.3, 0.2, 1.1))
  y <- c(1, 0, 1, 1, 0, 0, 1, 0, 0, 1)
  expect_identical(SpatialProbit(y, X, W), SpatialProbitGmm(y, X, W))
  expect_identical(
    SpatialProbit(y, X, W, method = "bayes", ndraw = 30, burn_in = 5, seed = 3),
    SarProbitGibbs(y, X, W, ndraw = 30, burn_in = 5, seed = 3)
  )
  expect_error(SpatialProbit(y, X, W, method = "default"))
  expect_identical(SpatialLogit(y, X, W), SpatialLogitGmm(y, X, W))
  Wc <- W
  Xc <- cbind(1, c(0.1, 0.3, 0.5, 0.9, 0.2, 0.4, 0.8, 0.6, 0.7, 0.05))
  yc <- c(0, 1, 3, 5, 0, 2, 4, 0, 3, 0)
  expect_identical(SpatialPoisson(yc, Xc, Wc), SarPoisson(yc, Xc, Wc))
  expect_identical(SpatialZip(yc, Xc, Wc, rho_bounds = c(-0.5, 0.5)), SarZip(yc, Xc, Wc, rho_bounds = c(-0.5, 0.5)))
})

test_that("spatial panel entry points return the panel estimators", {
  W <- rbind(c(0, 1, 0, 0), c(0.5, 0, 0.5, 0), c(0, 0.5, 0, 0.5), c(0, 0, 1, 0))
  y <- c(1, 2, 1.5, 0.3, 1.2, 2.4, 1.1, 0.8, 0.9, 2.2, 1.8, 0.1)
  X <- matrix(c(0.5, 1, 0.2, 0.3, 0.7, 1.3, 0.1, 0.6, 0.4, 0.9, 0.6, 0.2), ncol = 1)
  for (m in c("lag", "error", "durbin")) {
    for (e in c("individual", "time", "twoways")) {
      expect_identical(SpatialPanelFe(y, X, W, 4, model = m, effects = e), SpatialPanelMl(y, X, W, 4, model = m, effects = e))
    }
  }
  expect_error(SpatialPanelFe(y, X, W, 4, effects = "pooled"))
  expect_identical(SpatialPanelRe(y, X, W, 4), SpatialPanelReLag(y, X, W, 4))
})
