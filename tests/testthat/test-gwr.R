test_that("GWRBasic kernels, OLS limit and documented AICc", {
  expect_equal(round(GWRKernelWeights(c(0, 1, 2, 3), 2.5), 6), c(1, 0.7056, 0.1296, 0))
  expect_equal(GWRKernelWeights(c(0, 1, 2, 3), 2, "boxcar", adaptive = TRUE), c(1, 1, 0, 0))
  P <- as.matrix(expand.grid(y = 0:3, x = 0:3)[, 2:1])
  X <- cbind(1, (0.3 * (0:15)) %% 1.7)
  y <- 1 + 2 * X[, 2] + 0.1 * P[, 1]
  r <- GWRBasic(y, X, P, 3, kernel = "gaussian")
  expect_equal(round(r$diagnostics$AICc, 6), -20.824713)
  expect_equal(r$diagnostics$edf + r$diagnostics$enp, 16, tolerance = 1e-12)
  big <- GWRBasic(y, X, P, 1e6, kernel = "gaussian")
  expect_lt(max(abs(sweep(big$betas, 2, big$betas[1, ]))), 1e-6)
})
