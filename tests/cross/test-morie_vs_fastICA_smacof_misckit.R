# Cross tests: FastIca against the fastICA package (up to sign and order) and RstressMds (r = 1) against smacof.

test_that("FastIca recovers the same components as fastICA::fastICA", {
  skip_if_not_installed("fastICA")
  k <- 0:199
  S <- cbind(sin(k * 0.13), ((k * 7) %% 11) / 5 - 1, cos(k * 0.029)^3)
  X <- S %*% rbind(c(1, 0.3, 0.6), c(0.5, -1, 0.2), c(0.2, 0.4, -1))
  got <- FastIca(X, 3, tol = 1e-10, max_iter = 500)
  ref <- fastICA::fastICA(X, 3, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "R", tol = 1e-10, maxit = 500,
                          w.init = diag(3))
  C <- abs(stats::cor(got$S, ref$S))
  expect_true(all(apply(C, 1, max) > 0.9999))
})

test_that("RstressMds with r = 1 reaches smacof's ratio stress-1", {
  skip_if_not_installed("smacof")
  P <- cbind(cos(0:9), sin((0:9) * 1.3), 0.1 * (0:9))
  D <- as.matrix(stats::dist(P)) + 0.05 * (outer(0:9, 0:9) %% 3) * (1 - diag(10))
  D <- (D + t(D)) / 2
  ref <- smacof::mds(D, ndim = 2, type = "ratio", eps = 1e-12, itmax = 10000)
  got <- RstressMds(D, r = 1, ndim = 2)
  expect_equal(got$stress, ref$stress, tolerance = 1e-5)
})
