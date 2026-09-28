.cs_P <- cbind(0.3 * (0:5), (0.7 * (0:5)) %% 2.1)
.cs_M <- list(list(model = "Nug", psill = 0.1), list(model = "Exp", psill = 1, range = 0.8))

test_that("CholeskySim is mean + L e and matches the Python example", {
  r <- CholeskySim(.cs_P, .cs_M, nsim = 2, seed = 7, mean = 2)
  L <- t(chol(KrigingCovariance(as.matrix(stats::dist(.cs_P)), .cs_M)))
  for (s in 1:2) expect_equal(r$simulations[s, ], 2 + as.vector(L %*% .morie_random_normal(6, seed = 7, stream = s - 1)),
                              tolerance = 1e-14)
  e <- CholeskySim(rbind(c(0, 0), c(1, 0)), list(model = "Exp", psill = 1, range = 1), seed = 3)$simulations
  expect_equal(round(as.vector(e), 6), c(0.902691, -0.775404))
})

test_that("precision method solves R' x = e", {
  r <- CholeskySim(.cs_P, .cs_M, seed = 4, method = "precision")
  R <- t(chol(solve(KrigingCovariance(as.matrix(stats::dist(.cs_P)), .cs_M))))
  expect_equal(as.vector(t(R) %*% r$simulations[1, ]), .morie_random_normal(6, seed = 4, stream = 0), tolerance = 1e-12)
})

test_that("conditional law is simple kriging", {
  D <- rbind(c(0.1, 0.2), c(1, 1), c(1.4, 0.3))
  z <- c(1, 0.5, 2)
  r <- CholeskySim(.cs_P, .cs_M, nsim = 2, seed = 5, z = z, data_coords = D, mean = 1)
  k <- Krige(z, D, .cs_P, .cs_M, beta = 1)
  expect_equal(r$mean, k$prediction, tolerance = 1e-12)
  expect_equal(diag(r$cov), k$variance, tolerance = 1e-12)
})

test_that("PivotedCholesky is low rank and reconstructs", {
  P <- cbind(0.05 * (0:19), 0)
  A <- KrigingCovariance(as.matrix(stats::dist(P)), list(model = "Gau", psill = 1, range = 2))
  r <- PivotedCholesky(A, tol = 1e-10)
  expect_lt(r$rank, 20)
  expect_lt(max(abs(A - r$L %*% t(r$L))), 1e-9 * 20)
  f <- PivotedCholesky(matrix(c(4, 2, 2, 1), 2))
  expect_equal(c(f$rank, f$pivots, f$L[, 1]), c(1, 1, 2, 1))
})

test_that("CholeskySim validates its inputs", {
  expect_error(CholeskySim(.cs_P, .cs_M, method = "svd"), "method")
  expect_error(CholeskySim(.cs_P, .cs_M, z = 1), "data_coords")
})
