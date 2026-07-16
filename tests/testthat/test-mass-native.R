# SPDX-License-Identifier: AGPL-3.0-or-later
# Structural tests for native MASS utilities (module 30). No MASS needed.

test_that(".morie_ginv satisfies the Moore-Penrose conditions", {
  set.seed(1); A <- matrix(rnorm(20), 5, 4)
  G <- rmorie:::.morie_ginv(A)
  expect_equal(dim(G), c(4L, 5L))
  expect_equal(A %*% G %*% A, A, tolerance = 1e-8)          # AGA = A
  expect_equal(G %*% A %*% G, G, tolerance = 1e-8)          # GAG = G
  expect_equal(t(A %*% G), A %*% G, tolerance = 1e-8)       # (AG)' = AG
  # inverse of a full-rank square matrix equals solve()
  B <- crossprod(matrix(rnorm(9), 3))
  expect_equal(rmorie:::.morie_ginv(B), solve(B), tolerance = 1e-8)
})

test_that("morie_mvrnorm returns the right shape and recovers moments", {
  mu <- c(2, -1); Sig <- matrix(c(1, 0.5, 0.5, 2), 2)
  x1 <- morie_mvrnorm(1, mu, Sig)
  expect_length(x1, 2L)
  X <- morie_mvrnorm(5000, mu, Sig)
  expect_equal(dim(X), c(5000L, 2L))
  expect_equal(colMeans(X), mu, tolerance = 0.1)
  expect_equal(cov(X), Sig, tolerance = 0.15)
  expect_error(morie_mvrnorm(1, mu, matrix(c(1, 2, 2, 1), 2)),
               "not positive definite")
})
