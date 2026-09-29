# Tests for SpatialFunctional: functional PCA, graph layers, persistent homology, clustering, REML.

test_that("CurveFpca eigenfunctions are orthonormal and scores carry the eigenvalues", {
  t <- (0:4) / 4
  C <- t(sapply(0:7, function(i) sin(3 * t * (1 + 0.2 * i)) + 0.3 * i * t))
  r <- CurveFpca(C, t, 2)
  w <- c(0.125, 0.25, 0.25, 0.25, 0.125)
  expect_equal(r$eigenfunctions %*% (w * t(r$eigenfunctions)), diag(2), tolerance = 1e-12)
  expect_equal(colSums(r$scores^2) / 7, r$eigenvalues, tolerance = 1e-12)
})

test_that("GcnLayer, GatLayer and PersistenceLandscape", {
  A <- rbind(c(0, 1, 0), c(1, 0, 1), c(0, 1, 0))
  H <- matrix(c(1, 2, 4))
  S <- (A + diag(3)) / sqrt(outer(c(2, 3, 2), c(2, 3, 2)))
  expect_equal(as.numeric(GcnLayer(A, H, matrix(0.5), activation = "linear")), as.numeric(0.5 * S %*% H))
  expect_equal(GatLayer(A, H, matrix(1), c(0, 0))$output[2, 1], 7 / 3)
  L <- PersistenceLandscape(rbind(c(0, 0, 2), c(0, 1, 3), c(1, 0.5, 0.7)), c(0.5, 1.5, 2.5), k_max = 2, dimension = 0)
  expect_equal(L, rbind(c(0.5, 0.5, 0.5), c(0, 0.5, 0)))
})

test_that("RipsPersistence on a square", {
  d <- RipsPersistence(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1)))$diagram
  expect_equal(unname(d[d[, 1] == 1, ]), c(1, 1, sqrt(2)))
  expect_equal(sum(d[, 1] == 0 & is.infinite(d[, 3])), 1)
})

test_that("RemlComponents for a balanced one-way layout is ANOVA", {
  y <- c(1, 1.2, 3, 3.3, 2, 2.4)
  Z <- outer(rep(1:3, each = 2), 1:3, `==`) * 1
  r <- RemlComponents(y, matrix(1, 6, 1), list(Z))
  msw <- (0.02 + 0.045 + 0.08) / 3
  msb <- 2 * sum((c(1.1, 3.15, 2.2) - 2.15)^2) / 2
  expect_equal(r$variances, c((msb - msw) / 2, msw), tolerance = 1e-10)
})
