# SPDX-License-Identifier: AGPL-3.0-or-later
# Native classical MDS must reproduce the reference it replaces.
# Classical scaling is unique only up to a per-axis reflection, so axes
# are sign-aligned before comparison; the pairwise distances, which are
# reflection-invariant, are checked separately.

test_that("native cmdscale matches the reference across sizes and dims", {
  set.seed(9)
  for (m in c(8, 20, 40)) {
    P <- matrix(stats::rnorm(m * 3), m, 3)
    D <- as.matrix(stats::dist(P))
    for (k in c(2, 3)) {
      a <- .morie_sv_cmdscale(D, k)
      b <- stats::cmdscale(D, k = k)
      for (j in seq_len(k)) {
        if (sum((a[, j] - b[, j])^2) > sum((a[, j] + b[, j])^2)) a[, j] <- -a[, j]
      }
      expect_equal(as.numeric(a), as.numeric(b), tolerance = 1e-8)
      expect_equal(as.matrix(stats::dist(a)), as.matrix(stats::dist(b)),
                   tolerance = 1e-8)
    }
  }
})

test_that("points already in k dimensions have their distances recovered", {
  set.seed(3)
  P <- matrix(stats::rnorm(30), 15, 2)
  D <- as.matrix(stats::dist(P))
  X <- .morie_sv_cmdscale(D, 2)
  expect_equal(as.matrix(stats::dist(X)), D, tolerance = 1e-9)
})

test_that("a non-Euclidean D is handled by clamping negative eigenvalues", {
  set.seed(4)
  D <- matrix(stats::runif(36, 1, 2), 6, 6)
  D <- (D + t(D)) / 2
  diag(D) <- 0
  X <- .morie_sv_cmdscale(D, 2)
  expect_true(all(is.finite(X)))
  expect_identical(dim(X), c(6L, 2L))
})

test_that("cmdscale input validation", {
  D <- as.matrix(stats::dist(matrix(stats::rnorm(20), 10, 2)))
  expect_error(.morie_sv_cmdscale(D, 10), "1 <= k < nrow")
  expect_error(.morie_sv_cmdscale(D, 0), "1 <= k < nrow")
  expect_error(.morie_sv_cmdscale(matrix(1, 2, 3), 1), "must be square")
})
