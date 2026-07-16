# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: native MASS utilities (module 30) vs MASS.

test_that("native ginv matches MASS::ginv (square, rectangular, rank-deficient)", {
  skip_if_not_installed("MASS")
  set.seed(1)
  A <- matrix(rnorm(25), 5)
  B <- matrix(rnorm(30), 6, 5)
  C <- cbind(1:4, 2:5, c(1, 1, 1, 1))
  expect_equal(rmorie:::.morie_ginv(A), MASS::ginv(A), tolerance = 1e-12)
  expect_equal(rmorie:::.morie_ginv(B), MASS::ginv(B), tolerance = 1e-12)
  expect_equal(rmorie:::.morie_ginv(C), MASS::ginv(C), tolerance = 1e-10)
})

test_that("native mvrnorm matches MASS::mvrnorm bit-for-bit under a seed", {
  skip_if_not_installed("MASS")
  set.seed(2); mu <- c(1, -2, 0.5); Sig <- crossprod(matrix(rnorm(9), 3))
  set.seed(42); m1 <- morie_mvrnorm(100, mu, Sig)
  set.seed(42); m2 <- MASS::mvrnorm(100, mu, Sig)
  expect_equal(m1, m2, tolerance = 1e-12)
  set.seed(7); a <- morie_mvrnorm(1, mu, Sig)
  set.seed(7); b <- MASS::mvrnorm(1, mu, Sig)
  expect_equal(a, b, tolerance = 1e-12)
  set.seed(3); e1 <- morie_mvrnorm(50, mu, Sig, empirical = TRUE)
  set.seed(3); e2 <- MASS::mvrnorm(50, mu, Sig, empirical = TRUE)
  expect_equal(e1, e2, tolerance = 1e-10)
})
