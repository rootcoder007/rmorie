# SPDX-License-Identifier: AGPL-3.0-or-later
# CAR model, R side. Asserts the book's conditions, not stored output.
#
# The previous version of this file pinned rho = 0.854827586206896. That
# number was an artifact of a 30-point grid on (0.01, 0.99): it was a
# grid node, could never be zero or negative, and under the identity
# parameterization sat outside the valid parameter space. The test
# encoded the bug.
#
# Schabenberger & Gotway (2005), Sec 6.2.2.2, eqs (6.43)-(6.48).

chain <- function(n = 24) {
  W <- matrix(0, n, n)
  for (i in 1:(n - 1)) { W[i, i + 1] <- 1
  W[i + 1, i] <- 1 }
  W
}
zz <- function(n = 24) sapply(0:(n - 1), function(i) sin(0.7 * i) + 0.3 * cos(0.31 * i))

test_that("rho bounds come from the eigenvalue condition (eq 6.48)", {
  W <- chain()
  b <- car_rho_bounds(W, "identity")
  ev <- eigen(W, symmetric = TRUE, only.values = TRUE)$values
  expect_equal(b[1], 1 / min(ev), tolerance = 1e-12)
  expect_equal(b[2], 1 / max(ev), tolerance = 1e-12)
  n <- nrow(W)
  expect_gt(min(eigen(diag(n) - (b[2] - 1e-9) * W, only.values = TRUE)$values), 0)
  expect_lt(min(eigen(diag(n) - (b[2] + 1e-3) * W, only.values = TRUE)$values), 0)
})

test_that("the estimate stays inside the valid interval", {
  W <- chain()
  for (par in c("weighted", "identity")) {
    b <- car_rho_bounds(W, par)
    r <- sgcar(zz(), W, NULL, par)
    expect_gt(r$statistic, b[1])
    expect_lt(r$statistic, b[2])
  }
})

test_that("negative dependence is reachable", {
  W <- chain()
  alt <- (-1)^(0:(nrow(W) - 1))
  expect_lt(sgcar(alt, W)$statistic, 0)
})

test_that("recovers a known rho from simulated CAR data", {
  set.seed(11)
  n <- 30
  W <- chain(n)
  D <- diag(rowSums(W), n)
  for (true_rho in c(0, 0.5, -0.5)) {
    S <- solve(D - true_rho * W)
    L <- t(chol((S + t(S)) / 2))
    est <- replicate(60, sgcar(as.numeric(L %*% rnorm(n)), W)$statistic)
    # ABSOLUTE tolerance: testthat's `tolerance` is relative, which is the
    # wrong scale near rho = 0. ML for spatial dependence is biased toward
    # zero at small n, so this checks recovery, not unbiasedness.
    expect_lt(abs(mean(est) - true_rho), 0.30)
  }
})

test_that("the two parameterizations are different models", {
  W <- chain()
  z <- zz()
  expect_false(isTRUE(all.equal(sgcar(z, W, NULL, "weighted")$statistic,
                                sgcar(z, W, NULL, "identity")$statistic)))
  expect_lt(car_rho_bounds(W, "identity")[2], car_rho_bounds(W, "weighted")[2])
})

test_that("Haining rho_ols matches its closed form", {
  W <- chain()
  z <- zz()
  X <- matrix(1, length(z), 1)
  e <- as.numeric(z - X %*% qr.solve(X, z))
  expect_equal(car_rho_ols(z, W),
               as.numeric(crossprod(e, W %*% e) / crossprod(e, W %*% (W %*% e))),
               tolerance = 1e-12)
})

test_that("the estimate is not confined to the grid it replaced", {
  r <- sgcar(zz(), chain())
  grid <- seq(0.01, 0.99, length.out = 30)
  expect_false(any(abs(grid - r$statistic) < 1e-9))
})

test_that("asymmetric weights are rejected", {
  W <- chain()
  Wrs <- W / pmax(rowSums(W), 1e-12)
  expect_error(sgcar(zz(), Wrs), "symmetric")
})

test_that("input validation", {
  W <- chain()
  z <- zz()
  expect_error(sgcar(z[-1], W), "to match")
  expect_error(sgcar(z, W, matrix(1, 5, 1)), "one row per element")
  expect_error(sgcar(z, W, NULL, "nope"), "parameterization")
})

test_that("spcar delegates and forwards every argument", {
  W <- chain()
  z <- zz()
  X <- cbind(1, seq_along(z) / length(z))
  expect_equal(spcar(z, W)$statistic, sgcar(z, W)$statistic, tolerance = 1e-15)
  expect_equal(spcar(z, W, X)$statistic, sgcar(z, W, X)$statistic, tolerance = 1e-15)
  expect_equal(spcar(z, W, NULL, "identity")$statistic,
               sgcar(z, W, NULL, "identity")$statistic, tolerance = 1e-15)
  # a delegate that dropped an argument would still pass the equalities above
  expect_false(isTRUE(all.equal(spcar(z, W, X)$statistic, spcar(z, W)$statistic)))
  expect_false(isTRUE(all.equal(spcar(z, W, NULL, "identity")$statistic,
                                spcar(z, W)$statistic)))
})
