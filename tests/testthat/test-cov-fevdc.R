# SPDX-License-Identifier: AGPL-3.0-or-later

test_that("Fevdc validates its inputs", {
  expect_error(Fevdc(matrix(1:6, 2, 3), diag(2)), "must be square")
  expect_error(Fevdc(diag(2) * 0.5, diag(3)), "sigma_u must be k by k")
  expect_error(Fevdc(diag(2) * 0.5, diag(2), periods = -1), "non-negative")
})

test_that("Fevdc with zero periods gives Cholesky shares", {
  r <- Fevdc(diag(2) * 0.5, diag(2), periods = 0)
  expect_equal(dim(r$decomposition), c(1L, 2L, 2L))
  expect_equal(r$decomposition[1, , ], diag(2))
})

test_that("Fevdc shares sum to one across shocks", {
  A <- matrix(c(0.5, 0.2, 0.1, 0.3), 2, 2)
  S <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  r <- Fevdc(A, S, periods = 3)
  expect_equal(dim(r$decomposition), c(4L, 2L, 2L))
  expect_equal(apply(r$decomposition[4, , ], 1, sum), c(1, 1))
})
