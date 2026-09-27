# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: assignment, transportation and simplex vs lpSolve.

test_that("LinearAssignment matches lpSolve::lp.assign", {
  skip_if_not_installed("lpSolve")
  u <- .morie_random_uniform(4000, seed = 3, stream = 0)
  for (t in 0:5) {
    S <- matrix(round(20 * u[50 * t + 1:25], 3), 5, 5, byrow = TRUE)
    expect_equal(LinearAssignment(S)$total, lpSolve::lp.assign(S)$objval, tolerance = 1e-9)
  }
})

test_that("TransportationProblem matches lpSolve::lp.transport", {
  skip_if_not_installed("lpSolve")
  C <- rbind(c(4, 6, 9), c(5, 3, 8))
  lp <- lpSolve::lp.transport(C, "min", rep("<=", 2), c(15, 10), rep(">=", 3), c(8, 12, 4))
  expect_equal(TransportationProblem(C, c(15, 10), c(8, 12, 4))$total_cost, lp$objval, tolerance = 1e-9)
})

test_that("the SLP simplex matches lpSolve::lp", {
  skip_if_not_installed("lpSolve")
  A <- rbind(c(1, 0, 1, 0, 0, 0), c(0, 1, 0, 1, 0, 0), c(-2, -2, 0, 0, 1, -1))
  r <- .lp_simplex(c(-1, -2, 0, 0, 1, 0), A, c(1, 1, -4))
  expect_equal(sum(c(-1, -2) * r$x[1:2]), lpSolve::lp("min", c(-1, -2, 0, 0, 1, 0), A, rep("=", 3), c(1, 1, -4))$objval,
               tolerance = 1e-12)
})
