# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/smoopt_native.R (Platt 1998 sequential minimal
# optimisation). The error cache is f(x_i) - y_i with
# f = sum a y K - b, the KKT tests and threshold updates follow
# Platt's eqs. (12)-(20), and SMO on the separable points -2, -1, 1, 2
# must reach the hand-derived hard-margin solution alpha = (0, 1/2,
# 1/2, 0), b = 0.

.sm_x <- c(-2, -1, 1, 2)
.sm_y <- c(-1, -1, 1, 1)
.sm_K <- outer(.sm_x, .sm_x)

test_that("error_cache is f(x) - y with f = sum a y K - b", {
  a <- c(0.1, 0.3, 0.2, 0.4)
  E <- error_cache(a, .sm_y, .sm_K, 0.25)
  expect_equal(E, as.numeric(.sm_K %*% (a * .sm_y)) - 0.25 - .sm_y, tolerance = 1e-12)
})

test_that("violates_kkt flags y E on the wrong side of the tolerance", {
  E <- c(-0.5, 0.5, 0, 0.5)
  expect_true(violates_kkt(1, c(0, 0, 0, 0), c(1, 1, 1, 1), E, 1))
  expect_false(violates_kkt(1, c(1, 0, 0, 0), c(1, 1, 1, 1), E, 1))
  expect_true(violates_kkt(2, c(0, 0.4, 0, 0), c(1, 1, 1, 1), E, 1))
  expect_false(violates_kkt(2, c(0, 0, 0, 0), c(1, 1, 1, 1), E, 1))
  expect_false(violates_kkt(3, c(0, 0, 0.5, 0), c(1, 1, 1, 1), E, 1))
})

test_that("outer_loop_schedule alternates full and non-bound sweeps", {
  a <- c(0, 0.3, 1, 0.7)
  expect_equal(outer_loop_schedule(a, 1, TRUE)$indices, 1:4)
  nb <- outer_loop_schedule(a, 1, FALSE)
  expect_equal(nb$indices, c(2L, 4L))
  expect_identical(nb$kind, "non_bound")
})

test_that("second_choice follows Platt's hierarchy", {
  rng <- list(uniform = function() 0.6)
  E <- c(0.1, -0.8, 0.9, 0.2, -0.1)
  a <- c(0.5, 0.2, 0.3, 0, 1)
  s1 <- second_choice(1, a, rep(1, 5), E, 1, rng)
  # non-bound others: 2 and 3; |E1 - E| is 0.9 and 0.8
  expect_identical(s1$index, 2L)
  expect_equal(s1$gap, 0.9)
  a2 <- c(0.5, 0.2, 0, 0, 1)
  s2 <- second_choice(1, a2, rep(1, 5), E, 1, rng)
  expect_identical(s2$level, 2L)
  expect_identical(s2$index, 2L)
  s3 <- second_choice(4, c(0, 0, 0, 0, 1), rep(1, 5), E, 1, rng)
  # start = floor(0.6 * 5) = 3, so the scan begins at example 4 (i1) then 5
  expect_identical(s3$level, 3L)
  expect_identical(s3$index, 5L)
  s4 <- second_choice(1, 0, 1, 0.1, 1, rng)
  expect_null(s4$index)
  expect_identical(s4$level, 4L)
})

test_that("compute_threshold uses b1, b2 or their midpoint", {
  a <- c(0.2, 0.4)
  y <- c(1, -1)
  E <- c(0.3, -0.2)
  K <- matrix(c(2, 0.5, 0.5, 1), 2)
  b1 <- 0.1 + 0.3 + 1 * (0.5 - 0.2) * 2 + (-1) * (0.6 - 0.4) * 0.5
  b2 <- 0.1 - 0.2 + 1 * (0.5 - 0.2) * 0.5 + (-1) * (0.6 - 0.4) * 1
  t1 <- compute_threshold(1, 2, 0.5, 0.6, a, y, E, K, 0.1, 1)
  expect_equal(c(t1$b, t1$b1, t1$b2), c(b1, b1, b2), tolerance = 1e-12)
  expect_identical(t1$from, "i1")
  t2 <- compute_threshold(1, 2, 1, 0.6, a, y, E, K, 0.1, 1)
  expect_identical(t2$from, "i2")
  t3 <- compute_threshold(1, 2, 0, 1, a, y, E, K, 0.1, 1)
  expect_equal(t3$b, 0.5 * (t3$b1 + t3$b2))
})

test_that("smo_platt reaches the hard-margin solution", {
  for (fn in list(smo_platt, sequential_minimal_optimization, smo_solver, smosolver, morie_smoopt$smo_platt)) {
    r <- fn(.sm_y, .sm_K, C = 10, tol = 1e-6, eps = 1e-12)
    # SMO stops once every KKT condition holds to tol = 1e-6
    expect_equal(r$alpha, c(0, 0.5, 0.5, 0), tolerance = 1e-6)
    expect_equal(r$b, 0, tolerance = 1e-6)
    expect_equal(r$equality_residual, 0, tolerance = 1e-12)
    expect_equal(r$kkt_violations, 0L)
    expect_identical(r$support_vectors, 2:3)
    a <- r$alpha
    expect_equal(r$objective, sum(a) - 0.5 * sum(outer(a * .sm_y, a * .sm_y) * .sm_K), tolerance = 1e-12)
  }
  expect_error(smo_platt(c(0, 1), diag(2)), "-1 or \\+1")
  expect_error(smo_platt(c(-1, 1), diag(2), C = 0), "C must be positive")
  expect_match(morie_smoopt$cheatsheet(), "NO inner QP solver", fixed = TRUE)
})
