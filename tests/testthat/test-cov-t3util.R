# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/t3util.R (shared numerical helpers): golden-section
# minimisation, normal quadrature nodes, breadth-first distances,
# ReLU / expit and least squares (with the pseudo-inverse fallback).

test_that("t3golden finds the minimiser of a unimodal function", {
  # a smooth minimum is only located to about sqrt(machine eps) in x,
  # because f is flat to rounding there
  expect_equal(t3golden(function(x) (x - 1.3)^2 + 2, -5, 5), 1.3, tolerance = 1e-7)
  expect_equal(t3golden(function(x) abs(x + 0.25), -1, 1), -0.25, tolerance = 1e-9)
  expect_equal(t3golden(function(x) x, 0, 1, iters = 60), 0, tolerance = 1e-9)
})

test_that("t3nodes are normalised Gaussian weights on an even grid", {
  q <- t3nodes(201, 6)
  expect_equal(q$u, seq(-6, 6, length.out = 201))
  expect_equal(sum(q$w), 1, tolerance = 1e-12)
  expect_equal(sum(q$w * q$u^2), 1, tolerance = 1e-6)
  expect_equal(q$w / q$w[101], exp(-0.5 * q$u^2), tolerance = 1e-12)
})

test_that("t3bfs gives hop distances, Inf when unreachable", {
  A <- matrix(0, 5, 5)
  A[1, 2] <- A[2, 3] <- A[3, 4] <- A[1, 4] <- 1
  expect_equal(t3bfs(A, 1), c(0, 1, 2, 1, Inf))
  expect_equal(t3bfs(A + t(A), 3), c(2, 1, 0, 1, Inf))
})

test_that("t3relu and t3expit", {
  expect_equal(t3relu(c(-2, 0, 3.5)), c(0, 0, 3.5))
  expect_equal(t3expit(c(-3, 0, 2)), plogis(c(-3, 0, 2)), tolerance = 1e-12)
})

test_that("t3ols solves the normal equations; singular designs take the minimum-norm solution", {
  X <- cbind(1, c(1, 2, 3, 5), c(2, 1, 0, 1))
  y <- c(1, 3, 2, 6)
  expect_equal(t3ols(X, y), unname(coef(lm(y ~ X - 1))), tolerance = 1e-10)
  Xs <- cbind(1, 1:4, 2 * (1:4))
  s <- svd(crossprod(Xs))
  pinv <- s$v %*% diag(ifelse(s$d > 1e-9 * s$d[1], 1 / s$d, 0)) %*% t(s$u)
  expect_equal(t3ols(Xs, y), as.numeric(pinv %*% crossprod(Xs, y)), tolerance = 1e-8)
})
