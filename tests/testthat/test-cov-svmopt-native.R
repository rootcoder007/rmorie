# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/svmopt_native.R (SVM dual by two-variable
# decomposition, Chang & Lin 2011). Kernels, the dual objective, the
# box bounds and the analytic pair step are recomputed; SMO on four
# separable points in 1-D must reach the hand-derived hard-margin
# solution alpha = (0, 1/2, 1/2, 0), b = 0, and the KKT gap must close.

.sv_x <- matrix(c(-2, -1, 1, 2))
.sv_y <- c(-1, -1, 1, 1)
.sv_K <- .sv_x %*% t(.sv_x)

test_that("kernel_matrix builds the linear, polynomial and RBF kernels", {
  X <- rbind(c(1, 0), c(0, 2), c(-1, 1))
  expect_equal(kernel_matrix(X), X %*% t(X))
  expect_equal(kernel_matrix(X, "poly", gamma = 0.5, degree = 2, coef0 = 1),
               (0.5 * (X %*% t(X)) + 1)^2, tolerance = 1e-12)
  D2 <- unname(as.matrix(dist(X))^2)
  expect_equal(kernel_matrix(X, "rbf", gamma = 0.25), exp(-0.25 * D2), tolerance = 1e-12)
  expect_equal(diag(kernel_matrix(X, "rbf")), rep(1, 3))
  expect_error(kernel_matrix(X, "sigmoid"), "kernel must be linear, poly or rbf")
})

test_that("dual_objective is sum(a) - 0.5 a'Qa", {
  a <- c(0.5, 0.25, 0.75, 0)
  q <- sum(outer(a * .sv_y, a * .sv_y) * .sv_K)
  expect_equal(dual_objective(a, .sv_y, .sv_K), sum(a) - 0.5 * q, tolerance = 1e-12)
  expect_equal(dual_objective(rep(0, 4), .sv_y, .sv_K), 0)
})

test_that("the box bounds branch on whether the labels agree", {
  a <- c(0.3, 0.8, 0, 0)
  b <- morie_svmopt$.svmopt_bounds(1, 2, a, .sv_y, 1)
  expect_equal(c(b$L, b$H), c(max(0, 0.3 + 0.8 - 1), min(1, 0.3 + 0.8)))
  d <- morie_svmopt$.svmopt_bounds(1, 3, a, .sv_y, 1)
  expect_equal(c(d$L, d$H), c(max(0, 0 - 0.3), min(1, 1 + 0 - 0.3)))
})

test_that("solve_pair takes the clipped analytic step along the equality constraint", {
  a <- rep(0, 4)
  grad <- rep(-1, 4)
  r <- solve_pair(2, 3, a, .sv_y, .sv_K, grad, 10)
  eta <- .sv_K[2, 2] + .sv_K[3, 3] - 2 * .sv_K[2, 3]
  step <- ((-.sv_y[2] * grad[2]) - (-.sv_y[3] * grad[3])) / eta
  aj <- min(max(0 - .sv_y[3] * step, 0), 10)
  expect_equal(r$alpha[3], aj, tolerance = 1e-12)
  expect_equal(r$alpha[2], -.sv_y[2] * .sv_y[3] * (aj - 0), tolerance = 1e-12)
  expect_equal(r$eta, eta)
  expect_equal(r$step, step, tolerance = 1e-12)
  expect_false(r$clipped)
  # the equality constraint sum(y a) = 0 is preserved by the step
  expect_equal(sum(r$alpha * .sv_y), 0, tolerance = 1e-12)
  # a tight box clips the step
  cl <- solve_pair(2, 3, a, .sv_y, .sv_K, grad, 0.1)
  expect_true(cl$clipped)
  expect_equal(cl$alpha[3], 0.1)
  # no room at all
  none <- solve_pair(1, 2, c(0, 0, 0, 0), .sv_y, .sv_K, grad, 1e-13)
  expect_true(none$clipped)
  expect_equal(none$moved, 0)
  expect_error(solve_pair(2, 2, a, .sv_y, .sv_K, grad, 1), "two DIFFERENT indices")
})

test_that("kkt_violation picks the maximal violating pair and closes at the optimum", {
  a <- rep(0, 4)
  grad <- rep(-1, 4)
  v <- kkt_violation(a, .sv_y, grad, 1)
  # at alpha = 0 every index is in I_up or I_low by its label
  expect_equal(v$gap, 2)
  expect_true(v$i %in% 3:4)
  expect_true(v$j %in% 1:2)
  # the optimum: gradient y_i g_i equal across free indices, gap 0
  ao <- c(0, 0.5, 0.5, 0)
  go <- as.numeric(.sv_K %*% (ao * .sv_y)) * .sv_y - 1
  expect_lt(abs(kkt_violation(ao, .sv_y, go, 10)$gap), 1e-12)
  # every alpha at the bound C with positive labels leaves no pair
  none <- kkt_violation(c(1, 1, 1, 1), c(1, 1, 1, 1), rep(-1, 4), 1)
  expect_equal(none$gap, 0)
  expect_null(none$i)
})

test_that("recover_bias averages the free support vectors, else brackets", {
  a <- c(0, 0.5, 0.5, 0)
  grad <- as.numeric(.sv_K %*% (a * .sv_y)) * .sv_y - 1
  r <- recover_bias(a, .sv_y, grad, 10)
  vals <- -.sv_y[2:3] * grad[2:3]
  expect_equal(r$b, mean(vals), tolerance = 1e-12)
  expect_equal(r$n_free, 2L)
  expect_false(r$bracketed)
  expect_equal(r$spread, diff(range(vals)), tolerance = 1e-12)
  br <- recover_bias(rep(0, 4), .sv_y, rep(-1, 4), 1)
  expect_true(br$bracketed)
  expect_equal(br$n_free, 0L)
  # with no free support vector b sits midway between the bracketing
  # values -y g of the maximal violating pair (1 and -1 here)
  v0 <- kkt_violation(rep(0, 4), .sv_y, rep(-1, 4), 1)
  expect_equal(br$b, 0.5 * ((-.sv_y[v0$j] * -1) + (-.sv_y[v0$i] * -1)), tolerance = 1e-12)
})

test_that("smo reaches the hard-margin solution and its aliases agree", {
  for (fn in list(smo, svm_dual_qp, svm_dual, svmdual, morie_svmopt$smo)) {
    r <- fn(.sv_y, .sv_K, C = 10, tol = 1e-10)
    expect_true(r$converged)
    expect_equal(r$alpha, c(0, 0.5, 0.5, 0), tolerance = 1e-8)
    expect_equal(r$b, 0, tolerance = 1e-8)
    expect_equal(r$equality_residual, 0, tolerance = 1e-12)
    expect_identical(r$support_vectors, 2:3)
    expect_equal(r$objective, dual_objective(r$alpha, .sv_y, .sv_K), tolerance = 1e-12)
    # the dual optimum of this problem is 1/2
    expect_equal(r$objective, 0.5, tolerance = 1e-8)
    expect_lte(r$gap, 1e-10)
    expect_equal(r$n_free, 2L)
  }
  # a small C caps every multiplier
  cap <- smo(.sv_y, .sv_K, C = 0.1, tol = 1e-10)
  expect_true(all(cap$alpha <= 0.1 + 1e-12))
  expect_equal(sum(cap$alpha * .sv_y), 0, tolerance = 1e-12)
  # an RBF kernel on non-separable labels still converges
  rb <- smo(c(1, -1, 1, -1), kernel_matrix(.sv_x, "rbf", gamma = 0.5), C = 1, tol = 1e-9)
  expect_true(rb$converged)
  expect_equal(sum(rb$alpha * c(1, -1, 1, -1)), 0, tolerance = 1e-9)
  expect_error(smo(c(0, 1, 1, -1), .sv_K), "labels must be -1 or \\+1")
  expect_error(smo(.sv_y, .sv_K[1:3, 1:3]), "kernel matrix is 3x3 for 4 labels")
  expect_error(smo(.sv_y, .sv_K, C = 0), "C must be positive")
  expect_match(morie_svmopt$cheatsheet(), "MAXIMAL KKT VIOLATION", fixed = TRUE)
  expect_equal(morie_svmopt$kernel_matrix(.sv_x), .sv_K)
  expect_equal(morie_svmopt$dual_objective(c(0, 0.5, 0.5, 0), .sv_y, .sv_K), 0.5, tolerance = 1e-12)
})
