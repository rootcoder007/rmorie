# Primal-dual interior-point linear programming, standard form
#   min c'x  subject to  Ax = b,  x >= 0.
#
# Anchors outside the module: a linear program small enough to solve by
# hand, and the optimality certificate every correct solver must produce,
# namely primal feasibility, dual feasibility, and a duality gap that
# closes. None of these is read back from the solver's own claim.

# min x1 + 3 x2 + 5 x3  s.t.  x1 + x2 = 2,  x2 + x3 = 3,  x >= 0.
# Eliminating with x2 = t gives an objective 17 - 3t on t in [0, 2], so the
# unique optimum is t = 2:
#   x* = (0, 2, 1), value 11
# The dual is max 2 y1 + 3 y2 s.t. y1 <= 1, y1 + y2 <= 3, y2 <= 5, whose
# optimum is y* = (-2, 5), also 11, with slacks s* = c - A'y* = (3, 0, 0).
CV <- c(1, 3, 5)
AM <- list(c(1, 1, 0), c(0, 1, 1))
BV <- c(2, 3)

test_that("the solver reaches the hand-computed optimum", {
  r <- morie_linprm(CV, AM, BV, tol = 1e-9, max_iter = 200)
  expect_true(r$converged)
  expect_equal(r$x, c(0, 2, 1), tolerance = 1e-5)
  expect_equal(sum(CV * r$x), 11, tolerance = 1e-6)
  # the dual solution and its slacks are the hand-computed ones
  expect_equal(r$y, c(-2, 5), tolerance = 1e-5)
  expect_equal(r$s, c(3, 0, 0), tolerance = 1e-5)
  # the dual objective meets the primal one
  expect_equal(sum(BV * r$y), 11, tolerance = 1e-6)
})

test_that("the returned point carries a full optimality certificate", {
  r <- morie_linprm(CV, AM, BV, tol = 1e-9, max_iter = 200)
  A <- do.call(rbind, AM)
  # primal feasibility: Ax = b with x nonnegative
  expect_equal(as.numeric(A %*% r$x), BV, tolerance = 1e-6)
  expect_true(all(r$x > -1e-9))
  # dual feasibility: A'y + s = c with s nonnegative
  expect_equal(as.numeric(t(A) %*% r$y) + r$s, CV, tolerance = 1e-6)
  expect_true(all(r$s > -1e-9))
  # complementary slackness, which together with the above proves optimality
  expect_true(all(r$x * r$s < 1e-7))
  expect_equal(r$gap, sum(r$x * r$s))
  expect_true(r$gap < 1e-9)
  # the reported residuals are the ones the certificate uses
  expect_equal(r$primal_residual, sqrt(sum((as.numeric(A %*% r$x) - BV)^2)),
               tolerance = 1e-9)
  expect_equal(r$dual_residual,
               sqrt(sum((as.numeric(t(A) %*% r$y) + r$s - CV)^2)),
               tolerance = 1e-9)
  # strong duality
  expect_equal(sum(CV * r$x), sum(BV * r$y), tolerance = 1e-6)
})

test_that("the barrier parameter decreases monotonically", {
  r <- morie_linprm(CV, AM, BV, tol = 1e-9, max_iter = 200)
  expect_true(length(r$mu_history) >= 2)
  # mu is the average complementarity product and must be driven to zero
  expect_true(all(diff(r$mu_history) < 0))
  expect_true(r$mu_history[1] > 0)
  expect_true(tail(r$mu_history, 1) < 1e-9)
  # it is recorded once per iteration taken
  expect_equal(length(r$mu_history), r$iterations)
})

test_that("an exhausted iteration budget is reported, not disguised", {
  r <- morie_linprm(CV, AM, BV, tol = 1e-14, max_iter = 2)
  expect_false(r$converged)
  expect_equal(r$iterations, 2L)
  # the residuals are not claimed when the certificate was never reached
  expect_true(is.na(r$primal_residual))
  expect_true(is.na(r$dual_residual))
  # the gap is still reported, and is worse than a converged run's
  expect_true(r$gap > 1e-9)
  expect_length(r$mu_history, 2L)
})

test_that("a second program with a different optimum also solves", {
  # min 2 x1 + x2  s.t. x1 + x2 = 4,  x >= 0  has optimum x = (0, 4), 4
  r <- morie_linprm(c(2, 1), list(c(1, 1)), 4, tol = 1e-9)
  expect_true(r$converged)
  expect_equal(r$x, c(0, 4), tolerance = 1e-5)
  expect_equal(sum(c(2, 1) * r$x), 4, tolerance = 1e-6)
  expect_equal(r$y, 1, tolerance = 1e-5)
  # a transportation-style program with two equality rows
  cc <- c(4, 1, 1, 4)
  AA <- list(c(1, 1, 0, 0), c(0, 0, 1, 1))
  bb <- c(1, 1)
  r2 <- morie_linprm(cc, AA, bb, tol = 1e-9)
  expect_true(r2$converged)
  # each row spends its whole budget on its cheaper column
  expect_equal(r2$x, c(0, 1, 1, 0), tolerance = 1e-5)
  expect_equal(sum(cc * r2$x), 2, tolerance = 1e-6)
  # a cost vector of zeros makes every feasible point optimal
  r3 <- morie_linprm(c(0, 0), list(c(1, 1)), 2, tol = 1e-9)
  expect_equal(sum(as.numeric(do.call(rbind, list(c(1, 1))) %*% r3$x)), 2,
               tolerance = 1e-6)
})

test_that("mis-shaped programs are rejected", {
  expect_error(morie_linprm(CV, list(), BV), "linprm: A must be")
  # a row of the wrong width
  expect_error(morie_linprm(CV, list(c(1, 1), c(0, 1)), BV), "A must be")
  # a right-hand side of the wrong length
  expect_error(morie_linprm(CV, AM, c(1, 2, 3)), "A must be")
  expect_error(morie_linprm(CV, AM, numeric(0)), "A must be")
})
