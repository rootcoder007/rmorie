# Coverage for the logarithmic barrier method (Frisch 1956; Boyd &
# Vandenberghe ch. 11). The barrier, its gradient and Hessian, the
# central-path dual and the centering count are recomputed from their
# formulas, a one-dimensional central point against the root of its
# optimality condition, and the LP path against linprog::solveLP.

.lp_cons <- function() {
  list(list(f = function(z) z[1] + 2 * z[2] - 4, grad = function(z) c(1, 2), affine = TRUE),
       list(f = function(z) 3 * z[1] + z[2] - 6, grad = function(z) c(3, 1), affine = TRUE),
       list(f = function(z) -z[1], grad = function(z) c(-1, 0), affine = TRUE),
       list(f = function(z) -z[2], grad = function(z) c(0, -1), affine = TRUE))
}

test_that("barrier, Frisch potential, gradient, Hessian and dual follow Boyd ch. 11", {
  f <- c(-1, -0.5, -2)
  expect_equal(log_barrier(f), -sum(log(-f)), tolerance = 1e-12)
  expect_identical(log_barrier(c(-1, 0)), Inf)
  expect_equal(frisch_potential(c(1, 2, 0.5)), log(1), tolerance = 1e-12)
  expect_identical(frisch_potential(c(1, -1)), -Inf)
  J <- rbind(c(1, 2), c(-1, 0.5), c(0, 3))
  expect_equal(log_barrier_gradient(f, J), colSums(J / (-f)), tolerance = 1e-12)
  expect_equal(log_barrier_gradient(numeric(0), matrix(0, 0, 2)), c(0, 0))
  H <- list(diag(2), NULL, matrix(c(2, 1, 1, 2), 2))
  ref <- Reduce(`+`, lapply(1:3, function(i) tcrossprod(J[i, ]) / f[i]^2)) + diag(2) / 1 + matrix(c(2, 1, 1, 2), 2) / 2
  expect_equal(log_barrier_hessian(f, J, H), ref, tolerance = 1e-12)
  expect_equal(log_barrier_hessian(f, J), Reduce(`+`, lapply(1:3, function(i) tcrossprod(J[i, ]) / f[i]^2)), tolerance = 1e-12)
  expect_equal(central_path_dual(f, 4), -1 / (4 * f), tolerance = 1e-12)
  expect_error(central_path_dual(f, 0), "t must be positive")
  expect_identical(centering_steps(4, 1e-8, 1, 10), as.integer(ceiling(log(4 / 1e-8) / log(10))))
  expect_identical(centering_steps(1, 1, 10, 2), 0L)
  expect_error(centering_steps(4, 1e-8, 1, 1), "mu must exceed 1")
})

test_that("Fun wrappers use supplied derivatives or central differences", {
  q <- function(x) x[1]^2 + 3 * x[1] * x[2] + exp(x[2])
  fn <- .Fun(q)
  x <- c(0.4, -0.3)
  expect_equal(val.Fun(fn, x), q(x))
  # central differences carry O(h^2) truncation at h = 1e-6 (gradient)
  # and 1e-4 (Hessian)
  expect_equal(grad.Fun(fn, x), c(2 * x[1] + 3 * x[2], 3 * x[1] + exp(x[2])), tolerance = 1e-8)
  expect_equal(hess.Fun(fn, x), matrix(c(2, 3, 3, exp(x[2])), 2), tolerance = 1e-6)
  fs <- .Fun(q, grad = function(x) c(1, 2), hess = function(x) diag(c(5, 6)))
  expect_equal(grad.Fun(fs, x), c(1, 2))
  expect_equal(hess.Fun(fs, x), diag(c(5, 6)))
  expect_equal(hess.Fun(.Fun(function(x) sum(x), affine = TRUE), x), matrix(0, 2, 2))
})

test_that("a one-dimensional central point solves t - 1/x + 1/(1 - x) = 0", {
  for (tt in c(0.5, 3, 40)) {
    cp <- central_point(list(f = function(z) z, grad = function(z) 1, affine = TRUE),
                        list(list(f = function(z) -z, grad = function(z) -1, affine = TRUE),
                             list(f = function(z) z - 1, grad = function(z) 1, affine = TRUE)),
                        0.5, tt)
    xs <- ((tt + 2) - sqrt((tt + 2)^2 - 4 * tt)) / (2 * tt)
    x <- cp$x
    g <- tt - 1 / x + 1 / (1 - x)
    h <- 1 / x^2 + 1 / (1 - x)^2
    # Newton stops once lambda^2 / 2 = g^2 / (2 h) <= tol; the returned
    # decrement is that quantity at the returned point
    expect_equal(cp$decrement, g^2 / h, tolerance = 1e-9)
    expect_lte(cp$decrement / 2, 1e-10)
    expect_lte(abs(x - xs), 2 * sqrt(cp$decrement / h))
  }
  expect_error(central_point(function(z) z, list(function(z) z - 1), 2, 1), "not strictly feasible")
})

test_that("the barrier path reaches the LP optimum certified by m/t", {
  skip_if_not_installed("linprog")
  ref <- linprog::solveLP(c(-1, -1), c(4, 6), rbind(c(1, 2), c(3, 1)), maximum = FALSE)
  obj <- list(f = function(z) -z[1] - z[2], grad = function(z) c(-1, -1), affine = TRUE)
  r <- barrier_method(obj, .lp_cons(), c(0.5, 0.5))
  expect_true(r$converged)
  expect_lt(r$gap, 1e-8)
  # the primal gap is bounded by the certified duality gap m/t
  expect_gte(r$fun - ref$opt, -1e-12)
  expect_lte(r$fun - ref$opt, r$gap + 1e-12)
  # the vertex is reached to within the certified gap
  expect_equal(r$x, unname(ref$solution), tolerance = 1e-7)
  A <- rbind(c(1, 2), c(3, 1), c(-1, 0), c(0, -1))
  # dual feasibility c + A'lambda = 0 holds to the centering residual:
  # at t = 1e8 the stop lambda^2 / 2 <= 1e-10 in the barrier's own metric
  # leaves a relative residual of a few 1e-5 in the plain one
  expect_equal(as.numeric(crossprod(A, r$lambda_)), c(1, 1), tolerance = 1e-4)
  expect_equal(r$slack, -vapply(.lp_cons(), function(c) c$f(r$x), 1), tolerance = 1e-12)
  expect_identical(r$steps_predicted, centering_steps(4, 1e-8, 1, 10))
  expect_identical(r$outer, length(r$history))
  lp <- barrier_lp(c(-1, -1), rbind(c(1, 2), c(3, 1), c(-1, 0), c(0, -1)), c(4, 6, 0, 0))
  expect_equal(lp$x, unname(ref$solution), tolerance = 1e-7)
  # a single centering at t = m / eps: once Newton has converged the
  # objective is certified to within m / t of the optimum
  nn <- barrier_method(obj, .lp_cons(), c(0.5, 0.5), centering = "none", eps = 1e-2)
  expect_identical(nn$outer, 1L)
  expect_equal(nn$t, 4 / 1e-2)
  expect_lte(nn$decrement / 2, 1e-10)
  expect_lte(nn$fun - ref$opt, 1e-2 + 1e-12)
  # gradient centering (Frisch's own) is not run to convergence here; it
  # must still stay strictly feasible and improve on the start
  gd <- barrier_method(obj, .lp_cons(), c(0.5, 0.5), centering = "gradient", eps = 1e-2, max_inner = 50L)
  expect_true(all(gd$slack > 0))
  expect_lt(gd$fun, -1)
  expect_identical(morie_barerp(obj, .lp_cons(), c(0.5, 0.5))$x, r$x)
})

test_that("equality constraints ride along in the KKT system", {
  f0 <- list(f = function(z) sum(z^2), grad = function(z) 2 * z, hess = function(z) diag(2, 2))
  cons <- list(function(z) z[1] - 0.8, function(z) z[2] - 0.8)
  r <- barrier_method(f0, cons, c(0.3, 0.7), aeq = matrix(c(1, 1), 1), beq = 1)
  expect_equal(r$x, c(0.5, 0.5), tolerance = 1e-8)
  expect_error(barrier_method(f0, cons, c(0.3, 0.6), aeq = matrix(c(1, 1), 1), beq = 1), "violates equality row 0")
})

test_that("phase I finds a strictly feasible point or reports infeasibility", {
  ph <- phase1(.lp_cons(), c(5, 5))
  expect_true(ph$feasible)
  expect_true(all(vapply(.lp_cons(), function(c) c$f(ph$x), 1) < 0))
  bad <- list(function(z) z[1] + 1, function(z) 1 - z[1])
  pb <- phase1(bad, 0)
  expect_false(pb$feasible)
  expect_gte(pb$s, 1 - 1e-6)
  expect_error(barrier_method(function(z) z, list(), 0), "no inequality constraints")
  expect_error(barrier_method(function(z) z, .lp_cons()[1], c(5, 5)), "not strictly feasible")
  expect_error(barrier_method(function(z) z, .lp_cons()[1], c(0, 0), mu = 1), "mu must exceed 1")
  expect_error(barrier_method(function(z) z, .lp_cons()[1], c(0, 0), centering = "bfgs"), "centering must be one of")
  expect_error(barrier_lp(c(1, 1), rbind(c(1, 1)), c(1, 2)), "A_ub has 1 rows but b_ub has 2")
  one <- barrier_lp(c(-1), rbind(1), 2)
  expect_equal(one$x, 2, tolerance = 1e-7)
})
