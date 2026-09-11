# Anchors for limited-memory BFGS (Liu & Nocedal 1989).
#
# An optimiser can be held to problems whose answer is known: a positive
# definite quadratic is minimised at A^-1 b, Rosenbrock's minimum is at
# (1, 1), and base R's own optim(method = "L-BFGS-B") is an independent
# implementation to compare against. The file sat at 1.9% coverage with
# no test naming either of its functions.
#
# On the gradient tolerance: for a quadratic with f of order one, a
# gradient norm of 1e-8 already implies a function-value gap of about
# 1e-15, so tolerances below that are unreachable in double precision and
# the routine reports converged = FALSE rather than pretending. The tests
# below therefore ask for tolerances it can actually meet, and separately
# assert that it is honest about the ones it cannot.

test_that("the dot product is the inner product", {
  expect_equal(.lbfgsm_dot(c(1, 2, 3), c(4, 5, 6)), 32, tolerance = 1e-12)
  expect_equal(.lbfgsm_dot(c(1, 0), c(0, 1)), 0, tolerance = 1e-12)
  expect_equal(.lbfgsm_dot(numeric(0), numeric(0)), 0, tolerance = 1e-12)
})

test_that("a two-dimensional quadratic is minimised exactly", {
  A <- matrix(c(4, 1, 1, 3), 2, 2)
  b <- c(1, 2)
  f <- function(x) 0.5 * as.numeric(t(x) %*% A %*% x) - sum(b * x)
  g <- function(x) as.numeric(A %*% x) - b
  xstar <- as.numeric(solve(A, b))
  r <- morie_lbfgsm(f, c(0, 0), g)
  expect_equal(unlist(r$x), xstar, tolerance = 1e-8)
  expect_equal(as.numeric(r$fun), f(xstar), tolerance = 1e-12)
  # the gradient is driven to machine precision on a problem this small
  expect_lt(as.numeric(r$grad_norm), 1e-12)
  expect_true(as.logical(r$converged))
  # the reported gradient is the gradient at the reported point
  expect_equal(unlist(r$grad), g(unlist(r$x)), tolerance = 1e-12)
  expect_identical(r$estimate, r$x)
})

test_that("Rosenbrock is solved, and agrees with optim's L-BFGS-B", {
  ros <- function(x) (1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2
  dros <- function(x) c(-2 * (1 - x[1]) - 400 * x[1] * (x[2] - x[1]^2),
                        200 * (x[2] - x[1]^2))
  r <- morie_lbfgsm(ros, c(-1.2, 1), dros, max_iter = 2000, tol = 1e-10)
  expect_equal(unlist(r$x), c(1, 1), tolerance = 1e-5)
  expect_lt(as.numeric(r$fun), 1e-16)
  ref <- optim(c(-1.2, 1), ros, dros, method = "L-BFGS-B",
               control = list(maxit = 2000))
  # the two implementations land on the same point
  expect_equal(unlist(r$x), ref$par, tolerance = 1e-4)
})

quad20 <- function() {
  set.seed(1); p <- 20
  M <- matrix(rnorm(p * p), p, p)
  A <- crossprod(M) + diag(p)
  b <- rnorm(p)
  list(p = p, A = A, b = b,
       f = function(x) 0.5 * as.numeric(t(x) %*% A %*% x) - sum(b * x),
       g = function(x) as.numeric(A %*% x) - b,
       xstar = as.numeric(solve(A, b)))
}

test_that("a twenty-dimensional quadratic reaches the analytic optimum", {
  q <- quad20()
  r <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500, tol = 1e-7)
  fstar <- q$f(q$xstar)
  # the function value is at the analytic optimum to machine precision
  expect_lt(abs(as.numeric(r$fun) - fstar),
            8 * .Machine$double.eps * max(1, abs(fstar)))
  expect_equal(unlist(r$x), q$xstar, tolerance = 1e-6)
  expect_true(as.logical(r$converged))
})

test_that("more memory reaches the optimum in fewer iterations", {
  q <- quad20()
  its <- vapply(c(1, 3, 10, 30), function(m) {
    r <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = m, max_iter = 500, tol = 1e-7)
    as.numeric(r$iterations)
  }, numeric(1))
  # the point of the limited-memory scheme: a longer history is a better
  # inverse-Hessian model
  expect_lt(its[4], its[1])
  expect_true(all(diff(its) <= 0))
  # and the memory actually used is reported
  expect_identical(as.integer(morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 7)$memory),
                   7L)
})

test_that("the convergence flag is honest about what it reached", {
  q <- quad20()
  # a tolerance it can meet
  ok <- morie_lbfgsm(q$f, rep(0, q$p), q$g, max_iter = 500, tol = 1e-6)
  expect_true(as.logical(ok$converged))
  expect_lte(as.numeric(ok$grad_norm), 1e-6)
  # one it cannot, because the function value is already at machine
  # precision: it says so rather than claiming success
  no <- morie_lbfgsm(q$f, rep(0, q$p), q$g, max_iter = 500, tol = 1e-12)
  expect_false(as.logical(no$converged))
  expect_gt(as.numeric(no$grad_norm), 1e-12)
  # but it still returns the best point it found
  fstar <- q$f(q$xstar)
  expect_lt(abs(as.numeric(no$fun) - fstar),
            8 * .Machine$double.eps * max(1, abs(fstar)))
})

test_that("the objective never increases along the history", {
  q <- quad20()
  r <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 200, tol = 1e-7)
  h <- as.numeric(unlist(r$history))
  expect_gt(length(h), 1L)
  # a Wolfe line search guarantees descent at every accepted step
  expect_true(all(diff(h) <= 1e-12))
  expect_equal(h[1], q$f(rep(0, q$p)), tolerance = 1e-12)
  expect_equal(h[length(h)], as.numeric(r$fun), tolerance = 1e-12)
})

test_that("starting at the optimum stops immediately", {
  q <- quad20()
  r <- morie_lbfgsm(q$f, q$xstar, q$g, tol = 1e-6)
  expect_true(as.logical(r$converged))
  expect_identical(as.integer(r$iterations), 1L)
  expect_equal(unlist(r$x), q$xstar, tolerance = 1e-12)
})

test_that("the iteration cap is respected", {
  q <- quad20()
  r <- morie_lbfgsm(q$f, rep(0, q$p), q$g, max_iter = 3, tol = 1e-14)
  expect_lte(as.integer(r$iterations), 3L)
  expect_false(as.logical(r$converged))
  # and it counts the function evaluations it spent
  expect_gt(as.integer(r$n_fun), 1L)
})

test_that("the arguments are validated", {
  f <- function(x) sum(x^2)
  g <- function(x) 2 * x
  expect_error(morie_lbfgsm(f, c(1, 2), g, m = 0), "m must be at least 1")
  expect_error(morie_lbfgsm(f, numeric(0), g), "must be non-empty")
})

test_that("a one-dimensional problem works", {
  f <- function(x) (x - 3)^2
  g <- function(x) 2 * (x - 3)
  r <- morie_lbfgsm(f, 0, g, tol = 1e-10)
  expect_equal(unlist(r$x), 3, tolerance = 1e-7)
  expect_lt(as.numeric(r$fun), 1e-14)
  expect_true(as.logical(r$converged))
})
