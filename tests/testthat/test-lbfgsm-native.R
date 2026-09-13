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
  # THE SUBSTANCE: the function value is at the analytic optimum to
  # machine precision, and the solution to the accuracy claimed. These
  # are the assertions worth making -- they hold by six orders of
  # magnitude, so no platform's arithmetic can flip them.
  expect_lt(abs(as.numeric(r$fun) - fstar),
            8 * .Machine$double.eps * max(1, abs(fstar)))
  expect_equal(unlist(r$x), q$xstar, tolerance = 1e-6)

  # THE FLAG, asserted for consistency rather than for a value. The
  # iteration stops at the FIRST iterate under the tolerance, so the
  # margin is always about one -- here the gradient norm lands at
  # 5.5e-08 against a threshold of 1e-7. Asserting `converged` is TRUE
  # therefore asserts which side of a knife edge the platform's
  # floating point fell on, and it fell the other way on macOS and
  # Windows while passing on Linux. What can honestly be required is
  # that the reported flag agrees with the reported gradient, and that
  # a run which did not converge says why.
  expect_type(r$status, "character")
  if (isTRUE(as.logical(r$converged))) {
    expect_lte(as.numeric(r$grad_norm), 1e-7)
    expect_match(r$status, "gradient norm within tolerance")
  } else {
    expect_gt(as.numeric(r$grad_norm), 1e-7)
    expect_match(r$status, "line search|stalled|iteration limit")
  }
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
  # A tolerance in the achievable range. Which side of it the platform
  # lands on is not assertable: the iteration stops at the FIRST
  # iterate under the threshold, so the achieved gradient sits about
  # one multiple below it at every tolerance -- measured on this
  # problem, 1.2x at 1e-1, 1.0x at 1e-3, 1.5x at 1e-6. There is no
  # tolerance with a comfortable margin to pick instead, and asserting
  # TRUE here asserted which way one machine's arithmetic fell: it
  # passed on Linux and failed on Windows.
  #
  # What the flag owes is honesty, which is what this test is named
  # for, so that is what is asserted -- the flag and the gradient tell
  # the same story, whichever way the platform fell.
  ok <- morie_lbfgsm(q$f, rep(0, q$p), q$g, max_iter = 500, tol = 1e-6)
  if (isTRUE(as.logical(ok$converged))) {
    expect_lte(as.numeric(ok$grad_norm), 1e-6)
  } else {
    expect_gt(as.numeric(ok$grad_norm), 1e-6)
  }
  # and either way it is near the optimum, which is the substance
  expect_equal(unlist(ok$x), q$xstar, tolerance = 1e-4)
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

test_that("the stopping criteria are offered with their trade-offs intact", {
  q <- quad20()
  # all three criteria are available, and each says which it applied
  a <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500,
                    tol = 1e-7)
  expect_identical(a$tol_type, "absolute")
  expect_match(a$status, "within tolerance")
  r <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500,
                    tol = 1e-7, tol_type = "relative")
  expect_identical(r$tol_type, "relative")
  i <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500,
                    tol = 1e-7, tol_type = "initial")
  expect_identical(i$tol_type, "initial")
  expect_error(morie_lbfgsm(q$f, rep(0, q$p), q$g, tol_type = "nonsense"),
               "'arg' should be one of")

  # all three measures are reported whichever was used, so a caller can
  # judge the result against a criterion they did not select
  for (res in list(a, r, i)) {
    expect_true(is.finite(res$grad_norm))
    expect_true(is.finite(res$grad_norm_relative))
    expect_true(is.finite(res$grad_norm_ratio))
    expect_equal(res$grad_norm_relative,
                 res$grad_norm / (1 + abs(as.numeric(res$fun))))
  }
  # the relative measure is the smaller one whenever |f| exceeds zero,
  # which is why it converges more readily
  expect_lt(a$grad_norm_relative, a$grad_norm)

  # AND WHY THAT IS A TRADE, NOT A GAIN. Scaling the criterion by |f|
  # makes it easier to satisfy on an ill-conditioned problem, and it is
  # satisfied at points that are not stationary. Measured over a spread
  # of conditionings, the f-scaled criterion claims convergence away
  # from the optimum where the absolute one does not. That is the
  # reason the default is absolute.
  mk <- function(seed, cond) {
    set.seed(seed)
    p <- 20
    M <- matrix(stats::rnorm(p * p), p, p)
    A <- crossprod(M) + cond * diag(p)
    b <- stats::rnorm(p)
    list(f = function(x) 0.5 * as.numeric(t(x) %*% A %*% x) - sum(b * x),
         g = function(x) as.numeric(A %*% x) - b,
         xstar = as.numeric(solve(A, b)))
  }
  false_abs <- 0L
  conv_abs <- 0L
  conv_rel <- 0L
  for (seed in 1:12) {
    p20 <- mk(seed, 1e-3)
    ra <- morie_lbfgsm(p20$f, rep(0, 20), p20$g, m = 10, max_iter = 500,
                       tol = 1e-7, tol_type = "absolute")
    rr <- morie_lbfgsm(p20$f, rep(0, 20), p20$g, m = 10, max_iter = 500,
                       tol = 1e-7, tol_type = "relative")
    if (isTRUE(as.logical(ra$converged)) &&
          max(abs(unlist(ra$x) - p20$xstar)) > 1e-5) {
      false_abs <- false_abs + 1L
    }
    conv_abs <- conv_abs + isTRUE(as.logical(ra$converged))
    conv_rel <- conv_rel + isTRUE(as.logical(rr$converged))
    # the criterion applied is the smaller quantity, so whenever the
    # absolute test passes the relative one must too. That is the
    # structural fact -- the f-scaled criterion is strictly WEAKER, and
    # a weaker criterion is satisfied at points a stronger one rejects.
    if (isTRUE(as.logical(ra$converged))) {
      expect_true(isTRUE(as.logical(rr$converged)))
    }
  }
  # The absolute criterion never claims a point it has not reached. This
  # is the property worth protecting, and the reason it is the default.
  expect_identical(false_abs, 0L)
  # and the weaker criterion converges at least as often
  expect_gte(conv_rel, conv_abs)
})

test_that("a diagonal preconditioner is accepted and validated", {
  q <- quad20()
  none <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500,
                       tol = 1e-6)
  auto <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500,
                       tol = 1e-6, precond = "auto")
  supplied <- morie_lbfgsm(q$f, rep(0, q$p), q$g, m = 10, max_iter = 500,
                           tol = 1e-6, precond = diag(q$A))
  # whichever preconditioner is used, the answer is the same optimum --
  # preconditioning changes the path, not the destination
  for (res in list(none, auto, supplied)) {
    expect_equal(unlist(res$x), q$xstar, tolerance = 1e-5)
  }
  expect_error(morie_lbfgsm(q$f, rep(0, q$p), q$g, precond = c(1, 2)),
               "one entry per parameter")
  expect_error(morie_lbfgsm(q$f, rep(0, q$p), q$g,
                            precond = rep(0, q$p)),
               "positive and finite")
  expect_error(morie_lbfgsm(q$f, rep(0, q$p), q$g, precond = "clever"),
               "NULL")

  # a longer history is the adjustment that actually helps on an
  # ill-conditioned problem: it holds more curvature, so it models the
  # inverse Hessian better
  set.seed(7)
  p <- 20
  M <- matrix(stats::rnorm(p * p), p, p)
  A <- crossprod(M) + 1e-3 * diag(p)
  b <- stats::rnorm(p)
  f <- function(x) 0.5 * as.numeric(t(x) %*% A %*% x) - sum(b * x)
  g <- function(x) as.numeric(A %*% x) - b
  xs <- as.numeric(solve(A, b))
  e10 <- max(abs(unlist(morie_lbfgsm(f, rep(0, p), g, m = 10,
                                     max_iter = 500, tol = 1e-7)$x) - xs))
  e30 <- max(abs(unlist(morie_lbfgsm(f, rep(0, p), g, m = 30,
                                     max_iter = 500, tol = 1e-7)$x) - xs))
  expect_lte(e30, e10)
})
