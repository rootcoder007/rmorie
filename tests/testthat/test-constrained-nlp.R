hs071 <- function() {
  list(f = function(x) x[1] * x[4] * (x[1] + x[2] + x[3]) + x[3],
       eq = list(function(x) x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40),
       ineq = c(list(function(x) x[1] * x[2] * x[3] * x[4] - 25), lapply(1:4, function(i) function(x) x[i] - 1),
                lapply(1:4, function(i) function(x) 5 - x[i])))
}

test_that("the Goldfarb-Idnani QP matches quadprog and satisfies KKT", {
  u <- .morie_random_uniform(3000, seed = 4, stream = 0)
  k <- 0
  for (t in 1:15) {
    n <- 3 + t %% 3
    m <- 3 + t %% 2
    L <- matrix(u[k + seq_len(n * n)] - 0.5, n)
    k <- k + n * n
    G <- crossprod(L) + diag(n)
    a <- (u[k + seq_len(n)] - 0.5) * 4
    k <- k + n
    C <- matrix(u[k + seq_len(m * n)] - 0.5, m)
    k <- k + m * n
    b <- (u[k + seq_len(m)] - 0.5)
    k <- k + m
    r <- .gi_qp(G, a, C, b, meq = 1)
    s <- as.vector(C %*% r$x) - b
    expect_lt(abs(s[1]), 1e-9)
    expect_true(all(s[-1] > -1e-9) && all(r$u[-1] >= -1e-12) && all(abs(r$u * s) < 1e-9))
    expect_lt(max(abs(G %*% r$x + a - t(C) %*% r$u)), 1e-9)
  }
})

test_that("the simplex matches lpSolve", {
  r <- .lp_simplex(c(-1, -2, 0, 0, 0), rbind(c(1, 1, 1, 0, 0), c(1, 0, 0, 1, 0), c(0, 1, 0, 0, 1)), c(4, 3, 3))
  expect_equal(r$x[1:2], c(1, 3), tolerance = 1e-12)
  expect_identical(.lp_simplex(c(-1, 0), matrix(c(1, -1), 1), 0)$status, "unbounded")
  r <- .lp_simplex(c(-1, -2, 0, 0, 1, 0), rbind(c(1, 0, 1, 0, 0, 0), c(0, 1, 0, 1, 0, 0), c(-2, -2, 0, 0, 1, -1)), c(1, 1, -4))
  expect_equal(r$x, c(1, 1, 0, 0, 0, 0), tolerance = 1e-12)
})

test_that("SQP and SLP solve Hock-Schittkowski problems like the Python arm", {
  h <- hs071()
  r <- SequentialQuadraticProgramming(h$f, c(1, 5, 5, 1), eq = h$eq, ineq = h$ineq)
  expect_true(r$converged)
  expect_equal(r$fun, 17.0140173, tolerance = 1e-7)
  expect_equal(r$x, c(1, 4.7429994, 3.8211503, 1.3794082), tolerance = 1e-6)
  r <- SequentialLinearProgramming(function(x) sum((x - 5)^2), c(0, 0), ineq = list(function(x) 1 - x[1], function(x) 2 - x[2]))
  expect_equal(r$x, c(1, 2), tolerance = 1e-9)
  expect_identical(r$n_iter, 9)
  r <- SequentialLinearProgramming(function(x) -x[1] - 2 * x[2], c(0, 0), ineq = list(function(x) 4 - x[1]^2 - x[2]^2))
  expect_equal(r$fun, -sqrt(20), tolerance = 1e-8)
  expect_identical(r$n_iter, 34)
  r <- SequentialLinearProgramming(h$f, c(1, 5, 5, 1), eq = h$eq, ineq = h$ineq)
  expect_equal(r$fun, 17.0140173, tolerance = 1e-7)
})

test_that("SLP reports an unbounded l1 penalty instead of claiming convergence", {
  ineq <- list(function(z) 4 - z[1] - z[2], function(z) 3 - z[1], function(z) z[1], function(z) z[2])
  f <- function(z) -z[1] - 2 * z[2]
  ok <- SequentialLinearProgramming(f, c(1, 1), ineq = ineq, mu = 10)
  expect_true(ok$converged)
  expect_equal(ok$x, c(0, 4), tolerance = 1e-7)
  bad <- SequentialLinearProgramming(f, c(1, 1), ineq = ineq, mu = 1)
  expect_gt(bad$violation, 1)
  expect_false(bad$converged)
})
