# Coverage for the tail-2 batch t04: the signless Laplacian (zero
# eigenvalue multiplicity = number of bipartite components, checked with
# eigen()), spectral vs combinatorial bipartiteness, DPLL against brute
# force over all assignments, the EM monotonicity driver on a two-
# component Gaussian mixture, and the tableau simplex against vertex
# enumeration.

.cyc <- function(n) {
  A <- matrix(0, n, n)
  for (i in 1:n) A[i, i %% n + 1] <- A[i %% n + 1, i] <- 1
  A
}

test_that("signless Laplacian Q = D + A and its bipartite components", {
  A <- matrix(0, 7, 7)
  A[1:4, 1:4] <- .cyc(4)
  A[5:7, 5:7] <- .cyc(3)
  s <- SignlessL(A)
  expect_equal(s$Q, A + diag(rowSums(A)))
  ev <- eigen(s$Q, symmetric = TRUE)$values
  expect_identical(s$zero_eigenvalue_multiplicity, sum(abs(ev) < 1e-9))
  expect_identical(c(s$n_components, s$bipartite_components), c(2L, 1L))
  expect_equal(c(s$m, s$trace), c(7, 14))
  expect_error(SignlessL(matrix(1, 2, 2)), "zero diagonal")
  expect_error(SignlessL(rbind(c(0, 1), c(0, 0))), "symmetric")
  expect_error(SignlessL(matrix(0, 2, 3)), "square")
})

test_that("bipartiteness: odd closed-walk traces vanish iff 2-colourable", {
  b <- BipartSpec(.cyc(6))
  expect_true(b$bipartite)
  expect_identical(b$part_sizes, c(3L, 3L))
  expect_equal(b$evidence, c(0, 0, 0))
  o <- BipartSpec(.cyc(5))
  expect_false(o$bipartite)
  A <- .cyc(5)
  expect_equal(o$evidence, c(0, sum(diag(A %*% A %*% A)), sum(diag(A %*% A %*% A %*% A %*% A))))
  expect_equal(o$max_odd_trace, 10)
})

.brute <- function(cnf, n) {
  for (k in 0:(2^n - 1)) {
    v <- as.logical(bitwAnd(k, 2^(0:(n - 1))))
    if (all(vapply(cnf, function(cl) any(ifelse(cl > 0, v[abs(cl)], !v[abs(cl)])), TRUE))) return(TRUE)
  }
  FALSE
}

test_that("DPLL agrees with brute force and returns a satisfying model", {
  cnfs <- list(list(c(1, 2), c(-1, 3), c(-2, -3), c(2, 3)),
               list(1, -1),
               list(c(1, 2, 3), c(-1, -2), c(-2, -3), c(-1, -3), c(1, -2), c(2, -3), c(3, -1)),
               list(c(1, -2), c(2, 4), c(-1, -4), c(3)))
  for (cnf in cnfs) {
    r <- Dpll(cnf)
    n <- max(abs(unlist(cnf)))
    expect_identical(r$satisfiable, .brute(cnf, n))
    if (r$satisfiable) {
      v <- unlist(r$model[as.character(1:n)])
      expect_true(all(vapply(cnf, function(cl) any(ifelse(cl > 0, v[abs(cl)], !v[abs(cl)])), TRUE)))
    }
  }
  e <- Dpll(list())
  expect_true(e$satisfiable)
  expect_identical(e$n_vars, 0L)
  expect_error(Dpll(list(c(1, 0))), "0 is not a literal")
})

test_that("the EM driver records a monotone log-likelihood", {
  set.seed(2)
  x <- c(stats::rnorm(40, 0), stats::rnorm(40, 4))
  ll <- function(th) sum(log(th[1] * stats::dnorm(x, th[2]) + (1 - th[1]) * stats::dnorm(x, th[3])))
  Q <- function(th) {
    r <- th[1] * stats::dnorm(x, th[2])
    r <- r / (r + (1 - th[1]) * stats::dnorm(x, th[3]))
    c(mean(r), sum(r * x) / sum(r), sum((1 - r) * x) / sum(1 - r))
  }
  e <- EmAlgo(ll, Q, c(0.3, 1, 2), 25)
  th <- c(0.3, 1, 2)
  tr <- ll(th)
  for (i in 1:25) {
    th <- Q(th)
    tr <- c(tr, ll(th))
  }
  expect_equal(e$theta, th, tolerance = 1e-12)
  expect_equal(e$trace, tr, tolerance = 1e-12)
  expect_true(e$monotone)
  bad <- EmAlgo(function(t) -t^2, function(t) t + 1, 0, 3)
  expect_false(bad$monotone)
  expect_equal(bad$min_increment, -5)
  expect_error(EmAlgo(ll, Q, c(0.3, 1, 2), -1), "non-negative")
  expect_error(EmAlgo(ll, function(t) 1, c(0.3, 1, 2), 1), "changed the parameter length")
})

test_that("simplex reaches the best vertex of {Ax <= b, x >= 0}", {
  cc <- c(3, 5, 4)
  A <- rbind(c(2, 3, 0), c(0, 2, 5), c(3, 2, 4))
  b <- c(8, 10, 15)
  s <- SimplexLP(cc, A, b)
  G <- rbind(A, -diag(3))
  h <- c(b, 0, 0, 0)
  best <- -Inf
  for (idx in utils::combn(6, 3, simplify = FALSE)) {
    M <- G[idx, ]
    if (abs(det(M)) < 1e-12) next
    v <- solve(M, h[idx])
    if (all(G %*% v <= h + 1e-9)) best <- max(best, sum(cc * v))
  }
  expect_identical(s$status, "optimal")
  expect_equal(s$objective, best, tolerance = 1e-10)
  expect_equal(s$slack, as.numeric(b - A %*% s$x), tolerance = 1e-10)
  # strong duality: b'y = c'x with y the reduced costs of the slacks
  expect_equal(sum(b * s$dual), s$objective, tolerance = 1e-10)
  expect_identical(SimplexLP(c(1, 1), rbind(c(1, -1)), 1)$status, "unbounded")
  expect_error(SimplexLP(cc, A, c(8, -1, 15)), "non-negative")
  expect_error(SimplexLP(cc, A[, 1:2], b), "one entry per variable")
  expect_error(SimplexLP(cc, A, b, max_iter = 1), "pivot budget exhausted")
})
