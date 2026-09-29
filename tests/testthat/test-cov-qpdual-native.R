# Frank-Wolfe for quadratic programs (Frank & Wolfe 1956): the optimum
# over the simplex found by enumerating supports, the duality-gap bound,
# the box domain against coordinate KKT, and both step rules. FW is
# O(1/k) when the optimum sits on a face, so iterates are compared at
# 1e-3; the objective is held to the certified duality gap instead.

qd_Q <- matrix(c(4, 1, 0.5, 1, 3, 0.2, 0.5, 0.2, 2), 3)
qd_c <- c(-1, 0.5, -0.8)
qd_f <- function(x) 0.5 * sum(x * (qd_Q %*% x)) + sum(qd_c * x)
qd_simplex_opt <- function(Q, cc) {
  n <- length(cc)
  best <- Inf
  xb <- NULL
  for (k in 1:n) for (S in utils::combn(n, k, simplify = FALSE)) {
    K <- rbind(cbind(Q[S, S, drop = FALSE], 1), c(rep(1, length(S)), 0))
    sol <- solve(K, c(-cc[S], 1))
    x <- numeric(n)
    x[S] <- sol[seq_along(S)]
    if (all(x >= -1e-12)) {
      v <- 0.5 * sum(x * (Q %*% x)) + sum(cc * x)
      if (v < best) {
        best <- v
        xb <- x
      }
    }
  }
  list(x = xb, f = best)
}

test_that("Frank-Wolfe on the simplex reaches the enumerated optimum within its gap", {
  ref <- qd_simplex_opt(qd_Q, qd_c)
  r <- morie_qpdual(qd_Q, qd_c, max_iter = 4000, tol = 1e-10)
  expect_lt(max(abs(r$x - ref$x)), 1e-3)
  expect_gte(r$fun - ref$f, -1e-12)
  expect_lte(r$fun - ref$f, r$gap + 1e-12)
  expect_equal(sum(r$x), 1, tolerance = 1e-12)
  expect_true(all(diff(r$history) <= 1e-15))
  op <- frank_wolfe_qp(qd_Q, qd_c, step = "standard", max_iter = 4000, tol = 1e-8)
  expect_equal(op$fun, ref$f, tolerance = 1e-4)
  expect_identical(qpdual, morie_qpdual)
})

test_that("the box domain satisfies the coordinate KKT conditions", {
  lo <- c(-0.2, 0, -1)
  hi <- c(0.1, 1, 1)
  r <- quadratic_program(qd_Q, qd_c, domain = "box", lower = lo, upper = hi,
                         max_iter = 8000, tol = 1e-12)
  g <- as.numeric(qd_Q %*% r$x) + qd_c
  # KKT: a positive gradient pins the lower bound, a negative one the
  # upper; a free coordinate has zero gradient
  for (i in 1:3) {
    if (g[i] > 1e-3) expect_lt(r$x[i] - lo[i], 1e-3)
    else if (g[i] < -1e-3) expect_lt(hi[i] - r$x[i], 1e-3)
    else expect_true(r$x[i] > lo[i] - 1e-12 && r$x[i] < hi[i] + 1e-12)
  }
  expect_lte(r$gap, 1e-3)
  expect_true(all(r$x >= lo - 1e-12 & r$x <= hi + 1e-12))
})

test_that("inputs are validated", {
  expect_error(morie_qpdual(matrix(1, 2, 3), 1:2), "square")
  expect_error(morie_qpdual(qd_Q, 1:2), "c has length 2")
  expect_error(morie_qpdual(qd_Q, qd_c, domain = "ball"), "domain must be one of")
  expect_error(morie_qpdual(qd_Q, qd_c, step = "armijo"), "step must be one of")
  expect_error(morie_qpdual(qd_Q, qd_c, domain = "box"), "needs lower and upper")
  expect_error(morie_qpdual(qd_Q, qd_c, domain = "box", lower = c(1, 0, 0), upper = c(0, 1, 1)),
               "lower\\[0\\] exceeds")
  expect_match(qpdual_cheatsheet(), "Frank-Wolfe", fixed = TRUE)
})
