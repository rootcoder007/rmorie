# Branch and bound (Land & Doig 1960; Dakin 1965) over simplex / interior
# LP relaxations. LP optima are recomputed by enumerating the vertices of
# the polytope; integer optima by enumerating the integer box.

mp_vertex_opt <- function(A, b, cc, maximise = TRUE) {
  n <- ncol(A)
  G <- rbind(A, -diag(n))
  h <- c(b, rep(0, n))
  best <- NULL
  for (S in utils::combn(nrow(G), n, simplify = FALSE)) {
    M <- G[S, , drop = FALSE]
    if (abs(det(M)) < 1e-12) next
    x <- solve(M, h[S])
    if (all(G %*% x <= h + 1e-9)) {
      v <- sum(cc * x)
      if (is.null(best) || (if (maximise) v > best$v + 1e-12 else v < best$v - 1e-12))
        best <- list(v = v, x = x)
    }
  }
  best
}
mp_int_opt <- function(A, b, cc, upper, maximise = TRUE) {
  g <- as.matrix(expand.grid(rep(list(0:upper), ncol(A))))
  ok <- apply(g, 1, function(x) all(A %*% x <= b + 1e-9))
  v <- as.numeric(g[ok, , drop = FALSE] %*% cc)
  if (maximise) max(v) else min(v)
}
mp_A <- rbind(c(2, 3), c(4, 1), c(-1, 0))
mp_b <- c(12, 10, -1)
mp_c <- c(3, 4)
mp_rows <- function(A) lapply(seq_len(nrow(A)), function(i) A[i, ])

test_that("the simplex and interior relaxations reach the LP vertex optimum", {
  ref <- mp_vertex_opt(mp_A, mp_b, mp_c)
  for (solver in c("simplex", "interior")) {
    r <- morie_miprgr_solve_relaxation(mp_rows(mp_A), mp_b, mp_c, solver = solver)
    expect_true(r$feasible)
    tol <- if (solver == "simplex") 1e-12 else 1e-7
    expect_equal(r$x, ref$x, tolerance = tol)
    expect_equal(r$value, ref$v, tolerance = tol)
  }
  r2 <- miprgr_solve_relaxation(mp_A, mp_b, mp_c)
  expect_equal(r2$x, ref$x, tolerance = 1e-12)
  s <- miprgr_simplex(mp_A, mp_b, mp_c)
  expect_equal(s$value, ref$v, tolerance = 1e-12)
  # minimisation and branch bounds
  cmin <- c(1, 2)
  refm <- mp_vertex_opt(rbind(mp_A, c(0, -1)), c(mp_b, -1), cmin, maximise = FALSE)
  rm <- morie_miprgr_solve_relaxation(mp_rows(mp_A), mp_b, cmin,
                                      bounds = list(list(var = 2, sense = "ge", value = 1)),
                                      maximise = FALSE)
  expect_equal(rm$value, refm$v, tolerance = 1e-12)
  rm2 <- miprgr_solve_relaxation(mp_A, mp_b, cmin,
                                 bounds = list(list(var = 2, sense = "ge", value = 1)),
                                 maximise = FALSE)
  expect_equal(rm2$value, refm$v, tolerance = 1e-12)
  refle <- mp_vertex_opt(rbind(mp_A, c(1, 0)), c(mp_b, 1), mp_c)
  rle <- morie_miprgr_solve_relaxation(mp_rows(mp_A), mp_b, mp_c,
                                       bounds = list(list(var = 1, sense = "le", value = 1)))
  expect_equal(rle$value, refle$v, tolerance = 1e-12)
  # an empty region
  inf <- morie_miprgr_solve_relaxation(list(c(1, 0), c(-1, 0)), c(1, -2), c(1, 1))
  expect_false(inf$feasible)
  expect_false(miprgr_solve_relaxation(rbind(c(1, 0), c(-1, 0)), c(1, -2), c(1, 1))$feasible)
  expect_false(miprgr_simplex(rbind(c(1, 0), c(-1, 0)), c(1, -2), c(1, 1))$feasible)
  expect_error(morie_miprgr_solve_relaxation(mp_rows(mp_A), mp_b, mp_c, solver = "dual"),
               "simplex or interior")
  expect_error(miprgr_solve_relaxation(mp_A, mp_b, mp_c, solver = "interior"), "only simplex")
})

test_that("fractional_variable picks the most fractional integer variable", {
  x <- c(1.2, 2.5, 3.9, 4.45)
  for (f in list(morie_miprgr_fractional_variable, miprgr_fractional_variable)) {
    r <- f(x, 1:4)
    expect_identical(as.integer(r$index), 2L)
    expect_equal(r$fractionality, 0.5, tolerance = 1e-12)
    expect_false(r$integral)
    r3 <- f(x, c(1, 3))
    expect_identical(as.integer(r3$index), 1L)
    expect_equal(r3$fractionality, max(abs(x[c(1, 3)] - round(x[c(1, 3)]))), tolerance = 1e-12)
    expect_true(f(c(1, 2 + 1e-9), 1:2)$integral)
  }
})

test_that("rounding the relaxation reports the violated rows", {
  A <- rbind(c(2, 2), c(-2, 2))
  b <- c(7, 1)
  x <- c(1.6, 1.4)
  r <- morie_miprgr_round_relaxation(x, mp_rows(A), b, 1:2)
  expect_identical(r$x, round(x))
  lhs <- as.numeric(A %*% round(x))
  expect_identical(r$feasible, all(lhs <= b))
  expect_identical(vapply(r$violations, function(v) v$row, integer(1)),
                   which(lhs > b) - 1L)
  r2 <- miprgr_round_relaxation(c(1.4, 1.4), A, b, 1:2)
  expect_true(r2$feasible)
  r3 <- miprgr_round_relaxation(c(3.6, 0.2), A, b, 1)
  expect_identical(r3$x, c(4, 0.2))
  expect_identical(vapply(r3$violations, function(v) v$row, integer(1)), 1L)
  expect_equal(r3$violations[[1]]$lhs, 8.4, tolerance = 1e-12)
})

test_that("enumeration and branch and bound agree with the brute-force optimum", {
  probs <- list(list(A = rbind(c(2, 2), c(-2, 2)), b = c(7, 1), c = c(1, 1)),
                list(A = rbind(c(5, 7), c(3, -1)), b = c(29, 8), c = c(2, 3)),
                list(A = mp_A[1:2, ], b = mp_b[1:2], c = mp_c))
  for (p in probs) {
    ref <- mp_int_opt(p$A, p$b, p$c, 8)
    e1 <- morie_miprgr_enumerate_integer(mp_rows(p$A), p$b, p$c, 1:2, upper = 8)
    e2 <- miprgr_enumerate_integer(p$A, p$b, p$c, 1:2, upper = 8)
    expect_equal(e1$value, ref, tolerance = 1e-12)
    expect_equal(e2$value, ref, tolerance = 1e-12)
    expect_equal(sum(e1$x * p$c), ref, tolerance = 1e-12)
    for (prune in c(TRUE, FALSE)) {
      bb <- morie_miprgr_branch_and_bound(mp_rows(p$A), p$b, p$c, 1:2, prune = prune)
      expect_equal(bb$value, ref, tolerance = 1e-9)
      expect_true(all(p$A %*% bb$x <= p$b + 1e-9))
      bb2 <- miprgr_branch_and_bound(p$A, p$b, p$c, 1:2, prune = prune)
      expect_equal(bb2$value, ref, tolerance = 1e-9)
    }
    lp <- mp_vertex_opt(p$A, p$b, p$c)
    expect_equal(bb$root_bound, lp$v, tolerance = 1e-9)
  }
  # minimisation with a covering constraint
  A <- rbind(c(-3, -2), c(-1, -4))
  b <- c(-7, -6)
  ref <- mp_int_opt(A, b, c(4, 5), 8, maximise = FALSE)
  bb <- morie_miprgr_branch_and_bound(mp_rows(A), b, c(4, 5), 1:2, maximise = FALSE)
  expect_equal(bb$value, ref, tolerance = 1e-9)
  expect_equal(morie_miprgr_enumerate_integer(mp_rows(A), b, c(4, 5), 1:2, upper = 8,
                                              maximise = FALSE)$value, ref)
  # mixed: only x1 integer, x2 continuous
  mx <- morie_miprgr_branch_and_bound(list(c(2, 2), c(-2, 2)), c(7, 1), c(1, 1), 1)
  expect_equal(mx$x[1], round(mx$x[1]))
  expect_equal(mx$value, 3.5, tolerance = 1e-9)
  tr <- morie_miprgr_branch_and_bound(mp_rows(probs[[2]]$A), probs[[2]]$b, probs[[2]]$c,
                                      1:2, max_nodes = 1)
  expect_true(tr$truncated)
  expect_error(morie_miprgr_branch_and_bound(list(c(1, 1)), 1, c(1, 1), 3), "outside")
  expect_error(miprgr_branch_and_bound(rbind(c(1, 1)), 1, c(1, 1), 0), "outside")
})

test_that("the cheatsheet states the bound and branch rules", {
  expect_match(miprgr_cheatsheet(), "DAKIN", fixed = TRUE)
  expect_length(miprgr_cheatsheet(), 1L)
})
