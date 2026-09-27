# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: Goldfarb-Idnani QP vs quadprog::solve.QP.

test_that(".gi_qp solutions match quadprog::solve.QP", {
  skip_if_not_installed("quadprog")
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
    expect_equal(.gi_qp(G, a, C, b, meq = 1)$x, quadprog::solve.QP(G, -a, t(C), b, meq = 1)$solution, tolerance = 1e-10)
  }
})
