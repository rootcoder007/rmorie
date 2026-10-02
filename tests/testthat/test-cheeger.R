# SPDX-License-Identifier: AGPL-3.0-or-later
# Research P3: the Cheeger quantities must agree with research/lean/P3Cheeger.lean.

rand_graph <- function(n, p = 0.5) {
  repeat {
    A <- matrix(0, n, n)
    A[upper.tri(A)] <- rbinom(n * (n - 1) / 2, 1, p) * runif(n * (n - 1) / 2, 0.5, 2)
    A <- A + t(A)
    if (all(rowSums(A) > 0)) return(A)
  }
}

test_that("Rayleigh quotient of the test vector equals cut (1/vol S + 1/vol S^c), and lambda2 <= it <= 2 h(S)", {
  set.seed(1)
  for (k in 1:40) {
    n <- sample(4:9, 1)
    A <- rand_graph(n)
    S <- sample(n, sample(1:(n - 1), 1))
    r <- morie_cheeger_bound(A, S)
    d <- rowSums(A)
    in_s <- seq_len(n) %in% S
    vol_s <- sum(d[in_s]); vol_c <- sum(d[!in_s]); cut_s <- sum(A[in_s, !in_s])
    # the test vector, its D-norm, its Dirichlet form (testVec_dnorm, testVec_dirichlet)
    f <- ifelse(in_s, 1 / vol_s, -1 / vol_c)
    expect_equal(sum(d * f), 0, tolerance = 1e-12)                          # testVec_orth
    expect_equal(sum(d * f^2), 1 / vol_s + 1 / vol_c, tolerance = 1e-12)
    E <- 0.5 * sum(A * outer(f, f, "-")^2)
    expect_equal(E, cut_s * (1 / vol_s + 1 / vol_c)^2, tolerance = 1e-12)
    expect_equal(r$rayleigh_test, E / sum(d * f^2), tolerance = 1e-12)       # rayleigh_testVec
    expect_lte(r$rayleigh_test, 2 * r$conductance + 1e-12)                  # rayleigh_le_two_conductance
    expect_lte(r$lambda2, r$rayleigh_test + 1e-10)                          # lambda2_le_rayleigh_testVec
    expect_true(r$bound_holds)                                              # cheeger_easy
    expect_equal(r$cut, cut_s); expect_equal(r$conductance, cut_s / min(vol_s, vol_c))
  }
})

test_that("exhaustive Cheeger constant: lambda2 <= 2 h_G, and two blocks joined by one edge have a tiny h", {
  A <- matrix(0, 6, 6)
  A[1, 2] <- A[1, 3] <- A[2, 3] <- A[4, 5] <- A[4, 6] <- A[5, 6] <- A[3, 4] <- 1
  A <- A + t(A)
  r <- morie_cheeger_bound(A, S = 1:3, exhaustive = TRUE)
  expect_equal(r$cut, 1); expect_equal(r$conductance, 1 / 7)
  expect_equal(sort(r$argmin_set), 1:3)
  expect_equal(r$cheeger_constant, 1 / 7)
  expect_true(r$cheeger_bound_holds)
  expect_lte(r$lambda2, 2 / 7)
  expect_error(morie_cheeger_bound(A, S = 1:6), "proper subset")
  expect_error(morie_cheeger_bound(A[1:5, 1:6], 1), "square")
})
