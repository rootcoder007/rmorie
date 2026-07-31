# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Ch. 7 simulation family (Schabenberger & Gotway 2005).
#   Sec 7.1.1  Cholesky (LU) root
#   Sec 7.1.2  spectral decomposition root
#   Sec 7.2.2  conditioning by kriging, eq (7.1)
#
# The two unconditional methods are two square roots of the same Sigma, so
# they give DIFFERENT fields from the same stream. The tests say so rather
# than expecting them to agree pointwise.
#
# Because the draws come from morie's own generator, the pinned values below
# are the same numbers the Python arm produces -- the simulated field itself
# is shared, not merely its distribution.

sim_grid <- function(k = 5) {
  g <- 0:(k - 1)
  as.matrix(expand.grid(x = g, y = g))
}

sim_cov <- function(coords) {
  .schab_covariance_matrix(coords, 0.2, 1.5, 2.5, "exponential")
}

test_that("the Cholesky root is lower triangular and reconstructs Sigma", {
  S <- sim_cov(sim_grid())
  L <- .schab_cholesky_root(S)
  expect_true(all(L[upper.tri(L)] == 0))
  expect_lt(max(abs(L %*% t(L) - S)), 1e-12)
})

test_that("the spectral root is symmetric and reconstructs Sigma", {
  # Sec 7.1.2: Sigma^(1/2) = P Delta^(1/2) P' is symmetric, so squaring it
  # without a transpose returns Sigma.
  S <- sim_cov(sim_grid())
  P <- .schab_spectral_root(S)
  expect_equal(P, t(P), tolerance = 1e-12)
  expect_lt(max(abs(P %*% P - S)), 1e-10)
})

test_that("the two roots are genuinely different square roots", {
  S <- sim_cov(sim_grid())
  n <- nrow(S)
  a <- .schab_simulate_unconditional(rep(0, n), S, "cholesky", seed = 1)
  b <- .schab_simulate_unconditional(rep(0, n), S, "spectral", seed = 1)
  expect_false(isTRUE(all.equal(a, b)))
  expect_false(isTRUE(all.equal(.schab_cholesky_root(S), .schab_spectral_root(S))))
})

test_that("both methods reproduce the target covariance", {
  S <- sim_cov(sim_grid(4))
  n <- nrow(S)
  for (meth in c("cholesky", "spectral")) {
    draws <- t(vapply(0:3999,
                      function(s) .schab_simulate_unconditional(rep(0, n), S, meth,
                                                                seed = 11, stream = s),
                      numeric(n)))
    expect_lt(max(abs(stats::cov(draws) - S)), 0.2)
    expect_lt(max(abs(colMeans(draws))), 0.15)
  }
})

test_that("conditional simulation honors the data", {
  # Sec 7.2.2 property (i): Zc(s0) = Z(s0) at the sampled locations, exactly.
  S <- sim_cov(sim_grid())
  n <- nrow(S)
  truth <- .schab_simulate_unconditional(rep(4, n), S, seed = 99)
  zc <- .schab_simulate_conditional(S, truth[1:10], 10, mean = 4, seed = 1)
  expect_lt(max(abs(zc[1:10] - truth[1:10])), 1e-10)
})

test_that("conditional simulation satisfies the 2 sigma_sk^2 identity", {
  # Sec 7.2.2: E[(Zc(s) - Z(s))^2] = 2 sigma^2_sk. The expectation is over
  # BOTH the field and the simulation, so the truth is redrawn every
  # replicate; holding it fixed measures something else.
  S <- sim_cov(sim_grid(4))
  n <- nrow(S); m <- 6L
  sk <- .schab_simple_kriging_variance(S, m)[(m + 1L):n]
  reps <- 4000L
  acc <- numeric(n - m)
  for (r in seq_len(reps)) {
    truth <- .schab_simulate_unconditional(rep(0, n), S, seed = 4242, stream = 2 * r)
    zc <- .schab_simulate_conditional(S, truth[1:m], m, mean = 0,
                                      seed = 4242, stream = 2 * r + 1)
    acc <- acc + (zc[(m + 1L):n] - truth[(m + 1L):n])^2
  }
  expect_equal(mean((acc / reps) / (2 * sk)), 1, tolerance = 0.08)
})

test_that("the simulated field matches the Python arm exactly", {
  # The point of the native generator: the FIELD is shared, not just its
  # distribution.
  S <- sim_cov(sim_grid())
  n <- nrow(S)
  a <- .schab_simulate_unconditional(rep(0, n), S, "cholesky", seed = 1)
  b <- .schab_simulate_unconditional(rep(0, n), S, "spectral", seed = 1)
  expect_equal(a[1], 1.60099564270895, tolerance = 1e-12)
  expect_equal(b[1], 2.00858116119438, tolerance = 1e-12)
  zc <- .schab_simulate_conditional(S, a[1:8], 8, mean = 0, seed = 2)
  expect_equal(zc[9], 0.611244720302244, tolerance = 1e-12)
})

test_that("the simulation family rejects bad input", {
  S <- sim_cov(sim_grid(3))
  expect_error(.schab_simulate_unconditional(rep(0, 3), S))
  expect_error(.schab_simulate_unconditional(rep(0, nrow(S)), S, "nope"))
  expect_error(.schab_simulate_conditional(S, rep(0, 2), 3))
})
