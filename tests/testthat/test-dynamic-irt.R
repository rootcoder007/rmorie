# SPDX-License-Identifier: AGPL-3.0-or-later
# Martin-Quinn dynamic IRT (native Gibbs sampler): the paths, their spread and
# the evolution variance are recovered from simulated roll calls.

.dyn_sim <- function(N = 40L, Tn = 4L, kper = 30L, tau = 0.3, seed = 7L) {
  set.seed(seed)
  period <- rep(seq_len(Tn), each = kper)
  truth <- t(vapply(rnorm(N), function(s) s + cumsum(c(0, rnorm(Tn - 1, 0, tau))), numeric(Tn)))
  K <- Tn * kper
  a <- rnorm(K, 0, 0.5); b <- rnorm(K, 1.5, 0.5) * sample(c(-1, 1), K, TRUE)
  p <- pnorm(-rep(a, each = N) + rep(b, each = N) * truth[, period])
  list(V = matrix(rbinom(N * K, 1, p), N, K), period = period, truth = truth)
}

test_that("the native sampler recovers ideal-point paths and the evolution variance", {
  skip_heavy()
  s <- .dyn_sim()
  fit <- morie_spatial_voting_dynamic_irt(s$V, s$period, n_samples = 600L, burn_in = 200L,
                                          c0 = 4, d0 = 0.4, seed = 1L)
  expect_gt(abs(cor(as.numeric(fit$theta), as.numeric(s$truth))), 0.95)
  expect_equal(dim(fit$theta), dim(s$truth))
  expect_true(all(fit$theta_sd > 0))
  expect_gt(mean(fit$theta[fit$anchor, ]), 0)                   # the anchor fixes the sign
  expect_lt(abs(mean(fit$tau2) - 0.3^2), 0.15)                  # simulated with tau = 0.3
  expect_identical(fit$per_period[["2"]]$ideal_points, fit$theta[, 2, drop = FALSE])
  expect_identical(fit$n_periods, 4L)
  expect_match(fit$engine, "Martin and Quinn")
})

test_that("-1/1 votes are read as 0/1, NA is an absence, and a bad period vector is refused", {
  s <- .dyn_sim(N = 10L, Tn = 2L, kper = 10L)
  V <- s$V; V[1, 1:3] <- NA
  f01 <- morie_spatial_voting_dynamic_irt(V, s$period, n_samples = 50L, burn_in = 10L)
  fpm <- morie_spatial_voting_dynamic_irt(2 * V - 1, s$period, n_samples = 50L, burn_in = 10L)
  expect_equal(f01$theta, fpm$theta, tolerance = 1e-12)
  expect_error(morie_spatial_voting_dynamic_irt(V, 1:3), "one period per roll call")
  expect_error(morie_spatial_voting_dynamic_irt(V + 2, s$period), "0/1")
})
