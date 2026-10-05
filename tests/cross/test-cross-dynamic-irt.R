# SPDX-License-Identifier: AGPL-3.0-or-later
test_that("native dynamic IRT agrees with MCMCpack::MCMCdynamicIRT1d", {
  skip_if_not_installed("MCMCpack")
  set.seed(7)
  N <- 60; Tn <- 4; kper <- 40; K <- Tn * kper
  period <- rep(1:Tn, each = kper)
  truth <- t(vapply(rnorm(N), function(s) s + cumsum(c(0, rnorm(Tn - 1, 0, 0.3))), numeric(Tn)))
  a <- rnorm(K, 0, 0.5); b <- rnorm(K, 1.5, 0.5) * sample(c(-1, 1), K, TRUE)
  V <- matrix(rbinom(N * K, 1, pnorm(-rep(a, each = N) + rep(b, each = N) * truth[, period])), N, K)
  fit <- morie_spatial_voting_dynamic_irt(V, period, n_samples = 1500L, burn_in = 500L, c0 = 4, d0 = 0.4, seed = 1L)
  rownames(V) <- paste0("L", seq_len(N))
  ref <- suppressMessages(MCMCpack::MCMCdynamicIRT1d(V, item.time.map = period, burnin = 500, mcmc = 1500,
    theta.constraints = stats::setNames(list("+"), paste0("L", fit$anchor)), c0 = 4, d0 = 0.4, verbose = 0))
  th <- matrix(colMeans(ref[, grep("^theta", colnames(ref))]), N, Tn, byrow = TRUE)
  expect_gt(cor(as.numeric(th), as.numeric(fit$theta)), 0.97)
  expect_lt(abs(coef(lm(as.numeric(th) ~ as.numeric(fit$theta)))[[2]] - 1), 0.15)
  expect_lt(abs(mean(fit$tau2) - mean(colMeans(ref[, grep("^tau", colnames(ref)), drop = FALSE]))), 0.05)
})
