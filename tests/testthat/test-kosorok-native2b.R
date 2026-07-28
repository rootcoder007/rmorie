# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cross-language parity for the Kosorok Z/M-estimator shelf. Anchors
# from the Python modules at full double precision; LCG fixture so
# both languages see identical bits. The six deterministic estimators
# are mirrored; the condition-checking modules take user callables
# and are covered by the Python suite.

ks_fix <- function(n = 150L, seed = 1357) {
  m <- 4L * n
  u <- numeric(m)
  s <- seed
  for (k in seq_len(m)) {
    s <- (1664525 * s + 1013904223) %% 4294967296
    u[k] <- (s + 0.5) / 4294967296
  }
  list(u = u, z = stats::qnorm(u), n = n)
}

test_that("morie_residual_edf matches morie.fn.ksr021", {
  f <- ks_fix(); n <- f$n
  Z <- cbind(f$z[1:n], f$z[(n + 1):(2 * n)])
  beta <- c(1, -0.5)
  y <- as.numeric(Z %*% beta + f$z[(2 * n + 1):(3 * n)])
  o <- morie_residual_edf(y, Z, beta)
  expect_equal(o$F_hat[11], 0.07333333333333333, tolerance = 1e-12)
  expect_equal(o$residuals[1], 1.119451741282386, tolerance = 1e-10)
  expect_equal(o$t[1], -2.3687082219487845, tolerance = 1e-10)
  expect_true(all(diff(o$F_hat) >= -1e-12))
  expect_equal(o$F_hat[length(o$F_hat)], 1)
  # the point of the example
  expect_false(o$limit_is_brownian_bridge)
  expect_error(morie_residual_edf(y, Z, 1), "1 entries for 2")
})

test_that("morie_cox_score_process matches morie.fn.ksr023", {
  f <- ks_fix(); n <- f$n
  zz <- f$z[1:n]
  tt <- abs(f$z[(n + 1):(2 * n)]) + 0.05
  ev <- ifelse(f$u[(3 * n + 1):(4 * n)] > 0.25, 1, 0)
  o <- morie_cox_score_process(0.5, zz, tt, ev)
  expect_equal(o$U_final[1], -0.3960065010146778, tolerance = 1e-10)
  expect_equal(o$U[6, 1], -0.001372233097924118, tolerance = 1e-10)
  expect_equal(o$E_bar[4, 1], 0.5928852991983764, tolerance = 1e-10)
  expect_equal(o$n_events, 117)
  # a PROCESS, not a number
  expect_true(o$is_process)
  expect_equal(nrow(o$U), length(o$t_grid))
  expect_error(morie_cox_score_process(0.5, zz, tt, rep(0, n)), "no events")
})

test_that("morie_survival_psi computes the printed functional", {
  g <- seq(0.1, 2, length.out = 20)
  S0 <- exp(-g); L <- exp(-0.5 * g); G <- 1 - exp(-0.3 * g)
  o <- morie_survival_psi(S0, g, S0, L, G)
  expect_equal(o$psi[6], -0.051836351564624406, tolerance = 1e-12)
  expect_equal(o$sup_norm, 0.052586474922480586, tolerance = 1e-12)
  # at S = S0 the ratio S0/S is one, so the integral term collapses
  # to G(t) and the map reduces to S0(L + G - 1) exactly
  expect_equal(o$psi, S0 * (L + G - 1), tolerance = 1e-12)
  expect_true(o$components_supplied)
  expect_error(morie_survival_psi(S0[1:5], g, S0, L, G), "5 entries for 20")
})

test_that("morie_m_normality is a sandwich, not an inverse Hessian", {
  f <- ks_fix(); n <- f$n
  S <- cbind(f$z[1:n], f$z[(n + 1):(2 * n)])
  V <- matrix(c(2, 0, 0, 0.5), 2, 2)
  o <- morie_m_normality(S, V = V)
  expect_equal(o$avar[1, 1], 0.290911114133462, tolerance = 1e-10)
  expect_equal(o$se[1], 0.04403870374518774, tolerance = 1e-10)
  expect_equal(o$Sigma[1, 1], 1.163644456533848, tolerance = 1e-10)
  expect_false(o$information_equality_assumed)
  # the sandwich differs from V^{-1} whenever V is not Sigma
  expect_false(isTRUE(all.equal(o$avar, solve(V))))
  # omitting V ASSUMES the information equality, and says so
  a <- morie_m_normality(S)
  expect_true(a$information_equality_assumed)
  expect_true(a$information_equality_holds)
})

test_that("morie_semipar_efficiency never exceeds the full information", {
  f <- ks_fix(); n <- f$n
  S <- cbind(f$z[1:n], f$z[(n + 1):(2 * n)])
  o <- morie_semipar_efficiency(S[, 1, drop = FALSE],
                                nuisance_scores = S[, 2, drop = FALSE])
  expect_equal(o$efficient_information[1, 1], 1.160070297045507,
               tolerance = 1e-10)
  expect_equal(o$full_information[1, 1], 1.163644456533848, tolerance = 1e-10)
  expect_equal(o$information_loss, 0.0035741594883409444, tolerance = 1e-9)
  # the ordering is the theorem, and the gap is what the nuisance costs
  expect_lt(o$efficient_information[1, 1], o$full_information[1, 1])
  expect_gt(o$information_loss, 0)
  # no nuisance is the parametric case
  par <- morie_semipar_efficiency(S[, 1, drop = FALSE])
  expect_equal(par$efficient_information, par$full_information)
  expect_true(par$adaptive)
})

test_that("morie_joint_convergence keeps the correlation between blocks", {
  f <- ks_fix(); n <- f$n
  S <- cbind(f$z[1:n], f$z[(n + 1):(2 * n)])
  D <- matrix(c(1, 0, 0.2, 1), 2, 2)
  o <- morie_joint_convergence(D, S)
  expect_equal(o$avar[1, 1], 1.2351184079633934, tolerance = 1e-10)
  expect_equal(o$correlation[1, 2], -0.24649924034005727, tolerance = 1e-9)
  expect_equal(o$se[1], 0.09074206697974185, tolerance = 1e-10)
  expect_true(o$jointly)
  expect_true(o$operator_invertible)
  expect_error(morie_joint_convergence(diag(3), S), "2 by 2")
})
