# SPDX-License-Identifier: AGPL-3.0-or-later
# Parity tests for the causal cluster mirrors (batch 2).

test_that("ATT weights match the closed form and Kish ESS", {
  out <- morie_att_weights(c(1, 0, 0), c(0.5, 0.25, 0.5))
  expect_equal(out$weights, c(1, 1 / 3, 1))
  expect_equal(out$ess_control, 1.6)
  expect_error(morie_att_weights(c(1, 2), c(0.5, 0.5)), "binary")
  expect_error(morie_att_weights(c(0, 1), c(1, 0.5)), "strictly")
})

test_that("Firpo QTE recovers a constant shift", {
  set.seed(42)
  n <- 3000
  x <- stats::rnorm(n)
  e <- pmin(pmax(1 / (1 + exp(-x)), 0.05), 0.95)
  tr <- as.numeric(stats::runif(n) < e)
  y <- x + stats::rnorm(n) + 3 * tr
  out <- morie_qte_firpo(y, tr, e, tau = c(0.25, 0.5, 0.75))
  expect_true(all(abs(out$qte - 3) < 0.4))
  expect_error(morie_qte_firpo(y, tr, e, tau = 1.5), "strictly")
})

test_that("g-formula removes confounding by L", {
  set.seed(42)
  n <- 2500
  L <- stats::rnorm(n)
  a <- as.numeric(stats::runif(n) < 1 / (1 + exp(-1.5 * L)))
  y <- 2 * a + 1.5 * L + stats::rnorm(n, sd = 0.5)
  out <- morie_g_formula(y, a, L)
  naive <- mean(y[a == 1]) - mean(y[a == 0])
  expect_gt(abs(naive - 2), 0.5)
  expect_equal(out$ate, 2, tolerance = 0.1)
  expect_equal(out$ate, out$EY1 - out$EY0)
})

test_that("Granger test finds the true direction only", {
  set.seed(42)
  n <- 1500
  x <- numeric(n)
  y <- numeric(n)
  ex <- stats::rnorm(n)
  ey <- stats::rnorm(n)
  for (t in 2:n) {
    x[t] <- 0.5 * x[t - 1] + ex[t]
    y[t] <- 0.4 * y[t - 1] + 0.6 * x[t - 1] + ey[t]
  }
  expect_lt(morie_granger_test(x, y, 1L)$p_value, 0.01)
  expect_gt(morie_granger_test(y, x, 1L)$p_value, 0.01)
  expect_error(morie_granger_test(c(1, 2), c(1, 2), 1L), "observations")
})

test_that("Gaussian transfer entropy equals half the log RSS ratio", {
  set.seed(42)
  n <- 1500
  x <- numeric(n)
  y <- numeric(n)
  ex <- stats::rnorm(n)
  ey <- stats::rnorm(n)
  for (t in 2:n) {
    x[t] <- 0.5 * x[t - 1] + ex[t]
    y[t] <- 0.4 * y[t - 1] + 0.6 * x[t - 1] + ey[t]
  }
  te <- morie_transfer_entropy_gaussian(x, y, 1L)
  expect_gt(te$mi, 0.05)
  expect_lt(te$p_value, 0.01)
  expect_lt(morie_transfer_entropy_gaussian(y, x, 1L)$mi, 0.01)
})

test_that("serial mediation recovers all four paths", {
  set.seed(42)
  n <- 3000
  x <- stats::rnorm(n)
  m1 <- 0.6 * x + stats::rnorm(n, sd = 0.6)
  m2 <- 0.4 * x + 0.5 * m1 + stats::rnorm(n, sd = 0.6)
  y <- 0.3 * x + 0.7 * m1 + 0.9 * m2 + stats::rnorm(n, sd = 0.6)
  out <- morie_serial_mediation(x, m1, m2, y)
  expect_equal(out$direct, 0.3, tolerance = 0.1)
  expect_equal(out$via_m1, 0.42, tolerance = 0.1)
  expect_equal(out$serial, 0.27, tolerance = 0.1)
  expect_equal(out$total, out$direct + out$indirect_total)
})

test_that("cluster-robust SE exceeds the naive SE under strong ICC", {
  set.seed(42)
  G <- 40
  npc <- 50
  cl <- rep(seq_len(G), each = npc)
  d <- rep(as.numeric(stats::runif(G) < 0.5), each = npc)
  y <- 1 * d + rep(stats::rnorm(G, sd = 1.5), each = npc) +
    stats::rnorm(G * npc, sd = 0.5)
  out <- morie_cluster_robust_effect(y, d, cl)
  expect_gt(out$se_cluster, 3 * out$se_naive)
  expect_equal(out$n_clusters, G)
  expect_error(morie_cluster_robust_effect(y, d, rep(1, length(y))), "3 clusters")
})

test_that("partial tau shrinks a confounded association", {
  set.seed(42)
  n <- 1000
  z <- stats::rnorm(n)
  x <- z + stats::rnorm(n, sd = 0.4)
  y <- z + stats::rnorm(n, sd = 0.4)
  out <- morie_partial_tau(x, y, z)
  expect_gt(out$tau_xy, 0.4)
  # a shrinkage diagnostic, not a conditional-independence test
  expect_lt(abs(out$partial_tau), 0.5 * out$tau_xy)
  expect_error(morie_partial_tau(1:2, 1:2, 1:2), "4 observations")
})
