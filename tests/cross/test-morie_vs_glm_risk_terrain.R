# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: RiskTerrain Poisson IRLS vs stats::glm(family = poisson).

test_that("RiskTerrain equals glm poisson coefficients, standard errors and deviance", {
  U <- .morie_random_uniform(900, seed = 5, stream = 0)
  G <- as.matrix(expand.grid(0:14, 0:14))
  f1 <- cbind(14 * U[1:6], 14 * U[7:12])
  f2 <- cbind(14 * U[13:22], 14 * U[23:32])
  L1 <- RiskLayers(G, f1, 2)
  L2 <- RiskLayers(G, f2, 3, "density")
  y <- floor(4 * U[101:325] * (1 + 1.5 * L1) * (1 + 0.2 * L2))
  rt <- RiskTerrain(y, list(L1, L2))
  g <- stats::glm(y ~ L1 + L2, family = stats::poisson, control = stats::glm.control(epsilon = 1e-14, maxit = 100))
  expect_equal(rt$coefficients, unname(stats::coef(g)), tolerance = 1e-10)
  expect_equal(rt$se, unname(sqrt(diag(stats::vcov(g)))), tolerance = 1e-8)
  expect_equal(rt$deviance, stats::deviance(g), tolerance = 1e-10)
  expect_equal(rt$rrv, unname(exp(stats::coef(g)[-1])), tolerance = 1e-10)
})
