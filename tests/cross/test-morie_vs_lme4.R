# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: random-effects logit and probit vs lme4::glmer (adaptive quadrature).

test_that("PanelBinaryChoice random effects match lme4::glmer", {
  skip_if_not_installed("lme4")
  set.seed(2)
  n <- 40
  g <- rep(seq_len(n), each = 5)
  x1 <- rnorm(5 * n)
  y <- as.integer(runif(5 * n) < plogis(-0.2 + rep(rnorm(n, sd = 1.2), each = 5) + 0.8 * x1))
  d <- data.frame(y = y, x1 = x1, g = g)
  r <- PanelBinaryChoice(y, cbind(x1), g, model = "re_logit", n_quad = 40)
  f <- lme4::glmer(y ~ x1 + (1 | g), data = d, family = stats::binomial, nAGQ = 25)
  expect_equal(unname(r$coef), unname(lme4::fixef(f)), tolerance = 1e-3)
  expect_equal(r$sigma, as.data.frame(lme4::VarCorr(f))$sdcor, tolerance = 1e-3)
})
