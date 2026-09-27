# SPDX-License-Identifier: AGPL-3.0-or-later
# Cross-validation: conditional logit vs survival::clogit.

test_that("PanelBinaryChoice fe_logit matches survival::clogit", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 30
  g <- rep(seq_len(n), each = 4)
  x1 <- rnorm(4 * n)
  x2 <- runif(4 * n)
  y <- as.integer(runif(4 * n) < plogis(rep(rnorm(n), each = 4) + x1 - x2))
  r <- PanelBinaryChoice(y, cbind(x1, x2), g)
  # clogit is coxph on a constant time with exact ties; called directly because clogit
  # builds an unqualified coxph() call that needs survival attached
  tt <- rep(1, length(y))
  f <- survival::coxph(survival::Surv(tt, y) ~ x1 + x2 + survival::strata(g), method = "exact")
  expect_equal(unname(r$coef), unname(stats::coef(f)), tolerance = 1e-5)
  expect_equal(r$loglik, f$loglik[2], tolerance = 1e-6)
})
