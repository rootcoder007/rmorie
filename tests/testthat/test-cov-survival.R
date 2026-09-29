# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/survival.R exports: the Nelson-Aalen estimator, the
# log-rank test, Cox regression and Harrell's C, all checked against
# the survival package (Greenwood/Aalen variances, Efron and Breslow
# ties, survdiff and survival::concordance).

.sv_t <- c(4, 3, 1, 1, 2, 2, 3, 6, 7, 5, 8, 3, 9, 2, 6, 5, 10, 4, 7, 8)
.sv_e <- c(1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0)
.sv_g <- rep(c(0, 1), 10)
.sv_X <- cbind(z = sin(1:20), w = (1:20) %% 3)

test_that("nelson_aalen is the Aalen cumulative hazard with its variance", {
  skip_if_not_installed("survival")
  r <- morie_survival_nelsonaalen(.sv_t, .sv_e)
  ut <- sort(unique(.sv_t[.sv_e == 1]))
  d <- vapply(ut, function(u) sum(.sv_t == u & .sv_e == 1), 0)
  n <- vapply(ut, function(u) sum(.sv_t >= u), 0)
  expect_equal(r$time, ut)
  expect_equal(r$cumhaz, cumsum(d / n), tolerance = 1e-12)
  expect_equal(r$se, sqrt(cumsum(d / n^2)), tolerance = 1e-12)
  expect_equal(r$surv, exp(-r$cumhaz), tolerance = 1e-12)
  fh <- survival::survfit(survival::Surv(.sv_t, .sv_e) ~ 1, stype = 2, ctype = 1)
  expect_equal(r$cumhaz, fh$cumhaz[fh$n.event > 0], tolerance = 1e-12)
})

test_that("logrank_test matches survdiff", {
  skip_if_not_installed("survival")
  r <- morie_survival_logrank(.sv_t, .sv_e, .sv_g)
  sd <- survival::survdiff(survival::Surv(.sv_t, .sv_e) ~ .sv_g)
  expect_equal(r$statistic, sd$chisq, tolerance = 1e-10)
  expect_equal(r$observed, unname(sd$obs), tolerance = 1e-12)
  expect_equal(r$expected, unname(sd$exp), tolerance = 1e-10)
  expect_equal(r$df, 1L)
  expect_equal(r$p_value, pchisq(sd$chisq, 1, lower.tail = FALSE), tolerance = 1e-12)
  g3 <- rep(c(0, 1, 2), length.out = 20)
  r3 <- morie_survival_logrank(.sv_t, .sv_e, g3)
  sd3 <- survival::survdiff(survival::Surv(.sv_t, .sv_e) ~ g3)
  expect_equal(r3$statistic, sd3$chisq, tolerance = 1e-10)
  expect_equal(r3$df, 2L)
  expect_error(morie_survival_logrank(.sv_t, .sv_e, rep(1, 20)), "at least 2 groups")
})

test_that("cox_partial_loglik and cox_ph match coxph for Efron and Breslow ties", {
  skip_if_not_installed("survival")
  for (ti in c("efron", "breslow")) {
    b <- c(0.3, -0.2)
    ll <- morie_cox_partial_loglik(.sv_t, .sv_e, .sv_X, b, ti)
    f0 <- survival::coxph(survival::Surv(.sv_t, .sv_e) ~ .sv_X, ties = ti, init = b,
                          control = survival::coxph.control(iter.max = 0))
    expect_equal(ll, f0$loglik[2], tolerance = 1e-10)
    fit <- morie_survival_cox(.sv_t, .sv_e, .sv_X, ties = ti)
    ref <- survival::coxph(survival::Surv(.sv_t, .sv_e) ~ .sv_X, ties = ti)
    expect_equal(unname(fit$coef), unname(coef(ref)), tolerance = 1e-7)
    expect_equal(unname(fit$se), unname(sqrt(diag(vcov(ref)))), tolerance = 1e-6)
    expect_equal(fit$hazard_ratio, exp(fit$coef), tolerance = 1e-12)
    expect_equal(fit$loglik, ref$loglik[2], tolerance = 1e-9)
    expect_equal(fit$loglik_null, ref$loglik[1], tolerance = 1e-10)
    expect_equal(fit$lr_statistic, 2 * (fit$loglik - fit$loglik_null), tolerance = 1e-12)
    expect_equal(fit$n_events, sum(.sv_e))
  }
  expect_error(morie_survival_cox(.sv_t, rep(0, 20), .sv_X), "no events")
  # collinear covariates: no unique maximum
  expect_error(morie_survival_cox(.sv_t, .sv_e, cbind(.sv_X, .sv_X[, 1] * 2)), "singular information matrix")
  # perfect separation: the partial likelihood reaches its supremum
  expect_error(morie_survival_cox(1:6, c(1, 1, 1, 0, 0, 0), matrix(c(1, 1, 1, 0, 0, 0))),
               "singular information matrix")
})

test_that("concordance_index matches survival::concordance", {
  skip_if_not_installed("survival")
  risk <- as.numeric(.sv_X %*% c(0.4, -0.3))
  r <- morie_survival_concordance(.sv_t, .sv_e, risk)
  ref <- survival::concordance(survival::Surv(.sv_t, .sv_e) ~ risk, reverse = TRUE)
  expect_equal(r$c_index, unname(ref$concordance), tolerance = 1e-12)
  expect_equal(r$concordant + r$discordant + r$tied, r$n_pairs)
  expect_equal(unname(ref$count[["concordant"]]), r$concordant)
  expect_equal(unname(ref$count[["discordant"]]), r$discordant)
  # a perfectly ordered risk scores 1, a reversed one 0
  expect_equal(morie_survival_concordance(c(1, 2, 3), c(1, 1, 1), c(3, 2, 1))$c_index, 1)
  expect_equal(morie_survival_concordance(c(1, 2, 3), c(1, 1, 1), c(1, 2, 3))$c_index, 0)
  expect_equal(morie_survival_concordance(c(1, 2), c(1, 1), c(5, 5))$c_index, 0.5)
  expect_error(morie_survival_concordance(c(1, 2), c(0, 0), c(1, 2)), "no comparable pairs")
})
