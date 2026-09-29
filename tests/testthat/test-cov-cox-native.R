# Coverage for the Cox proportional-hazards helpers against
# survival::coxph (Breslow and Efron ties, strata) and its residuals
# (martingale, deviance, Schoenfeld, dfbeta) and baseline hazard.

.cdat <- function(ties = FALSE) {
  set.seed(21)
  n <- 40
  x1 <- stats::rnorm(n)
  x2 <- stats::rbinom(n, 1, 0.4)
  tt <- stats::rexp(n, exp(0.6 * x1 - 0.5 * x2))
  if (ties) tt <- ceiling(tt * 4) / 4
  cz <- stats::runif(n, 0.5, 3)
  list(time = pmin(tt, cz), event = as.numeric(tt <= cz), X = cbind(x1, x2), s = rep(1:2, 20))
}

test_that("Breslow and Efron fits reproduce coxph on tied data", {
  skip_if_not_installed("survival")
  d <- .cdat(ties = TRUE)
  for (tie in c("breslow", "efron")) {
    fit <- if (tie == "breslow") morie_breslow_tie_correction(d$time, d$event, d$X) else morie_efron_tie_correction(d$time, d$event, d$X)
    ref <- survival::coxph(survival::Surv(d$time, d$event) ~ d$X, ties = tie)
    expect_gt(fit$n_ties, 0L)
    expect_equal(fit$beta, unname(stats::coef(ref)), tolerance = 1e-8, info = tie)
    expect_equal(unname(fit$se), unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-8, info = tie)
    expect_equal(fit$loglik, ref$loglik[2], tolerance = 1e-9, info = tie)
    expect_equal(fit$hazard_ratio, exp(fit$beta), tolerance = 1e-12)
  }
  expect_error(morie_efron_tie_correction(d$time, c(d$event[-1], 2), d$X), "0 \\(censored\\) or 1")
})

test_that("stratified fits reproduce coxph with strata()", {
  skip_if_not_installed("survival")
  d <- .cdat(ties = TRUE)
  st <- morie_cox_stratified(d$time, d$event, d$X, d$s)
  ref <- survival::coxph(survival::Surv(d$time, d$event) ~ d$X + survival::strata(d$s), ties = "efron")
  expect_equal(st$beta, unname(stats::coef(ref)), tolerance = 1e-8)
  expect_equal(unname(st$se), unname(sqrt(diag(stats::vcov(ref)))), tolerance = 1e-8)
  expect_equal(st$loglik, ref$loglik[2], tolerance = 1e-9)
  expect_equal(unname(st$events_per_stratum), as.integer(tapply(d$event, d$s, sum)))
  expect_error(morie_cox_stratified(d$time, d$event, d$X, 1:3), "stratum has 3 entries")
})

test_that("baseline hazard and residuals match survival on untied data", {
  skip_if_not_installed("survival")
  d <- .cdat()
  fit <- morie_breslow_tie_correction(d$time, d$event, d$X)
  ref <- survival::coxph(survival::Surv(d$time, d$event) ~ d$X, ties = "breslow")
  bs <- morie_cox_breslow_step(d$time, d$event, d$X, beta = fit$beta)
  bh <- survival::basehaz(ref, centered = FALSE)
  at <- match(bs$times, bh$time)
  expect_equal(bs$cumhazard, bh$hazard[at], tolerance = 1e-8)
  expect_equal(bs$survival, exp(-bs$cumhazard), tolerance = 1e-12)
  mr <- morie_cox_martingale_residuals(fit)
  expect_equal(mr$residuals, unname(stats::residuals(ref, type = "martingale")), tolerance = 1e-8)
  dv <- morie_deviance_residual_cox(fit)
  expect_equal(dv$residuals, unname(stats::residuals(ref, type = "deviance")), tolerance = 1e-8)
  sr <- morie_cox_schoenfeld_residuals(fit, transform = "identity")
  rs <- stats::residuals(ref, type = "schoenfeld")
  # survival returns one row per event, in event-time order
  expect_equal(unname(sr$residuals), unname(rs), tolerance = 1e-8)
  expect_equal(sr$times, sort(d$time[d$event == 1]))
  expect_equal(sr$correlation[1], stats::cor(sr$times, sr$residuals[, 1]), tolerance = 1e-12)
  df <- morie_cox_dfbeta_influence(fit)
  expect_equal(unname(df$dfbeta), unname(stats::residuals(ref, type = "dfbeta")), tolerance = 1e-8)
  expect_equal(unname(df$score_residuals), unname(stats::residuals(ref, type = "score")), tolerance = 1e-8)
  db <- morie_dfbeta_cox(d$time, d$event, d$X, ties = "breslow")
  expect_equal(db$dfbeta, df$dfbeta, tolerance = 1e-10)
  expect_error(morie_cox_martingale_residuals(list(time = 1)), "fit is missing")
  expect_error(morie_cox_breslow_step(d$time, d$event, d$X, beta = 1), "beta has 1 entries")
})
