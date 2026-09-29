# Coverage tests for R/competing_risks_native.R: cause-specific Cox
# against survival::coxph, Fine-Gray against cmprsk::crr and
# survival::finegray (coefficients, model-based SEs and the cumulative
# incidence), and the gamma shared frailty against coxph(frailty()).

cr_data <- function() {
  n <- 60
  x1 <- round(sin(1:n) * 2, 3)
  x2 <- rep(0:1, 30)
  tm <- round(-log((1:n - 0.5) / n) * exp(-0.3 * x1) * (1 + 0.1 * x2), 4)[order(sin((1:n) * 7))]
  ev <- rep(c(1, 2, 0, 1, 0, 2, 1, 1, 0, 2), 6)
  list(tm = tm, ev = ev, x1 = x1, x2 = x2, X = cbind(x1, x2))
}

test_that("cause-specific hazard is Cox with competing events censored", {
  skip_if_not_installed("survival")
  d <- cr_data()
  cs <- morie_cause_specific_hazard(d$tm, d$ev, d$X)
  cf <- survival::coxph(survival::Surv(d$tm, d$ev == 1) ~ d$X, ties = "efron")
  expect_equal(cs$beta, unname(coef(cf)), tolerance = 1e-8)
  expect_equal(unname(cs$se), unname(sqrt(diag(vcov(cf)))), tolerance = 1e-8)
  expect_equal(cs$p_value, 2 * pnorm(-abs(cs$beta / cs$se)))
  expect_equal(c(cs$n_cause, cs$n_competing, cs$n_censored), c(24L, 18L, 18L))
  # every cause-2 event has x2 = 1, so only x1 is identified for cause 2
  c2 <- morie_cause_specific_hazard(d$tm, d$ev, d$x1, cause = 2, ties = "breslow")
  cf2 <- survival::coxph(survival::Surv(d$tm, d$ev == 2) ~ d$x1, ties = "breslow")
  expect_equal(c2$beta, unname(coef(cf2)), tolerance = 1e-8)
  expect_error(morie_cause_specific_hazard(d$tm[-1], d$ev, d$X), "time has 59 entries")
  expect_error(morie_cause_specific_hazard(d$tm, d$ev, d$X, cause = 3), "no events of cause 3")
})

test_that("Fine-Gray coefficients, SEs and cumulative incidence", {
  skip_if_not_installed("survival")
  d <- cr_data()
  fg <- morie_competing_risks_fg(d$tm, d$ev, d$X)
  dd <- data.frame(tm = d$tm, ev = factor(d$ev, 0:2), x1 = d$x1, x2 = d$x2)
  fgd <- survival::finegray(survival::Surv(tm, ev) ~ ., data = dd, etype = "1")
  fc <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x1 + x2, weights = fgwt, data = fgd, robust = FALSE)
  expect_equal(fg$beta, unname(coef(fc)), tolerance = 1e-7)
  expect_equal(unname(fg$se), unname(sqrt(diag(vcov(fc)))), tolerance = 1e-7)
  expect_equal(fg$n_competing, 18L)
  sd <- morie_fine_gray_subdistribution_hazard(d$tm, d$ev, d$X)
  sf <- survival::survfit(fc, newdata = data.frame(x1 = 0, x2 = 0))
  expect_equal(sd$baseline_cif, 1 - sf$surv[match(sd$times, sf$time)], tolerance = 1e-7)
  lin <- exp(as.numeric(d$X %*% sd$beta))
  expect_equal(sd$cumulative_incidence, 1 - exp(-outer(lin, sd$cumhazard)), tolerance = 1e-12)
  expect_equal(sd$times, sort(unique(d$tm[d$ev == 1])))
  expect_error(morie_competing_risks_fg(d$tm, d$ev, d$X, ties = "breslow"), "only ties = 'efron'")
  expect_error(morie_competing_risks_fg(d$tm, d$ev[-1], d$X), "event_type has 59")
  expect_error(morie_competing_risks_fg(d$tm, d$ev, d$X, cause = 5), "no events of cause 5")
})

test_that("Fine-Gray coefficients agree with cmprsk::crr", {
  skip_if_not_installed("cmprsk")
  d <- cr_data()
  fg <- morie_competing_risks_fg(d$tm, d$ev, d$X)
  cr <- cmprsk::crr(d$tm, d$ev, d$X, failcode = 1, cencode = 0)
  expect_equal(fg$beta, unname(cr$coef), tolerance = 1e-6)
})

test_that("gamma shared frailty matches coxph(frailty(theta))", {
  skip_if_not_installed("survival")
  d <- cr_data()
  cl <- rep(1:12, each = 5)
  ev <- as.numeric(d$ev == 1)
  fr <- morie_cox_frailty(d$tm, ev, d$X, cl, theta = 0.5)
  cfr <- survival::coxph(survival::Surv(d$tm, ev) ~ d$x1 + d$x2 + survival::frailty(cl, theta = 0.5))
  expect_equal(fr$beta, unname(coef(cfr)), tolerance = 1e-5)
  expect_true(fr$converged)
  expect_equal(fr$kendall_tau, 0.5 / 2.5)
  expect_equal(fr$n_clusters, 12L)
  est <- morie_cox_frailty(d$tm, ev, d$X, cl)
  expect_true(est$theta > 1e-4 && est$theta < 10)
  expect_equal(est$hazard_ratio, exp(est$beta))
  expect_error(morie_cox_frailty(d$tm, ev, d$X, cl[-1]), "cluster has 59 entries")
  expect_error(morie_cox_frailty(d$tm, ev, d$X, 1:60), "not identifiable")
})
