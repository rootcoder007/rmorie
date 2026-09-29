# Survival extras, checked against the survival package: Cox fits for
# the event and censoring hazards, KM-based IPCW weights, piecewise
# (time-varying) Cox coefficients, and the generalized gamma AFT nesting
# the Weibull and lognormal fits.

sx_data <- function() {
  set.seed(51)
  n <- 120
  X <- cbind(stats::rnorm(n), stats::rbinom(n, 1, 0.5))
  T <- stats::rweibull(n, 1.5, exp(0.5 - 0.4 * X[, 1] + 0.3 * X[, 2]))
  C <- stats::rexp(n, exp(-1 + 0.6 * X[, 1]))
  list(time = pmin(T, C), event = as.integer(T <= C), X = X)
}

test_that("censoring and event hazards match coxph", {
  skip_if_not_installed("survival")
  d <- sx_data()
  r <- morie_dependent_censoring_hazard(d$time, d$event, d$X)
  fc <- survival::coxph(survival::Surv(d$time, 1 - d$event) ~ d$X, ties = "efron")
  fe <- survival::coxph(survival::Surv(d$time, d$event) ~ d$X, ties = "efron")
  expect_equal(unname(r$beta_censoring), unname(stats::coef(fc)), tolerance = 1e-6)
  expect_equal(unname(r$beta_event), unname(stats::coef(fe)), tolerance = 1e-6)
  expect_equal(unname(r$se), unname(sqrt(diag(stats::vcov(fc)))), tolerance = 1e-5)
  expect_identical(r$n_censored, sum(1L - d$event))
  expect_error(morie_dependent_censoring_hazard(d$time, rep(1, 120), d$X), "no censored")
})

test_that("IPCW weights invert the censoring Kaplan-Meier at each time", {
  skip_if_not_installed("survival")
  d <- sx_data()
  cens <- 1 - d$event
  r <- morie_censoring_at_risk_weight(d$time, cens, stabilize = FALSE)
  sf <- survival::survfit(survival::Surv(d$time, cens) ~ 1)
  G <- c(1, sf$surv)[findInterval(d$time, sf$time) + 1]
  expect_equal(r$G, pmax(G, 1e-8), tolerance = 1e-12)
  expect_equal(r$weights, ifelse(cens == 0, 1 / pmax(G, 1e-8), 0), tolerance = 1e-12)
  st <- morie_censoring_at_risk_weight(d$time, cens)
  expect_equal(st$weights, r$weights * mean(G), tolerance = 1e-12)
  at <- morie_censoring_at_risk_weight(d$time, cens, at = 1, stabilize = FALSE)
  g1 <- summary(survival::survfit(survival::Surv(d$time, cens) ~ 1), times = 1)$surv
  expect_equal(unique(at$G), g1, tolerance = 1e-12)
  expect_equal(r$ess, sum(r$weights)^2 / sum(r$weights^2), tolerance = 1e-12)
  expect_error(morie_censoring_at_risk_weight(d$time, cens[-1]), "entries but censor")
  expect_error(morie_censoring_at_risk_weight(d$time, cens * 2), "0/1")
})

test_that("piecewise Cox fits each interval on its own risk set", {
  skip_if_not_installed("survival")
  d <- sx_data()
  r <- morie_cox_time_varying(d$time, d$event, d$X, n_intervals = 2)
  cut <- stats::quantile(sort(d$time[d$event == 1]), 0.5, names = FALSE)
  expect_equal(r$cutpoints, cut)
  k1 <- rep(TRUE, 120)
  f1 <- survival::coxph(survival::Surv(pmin(d$time, cut), ifelse(d$time <= cut, d$event, 0)) ~ d$X)
  k2 <- d$time > cut
  f2 <- survival::coxph(survival::Surv(d$time[k2] - cut, d$event[k2]) ~ d$X[k2, ])
  expect_equal(r$beta[1, ], unname(stats::coef(f1)), tolerance = 1e-6)
  expect_equal(r$beta[2, ], unname(stats::coef(f2)), tolerance = 1e-6)
  expect_identical(r$events_per_interval, c(sum(d$event[d$time <= cut]), sum(d$event[k2])))
  one <- morie_cox_time_varying(d$time, d$event, d$X, n_intervals = 1)
  expect_equal(one$beta[1, ], one$constant_beta, tolerance = 1e-10)
  expect_error(morie_cox_time_varying(d$time, d$event, d$X, n_intervals = 0), "at least 1")
  expect_error(morie_cox_time_varying(d$time[1:3], c(1, 0, 0), d$X[1:3, ], 2), "too few")
})

test_that("the generalized gamma AFT nests the Weibull and lognormal fits", {
  skip_if_not_installed("survival")
  d <- sx_data()
  g <- morie_generalized_gamma_aft(d$time, d$event, d$X)
  fw <- survival::survreg(survival::Surv(d$time, d$event) ~ d$X, dist = "weibull")
  fl <- survival::survreg(survival::Surv(d$time, d$event) ~ d$X, dist = "lognormal")
  # the nested fits cannot beat the full one (Nelder-Mead reltol 1e-6 slack)
  expect_gte(g$loglik, fw$loglik[2] - 1e-3)
  expect_gte(g$loglik, fl$loglik[2] - 1e-3)
  expect_equal(g$aic, 2 * 5 - 2 * g$loglik, tolerance = 1e-12)
  expect_equal(g$p_vs_weibull, stats::pchisq(g$lr_vs_weibull, 1, lower.tail = FALSE), tolerance = 1e-12)
  expect_error(morie_generalized_gamma_aft(c(0, d$time[-1]), d$event, d$X), "strictly positive")
})

test_that("the gamma frailty Cox is the frailty fit flagged as marginal", {
  d <- sx_data()
  cl <- rep(1:20, each = 6)
  a <- morie_gamma_frailty_cox(d$time, d$event, d$X, cl)
  b <- morie_cox_frailty(d$time, d$event, d$X, cl)
  expect_equal(a$beta, b$beta)
  expect_true(a$marginal_attenuation)
  expect_identical(a$method, "gamma_frailty_cox")
})
