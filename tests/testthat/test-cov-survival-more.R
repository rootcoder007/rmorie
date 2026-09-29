# Coverage tests for R/survival_more.R, checked against the survival
# package (survfit, coxph residuals, survreg, finegray) on small
# deterministic data.

sm_data <- function() {
  i <- 1:24
  x1 <- ((i * 7) %% 11) / 11 - 0.5
  x2 <- as.numeric(i %% 3 == 0)
  t <- round(5 * exp(-0.8 * x1 + 0.4 * x2) * (0.5 + ((i * 13) %% 17) / 17), 3)
  e <- as.integer(i %% 5 != 0)
  list(t = t, e = e, X = cbind(x1, x2))
}

test_that("RMST and its variance match survfit's restricted mean", {
  skip_if_not_installed("survival")
  d <- sm_data()
  r <- morie_survival_rmst(d$t, d$e, tau = 5)
  tab <- summary(survival::survfit(survival::Surv(d$t, d$e) ~ 1), rmean = 5)$table
  expect_equal(r$rmst, unname(tab["rmean"]), tolerance = 1e-12)
  expect_equal(r$se, unname(tab["se(rmean)"]), tolerance = 1e-9)
  expect_equal(r$upper - r$lower, 2 * qnorm(0.975) * r$se, tolerance = 1e-12)
  expect_false(r$tau_beyond_data)
  expect_true(morie_survival_rmst(d$t, d$e, tau = 50)$tau_beyond_data)
  expect_error(morie_survival_rmst(d$t, d$e, tau = 0), "positive")
  expect_error(morie_survival_rmst(d$t, d$e + 1L), "0 \\(censored\\)")
  g <- rep(c("a", "b"), 12)
  df <- morie_survival_rmst_diff(d$t, d$e, g, tau = 4)
  ra <- morie_survival_rmst(d$t[g == "a"], d$e[g == "a"], tau = 4)
  rb <- morie_survival_rmst(d$t[g == "b"], d$e[g == "b"], tau = 4)
  expect_equal(df$difference, ra$rmst - rb$rmst, tolerance = 1e-12)
  expect_equal(df$se, sqrt(ra$variance + rb$variance), tolerance = 1e-12)
  expect_equal(df$p_value, 2 * pnorm(-abs(df$z)), tolerance = 1e-12)
  cap <- min(max(d$t[g == "a"]), max(d$t[g == "b"]))
  expect_equal(morie_survival_rmst_diff(d$t, d$e, g, tau = 99)$tau, cap)
  expect_error(morie_survival_rmst_diff(d$t, d$e, rep(1:3, 8)), "exactly two groups")
})

test_that("martingale, deviance and Cox-Snell residuals equal coxph's at fixed beta", {
  skip_if_not_installed("survival")
  d <- sm_data()
  beta <- c(-0.6, 0.3)
  fit <- survival::coxph(survival::Surv(d$t, d$e) ~ d$X, init = beta, ties = "breslow",
    control = survival::coxph.control(iter.max = 0))
  m <- morie_survival_martingale(d$t, d$e, d$X, beta)
  expect_equal(m$residuals, unname(residuals(fit, "martingale")), tolerance = 1e-12)
  expect_equal(m$sum, sum(m$residuals), tolerance = 1e-12)
  dv <- morie_survival_deviance(d$t, d$e, d$X, beta)
  expect_equal(dv$residuals, unname(residuals(fit, "deviance")), tolerance = 1e-12)
  cs <- morie_survival_coxsnell(d$t, d$e, d$X, beta)
  expect_equal(cs$residuals, d$e - m$residuals, tolerance = 1e-12)
  na <- survival::survfit(survival::Surv(cs$residuals, d$e) ~ 1, ctype = 1)
  expect_equal(cs$diagnostic_h[length(cs$diagnostic_h)], max(na$cumhaz), tolerance = 1e-12)
  expect_error(morie_survival_martingale(d$t, d$e, d$X, 1), "one entry per column")
})

test_that("Schoenfeld residuals and their scaled form", {
  skip_if_not_installed("survival")
  d <- sm_data()
  ok <- !duplicated(d$t[d$e == 1])
  expect_true(all(ok))
  fit <- survival::coxph(survival::Surv(d$t, d$e) ~ d$X, ties = "breslow")
  b <- unname(coef(fit))
  V <- unname(vcov(fit))
  s <- morie_survival_schoenfeld(d$t, d$e, d$X, b, vcov = V)
  expect_equal(unname(s$residuals), unname(residuals(fit, "schoenfeld")), tolerance = 1e-9)
  sc <- t(b + nrow(s$residuals) * V %*% t(s$residuals))
  expect_equal(unname(s$scaled), sc, tolerance = 1e-12)
  expect_equal(s$ph_test[[1]]$rho, cor(s$time, sc[, 1]), tolerance = 1e-12)
  expect_equal(s$ph_test[[2]]$z, cor(s$time, sc[, 2]) * sqrt(length(s$time) - 1), tolerance = 1e-12)
  expect_null(morie_survival_schoenfeld(d$t, d$e, d$X, b, scaled = FALSE)$scaled)
  expect_error(morie_survival_schoenfeld(d$t, d$e, d$X, b), "vcov")
})

test_that("hazard ratios with log-scale intervals", {
  h <- morie_survival_hr(c(0.4, -0.2), c(0.1, 0.25), names = c("a", "b"))
  expect_equal(h$hazard_ratio, exp(c(0.4, -0.2)), tolerance = 1e-12)
  expect_equal(h$lower, exp(c(0.4, -0.2) - qnorm(0.975) * c(0.1, 0.25)), tolerance = 1e-12)
  expect_equal(h$p_value, 2 * pnorm(-abs(c(4, -0.8))), tolerance = 1e-12)
  expect_error(morie_survival_hr(1, -1), "negative")
  expect_error(morie_survival_hr(1:2, 1), "same length")
})

test_that("cumulative incidence is the Aalen-Johansen estimate", {
  skip_if_not_installed("survival")
  t <- c(1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  cause <- c(1, 2, 0, 1, 1, 2, 0, 1, 2, 1, 0, 2)
  r <- morie_survival_cif(t, cause, code = 1)
  aj <- survival::survfit(survival::Surv(t, factor(cause, 0:2)) ~ 1)
  pst <- aj$pstate[match(r$time, aj$time), 2]
  expect_equal(r$cif, pst, tolerance = 1e-12)
  expect_gt(r$naive_overstates_by, 0)
  expect_error(morie_survival_cif(t, cause, code = 0), "censoring")
  expect_error(morie_survival_cif(t, cause, code = 3), "does not occur")
})

test_that("Fine-Gray solves the IPCW subdistribution score equation", {
  i <- 1:30
  x <- ((i * 7) %% 13) / 13
  t <- round(2 + 6 * ((i * 11) %% 17) / 17 - 2 * x, 3)
  cause <- c(1, 2, 0)[(i %% 3) + 1]
  r <- morie_survival_finegray(t, cause, cbind(x), code = 1)
  expect_equal(r$subdistribution_hazard_ratio, exp(r$coef))
  expect_equal(r$n_competing, sum(cause == 2))
  skip_if_not_installed("survival")
  fg <- survival::finegray(survival::Surv(t, factor(cause, 0:2)) ~ x, data = data.frame(t, cause, x), etype = "1")
  cf <- survival::coxph(survival::Surv(fgstart, fgstop, fgstatus) ~ x, weights = fgwt, data = fg, ties = "breslow")
  # both weight the competing-event risk set by G(u)/G(t_i); survival's
  # G uses left limits, which can move the estimate by a small amount
  expect_equal(unname(r$coef), unname(coef(cf)), tolerance = 1e-3)
  expect_error(morie_survival_finegray(t, cause, cbind(x), code = 3), "does not occur")
})

test_that("left-truncated Kaplan-Meier equals the counting-process survfit", {
  skip_if_not_installed("survival")
  entry <- c(0, 0.5, 1, 0, 2, 1.5, 0.2, 3, 0, 1)
  time <- c(2, 3.5, 4, 1.5, 6, 5, 2.5, 7, 3, 4.5)
  ev <- c(1, 1, 0, 1, 1, 1, 0, 1, 1, 0)
  r <- morie_survival_left_truncated_km(entry, time, ev)
  sf <- survival::survfit(survival::Surv(entry, time, ev) ~ 1)
  expect_equal(r$surv, sf$surv[sf$n.event > 0], tolerance = 1e-12)
  expect_equal(r$n_risk, sf$n.risk[sf$n.event > 0])
  expect_error(morie_survival_left_truncated_km(time, time, ev), "strictly before")
})

test_that("landmark analysis resets the clock and drops early exits", {
  skip_if_not_installed("survival")
  d <- sm_data()
  r <- morie_survival_landmark(d$t, d$e, 2, X = d$X, group = rep(c("a", "b"), 12))
  keep <- which(d$t > 2)
  expect_equal(r$kept_index, keep)
  sf <- survival::survfit(survival::Surv(d$t[keep] - 2, d$e[keep]) ~ 1)
  expect_equal(r$km_surv, sf$surv[sf$n.event > 0], tolerance = 1e-12)
  expect_equal(nrow(r$X), length(keep))
  expect_setequal(names(r$by_group), c("a", "b"))
  expect_error(morie_survival_landmark(d$t, d$e, 100), "landmark leaves")
})

test_that("Turnbull NPMLE: exact data give the empirical masses", {
  x <- c(1, 2, 2, 4, 5)
  r <- morie_survival_turnbull(x, x)
  expect_equal(r$mass, c(0.2, 0.4, 0.2, 0.2), tolerance = 1e-9)
  expect_true(r$converged)
  L <- c(0, 1, 2, 2, 4, 0)
  R <- c(2, 3, 5, NA, 6, 1)
  tb <- morie_survival_turnbull(L, R)
  A <- t(vapply(seq_along(L), function(i) vapply(tb$intervals, function(v) as.numeric(L[i] <= v[1] && v[2] <= ifelse(is.na(R[i]), Inf, R[i])), 0), numeric(length(tb$intervals))))
  p <- tb$mass
  expect_equal(colSums(A * outer(1 / as.numeric(A %*% p), p)) / length(L), p, tolerance = 1e-8)
  expect_equal(tb$loglik, sum(log(A %*% p)), tolerance = 1e-12)
  expect_error(morie_survival_turnbull(2, 1), "must not exceed")
})

test_that("parametric fits and AFT equal survreg (log-time likelihood)", {
  skip_if_not_installed("survival")
  d <- sm_data()
  jac <- sum(log(d$t[d$e == 1]))
  # Nelder-Mead with reltol 1e-12 locates the optimum to ~1e-6
  for (dist in c("weibull", "lognormal", "loglogistic", "exponential")) {
    p <- morie_survival_parametric(d$t, d$e, dist = dist)
    sr <- survival::survreg(survival::Surv(d$t, d$e) ~ 1, dist = dist)
    expect_equal(p$loglik, sr$loglik[2] + jac, tolerance = 1e-6, info = dist)
    expect_equal(unname(p$coef), unname(coef(sr)), tolerance = 1e-4, info = dist)
  }
  w <- morie_survival_parametric(d$t, d$e, "weibull")
  ex <- morie_survival_parametric(d$t, d$e, "exponential")
  expect_equal(w$lr_vs_exponential, 2 * (w$loglik - ex$loglik), tolerance = 1e-12)
  expect_equal(w$weibull_shape, 1 / w$scale)
  a <- morie_survival_aft(d$t, d$e, d$X, dist = "weibull")
  sa <- survival::survreg(survival::Surv(d$t, d$e) ~ d$X, dist = "weibull")
  expect_equal(a$loglik, sa$loglik[2] + jac, tolerance = 1e-6)
  expect_equal(unname(a$beta), unname(coef(sa)[-1]), tolerance = 1e-3)
  expect_equal(a$hazard_ratio, exp(-a$beta / a$scale), tolerance = 1e-12)
  expect_error(morie_survival_parametric(d$t, d$e, "gamma"), "unknown distribution")
  cmp <- morie_survival_compare_parametric(d$t, d$e)
  expect_equal(cmp$table$aic, sort(cmp$table$aic))
  expect_equal(cmp$best_aic, cmp$table$dist[1])
  expect_equal(cmp$lr_weibull_vs_exponential, w$lr_vs_exponential, tolerance = 1e-9)
  expect_error(morie_survival_compare_parametric(d$t, d$e, dists = "gamma"), "no family could be fitted")
})
