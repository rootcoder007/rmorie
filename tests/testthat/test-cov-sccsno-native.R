# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/sccsno_native.R (self-controlled case series,
# Farrington 1995). The conditional likelihood equals a Poisson model
# with one intercept per case and log exposure time offsets, so the
# fit is checked against glm(poisson) with person fixed effects; the
# interval cutting is derived by hand.

.sc_cases <- list(
  list(start = 0, end = 100, exposure = 20, events = c(25, 70)),
  list(start = 0, end = 100, exposure = 50, events = c(55, 58, 90)),
  list(start = 10, end = 90, exposure = 30, events = c(35)),
  list(start = 0, end = 100, exposure = NULL, events = c(40)),
  list(start = 0, end = 100, exposure = 70, events = c(10, 75, 80)),
  list(start = 0, end = 100, exposure = 5, events = numeric(0)))
.sc_rp <- matrix(c(0, 14), 1)

.sc_long <- function(age_breaks) {
  out <- NULL
  k <- 0
  for (cs in .sc_cases) {
    if (length(cs$events) == 0) next
    k <- k + 1
    m <- morie_sccsno_build_intervals(cs$start, cs$end, cs$exposure, cs$events, .sc_rp, age_breaks)
    out <- rbind(out, cbind(person = k, m))
  }
  as.data.frame(out)
}

test_that("build_intervals cuts follow-up at age breaks and risk-window ends", {
  m <- morie_sccsno_build_intervals(0, 100, 20, c(25, 70, 34), .sc_rp, c(50))
  # cuts 0, 20, 34, 50, 100: risk (20, 34], age band 1 from 50
  expect_equal(unname(m[, "e"]), c(20, 14, 16, 50))
  expect_equal(unname(m[, "risk_period"]), c(0, 1, 0, 0))
  expect_equal(unname(m[, "age_band"]), c(0, 0, 0, 1))
  expect_equal(unname(m[, "n"]), c(0, 2, 0, 1))
  z <- morie_sccsno_build_intervals(0, 10, NULL, 0, list(c(0, 3)), numeric(0))
  expect_equal(unname(z[, "n"]), 1)
  expect_error(morie_sccsno_build_intervals(5, 5, NULL, 1, .sc_rp, numeric(0)), "positive length")
  expect_error(morie_sccsno_build_intervals(0, 10, NULL, 1, matrix(c(3, 3), 1), numeric(0)), "b > a")
  expect_error(morie_sccsno_build_intervals(0, 10, 20, 1, .sc_rp, numeric(0)), "exposure at 20")
  expect_error(morie_sccsno_build_intervals(0, 10, 2, 11, .sc_rp, numeric(0)), "event at 11")
})

test_that("fit matches the Poisson model with person intercepts", {
  L <- .sc_long(c(60))
  gl <- glm(n ~ factor(person) + factor(risk_period) + factor(age_band) + offset(log(e)), poisson, data = L,
            control = list(epsilon = 1e-14, maxit = 100))
  for (fn in list(morie_sccsno_fit, morie_sccsno, morie_sccsno_sccsnoevent, morie_sccsno_sccs_no_replacement)) {
    f <- fn(.sc_cases, .sc_rp, age_breaks = 60)
    expect_true(f$converged)
    expect_equal(f$log_ri, unname(coef(gl)["factor(risk_period)1"]), tolerance = 1e-8)
    expect_equal(f$age_effects, unname(coef(gl)["factor(age_band)1"]), tolerance = 1e-8)
    expect_equal(f$se_log_ri, unname(sqrt(diag(vcov(gl)))["factor(risk_period)1"]), tolerance = 1e-6)
    expect_equal(f$n_cases, 5L)
    expect_equal(f$relative_incidence, exp(f$log_ri))
  }
  f0 <- morie_sccsno_fit(.sc_cases, .sc_rp)
  gl0 <- glm(n ~ factor(person) + factor(risk_period) + offset(log(e)), poisson, data = .sc_long(numeric(0)),
             control = list(epsilon = 1e-14, maxit = 100))
  expect_equal(f0$log_ri, unname(coef(gl0)["factor(risk_period)1"]), tolerance = 1e-8)
  expect_error(morie_sccsno_fit(.sc_cases, .sc_rp, age_breaks = c(60, 20)), "increasing")
  expect_error(morie_sccsno_fit(list(.sc_cases[[6]]), .sc_rp), "no case contributed")
  expect_error(morie_sccsno_fit(.sc_cases, matrix(numeric(0), 0, 2)), "at least one risk period")
})

test_that("loglik is the conditional multinomial log-likelihood", {
  L <- .sc_long(c(60))
  par <- c(0.4, -0.2)
  ex <- 0
  for (k in unique(L$person)) {
    d <- L[L$person == k, ]
    lin <- par[1] * (d$risk_period == 1) + par[2] * (d$age_band == 1)
    ex <- ex + sum(d$n * lin) - sum(d$n) * log(sum(d$e * exp(lin)))
  }
  cells <- lapply(split(L[, -1], L$person), as.matrix)
  expect_equal(morie_sccsno_loglik(par, cells, 1L, 2L), ex, tolerance = 1e-12)
  # without age bands: sum n (b r) - N log(sum e exp(b r)) per person
  L0 <- .sc_long(numeric(0))
  c0 <- lapply(split(L0[, -1], L0$person), as.matrix)
  ex0 <- sum(vapply(split(L0, L0$person), function(d) {
    sum(d$n * 0.3 * d$risk_period) - sum(d$n) * log(sum(d$e * exp(0.3 * d$risk_period)))
  }, 0))
  expect_equal(morie_sccsno_loglik(0.3, c0, 1L, 1L), ex0, tolerance = 1e-12)
})

test_that("relative_incidence gives Wald intervals; check_assumptions reads a pre window", {
  f <- morie_sccsno_fit(.sc_cases, .sc_rp)
  ri <- morie_sccsno_relative_incidence(f, 0.9)
  z <- qnorm(0.95)
  expect_equal(ri$intervals[[1]]$lower, exp(f$log_ri - z * f$se_log_ri), tolerance = 1e-12)
  expect_equal(ri$intervals[[1]]$upper, exp(f$log_ri + z * f$se_log_ri), tolerance = 1e-12)
  ca <- morie_sccsno_check_assumptions(list(relative_incidence = c(1.1, 3)), 0, tol = 0.25)
  expect_true(ca$consistent_with_design)
  expect_false(morie_sccsno_check_assumptions(list(relative_incidence = c(1.1, 3)), 1)$consistent_with_design)
})

test_that("morie_sccsno_cheatsheet names the cancellation", {
  expect_match(morie_sccsno_cheatsheet(), "cancels phi_i exactly", fixed = TRUE)
})
