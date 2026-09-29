# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage -- R/smatch_native.R (the SCCS fitted as an associated
# Poisson model, Whitaker et al. 2006). The design matrix is rebuilt
# from the interval cells, the fit is checked against glm(poisson) with
# per-individual factors AND against the conditional multinomial fit of
# sccsno, and the Sec. 7.6 sample size is recomputed from its formulas.

.sm_cases <- list(
  list(start = 0, end = 100, exposure = 20, events = c(25, 70)),
  list(start = 0, end = 100, exposure = 50, events = c(55, 58, 90)),
  list(start = 10, end = 90, exposure = 30, events = 35),
  list(start = 0, end = 100, exposure = 70, events = c(10, 75, 80)),
  list(start = 0, end = 100, exposure = 5, events = numeric(0)))
.sm_rp <- list(c(0, 14))

test_that("poisson_design has one row per non-empty cell and a factor per person", {
  d <- morie_smatch_poisson_design(.sm_cases, .sm_rp, age_breaks = 60)
  expect_equal(d$n_risk, 1L)
  expect_equal(d$n_age, 2L)
  expect_equal(d$n_people, 4L)
  expect_equal(ncol(d$X), 1L + 1L + 4L)
  expect_equal(nrow(d$X), d$n_rows)
  # every row carries exactly one individual indicator, and the offsets are
  # the log exposure times from the interval construction
  expect_equal(rowSums(d$X[, 3:6, drop = FALSE]), rep(1, d$n_rows))
  cells <- morie_sccsno_build_intervals(0, 100, 20, c(25, 70), matrix(c(0, 14), 1), 60)
  expect_equal(exp(d$offset[seq_len(nrow(cells))]), unname(cells[, "e"]), tolerance = 1e-12)
  expect_equal(d$y[seq_len(nrow(cells))], unname(cells[, "n"]))
  expect_equal(sum(d$y), 9)
  expect_error(morie_smatch_poisson_design(list(.sm_cases[[5]]), .sm_rp), "no case contributed an event")
})

test_that("sccs_poisson_fit equals glm(poisson) and the conditional multinomial fit", {
  d <- morie_smatch_poisson_design(.sm_cases, .sm_rp, age_breaks = 60)
  df <- data.frame(y = d$y, off = d$offset, risk = d$X[, 1], age = d$X[, 2],
                   who = factor(max.col(d$X[, 3:6, drop = FALSE])))
  g <- glm(y ~ risk + age + who - 1 + offset(off), poisson, data = df,
           control = list(epsilon = 1e-14, maxit = 200))
  for (fn in list(morie_smatch_sccs_poisson_fit, morie_smatch_selfcontrolledcaseseries,
                  morie_smatch_sccs_design, morie_smatch_sccsdesign)) {
    f <- fn(.sm_cases, .sm_rp, age_breaks = 60)
    expect_true(f$converged)
    expect_equal(f$log_ri, unname(coef(g)["risk"]), tolerance = 1e-7)
    expect_equal(f$age_effects, unname(coef(g)["age"]), tolerance = 1e-7)
    expect_equal(f$relative_incidence, exp(f$log_ri), tolerance = 1e-12)
    expect_equal(f$n_people, 4L)
  }
  # the point of the paper: the same estimate as the conditional likelihood
  cm <- morie_sccsno_fit(.sm_cases, matrix(c(0, 14), 1), age_breaks = 60)
  f <- morie_smatch_sccs_poisson_fit(.sm_cases, .sm_rp, age_breaks = 60)
  expect_equal(f$log_ri, cm$log_ri, tolerance = 1e-6)
  expect_equal(f$age_effects, cm$age_effects, tolerance = 1e-6)
  n0 <- morie_smatch_sccs_poisson_fit(.sm_cases, .sm_rp)
  expect_equal(n0$log_ri, morie_sccsno_fit(.sm_cases, matrix(c(0, 14), 1))$log_ri, tolerance = 1e-6)
})

test_that("sample_size, power and relative_efficiency follow Sec. 7.5-7.6", {
  b <- log(2)
  r <- 0.2
  p <- 0.5
  eb <- exp(b)
  den <- r * eb + 1 - r
  rho <- r * eb / den
  A <- 2 * (rho * b - log(den))
  B <- b^2 * rho * (1 - rho) / A
  C <- 1 + (1 - p) / (p * den)
  n <- (C / A) * (qnorm(0.975) + qnorm(0.8) * sqrt(B))^2
  s <- morie_smatch_sample_size(b, r, p)
  expect_equal(c(s$rho, s$A, s$B, s$C), c(rho, A, B, C), tolerance = 1e-12)
  expect_equal(s$n_events, n, tolerance = 1e-12)
  expect_equal(s$n_events_ceiling, as.integer(ceiling(n)))
  # more power needs more events; a bigger effect needs fewer
  expect_gt(morie_smatch_sample_size(b, r, p, power = 0.95)$n_events, n)
  expect_lt(morie_smatch_sample_size(log(4), r, p)$n_events, n)
  # the power at exactly that sample size is the power asked for
  pw <- morie_smatch_power(n, b, r, p)
  expect_equal(pw$power, 0.8, tolerance = 1e-9)
  expect_equal(pw$z_power, qnorm(0.8), tolerance = 1e-9)
  expect_lt(morie_smatch_power(n / 4, b, r, p)$power, 0.8)
  eff <- morie_smatch_relative_efficiency(r, b)
  expect_equal(eff$rho, rho, tolerance = 1e-12)
  expect_equal(eff$efficiency, 1 - rho, tolerance = 1e-12)
  # a shorter risk period keeps efficiency high
  expect_gt(morie_smatch_relative_efficiency(0.05, b)$efficiency, eff$efficiency)
  expect_error(morie_smatch_sample_size(0, r, p), "unbounded")
  expect_error(morie_smatch_sample_size(b, 1, p), "r must lie strictly")
  expect_error(morie_smatch_sample_size(b, r, 0), "p_exposed must lie")
  expect_error(morie_smatch_sample_size(b, r, p, alpha = 0), "alpha must lie")
  expect_error(morie_smatch_sample_size(b, r, p, power = 1), "power must lie")
  expect_error(morie_smatch_relative_efficiency(0, b), "r must lie strictly")
})

test_that("morie_smatch returns the design", {
  expect_equal(morie_smatch(.sm_cases, .sm_rp)$n_people, 4L)
})
