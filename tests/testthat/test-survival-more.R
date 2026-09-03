# The fifteen survival methods restored after de-externalization.
#
# Expected values are computed here from the definitions, or are exact
# properties of the estimators (martingale residuals sum to zero; the
# naive one-minus-KM overstates the cumulative incidence; an AFT time
# ratio above 1 must pair with a hazard ratio below 1).

ST <- c(5, 6, 6, 2.5, 4, 4, 3, 3, 1, 2, 2, 3, 7, 8, 9, 10)
SE <- c(1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1)
SX <- matrix((seq_along(ST) - 1) %% 2, ncol = 1)
SC <- c(1, 0, 2, 1, 1, 0, 2, 1, 1, 1, 0, 2, 1, 0, 1, 2)

test_that("RMST is the area under the KM curve", {
  r <- Rmst(ST, SE, tau = 8)
  # recompute the area independently from the KM steps
  km <- .ms_km(ST, SE)
  area <- 0
  pt <- 0
  ps <- 1
  for (i in seq_along(km$time)) {
    if (km$time[i] >= 8) break
    area <- area + ps * (km$time[i] - pt)
    pt <- km$time[i]
    ps <- km$surv[i]
  }
  area <- area + ps * (8 - pt)
  expect_equal(r$rmst, area)
  expect_gt(r$se, 0)
  expect_lt(r$lower, r$rmst)
  expect_gt(r$upper, r$rmst)
})

test_that("RMST flags a horizon past the data", {
  expect_true(Rmst(ST, SE, tau = 99)$tau_beyond_data)
  expect_false(Rmst(ST, SE, tau = 5)$tau_beyond_data)
  expect_error(Rmst(ST, SE, tau = 0), "positive")
})

test_that("RMST of a censoring-free sample is the mean up to tau", {
  t <- c(1, 2, 3, 4)
  e <- c(1, 1, 1, 1)
  # S steps 0.75, 0.5, 0.25, 0; area to tau = 4 is 1 + .75 + .5 + .25
  expect_equal(Rmst(t, e, tau = 4)$rmst, 2.5)
})

test_that("rmst_diff uses a common horizon and adds variances", {
  g <- c(rep(0, 8), rep(1, 8))
  r <- Rmstdiff(ST, SE, g)
  a <- Rmst(ST[1:8], SE[1:8], tau = r$tau)
  b <- Rmst(ST[9:16], SE[9:16], tau = r$tau)
  expect_equal(r$difference, a$rmst - b$rmst)
  expect_equal(r$se, sqrt(a$variance + b$variance))
  expect_true(Rmstdiff(ST, SE, g, tau = 99)$tau_capped)
  expect_error(Rmstdiff(ST, SE, rep(1, 16)), "exactly two")
})

test_that("martingale residuals sum to zero", {
  m <- Martingale(ST, SE, SX, 0.3)
  expect_equal(m$sum, 0, tolerance = 1e-8)
  expect_true(m$sums_to_zero)
  expect_lte(m$max, 1)
  expect_equal(length(m$residuals), length(ST))
})

test_that("Cox-Snell residuals are delta minus the martingale", {
  m <- Martingale(ST, SE, SX, 0.3)
  cs <- Coxsnell(ST, SE, SX, 0.3)
  expect_equal(cs$residuals, SE - m$residuals, tolerance = 1e-12)
  expect_true(all(cs$residuals >= 0))
})

test_that("deviance residuals are the symmetrized martingale", {
  d <- Devresid(ST, SE, SX, 0.3)
  m <- d$martingale
  expect_equal(sign(d$residuals), ifelse(m >= 0, 1, -1))
  expect_false(d$is_model_deviance)
  # less skewed than the martingale residuals they transform
  expect_lt(abs(mean(d$residuals^3)) / mean(d$residuals^2)^1.5,
            abs(mean(m^3)) / mean(m^2)^1.5)
})

test_that("Schoenfeld residuals and the PH test", {
  s <- Schoenfeld(ST, SE, SX, 0.3, vcov = matrix(0.25, 1, 1))
  expect_equal(nrow(s$residuals), length(unique(ST[SE == 1])))
  expect_equal(length(s$time), nrow(s$residuals))
  expect_false(is.null(s$ph_test))
  expect_true(abs(s$ph_test[[1]]$rho) <= 1)
  expect_error(Schoenfeld(ST, SE, SX, 0.3), "vcov")
  u <- Schoenfeld(ST, SE, SX, 0.3, scaled = FALSE)
  expect_null(u$scaled)
})

test_that("hazard ratios are exponentiated from the log scale", {
  r <- Hazratio(c(0.5, -0.2), c(0.2, 0.1))
  expect_equal(r$hazard_ratio, exp(c(0.5, -0.2)))
  expect_equal(r$lower, exp(c(0.5, -0.2) - stats::qnorm(0.975) * c(0.2, 0.1)))
  expect_true(all(r$lower > 0))          # cannot cross zero
  # asymmetric about the HR, unlike HR +/- z se
  expect_false(isTRUE(all.equal(r$hazard_ratio - r$lower,
                                r$upper - r$hazard_ratio)))
  expect_error(Hazratio(0.5, -0.1), "cannot be negative")
})

test_that("the CIF is below the naive one-minus-KM", {
  r <- Cif(ST, SC, code = 1)
  expect_lt(tail(r$cif, 1), tail(r$naive_one_minus_km, 1))
  expect_gt(r$naive_overstates_by, 0)
  expect_true(all(diff(r$cif) >= -1e-12))     # monotone
  expect_true(all(r$cif >= 0 & r$cif <= 1))
  expect_error(Cif(ST, SC, code = 0), "censoring")
  expect_error(Cif(ST, SC, code = 9), "does not occur")
})

test_that("CIFs over all causes sum to one minus overall survival", {
  r1 <- Cif(ST, SC, code = 1)
  r2 <- Cif(ST, SC, code = 2)
  expect_equal(tail(r1$cif, 1) + tail(r2$cif, 1),
               1 - r1$overall_survival_at_end, tolerance = 1e-9)
})

test_that("Fine-Gray fits the subdistribution hazard", {
  r <- Finegray(ST, SC, SX, code = 1)
  expect_equal(length(r$coef), 1L)
  expect_equal(r$subdistribution_hazard_ratio, exp(r$coef))
  expect_true(r$differs_from_cause_specific)
  expect_gt(r$n_competing, 0)
})

test_that("left truncation changes the risk set", {
  en <- c(0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 2, 2, 2, 2)
  r <- Ltkm(en, ST, SE)
  expect_equal(length(r$surv), length(r$ignoring_truncation))
  expect_gt(r$max_difference, 0)             # truncation matters here
  expect_true(all(r$n_risk <= length(ST)))
  expect_error(Ltkm(ST, ST, SE), "strictly before")
})

test_that("left truncation with entry at zero reproduces plain KM", {
  r <- Ltkm(rep(0, length(ST)), ST, SE)
  expect_equal(r$surv, .ms_km(ST, SE)$surv, tolerance = 1e-12)
  expect_equal(r$max_difference, 0, tolerance = 1e-12)
})

test_that("the landmark drops early subjects and resets the clock", {
  r <- Landmark(ST, SE, 3)
  expect_equal(r$n_retained, sum(ST > 3))
  expect_equal(r$n_dropped, length(ST) - sum(ST > 3))
  expect_true(all(r$time > 0))
  expect_equal(r$time, ST[ST > 3] - 3)
  expect_true(r$conditional_on_surviving_to_landmark)
  expect_error(Landmark(ST, SE, 9.5), "leaves")
})

test_that("Turnbull converges and puts unit mass on its intervals", {
  r <- Turnbull(c(0, 1, 2, 3, 1, 2), c(2, 3, 4, Inf, 2, 5))
  expect_true(r$converged)
  expect_equal(sum(r$mass), 1, tolerance = 1e-8)
  expect_true(all(r$mass >= -1e-12))
  expect_true(all(diff(r$surv) <= 1e-12))    # nonincreasing
  expect_true(r$npmle_not_unique_within_intervals)
})

test_that("Turnbull on exact observations matches the empirical CDF", {
  x <- c(1, 2, 3, 4)
  r <- Turnbull(x, x)
  expect_equal(sum(r$mass), 1, tolerance = 1e-8)
  expect_equal(r$n_intervals, 4L)
  expect_equal(r$mass, rep(0.25, 4), tolerance = 1e-6)
})

test_that("parametric fits rank the families and test the exponential", {
  w <- Parasurv(ST, SE, "weibull")
  ex <- Parasurv(ST, SE, "exponential")
  expect_gte(w$loglik, ex$loglik)            # exponential is nested
  expect_equal(w$lr_vs_exponential, 2 * (w$loglik - ex$loglik),
               tolerance = 1e-6)
  expect_true(ex$fixed_scale)
  expect_false(w$fixed_scale)
  expect_error(Parasurv(ST, SE, "gompertz"), "unknown")
})

test_that("AFT time ratios and hazard ratios point opposite ways", {
  a <- Aftfit(ST, SE, SX, "weibull")
  expect_equal(a$time_ratio, exp(a$beta))
  expect_true(a$ph_equivalent)
  expect_equal(a$hazard_ratio, exp(-a$beta / a$scale))
  # a covariate that lengthens survival must lower the hazard
  expect_equal(a$time_ratio > 1, a$hazard_ratio < 1)
  ln <- Aftfit(ST, SE, SX, "lognormal")
  expect_false(ln$ph_equivalent)
  expect_null(ln$hazard_ratio)
})

test_that("compare_parametric ranks by AIC and reports failures", {
  r <- Paracompare(ST, SE)
  expect_equal(nrow(r$table), 4L)
  expect_equal(r$table$aic, sort(r$table$aic))
  expect_true(r$best_aic %in% r$table$dist)
  expect_true(r$families_not_nested)
  expect_false(is.null(r$lr_weibull_vs_exponential))
  expect_equal(length(r$failed), 0L)
})

test_that("every restored method is exported under both spellings", {
  canonical <- c("Rmst", "Rmstdiff", "Martingale", "Devresid", "Coxsnell",
                 "Schoenfeld", "Hazratio", "Cif", "Finegray", "Ltkm",
                 "Landmark", "Turnbull", "Parasurv", "Aftfit", "Paracompare")
  legacy <- paste0("morie_survival_",
                   c("rmst", "rmst_diff", "martingale", "deviance", "coxsnell",
                     "schoenfeld", "hr", "cif", "finegray", "left_truncated_km",
                     "landmark", "turnbull", "parametric", "aft",
                     "compare_parametric"))
  for (i in seq_along(canonical)) {
    expect_true(is.function(get(canonical[i])), info = canonical[i])
    # the pre-policy spelling is the same object, not a reimplementation
    expect_identical(get(legacy[i]), get(canonical[i]), info = legacy[i])
  }
})
