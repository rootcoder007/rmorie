# Attributable risk from case-control data (Bruzzi et al. 1985; Levin
# 1953): the case-based adjusted AR, Levin's formula, partial ARs, rate
# ratios from a weighted logistic fit against glm, and the simulation
# interval replayed on the shared stream.

test_that("Bruzzi's AR is 1 - sum_j rho_j / RR_j and agrees with Levin for one exposure", {
  cc <- c(30, 50, 20)
  rr <- c(1, 2, 4)
  r <- morie_rapaf_population_attributable_risk(cc, rr)
  expect_equal(r$ar, 1 - sum((cc / 100) / rr), tolerance = 1e-15)
  # one binary exposure: Levin with the case-implied prevalence agrees
  p <- 0.3
  R <- 2.5
  cases <- c((1 - p), p * R)
  expect_equal(morie_rapaf_population_attributable_risk(cases, c(1, R))$ar,
               morie_rapaf_levin_ar(p, R), tolerance = 1e-14)
  expect_equal(morie_rapaf_levin_ar(p, R), p * (R - 1) / (1 + p * (R - 1)), tolerance = 1e-15)
  expect_error(morie_rapaf_population_attributable_risk(cc, rr[-1]), "3 strata of cases but 2")
  expect_error(morie_rapaf_population_attributable_risk(1, 1), "at least 2 strata")
  expect_error(morie_rapaf_population_attributable_risk(c(0, 0), c(1, 2)), "no cases")
  expect_error(morie_rapaf_population_attributable_risk(cc, c(1, 0, 2)), "positive")
  expect_error(morie_rapaf_levin_ar(2, 1), "prevalence")
  expect_error(morie_rapaf_levin_ar(0.5, 0), "positive")
  expect_identical(morie_rapaf, morie_rapaf_population_attributable_risk)
})

test_that("partial ARs move each stratum to its baseline target", {
  cc <- c(10, 20, 30, 40)
  rr <- c(1, 2, 3, 6)
  bm <- c(0, 0, 2, 2)
  r <- morie_rapaf_partial_ar(cc, rr, bm)
  expect_equal(r$ar, 1 - sum(cc / 100 * rr[bm + 1] / rr), tolerance = 1e-15)
  expect_equal(morie_rapaf_partial_ar(cc, rr, rep(0, 4))$ar,
               morie_rapaf_population_attributable_risk(cc, rr)$ar, tolerance = 1e-15)
  expect_error(morie_rapaf_partial_ar(cc, rr, c(0, 0, 4, 0)), "out of range")
  expect_error(morie_rapaf_partial_ar(cc, rr, 0), "1 baseline targets for 4")
})

test_that("rate ratios from the weighted logistic fit match glm odds ratios", {
  ca <- c(20, 35, 15, 30)
  co <- c(80, 60, 20, 15)
  D <- cbind(c(0, 1, 0, 1), c(0, 0, 1, 1))
  r <- morie_rapaf_rate_ratios_from_logit(ca, co, D)
  f <- stats::glm(cbind(ca, co) ~ D, family = stats::binomial())
  lin <- as.numeric(cbind(1, D) %*% stats::coef(f))
  expect_equal(r$rate_ratios, exp(lin - lin[1]), tolerance = 1e-6)
  expect_error(morie_rapaf_rate_ratios_from_logit(ca, co[-1], D), "agree in length")
  expect_error(morie_rapaf_rate_ratios_from_logit(c(0, 0), c(0, 0), D[1:2, ]), "no stratum")
})

test_that("the AR interval perturbs log rate ratios and takes quantiles", {
  cc <- c(30, 50, 20)
  rr <- c(1, 2, 4)
  se <- c(0, 0.2, 0.3)
  r <- morie_rapaf_ar_confidence_interval(cc, rr, se, level = 0.9, draws = 200, seed = 5)
  e <- .ghc_rng(5)
  v <- sort(vapply(1:200, function(k) 1 - sum((cc / 100) / exp(log(rr) + se * .ghc_norm(e, 3L))), 1))
  lo <- (1 - 0.9) / 2
  expect_equal(c(r$lower, r$upper), c(v[as.integer(lo * 200)], v[as.integer((1 - lo) * 200)]),
               tolerance = 1e-15)
  expect_equal(r$estimate, 1 - sum((cc / 100) / rr), tolerance = 1e-15)
  expect_error(morie_rapaf_ar_confidence_interval(cc, rr, se[-1]), "2 standard errors for 3")
  expect_error(morie_rapaf_ar_confidence_interval(cc, rr, -se), "non-negative")
  expect_error(morie_rapaf_ar_confidence_interval(cc, rr, se, level = 1), "level")
})
