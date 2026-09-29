# Coverage for the epidemiologic ratio and difference measures (Rothman,
# Greenland & Lash): incidence rate ratio with its log-scale Wald CI, the
# Mantel-Haenszel rate ratio with the Greenland-Robins (1985) variance,
# and risk difference / risk ratio with Wald intervals; z values are
# checked against qnorm.

test_that("incidence rate ratio and its CI", {
  r <- Incrtio(0.012, 0.004, cases_exposed = 24, cases_unexposed = 16, confidence = 0.9)
  se <- sqrt(1 / 24 + 1 / 16)
  expect_equal(r$estimate, 3, tolerance = 1e-12)
  expect_equal(r$se_ln, se, tolerance = 1e-12)
  expect_equal(c(r$ci_lower, r$ci_upper), 3 * exp(c(-1, 1) * stats::qnorm(0.95) * se), tolerance = 1e-12)
  expect_null(Incrtio(1, 2)$ci_lower)
  expect_error(Incrtio(1, 0), "non-zero")
  expect_error(Incrtio(1, 2, 0, 3), "positive for a CI")
  expect_error(Incrtio(1, 2, 3, 3, confidence = 0.8), "one of 0.90, 0.95, 0.99")
})

test_that("Mantel-Haenszel rate ratio with the Greenland-Robins variance", {
  st <- list(c(a = 10, T1 = 1000, b = 5, T0 = 1500), list(12, 800, 9, 2000), c(3, 400, 1, 600))
  m <- Mhrate(st, confidence = 0.99)
  S <- rbind(c(10, 1000, 5, 1500), c(12, 800, 9, 2000), c(3, 400, 1, 600))
  Tt <- S[, 2] + S[, 4]
  num <- sum(S[, 1] * S[, 4] / Tt)
  den <- sum(S[, 3] * S[, 2] / Tt)
  v <- sum((S[, 1] + S[, 3]) * S[, 2] * S[, 4] / Tt^2) / (num * den)
  expect_equal(m$estimate, num / den, tolerance = 1e-12)
  expect_equal(m$se_ln, sqrt(v), tolerance = 1e-12)
  expect_equal(m$ci_upper, num / den * exp(stats::qnorm(0.995) * sqrt(v)), tolerance = 1e-12)
  expect_identical(m$n_strata, 3L)
  one <- Mhrate(list(c(10, 1000, 5, 1500)))
  expect_equal(one$estimate, (10 / 1000) / (5 / 1500), tolerance = 1e-12)
  expect_error(Mhrate(list()), "at least one stratum")
  expect_error(Mhrate(list(c(1, 2, 3))), "needs \\(a, T1, b, T0\\)")
  expect_error(Mhrate(list(c(0, 10, 3, 10))), "both arms need cases")
  expect_error(Mhrate(list(c(1, 0, 3, 0))), "person-time must be positive")
})

test_that("risk difference and risk ratio with Wald intervals", {
  d <- Riskdf(0.3, 0.2, 100, 150)
  se <- sqrt(0.3 * 0.7 / 100 + 0.2 * 0.8 / 150)
  expect_equal(d$estimate, 0.1, tolerance = 1e-12)
  expect_equal(d$se, se, tolerance = 1e-12)
  expect_equal(d$ci_lower, 0.1 - stats::qnorm(0.975) * se, tolerance = 1e-12)
  expect_null(Riskdf(0.3, 0.2)$se)
  expect_error(Riskdf(1.2, 0.2), "\\[0, 1\\]")
  expect_error(Riskdf(0.3, 0.2, 0, 10), "arm sizes must be positive")
  r <- Riskrt(0.3, 0.2, 100, 150)
  sl <- sqrt(0.7 / 30 + 0.8 / 30)
  expect_equal(r$estimate, 1.5, tolerance = 1e-12)
  expect_equal(r$se_ln, sl, tolerance = 1e-12)
  expect_equal(r$ci_upper, 1.5 * exp(stats::qnorm(0.975) * sl), tolerance = 1e-12)
  expect_error(Riskrt(0.3, 0), "non-zero")
  expect_error(Riskrt(0, 0.2, 10, 10), "positive for a CI")
  expect_error(Riskrt(0.3, 0.2, 10, -1), "arm sizes must be positive")
})
