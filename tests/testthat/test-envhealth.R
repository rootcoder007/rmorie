# Pollution -> health pipeline: every formula recomputed in the test body and
# compared to the implementation; the Python arm's canonical tests mirrored.

test_that("PM2.5 IER: 1 at and below the counterfactual, monotone, Burnett form", {
  expect_equal(morie_envhealth_crf_pm25(5.8)$rr, 1)
  expect_equal(morie_envhealth_crf_pm25(2)$rr, 1)
  r <- morie_envhealth_crf_pm25(12)
  expect_equal(r$rr, 1 + 1.2 * (1 - exp(-0.34 * (12 - 5.8)^0.72)))
  expect_equal(r$log_rr, log(r$rr))
  expect_true(morie_envhealth_crf_pm25(30)$rr > r$rr)
  expect_equal(r$citation, "Burnett et al. (2014) EHP 122(4):397-403")
  v <- morie_envhealth_crf_pm25(c(5, 12, 20))
  expect_equal(v$rr, mean(v$extra$rr_per_unit))
  expect_length(v$extra$rr_per_unit, 3L)
  expect_error(morie_envhealth_crf_pm25(10, outcome = "nope"), "Unknown outcome")
})

test_that("NO2 log-linear: doubling the increment doubles log-RR, beta override", {
  expect_equal(morie_envhealth_crf_no2(10)$rr, 1)
  a <- morie_envhealth_crf_no2(20)
  b <- morie_envhealth_crf_no2(30)
  expect_equal(a$log_rr, 0.039)
  expect_equal(b$log_rr, 2 * a$log_rr)
  expect_equal(morie_envhealth_crf_no2(20, beta_per_10 = 0.1)$log_rr, 0.1)
  expect_equal(morie_envhealth_crf_no2(20, outcome = "respiratory")$log_rr, 0.029)
  expect_error(morie_envhealth_crf_no2(20, outcome = "nope"), "Unknown outcome")
})

test_that("PAF: Levin's formula and its boundaries", {
  expect_equal(morie_envhealth_attributable_fraction(1.5, 0), 0)
  expect_equal(morie_envhealth_attributable_fraction(1, 0.7), 0)
  expect_equal(morie_envhealth_attributable_fraction(2, 1), 1 - 1 / 2)
  expect_equal(morie_envhealth_attributable_fraction(1.5, 0.4), 0.4 * 0.5 / (1 + 0.4 * 0.5))
  expect_true(morie_envhealth_attributable_fraction(1.5, 0.6) > morie_envhealth_attributable_fraction(1.5, 0.3))
  expect_error(morie_envhealth_attributable_fraction(1.5, 1.2), "must be in")
})

test_that("mortality displaced: BenMAP form, linear in N, small-beta limit", {
  expect_equal(morie_envhealth_mortality_displaced(0, 1e6, 0.008, 0.0039), 0)
  one <- morie_envhealth_mortality_displaced(10, 1e6, 0.008, 0.0039)
  expect_equal(one, 0.008 * 1e6 * (1 - exp(-0.039)))
  expect_equal(morie_envhealth_mortality_displaced(10, 2e6, 0.008, 0.0039), 2 * one)
  expect_equal(morie_envhealth_mortality_displaced(1, 1e6, 0.008, 1e-5), 0.008 * 1e6 * 1e-5, tolerance = 1e-5)
  expect_error(morie_envhealth_mortality_displaced(1, -1, 0.008, 0.001), "non-negative")
})

test_that("burden chains CRF -> PAF -> cases for both pollutants", {
  b <- morie_envhealth_burden(25, 1, 0.008, 1e6, pollutant = "NO2")
  rr <- exp(0.039 * 1.5)
  expect_equal(b$extra$rr, rr)
  expect_equal(b$paf, (rr - 1) / rr)
  expect_equal(b$baseline_cases, 8000)
  expect_equal(b$attributable_cases, b$paf * 8000)
  p <- morie_envhealth_burden(12, 0.9, 0.005, 5e5, pollutant = "PM2.5")
  expect_equal(p$extra$rr, morie_envhealth_crf_pm25(12)$rr)
  expect_equal(morie_envhealth_burden(4, 1, 0.008, 1e6, pollutant = "PM2.5")$attributable_cases, 0)
  expect_error(morie_envhealth_burden(4, 1, 0.008, 1e6, pollutant = "O3"), "Unknown pollutant")
})

test_that("concentration index: zero, pro-poor, pro-rich; population covariance", {
  flat <- data.frame(exposure = rep(10, 20), income = 1:20)
  expect_equal(morie_envhealth_equity(flat, "exposure", "income")$concentration_index, 0)
  poor <- data.frame(exposure = 30:11, income = 1:20)
  e <- morie_envhealth_equity(poor, "exposure", "income")
  expect_true(e$concentration_index < 0)
  R <- (seq_len(20) - 0.5) / 20
  h <- 30:11
  expect_equal(e$concentration_index, 2 * (sum((h - mean(h)) * (R - mean(R))) / 20) / mean(h))
  expect_match(e$interpretation, "Pro-poor")
  rich <- data.frame(exposure = 11:30, income = 1:20)
  expect_true(morie_envhealth_equity(rich, "exposure", "income")$concentration_index > 0)
  expect_error(morie_envhealth_equity(poor[1, ], "exposure", "income"), "at least 2")
})

test_that("burden by area sorts worst first and rejects missing columns", {
  t <- data.frame(fsa = c("M5V", "M6H"), exposure = c(18, 28), population = c(60000, 40000), baseline_rate = 0.008)
  r <- morie_envhealth_burden_by_fsa(t)
  expect_equal(r$fsa, c("M6H", "M5V"))
  expect_equal(r$attributable_cases[1], morie_envhealth_burden(28, 1, 0.008, 40000)$attributable_cases)
  expect_error(morie_envhealth_burden_by_fsa(t[, 1:2]), "missing columns")
})

test_that("sensitivity wraps double ML with a percentile bootstrap", {
  set.seed(7)
  n <- 150
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$exposure <- 20 + 2 * d$x1 + rnorm(n)
  d$asthma <- 5 + 0.3 * d$exposure + d$x2 + rnorm(n)
  r <- morie_envhealth_sensitivity(d, outcome = "asthma", exposure = "exposure",
                                   confounders = c("x1", "x2"), n_bootstrap = 15L)
  expect_equal(r$n_bootstrap, 15L)
  expect_true(r$ci_lower_bs < r$ate && r$ate < r$ci_upper_bs)
  ols <- unname(stats::coef(stats::lm(asthma ~ exposure + x1 + x2, d))["exposure"])
  expect_lt(abs(r$ate - ols), 0.05)
})

test_that("verify-pollution: ok, assumption failure, csv and error paths", {
  ok <- morie_verify_pollution("no2", exposure_mean = 25, exposure_prevalence = 0.9)
  expect_equal(ok$status, "ok")
  expect_equal(attr(ok, "exit_status"), 0L)
  expect_equal(ok$pipeline$crf$rr, exp(0.039 * (25 - 5.8) / 10))
  expect_equal(ok$pipeline$paf, morie_envhealth_attributable_fraction(ok$pipeline$crf$rr, 0.9))
  expect_equal(ok$pipeline$displaced$deaths_displaced, 500 / 1e5 * 1e6 * (1 - 1 / ok$pipeline$crf$rr))
  bad <- morie_verify_pollution("pm25", exposure_mean = 2, exposure_prevalence = 0.5)
  expect_equal(bad$status, "assumption_failure")
  expect_equal(attr(bad, "exit_status"), 1L)
  d <- withr::local_tempdir()
  f <- file.path(d, "e.csv")
  utils::write.csv(data.frame(exposure = c(20, 30, 25), income = c(1, 3, 2)), f, row.names = FALSE)
  c <- morie_verify_pollution("no2", exposure_csv = f)
  expect_equal(c$status, "ok")
  expect_false(is.null(c$pipeline$equity))
  utils::write.csv(data.frame(x = 1), f, row.names = FALSE)
  expect_equal(attr(morie_verify_pollution("no2", exposure_csv = f), "exit_status"), 2L)
  expect_equal(attr(morie_verify_pollution("no2", exposure_csv = file.path(d, "none.csv")), "exit_status"), 2L)
  demo <- morie_verify_pollution("pm25", demo = TRUE)
  expect_equal(demo$status, "ok")
  expect_match(.envhealth_report_text(demo), "attributable deaths:   [0-9.]+")
})
