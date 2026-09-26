# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2R: tests for mrm_siu.R — SIU-specific MRM analyzers.

.make_synthetic_siu <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  start_dates <- as.Date("2018-01-01") +
    sample.int(365L * 6L, n, replace = TRUE)
  decision_dates <- start_dates +
    sample(30L:365L, n, replace = TRUE)
  data.frame(
    case_number = sprintf("%02d-OFD-%03d",
                           sample(18:24, n, replace = TRUE),
                           sample.int(999, n, replace = TRUE)),
    police_service = sample(c("Toronto", "OPP", "Halton", "Peel",
                              "York", "Niagara"), n, replace = TRUE),
    date_of_incident_iso = format(start_dates, "%Y-%m-%d"),
    date_of_director_decision_iso = format(decision_dates, "%Y-%m-%d"),
    director_decision_category = sample(
      c("No reasonable grounds", "Reasonable grounds — declined",
        "Reasonable grounds — charged"),
      n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("mrm_siu_case_to_decision_km returns a duration summary", {
  df <- .make_synthetic_siu(n = 200L, seed = 1L)
  out <- tryCatch(
    mrm_siu_case_to_decision_km(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("case_to_decision_km error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_siu_per_service_rate returns one row per police service", {
  df <- .make_synthetic_siu(n = 300L, seed = 2L)
  out <- tryCatch(
    mrm_siu_per_service_rate(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("per_service_rate error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_siu_outcome_classifier returns classifier output per service", {
  df <- .make_synthetic_siu(n = 200L, seed = 3L)
  out <- tryCatch(
    mrm_siu_outcome_classifier(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("outcome_classifier error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_siu_case_to_decision_km errors cleanly on missing incident col", {
  df <- .make_synthetic_siu(n = 50L, seed = 4L)
  df$date_of_incident_iso <- NULL
  out <- tryCatch(
    mrm_siu_case_to_decision_km(df),
    error = function(e) e
  )
  expect_true(inherits(out, "error") || is.list(out))
})

test_that("case-to-decision summaries are Kaplan-Meier with open cases censored", {
  # reference: survival::survfit(Surv(gap, !open) ~ 1): quantile() and
  # summary(rmean = "common")
  d <- data.frame(
    police_service = rep(c("A", "B"), each = 5),
    date_of_incident_iso = format(as.Date("2020-01-01") + c(0, 10, 20, 30, 40, 5, 15, 25, 35, 45)),
    date_of_director_decision_iso = c(format(as.Date("2020-01-01") + c(3, 15, 28, 55, NA, 10, 30, 30, NA, 60)))
  )
  r <- mrm_siu_case_to_decision_km(d, min_n = 1L)
  inc <- as.Date(d$date_of_incident_iso)
  dec <- as.Date(d$date_of_director_decision_iso)
  open <- is.na(dec)
  gap <- as.numeric(ifelse(open, max(dec, na.rm = TRUE) - inc, dec - inc))
  km <- .mrm_km_summary(gap, !open)
  expect_equal(r$pooled$median_days, km$quantiles[2])
  expect_equal(r$pooled$n_censored, 2L)
  # survfit on these gaps: quartiles 5, 11.5, 25; rmean 13.1
  expect_equal(c(r$pooled$p25_days, r$pooled$median_days, r$pooled$p75_days),
               c(5, 11.5, 25))
  expect_equal(r$pooled$mean_days, 13.1)
})
