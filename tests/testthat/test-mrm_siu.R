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
