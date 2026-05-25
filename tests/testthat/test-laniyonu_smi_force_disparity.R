# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/laniyonu_smi_force_disparity.R

set.seed(2026L)

mk_smi_data <- function(n_areas = 30L, n_years = 4L, n_survey = 500L) {
  geogs <- sprintf("T%03d", seq_len(n_areas))
  years <- 2020 + seq_len(n_years) - 1L
  grid <- expand.grid(tract_id = geogs, year = years, stringsAsFactors = FALSE)
  grid$pop_18plus <- sample(500:5000, nrow(grid), replace = TRUE)
  grid$poverty_rate <- runif(nrow(grid), 0.05, 0.45)
  grid$nonwhite_share <- runif(nrow(grid), 0.1, 0.8)
  grid$force_events <- rpois(nrow(grid), lambda = 2)
  grid$total_force_events <- grid$force_events + rpois(nrow(grid), lambda = 15)

  survey <- data.frame(
    smi = rbinom(n_survey, 1, 0.08),
    poverty_rate = runif(n_survey, 0, 1),
    nonwhite_share = runif(n_survey, 0, 1)
  )
  list(df = grid, survey = survey)
}

test_that("morie_laniyonu_smi_force_disparity runs with default total_force_events", {
  d <- mk_smi_data()
  res <- suppressWarnings(
    morie_laniyonu_smi_force_disparity(
      df = d$df,
      survey_df = d$survey,
      survey_trait_col = "smi",
      survey_covariate_cols = c("poverty_rate", "nonwhite_share"),
      max_iter = 50L
    )
  )
  expect_s3_class(res, "morie_laniyonu_smi_result")
  expect_true("alpha_v" %in% names(res))
  expect_true("intercept" %in% names(res))
  expect_true(res$n_events > 0L)
})

test_that("morie_laniyonu_smi_force_disparity with explicit non_smi_count_col", {
  d <- mk_smi_data()
  d$df$non_smi_force <- d$df$total_force_events - d$df$force_events
  res <- suppressWarnings(
    morie_laniyonu_smi_force_disparity(
      df = d$df,
      survey_df = d$survey,
      survey_trait_col = "smi",
      survey_covariate_cols = c("poverty_rate", "nonwhite_share"),
      non_smi_count_col = "non_smi_force",
      include_year_fe = FALSE,
      include_area_re = FALSE,
      max_iter = 50L
    )
  )
  expect_s3_class(res, "morie_laniyonu_smi_result")
})

test_that("morie_laniyonu_smi_force_disparity with return_design=TRUE", {
  d <- mk_smi_data()
  res <- suppressWarnings(
    morie_laniyonu_smi_force_disparity(
      df = d$df,
      survey_df = d$survey,
      survey_trait_col = "smi",
      survey_covariate_cols = c("poverty_rate"),
      include_year_fe = TRUE,
      include_area_re = TRUE,
      return_design = TRUE,
      max_iter = 30L
    )
  )
  expect_s3_class(res, "morie_laniyonu_smi_result")
  expect_true(!is.null(res$exposure_summary$X))
})

test_that("morie_laniyonu_smi_force_disparity errors on missing total_force_events", {
  d <- mk_smi_data()
  d$df$total_force_events <- NULL
  expect_error(
    suppressWarnings(morie_laniyonu_smi_force_disparity(
      df = d$df,
      survey_df = d$survey,
      survey_trait_col = "smi",
      survey_covariate_cols = c("poverty_rate"),
      max_iter = 20L
    )),
    "total_force_events"
  )
})

test_that("morie_laniyonu_smi_force_disparity errors on covariate mismatch", {
  d <- mk_smi_data()
  expect_error(
    suppressWarnings(morie_laniyonu_smi_force_disparity(
      df = d$df,
      survey_df = d$survey,
      survey_trait_col = "smi",
      survey_covariate_cols = c("poverty_rate", "nonwhite_share"),
      area_covariate_cols = c("poverty_rate"),  # length mismatch
      max_iter = 10L
    )),
    "same length"
  )
})

test_that("print.morie_laniyonu_smi_result emits header", {
  d <- mk_smi_data()
  res <- suppressWarnings(morie_laniyonu_smi_force_disparity(
    df = d$df, survey_df = d$survey,
    survey_trait_col = "smi",
    survey_covariate_cols = c("poverty_rate"),
    max_iter = 20L
  ))
  out <- capture.output(print(res))
  expect_true(any(nchar(out) > 0))
})
