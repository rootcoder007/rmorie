# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/mrm_primitives_synthetic_exposure.R

set.seed(2026L)

mk_synth_data <- function(n_survey = 500L, n_area = 20L) {
  x1 <- rnorm(n_survey)
  x2 <- rnorm(n_survey)
  p <- 1 / (1 + exp(-(-2 + 0.6 * x1 - 0.4 * x2)))
  y <- rbinom(n_survey, 1, p)
  survey <- data.frame(trait = y, x1 = x1, x2 = x2)
  area <- data.frame(
    x1 = rnorm(n_area), x2 = rnorm(n_area),
    pop = sample(800:1500, n_area, replace = TRUE)
  )
  rownames(area) <- paste0("area_", seq_len(n_area))
  list(survey = survey, area = area)
}

test_that("mrm_synthetic_area_exposure happy path", {
  d <- mk_synth_data()
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_true("exposure" %in% names(res))
  expect_length(res$exposure, 20L)
  expect_true(all(is.finite(res$exposure)))
})

test_that("mrm_synthetic_area_exposure with return_per_area_rate", {
  d <- mk_synth_data()
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop",
    return_per_area_rate = TRUE
  )
  expect_true("predicted_rate" %in% names(res))
})

test_that("mrm_synthetic_area_exposure custom fit_callable", {
  d <- mk_synth_data()
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop",
    fit_callable = function(X, y) c(0.0, 0.1, -0.1)
  )
  expect_s3_class(res, "morie_mrm_result")
})

test_that("mrm_synthetic_area_exposure handles logical trait", {
  d <- mk_synth_data()
  d$survey$trait <- as.logical(d$survey$trait)
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
})

test_that("mrm_synthetic_area_exposure handles character trait (yes/no)", {
  d <- mk_synth_data()
  d$survey$trait <- ifelse(d$survey$trait == 1, "yes", "no")
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
})

test_that("mrm_synthetic_area_exposure errors on bad fit_callable output", {
  d <- mk_synth_data()
  expect_error(
    mrm_synthetic_area_exposure(
      survey_df = d$survey,
      survey_trait_col = "trait",
      survey_covariate_cols = c("x1", "x2"),
      area_df = d$area,
      area_population_col = "pop",
      fit_callable = function(X, y) c(0.0, 0.1)  # wrong length
    ),
    "length"
  )
})

test_that("mrm_synthetic_area_exposure returns null-analysis when survey missing", {
  d <- mk_synth_data()
  d$survey$x2 <- NULL
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_equal(res$n, 0L)
})

test_that("mrm_synthetic_area_exposure returns null-analysis when area missing", {
  d <- mk_synth_data()
  d$area$x1 <- NULL
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_equal(res$n, 0L)
})

test_that("mrm_synthetic_area_exposure handles non-finite trait/cov rows", {
  d <- mk_synth_data()
  d$survey$x1[1:3] <- NA
  d$survey$trait[4] <- NA
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_true(any(grepl("dropped", res$warnings)))
})

test_that("mrm_synthetic_area_exposure constant trait returns null-analysis", {
  d <- mk_synth_data()
  d$survey$trait <- rep(0L, nrow(d$survey))
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_equal(res$n, 0L)
})

test_that("mrm_synthetic_area_exposure happy path on realistic survey size", {
  # Vee 2026-05-25: realistic survey panels for area-exposure
  # estimation are O(hundreds-to-thousands); n_survey=200 sits well
  # above the function's minimum sample-size guard and exercises the
  # primary estimation path.
  d <- mk_synth_data(n_survey = 200L)
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_gt(res$n, 0L)
})

test_that("mrm_synthetic_area_exposure too-few-survey-rows guard returns n=0", {
  # Edge-case: n=2 deliberately undershoots the survey-rows guard.
  # Paired with the realistic happy-path test above; this is the ONE
  # place we under-size on purpose.
  d <- mk_synth_data(n_survey = 2L)
  d$survey$trait <- c(0L, 1L)
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_equal(res$n, 0L)
})

test_that("mrm_synthetic_area_exposure warns on bad pop values", {
  d <- mk_synth_data()
  d$area$pop[1] <- -5
  d$area$pop[2] <- NA
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_true(any(grepl("populations", res$warnings)))
})

test_that("mrm_synthetic_area_exposure with rownames-less area_df", {
  d <- mk_synth_data()
  rownames(d$area) <- NULL
  res <- mrm_synthetic_area_exposure(
    survey_df = d$survey,
    survey_trait_col = "trait",
    survey_covariate_cols = c("x1", "x2"),
    area_df = d$area,
    area_population_col = "pop"
  )
  expect_s3_class(res, "morie_mrm_result")
  expect_true(!is.null(names(res$exposure)))
})
