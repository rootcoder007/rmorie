# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/dataset.R -- NOIR profiling + roles + plan.

set.seed(1)

test_that("infer_level: character without ordinal hit is nominal", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(c("a", "b", "c")), "nominal")
  expect_equal(morie_dataset_infer_level(c("a", "b"), name = "city"), "nominal")
})

test_that("infer_level: small character with ordinal name -> ordinal", {
  set.seed(1)
  x <- c("low", "med", "high")
  expect_equal(morie_dataset_infer_level(x, name = "satisfaction_scale"), "ordinal")
})

test_that("infer_level: logical -> nominal", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(c(TRUE, FALSE, NA)), "nominal")
})

test_that("infer_level: numeric binary -> nominal", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(c(0, 1, 1, 0)), "nominal")
})

test_that("infer_level: numeric with ordinal name + few unique -> ordinal", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(c(1, 2, 3, 4, 5), name = "likert_q1"), "ordinal")
})

test_that("infer_level: double with interval name -> interval", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(c(2001.0, 2002.0, 2003.5), name = "year_obs"), "interval")
})

test_that("infer_level: positive doubles default to ratio", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(c(1.1, 2.2, 3.3)), "ratio")
})

test_that("infer_level: Date column -> interval", {
  set.seed(1)
  expect_equal(morie_dataset_infer_level(as.Date(c("2020-01-01", "2021-02-02"))), "interval")
})

test_that("detect_role recognises id/weight/treatment/outcome", {
  set.seed(1)
  expect_equal(morie_dataset_detect_role(1:5, "case_id"), "id")
  expect_equal(morie_dataset_detect_role(c(1.1, 2.2), "survey_wt"), "weight")
  expect_equal(morie_dataset_detect_role(c(0, 1), "treatment_arm"), "treatment")
  expect_equal(morie_dataset_detect_role(c(0, 1, 0), "outcome_event"), "outcome")
  expect_equal(morie_dataset_detect_role(c(1, 2, 3), "foo"), "covariate")
  expect_equal(morie_dataset_detect_role(c(1, 2), "stratum_psu"), "stratum")
  expect_equal(morie_dataset_detect_role(c(1, 2), "cluster_id"), "id")
})

test_that("summarize_column returns mean/sd for ratio numerics", {
  set.seed(1)
  out <- morie_dataset_summarize_column(c(1, 2, 3, 4, 5), "ratio")
  expect_true(all(c("mean", "std", "min", "max") %in% names(out)))
  expect_equal(out$mean, 3)
})

test_that("summarize_column returns top_counts for nominal", {
  set.seed(1)
  out <- morie_dataset_summarize_column(c("a", "b", "a", "c"), "nominal")
  expect_true("top_counts" %in% names(out))
})

test_that("column_profile composes everything", {
  set.seed(1)
  cp <- morie_dataset_column_profile(c(1, 2, 3, NA), "year_obs")
  expect_true(all(c("name", "dtype", "level", "n_unique", "missing_pct",
                    "is_binary", "is_constant", "suggested_role", "summary_stats") %in% names(cp)))
  expect_equal(cp$name, "year_obs")
  expect_gt(cp$missing_pct, 0)
})

test_that("profile validates input type + shape", {
  set.seed(1)
  expect_error(morie_dataset_profile(list()), "data.frame")
  expect_error(morie_dataset_profile(data.frame()), "at least one row")
})

test_that("profile returns suggested treatment+outcome+weight when present", {
  set.seed(1)
  df <- data.frame(
    case_id = 1:6,
    treat = c(0, 1, 0, 1, 0, 1),
    outcome_y = c(1.1, 2.2, 1.4, 3.0, 0.9, 2.8),
    survey_wt = c(1, 1, 1, 1, 1, 1),
    age = c(20, 25, 30, 35, 40, 45)
  )
  p <- morie_dataset_profile(df)
  expect_s3_class(p, "morie_dataset_profile")
  expect_equal(p$n_rows, 6L)
  expect_equal(p$n_cols, 5L)
  expect_true(!is.null(p$suggested_treatment))
  expect_true(!is.null(p$suggested_outcome))
  expect_true(!is.null(p$suggested_weights))
})

test_that("profile honours hints", {
  set.seed(1)
  df <- data.frame(a = c(0, 1, 0, 1), b = c(1.1, 2.2, 3.3, 4.4))
  p <- morie_dataset_profile(df, hint_treatment = "a", hint_outcome = "b")
  expect_equal(p$suggested_treatment, "a")
  expect_equal(p$suggested_outcome, "b")
})

test_that("profile_to_list yields plain nested list", {
  set.seed(1)
  df <- data.frame(x = c(0, 1, 0, 1), y = c(1, 2, 3, 4))
  p <- morie_dataset_profile(df, hint_treatment = "x", hint_outcome = "y")
  out <- morie_dataset_profile_to_list(p)
  expect_type(out, "list")
  expect_true(all(c("n_rows", "n_cols", "columns") %in% names(out)))
})

test_that("profile_summary_table renders text with header", {
  set.seed(1)
  df <- data.frame(x = c(0, 1, 0, 1), y = c(1, 2, 3, 4))
  p <- morie_dataset_profile(df, hint_treatment = "x", hint_outcome = "y")
  s <- morie_dataset_profile_summary_table(p)
  expect_type(s, "character")
  expect_match(s, "Dataset Profile")
})

test_that("dataset_load reads CSV", {
  set.seed(1)
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(a = 1:3, b = c("x", "y", "z")), tmp, row.names = FALSE)
  df <- morie_dataset_load(tmp)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3L)
  unlink(tmp)
})

test_that("dataset_load reads TSV", {
  set.seed(1)
  tmp <- tempfile(fileext = ".tsv")
  utils::write.table(data.frame(a = 1:2, b = c("p", "q")), tmp,
                     row.names = FALSE, sep = "\t", quote = FALSE)
  df <- morie_dataset_load(tmp)
  expect_s3_class(df, "data.frame")
  unlink(tmp)
})

test_that("dataset_load errors on missing file and unknown extension", {
  set.seed(1)
  expect_error(morie_dataset_load("/nonexistent/path.csv"), "not found")
  tmp <- tempfile(fileext = ".xyz")
  file.create(tmp)
  expect_error(morie_dataset_load(tmp), "Unsupported")
  unlink(tmp)
})

test_that("dataset_load reads JSON when jsonlite present", {
  skip_if_not_installed("jsonlite")
  set.seed(1)
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(data.frame(a = 1:2), tmp)
  df <- morie_dataset_load(tmp)
  expect_s3_class(df, "data.frame")
  unlink(tmp)
})

test_that("suggest_plan always includes descriptive_profile", {
  set.seed(1)
  df <- data.frame(
    treat = c(0, 1, 0, 1, 0, 1),
    outcome_y = c(1.1, 2.2, 1.4, 3.0, 0.9, 2.8),
    age = c(20, 25, 30, 35, 40, 45)
  )
  p <- morie_dataset_profile(df, hint_treatment = "treat", hint_outcome = "outcome_y")
  plan <- morie_dataset_suggest_plan(p)
  expect_type(plan, "list")
  expect_true(any(vapply(plan, function(s) identical(s$analysis, "descriptive_profile"), logical(1))))
  expect_true(any(vapply(plan, function(s) identical(s$analysis, "ipw_ate"), logical(1))))
})

test_that("suggest_plan adds survey_weighted_estimates when weight present", {
  set.seed(1)
  df <- data.frame(
    treat = c(0, 1), outcome_y = c(1.1, 2.2), survey_wt = c(1.0, 2.0)
  )
  p <- morie_dataset_profile(df, hint_treatment = "treat",
                             hint_outcome = "outcome_y", hint_weights = "survey_wt")
  plan <- morie_dataset_suggest_plan(p)
  hits <- vapply(plan, function(s) identical(s$analysis, "survey_weighted_estimates"), logical(1))
  expect_true(any(hits))
})

test_that("internal match returns FALSE for empty name", {
  set.seed(1)
  expect_false(morie:::.morie_dataset_match("", "id"))
  expect_true(morie:::.morie_dataset_match("case_id", "id"))
})