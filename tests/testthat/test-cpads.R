# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/cpads.R -- contract + validation + canonicalize.

set.seed(1)

test_that("contract returns canonical metadata", {
  set.seed(1)
  c0 <- morie_cpads_contract()
  expect_type(c0, "list")
  expect_true(all(c("source_kind", "expected_wrangled_path",
                    "required_variables", "raw_column_map", "note") %in% names(c0)))
  expect_true(length(c0$required_variables) > 0)
  # 3MMM.2: CPADS PUMF is open data on open.canada.ca CKAN, not
  # local-private. source_kind reflects that.
  expect_equal(c0$source_kind, "open_data_pumf")
})

test_that("missing_variables detects gaps", {
  set.seed(1)
  miss <- morie_cpads_missing_variables(c("weight", "alcohol_past12m"))
  expect_true(length(miss) > 0)
  expect_true("gender" %in% miss)
  expect_equal(
    morie_cpads_missing_variables(morie_cpads_contract()$required_variables),
    character(0)
  )
})

test_that("validate_frame rejects non-data.frame", {
  set.seed(1)
  expect_error(morie_cpads_validate_frame(list()), "data.frame")
})

test_that("validate_frame strict raises when columns missing", {
  set.seed(1)
  df <- data.frame(weight = 1, foo = "x")
  expect_error(morie_cpads_validate_frame(df, strict = TRUE), "missing")
})

test_that("validate_frame strict=FALSE returns missing vector", {
  set.seed(1)
  df <- data.frame(weight = 1, foo = "x")
  miss <- morie_cpads_validate_frame(df, strict = FALSE)
  expect_true(length(miss) > 0L)
})

test_that("has_raw_columns rejects non-frame", {
  set.seed(1)
  expect_false(morie_cpads_has_raw_columns(list()))
})

test_that("has_raw_columns detects PUMF schema with wtpumf", {
  set.seed(1)
  raw <- morie_cpads_contract()$raw_column_map
  df <- as.data.frame(
    stats::setNames(rep(list(rep(NA, 1L)), length(raw)), unname(unlist(raw)))
  )
  expect_true(morie_cpads_has_raw_columns(df))
})

test_that("has_raw_columns accepts wtdf substitute", {
  set.seed(1)
  raw <- unname(unlist(morie_cpads_contract()$raw_column_map))
  cols <- c(setdiff(raw, "wtpumf"), "wtdf")
  df <- as.data.frame(stats::setNames(rep(list(rep(NA, 1L)), length(cols)), cols))
  expect_true(morie_cpads_has_raw_columns(df))
})

test_that("has_raw_columns false on canonical schema only", {
  set.seed(1)
  df <- data.frame(weight = 1, alcohol_past12m = 1)
  expect_false(morie_cpads_has_raw_columns(df))
})

test_that("canonicalize_frame errors on non-data.frame", {
  set.seed(1)
  expect_error(morie_cpads_canonicalize_frame(list()), "data.frame")
})

test_that("canonicalize_frame validates a canonical frame and returns it", {
  set.seed(1)
  vars <- morie_cpads_contract()$required_variables
  df <- as.data.frame(stats::setNames(rep(list(rep(NA, 1L)), length(vars)), vars))
  out <- morie_cpads_canonicalize_frame(df)
  expect_s3_class(out, "data.frame")
  expect_true(all(vars %in% colnames(out)))
})

test_that("canonicalize_frame remaps raw PUMF columns + recodes 98/99", {
  set.seed(1)
  raw_map <- morie_cpads_contract()$raw_column_map
  cols <- unname(unlist(raw_map))
  df <- as.data.frame(stats::setNames(rep(list(1L), length(cols)), cols))
  df$alc05 <- 2L
  df$can05 <- 1L
  df$age_groups <- 99L
  df$dvdemq01 <- 98L
  df$alc12_30d_prev_total <- 0L
  df$alc12_30d_prev <- 1L
  out <- morie_cpads_canonicalize_frame(df)
  expect_s3_class(out, "data.frame")
  expect_equal(out$alcohol_past12m, 0)
  expect_equal(out$cannabis_any_use, 1)
  expect_true(is.na(out$age_group))
  expect_true(is.na(out$gender))
  expect_equal(out$heavy_drinking_30d, 0)
})

test_that("infer_file_format recognises csv/xlsx/rds", {
  set.seed(1)
  expect_equal(morie_cpads_infer_file_format("a.csv"), "csv")
  expect_equal(morie_cpads_infer_file_format("a.xlsx"), "excel")
  expect_equal(morie_cpads_infer_file_format("a.xls"), "excel")
  expect_equal(morie_cpads_infer_file_format("a.rds"), "rds")
})

test_that("infer_file_format rejects unsupported / bad input", {
  set.seed(1)
  expect_error(morie_cpads_infer_file_format("a.parquet"), "Unsupported")
  expect_error(morie_cpads_infer_file_format(c("a.csv", "b.csv")), "scalar")
  expect_error(morie_cpads_infer_file_format(123), "scalar")
})