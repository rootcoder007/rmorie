# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/audit_variables.R

mk_spec <- function(nm, dtype = "string", valid_values = NULL) {
  list(name = nm, dtype = dtype, valid_values = valid_values)
}

mk_otis_specs <- function() {
  list(
    otis_v1 = list(
      mk_spec("UniqueIndividual_ID", "string"),
      mk_spec("NumberConsecutiveDays_Segregation", "int"),
      mk_spec("Region_AtTimeOfPlacement", "string",
              c("Central", "Eastern", "Western", "Northern"))
    )
  )
}

mk_arsau_specs <- function() {
  list(
    arsau_v1 = list(
      mk_spec("PoliceService", "string", c("OPP", "TPS")),
      mk_spec("Race", "string", c("Black", "White", "Indigenous")),
      mk_spec("AgeCategory", "string", c("Youth", "Adult", "Senior")),
      mk_spec("REPORTING_YEAR", "int")
    )
  )
}

test_that("morie_audit_otis_variables runs with no specs", {
  res <- morie_audit_otis_variables()
  expect_s3_class(res, "morie_audit_result")
  expect_equal(res$domain, "otis")
  expect_equal(res$n, 0)
})

test_that("morie_audit_otis_variables classifies provided specs", {
  res <- morie_audit_otis_variables(mk_otis_specs())
  expect_s3_class(res, "morie_audit_result")
  expect_equal(res$domain, "otis")
  expect_equal(res$n, 3)
  expect_true(res$n_analyzed >= 1)
  expect_true(is.numeric(res$coverage_pct))
})

test_that("morie_audit_arsau_variables runs with NULL specs", {
  res <- morie_audit_arsau_variables()
  expect_s3_class(res, "morie_audit_result")
  expect_equal(res$domain, "arsau")
  expect_equal(res$n, 0)
})

test_that("morie_audit_arsau_variables classifies provided specs", {
  res <- morie_audit_arsau_variables(mk_arsau_specs())
  expect_s3_class(res, "morie_audit_result")
  expect_equal(res$n, 4)
  expect_true(res$n_analyzed >= 1)
})

test_that("morie_audit_all_variables returns paired list", {
  res <- morie_audit_all_variables(mk_otis_specs(), mk_arsau_specs())
  expect_named(res, c("otis", "arsau"))
  expect_s3_class(res$otis, "morie_audit_result")
  expect_s3_class(res$arsau, "morie_audit_result")
})

test_that("morie_audit_all_variables runs with no args", {
  res <- morie_audit_all_variables()
  expect_named(res, c("otis", "arsau"))
})

test_that("morie_specs_from_df handles all column types", {
  df <- data.frame(
    int_col = 1:5,
    float_col = c(1.1, 2.2, 3.3, 4.4, 5.5),
    bool_col = c(TRUE, FALSE, TRUE, NA, FALSE),
    str_col = letters[1:5],
    date_col = as.Date("2024-01-01") + 0:4,
    dt_col = as.POSIXct("2024-01-01") + 0:4,
    stringsAsFactors = FALSE
  )
  specs <- morie_specs_from_df(df)
  expect_length(specs, 6)
  dtypes <- vapply(specs, function(s) s$dtype, character(1))
  expect_true("int" %in% dtypes)
  expect_true("float" %in% dtypes)
  expect_true("bool" %in% dtypes)
  expect_true("string" %in% dtypes)
  expect_true("date" %in% dtypes)
  expect_true("datetime" %in% dtypes)
})

test_that("morie_specs_from_df handles empty df", {
  specs <- morie_specs_from_df(data.frame())
  expect_length(specs, 0)
})

test_that("morie_write_audit_markdown writes file for single result", {
  res <- morie_audit_otis_variables(mk_otis_specs())
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  path <- morie_write_audit_markdown(tmp, res)
  expect_equal(path, tmp)
  expect_true(file.exists(tmp))
  txt <- readLines(tmp)
  expect_true(any(grepl("morie variable-coverage audit", txt)))
  expect_true(any(grepl("OTIS", txt)))
})

test_that("morie_write_audit_markdown writes file for list of results", {
  res_list <- morie_audit_all_variables(mk_otis_specs(), mk_arsau_specs())
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp), add = TRUE)
  path <- morie_write_audit_markdown(tmp, res_list)
  expect_true(file.exists(tmp))
  txt <- readLines(tmp)
  expect_true(any(grepl("OTIS", txt)))
  expect_true(any(grepl("ARSAU", txt)))
})

test_that("morie_write_audit_markdown errors on bad input", {
  expect_error(morie_write_audit_markdown(tempfile(), list(1, 2, 3)),
               "morie_audit_result")
})

test_that("print.morie_audit_result emits header lines", {
  res <- morie_audit_otis_variables(mk_otis_specs())
  out <- capture.output(print(res))
  expect_true(any(grepl("audit", out, ignore.case = TRUE)))
  expect_true(any(grepl("Total variables", out)))
})
