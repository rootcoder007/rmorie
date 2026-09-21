# SPDX-License-Identifier: AGPL-3.0-or-later
# CanLII API client: id derivation, validation and record shaping (no network;
# the API needs a key).

set.seed(1)

test_that("case_id derives CanLII ids from neutral citations", {
  set.seed(1)
  ids <- morie_ingest_canlii_case_id(c("2007 BCSC 1700", "2008 SCC 9", "[1959] SCR 121"))
  expect_equal(ids$database_id, c("bcsc", "csc-scc", NA))
  expect_equal(ids$case_id, c("2007bcsc1700", "2008scc9", NA))
  expect_equal(ids$citation[3], "[1959] SCR 121")
})

test_that("a missing key is a clear error before any request", {
  set.seed(1)
  old <- Sys.getenv("CANLII_API_KEY", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("CANLII_API_KEY") else Sys.setenv(CANLII_API_KEY = old))
  Sys.unsetenv("CANLII_API_KEY")
  expect_error(morie_ingest_canlii_databases(), "CANLII_API_KEY")
  expect_equal(rmorie:::.morie_canlii_key("abc"), "abc")
  Sys.setenv(CANLII_API_KEY = "fromenv")
  expect_equal(rmorie:::.morie_canlii_key(NULL), "fromenv")
})

test_that("identifiers and dates are validated", {
  set.seed(1)
  expect_error(morie_ingest_canlii_cases("ON HRT", api_key = "k"), "lower-case")
  expect_error(morie_ingest_canlii_cases("onhrt", result_count = 0, api_key = "k"),
    "between 1 and 10000"
  )
  expect_error(
    morie_ingest_canlii_cases("onhrt", decision_date_after = "2024/01/01", api_key = "k"),
    "YYYY-MM-DD"
  )
  expect_error(morie_ingest_canlii_citator("bcsc", "x", type = "citedThings", api_key = "k"))
  expect_equal(
    rmorie:::.morie_canlii_date_params(a = NULL, publishedAfter = "2024-01-01"),
    list(publishedAfter = "2024-01-01")
  )
})

test_that("language-keyed ids collapse to strings", {
  set.seed(1)
  df <- rmorie:::.morie_canlii_records_df(list(
    list(databaseId = "onhrt", caseId = list(en = "2024hrto1"), title = "A v. B",
      citation = "2024 HRTO 1"),
    list(databaseId = "onhrt", caseId = "2024hrto2", title = "C v. D")
  ))
  expect_equal(df$caseId, c("2024hrto1", "2024hrto2"))
  expect_equal(df$citation, c("2024 HRTO 1", NA))
})
