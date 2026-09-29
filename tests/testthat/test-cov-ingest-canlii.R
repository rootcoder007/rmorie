# Coverage tests for the case and legislation readers of
# R/ingest_canlii.R. The HTTP call is replaced by a local fixture, so
# these check the request paths, identifier validation and the record
# flattening without the network or an API key.

cl_mock <- function(env) {
  function(path, params = NULL, api_key = NULL, timeout = NULL, user_agent = NULL) {
    env$path <- path
    env$params <- params
    if (grepl("^legislationBrowse/[a-z]+/$", path)) {
      return(list(legislationDatabases = list(
        list(databaseId = "ons", type = "STATUTE", jurisdiction = "on", name = "Ontario Statutes"),
        list(databaseId = "bcs", type = "STATUTE", jurisdiction = "bc", name = "BC Statutes")
      )))
    }
    if (grepl("^legislationBrowse/[a-z]+/[a-z0-9-]+/$", path)) {
      return(list(legislations = list(
        list(legislationId = "rso-1990-c-h19", title = "Human Rights Code", type = "STATUTE"),
        list(legislationId = "so-2006-c-35", title = "Accessibility Act", type = "STATUTE")
      )))
    }
    list(databaseId = "onsc", caseId = list(en = "2020onsc1234"), title = "R v X", citation = "2020 ONSC 1234")
  }
}

test_that("case and legislation readers build the documented paths", {
  env <- new.env()
  local_mocked_bindings(.package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie", .morie_canlii_call = cl_mock(env))
  cs <- morie_ingest_canlii_case("onsc", "2020onsc1234")
  expect_equal(env$path, "caseBrowse/en/onsc/2020onsc1234/")
  expect_equal(nrow(cs), 1L)
  expect_equal(cs$caseId, "2020onsc1234")
  expect_equal(cs$citation, "2020 ONSC 1234")
  db <- morie_ingest_canlii_legislation_databases("fr")
  expect_equal(env$path, "legislationBrowse/fr/")
  expect_equal(db$databaseId, c("ons", "bcs"))
  lg <- morie_ingest_canlii_legislations("ons")
  expect_equal(env$path, "legislationBrowse/en/ons/")
  expect_equal(lg$legislationId, c("rso-1990-c-h19", "so-2006-c-35"))
  one <- morie_ingest_canlii_legislation("ons", "rso-1990-c-h19")
  expect_equal(env$path, "legislationBrowse/en/ons/rso-1990-c-h19/")
  expect_equal(nrow(one), 1L)
})

test_that("identifiers and languages are validated before any request", {
  env <- new.env()
  local_mocked_bindings(.package = if (isNamespaceLoaded("rmorie")) "rmorie" else "morie", .morie_canlii_call = cl_mock(env))
  expect_error(morie_ingest_canlii_case("ONSC", "x"), "database_id must be one lower-case")
  expect_error(morie_ingest_canlii_case("onsc", "2020 onsc"), "case_id must be one lower-case")
  expect_error(morie_ingest_canlii_legislations(c("a", "b")), "database_id")
  expect_error(morie_ingest_canlii_legislation("ons", "Bad_Id"), "legislation_id")
  expect_error(morie_ingest_canlii_legislation_databases("de"), "should be one of")
  expect_null(env$path)
})
