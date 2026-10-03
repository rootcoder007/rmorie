# SPDX-License-Identifier: AGPL-3.0-or-later
# Every catalog key has a route a user can take: portal download, the data.rmorie.com copy, or a named own file.

test_that("the Health Infobase keys carry a portal URL or a hosted copy, the OTIS environments a hosted file", {
  cat <- morie_dataset_catalog()
  hib <- cat[grepl("^hib", cat$key), ]
  expect_equal(nrow(hib), 14)
  expect_true(all(nzchar(hib$hosted_key)))
  expect_true(all(nzchar(hib$download_url[hib$key != "hibp"])))
  expect_equal(cat$hosted_file[cat$key == "otisexp"], "otis/dt_expanded.rds")
  expect_true("otisloc" %in% cat$key)
  expect_match(cat$download_url[cat$key == "otisloc"], "data.ontario.ca")
})

test_that("a hosted copy is used when the portal fails, and a login is asked for when no key is stored", {
  tmp <- withr::local_tempdir()
  db <- file.path(tmp, "c.duckdb")
  testthat::local_mocked_bindings(
    morie_fetch = function(url, ...) stop("portal down"),
    .morie_llm_hosted_key = function() NULL
  )
  expect_error(morie_load_dataset("hibub", db_path = db), "rmorie login")
  testthat::local_mocked_bindings(
    morie_fetch = function(url, ...) stop("portal down"),
    .morie_llm_hosted_key = function() "sk-test",
    morie_load_hosted_dataset = function(key, db_path = NULL, refresh = FALSE) data.frame(k = key),
    morie_cache_store = function(data, table_name, db_path = NULL, con = NULL) invisible(TRUE)
  )
  expect_equal(morie_load_dataset("hibub", db_path = db)$k, "hib/csus_cannabis")
  # no portal file catalogued: straight to the copy
  expect_equal(morie_load_dataset("hibp", db_path = db)$k, "hib/cpads_cpads")
})

test_that("an R object at data.rmorie.com is fetched into the data directory and opened", {
  tmp <- withr::local_tempdir()
  withr::local_envvar(MORIE_DATA_DIR = tmp)
  obj <- list(a = 1:3)
  testthat::local_mocked_bindings(
    .morie_data_get = function(path, dest, timeout = 600, size = NULL) {
      expect_equal(path, "/files/otis/dt_expanded.rds")
      saveRDS(obj, dest)
      invisible(dest)
    },
    .morie_llm_hosted_key = function() "sk-test"
  )
  got <- morie_load_dataset("otisexp", db_path = file.path(tmp, "c.duckdb"))
  expect_equal(got, obj)
  expect_true(file.exists(file.path(tmp, "data/cache/dt_expanded.rds")))
  # second call reads the saved file without a fetch
  testthat::local_mocked_bindings(.morie_data_get = function(...) stop("must not fetch twice"))
  expect_equal(morie_load_dataset("otisexp", db_path = file.path(tmp, "c.duckdb")), obj)
})
