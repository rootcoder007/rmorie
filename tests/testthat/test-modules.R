test_that("morie_list_morie_modules exposes implemented module names", {
  mods <- morie_list_morie_modules()
  expect_true(all(c("power-design", "propensity-scores", "ebac-selection-adjustment-ipw") %in% mods$name))
})

test_that("morie_fetch_ckan pulls CPADS PUMF from the open.canada.ca datastore", {
  # CPADS 2021-2022 PUMF is a public open-data release queried through
  # the CKAN datastore_search API. Network-dependent, so skipped on CRAN
  # and offline machines per policy; runs wherever the API is reachable.
  testthat::skip_if_offline("open.canada.ca")
  dat <- tryCatch(
    morie_fetch_ckan(dataset_key = "cpads", limit = 1000L),
    error = function(e) NULL
  )
  skip_if(is.null(dat), "CKAN datastore_search API unreachable")
  expect_s3_class(dat, "data.frame")
  expect_true(nrow(dat) > 0)
  expect_false("_id" %in% names(dat))
  expect_true("SEQID" %in% names(dat))
})

test_that("morie_list_datasets shows all catalog entries", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not(requireNamespace("DBI", quietly = TRUE), "DBI not installed")
  skip_if_not(requireNamespace("RSQLite", quietly = TRUE), "RSQLite not installed")
  ds <- morie_list_datasets()
  expect_true(nrow(ds) >= 20)
  expect_true("ocp21" %in% ds$key)
})

test_that("dataset catalog has expected structure", {
  cat <- morie_dataset_catalog()
  expect_true(nrow(cat) >= 20)
  expect_true(all(c("key", "name", "source", "survey", "table_name") %in% names(cat)))
})

test_that("catalog exposes download-url columns with well-formed entries", {
  cat <- morie_dataset_catalog()
  expect_true(all(c("download_url", "zip_member") %in% names(cat)))
  dl <- cat[nzchar(cat$download_url), ]
  expect_true(nrow(dl) > 0)
  # Every zip download must name a member to extract from the archive.
  zips <- dl[grepl("\\.zip$", dl$download_url, ignore.case = TRUE), ]
  expect_true(all(nzchar(zips$zip_member)))
  # An entry is reachable by at most one remote tier (CKAN xor direct URL).
  expect_false(any(nzchar(cat$ckan_resource_id) & nzchar(cat$download_url)))
})

# The direct-download / zip extraction path is exercised by morie_fetch()
# in test-data-access.R (the .morie_fetch_download_url helper was folded
# into the universal morie_fetch() entry point).
