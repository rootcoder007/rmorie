# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 11 -- R/database.R part 2: dataset metadata, userguide
# listing, bootstrap downloads, and the download-URL / ArcGIS
# resolution tiers of morie_load_dataset().

.cdb2_have_db <- function() {
}

test_that("morie_dataset_info returns metadata and fuzzy-matches", {
  info <- morie_dataset_info("ocp21")
  expect_type(info, "list")
  expect_equal(info$key, "ocp21")
  expect_true(nzchar(info$source))
  expect_equal(morie_dataset_info("cpads")$key, "ocp21")
  expect_error(morie_dataset_info("no-such-key-xyzzy"), "Unknown")
})

test_that("morie_userguide lists the bundled userguides", {
  expect_type(morie_userguide(), "character")
})

test_that("morie_download_bootstrap validates the survey argument", {
  expect_error(morie_download_bootstrap("not-a-survey"), "Unknown survey")
  # the legacy bootstrap keys are not current catalog keys, so each
  # target falls through the 'unknown key' branch -- still exercised.
  res <- suppressMessages(
    morie_download_bootstrap("csus_2023", db_path = tempfile(fileext = ".db"))
  )
  expect_null(res)
  res_all <- suppressMessages(
    morie_download_bootstrap("all", db_path = tempfile(fileext = ".db"))
  )
  expect_null(res_all)
})

test_that("morie_list_datasets reports per-dataset cache status", {
  .cdb2_have_db()
  ds <- morie_list_datasets(db_path = tempfile(fileext = ".db"))
  expect_s3_class(ds, "data.frame")
  expect_true(all(c("key", "cached", "rows") %in% names(ds)))
  expect_true(nrow(ds) >= 20L)
})

test_that("morie_load_dataset resolves the direct-download tier", {
  .cdb2_have_db()
  testthat::local_mocked_bindings(
    morie_fetch = function(url, ...) data.frame(a = 1:4),
    .package = "morie"
  )
  d <- morie_load_dataset("cu23bt",
    db_path = tempfile(fileext = ".db"),
    refresh = TRUE
  )
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 4L)
})

test_that("morie_load_dataset resolves the ArcGIS tier", {
  .cdb2_have_db()
  testthat::local_mocked_bindings(
    morie_fetch_arcgis = function(layer_url, ...) data.frame(b = 1:7),
    .package = "morie"
  )
  d <- morie_load_dataset("tpsassault",
    db_path = tempfile(fileext = ".db"),
    refresh = TRUE
  )
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 7L)
})
