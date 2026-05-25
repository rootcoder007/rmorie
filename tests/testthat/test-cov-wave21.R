# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage wave 21 -- R/database.R: morie_fetch_ckan's package-metadata
# resolution path and morie_download_bootstrap's per-key loop.

.cw21_db <- function() {
}

test_that("morie_fetch_ckan resolves a resource id from package metadata", {
  .cw21_db()
  pkg_show <- paste0(
    '{"result":{"resources":[',
    '{"id":"res-xml","format":"XML"},',
    '{"id":"res-csv","format":"CSV"}]}}'
  )
  records <- paste0(
    '{"success":true,"result":{"total":2,"records":',
    '[{"_id":1,"SEQID":"a"},{"_id":2,"SEQID":"b"}]}}'
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    readLines = function(con, ...) {
      calls <<- calls + 1L
      if (calls == 1L) pkg_show else records
    }, .package = "base"
  )
  # csus has no baked-in resource id -> the metadata path runs and
  # should pick the CSV resource (res-csv).
  dat <- morie_fetch_ckan(
    dataset_key = "csus",
    db_path = tempfile(fileext = ".db")
  )
  expect_s3_class(dat, "data.frame")
  expect_equal(nrow(dat), 2L)
  expect_true("SEQID" %in% names(dat))
})

test_that("morie_fetch_ckan errors on a dataset with no metadata url", {
  .cw21_db()
  expect_error(
    morie_fetch_ckan(
      dataset_key = "not-a-known-survey",
      db_path = tempfile(fileext = ".db")
    ),
    "no CKAN resource id"
  )
})

test_that("morie_download_bootstrap validates survey and runs the loop", {
  .cw21_db()
  expect_error(morie_download_bootstrap("not-a-survey"), "Unknown survey")

  # cu23bt has a download_url but no ckan_resource_id -> the
  # "no CKAN resource ID" branch.
  expect_null(suppressMessages(
    morie_download_bootstrap("csus_2023",
      db_path = tempfile(fileext = ".db")
    )
  ))

  # csads_2021 -> ocs22bt, which carries a ckan_resource_id -> the CKAN
  # branch (morie_fetch_ckan mocked so no network is touched).
  testthat::local_mocked_bindings(
    morie_fetch_ckan = function(dataset_key, ...) data.frame(z = 1:3),
    .package = "morie"
  )
  expect_null(suppressMessages(
    morie_download_bootstrap("csads_2021",
      db_path = tempfile(fileext = ".db")
    )
  ))
})

test_that("morie_download_bootstrap ingests a present local bootstrap file", {
  .cw21_db()
  wd <- tempfile("bs-")
  dir.create(wd)
  withr::local_dir(wd)
  cat <- morie_dataset_catalog()
  lp <- cat$local_path[cat$key == "cu23bt"][1]
  dir.create(dirname(lp), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(bw1 = 1:4), lp, row.names = FALSE)
  expect_null(suppressMessages(
    morie_download_bootstrap("csus_2023",
      db_path = tempfile(fileext = ".db")
    )
  ))
})
