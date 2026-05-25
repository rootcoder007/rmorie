# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD3: StatCan CCJS WDS REST API loaders.

test_that("morie_datasets_statcan_ccjs_cubes returns 10-row registry", {
  c <- morie_datasets_statcan_ccjs_cubes()
  expect_s3_class(c, "data.frame")
  expect_equal(nrow(c), 10L)
  expect_setequal(names(c),
                  c("product_id", "cube_title_en",
                    "dimensions", "frequency"))
  expect_type(c$product_id, "integer")
  # All 10 product_ids are 8-digit catalogue IDs in the 35xxxxxx range
  # (CCJS subject matter code 35).
  expect_true(all(c$product_id > 35000000L &
                     c$product_id < 36000000L))
})

test_that("registered cubes include the canonical CCJS flagships", {
  c <- morie_datasets_statcan_ccjs_cubes()
  expect_true(35100177L %in% c$product_id)  # Incident-based
  expect_true(35100068L %in% c$product_id)  # Police personnel
  expect_true(35100026L %in% c$product_id)  # Homicide victims
})

test_that("morie_datasets_statcan_cube_metadata reaches WDS for 35100002", {
  skip_on_cran()
  skip_if_offline("www150.statcan.gc.ca")
  m <- morie_datasets_statcan_cube_metadata(35100002L)
  expect_equal(m$status, "SUCCESS")
  expect_true(!is.null(m$object$cubeTitleEn))
  expect_true(grepl("cybercrime|Cybercrime",
                     m$object$cubeTitleEn))
})

test_that("morie_datasets_statcan_full_csv_url returns canonical zip URL", {
  skip_on_cran()
  skip_if_offline("www150.statcan.gc.ca")
  u <- morie_datasets_statcan_full_csv_url(35100002L)
  expect_match(u, "^https://www150\\.statcan\\.gc\\.ca/")
  expect_match(u, "35100002-eng\\.zip$")
})

test_that("morie_datasets_statcan_full_csv_url respects language=fr", {
  skip_on_cran()
  skip_if_offline("www150.statcan.gc.ca")
  u <- morie_datasets_statcan_full_csv_url(35100002L, language = "fr")
  expect_match(u, "fra\\.zip$")
})
