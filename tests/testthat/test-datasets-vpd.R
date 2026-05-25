# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD2: VPD crime data loader (offline sample + user-supplied
# zip path). VPD's GeoDASH gates downloads behind manual T&C
# acceptance so morie cannot auto-fetch.

test_that("morie_datasets_vpd_crime(offline=TRUE) reads bundled 550-row sample", {
  df <- morie_datasets_vpd_crime(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 550L)
  expect_setequal(names(df),
                  c("TYPE", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE",
                    "HUNDRED_BLOCK", "NEIGHBOURHOOD", "X", "Y"))
  # All 11 VPD TYPE categories present with 50 rows each.
  tt <- table(df$TYPE)
  expect_equal(length(tt), 11L)
  expect_true(all(tt == 50L))
})

test_that("VPD sample covers all 25 VPD neighbourhoods", {
  df <- morie_datasets_vpd_crime(offline = TRUE)
  expect_true(length(unique(df$NEIGHBOURHOOD)) >= 20L)
  # Spot-check known VPD neighbourhood labels.
  expect_true("West End" %in% df$NEIGHBOURHOOD)
})

test_that("VPD sample TYPE includes the 11 canonical categories", {
  df <- morie_datasets_vpd_crime(offline = TRUE)
  expect_setequal(unique(df$TYPE),
                  c("Break and Enter Commercial",
                    "Break and Enter Residential/Other",
                    "Homicide",
                    "Mischief",
                    "Offence Against a Person",
                    "Other Theft",
                    "Theft from Vehicle",
                    "Theft of Bicycle",
                    "Theft of Vehicle",
                    "Vehicle Collision or Pedestrian Struck (with Fatality)",
                    "Vehicle Collision or Pedestrian Struck (with Injury)"))
})

test_that("VPD sample YEAR range spans 2003-2026", {
  df <- morie_datasets_vpd_crime(offline = TRUE)
  yr <- range(df$YEAR)
  expect_true(yr[1] <= 2010L)
  expect_true(yr[2] >= 2025L)
})

test_that("max_features cap honoured", {
  df <- morie_datasets_vpd_crime(offline = TRUE, max_features = 10L)
  expect_equal(nrow(df), 10L)
})

test_that("morie_datasets_vpd_legal_disclaimer returns bundled text", {
  d <- morie_datasets_vpd_legal_disclaimer()
  expect_type(d, "character")
  expect_true(length(d) > 0L)
  # Heading appears.
  expect_true(any(grepl("Legal Disclaimer", d)))
  # Key phrase about FOIPPA.
  expect_true(any(grepl("FIPPA|BC FIPPA|Privacy Act", d)))
})

test_that("morie_datasets_vpd_crime errors on offline=FALSE without paths", {
  expect_error(
    morie_datasets_vpd_crime(offline = FALSE,
                              zip_path = NULL, csv_path = NULL),
    regexp = "no automation API")
})

test_that("morie_datasets_vpd_crime errors when both paths given", {
  expect_error(
    morie_datasets_vpd_crime(zip_path = "/x", csv_path = "/y"),
    regexp = "only one")
})

test_that("morie_datasets_vpd_crime errors when zip path missing", {
  expect_error(
    morie_datasets_vpd_crime(zip_path = "/nonexistent/path.zip"),
    regexp = "VPD zip not found")
})
