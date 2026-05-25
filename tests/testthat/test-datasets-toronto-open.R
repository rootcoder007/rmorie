# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3EEE2: Toronto Open Data CKAN loaders beyond TPS Hub.

test_that("morie_datasets_toronto_open_crime_adjacent_layers reads 208-row catalog", {
  d <- morie_datasets_toronto_open_crime_adjacent_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_true(nrow(d) >= 100L)
  expect_setequal(names(d),
                  c("package_name", "title", "num_resources",
                    "metadata_modified", "search_keyword"))
  expect_true("ambulance-station-locations" %in% d$package_name)
  expect_true("police-annual-statistical-report-miscellaneous-data" %in%
                d$package_name)
})

test_that("morie_datasets_toronto_ambulance_stations reads 46-row fixture", {
  df <- morie_datasets_toronto_ambulance_stations(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 46L)
  expect_true("EMS_NAME" %in% names(df))
  expect_true("EMS_ADDRESS" %in% names(df))
})

test_that("morie_datasets_toronto_asr_miscellaneous reads 40-row fixture", {
  df <- morie_datasets_toronto_asr_miscellaneous(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 40L)
  expect_setequal(names(df),
                  c("X_id", "YEAR", "SECTION", "CATEGORY",
                    "SUBTYPE", "COUNT_"))
  # Hate Crimes section is present (one of the canonical sub-aggregates).
  expect_true("Hate Crimes" %in% df$CATEGORY)
})

test_that("max_features cap honoured on TO loaders", {
  expect_equal(
    nrow(morie_datasets_toronto_ambulance_stations(offline = TRUE,
                                                      max_features = 5L)),
    5L)
  expect_equal(
    nrow(morie_datasets_toronto_asr_miscellaneous(offline = TRUE,
                                                     max_features = 5L)),
    5L)
})
