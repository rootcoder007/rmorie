# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3FFF3: Calgary + Edmonton + Ottawa loaders.

test_that("morie_datasets_calgary_open_crime_adjacent_layers reads 157 catalog", {
  d <- morie_datasets_calgary_open_crime_adjacent_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_true(nrow(d) >= 100L)
  expect_setequal(names(d),
                  c("soda_id", "title", "type", "search_keyword"))
  expect_true("78gh-n26t" %in% d$soda_id)
})

test_that("morie_datasets_edmonton_open_crime_adjacent_layers reads 195 catalog", {
  d <- morie_datasets_edmonton_open_crime_adjacent_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_true(nrow(d) >= 100L)
  expect_true("e7aq-scxv" %in% d$soda_id)
})

test_that("morie_datasets_ottawa_open_crime_adjacent_layers reads Hub catalog", {
  d <- morie_datasets_ottawa_open_crime_adjacent_layers(offline = TRUE)
  expect_s3_class(d, "data.frame")
  expect_true(nrow(d) >= 50L)
  expect_true("source" %in% names(d))
  expect_true(all(d$source == "City of Ottawa"))
})

test_that("morie_datasets_calgary_community_crime_stats reads 200 sample", {
  df <- morie_datasets_calgary_community_crime_stats(offline = TRUE)
  expect_equal(nrow(df), 200L)
  expect_setequal(names(df),
                  c("community", "category", "crime_count",
                    "year", "month"))
})

test_that("morie_datasets_calgary_fire_response_calls reads 200 sample", {
  df <- morie_datasets_calgary_fire_response_calls(offline = TRUE)
  expect_equal(nrow(df), 200L)
  expect_true("major_incident_type" %in% names(df))
})

test_that("morie_datasets_calgary_fire_stations reads 43 stations", {
  df <- morie_datasets_calgary_fire_stations(offline = TRUE)
  expect_equal(nrow(df), 43L)
  expect_true("stn_num" %in% names(df))
})

test_that("morie_datasets_edmonton_police_stations reads 10 stations", {
  df <- morie_datasets_edmonton_police_stations(offline = TRUE)
  expect_equal(nrow(df), 10L)
  expect_true("name" %in% names(df))
  expect_true("latitude" %in% names(df))
})

test_that("morie_datasets_edmonton_fire_stations reads 31 stations", {
  df <- morie_datasets_edmonton_fire_stations(offline = TRUE)
  expect_equal(nrow(df), 31L)
  expect_true("name" %in% names(df))
})

test_that("max_features cap honoured on bundled wrappers", {
  for (fn in list(morie_datasets_calgary_community_crime_stats,
                   morie_datasets_calgary_fire_response_calls,
                   morie_datasets_calgary_fire_stations,
                   morie_datasets_edmonton_police_stations,
                   morie_datasets_edmonton_fire_stations)) {
    expect_equal(nrow(fn(offline = TRUE, max_features = 3L)), 3L)
  }
})

test_that("live mode errors when offline=FALSE on catalog wrappers", {
  expect_error(
    morie_datasets_calgary_open_crime_adjacent_layers(offline = FALSE),
    regexp = "Live mode")
})
