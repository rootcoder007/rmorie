# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD1: Vancouver Open Data crime-adjacent civic loaders.

test_that("morie_datasets_vancouver_graffiti returns 100-row sample", {
  df <- morie_datasets_vancouver_graffiti(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 100L)
  for (col in c("count", "geo_local_area", "lon", "lat"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
  expect_true(all(!is.na(df$geo_local_area) & nzchar(df$geo_local_area)))
  # Vancouver lat/lon range sanity.
  expect_true(all(df$lon > -124 & df$lon < -122))
  expect_true(all(df$lat > 49 & df$lat < 50))
})

test_that("morie_datasets_vancouver_noise_control_areas returns 3 zones", {
  df <- morie_datasets_vancouver_noise_control_areas(offline = TRUE)
  expect_equal(nrow(df), 3L)
  expect_true("name" %in% names(df))
})

test_that("morie_datasets_vancouver_homeless_shelters returns 17 facilities", {
  df <- morie_datasets_vancouver_homeless_shelters(offline = TRUE)
  expect_equal(nrow(df), 17L)
  for (col in c("facility", "category", "geo_local_area"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
})

test_that("morie_datasets_vancouver_property_use_inspection_districts returns 23 districts", {
  df <- morie_datasets_vancouver_property_use_inspection_districts(
    offline = TRUE)
  expect_equal(nrow(df), 23L)
  expect_true("name" %in% names(df))
})

test_that("morie_datasets_vancouver_fire_halls returns 20 halls", {
  df <- morie_datasets_vancouver_fire_halls(offline = TRUE)
  expect_equal(nrow(df), 20L)
  for (col in c("name", "address", "geo_local_area"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
})

test_that("max_features cap honoured on all 5 wrappers", {
  for (fn in list(morie_datasets_vancouver_graffiti,
                   morie_datasets_vancouver_noise_control_areas,
                   morie_datasets_vancouver_homeless_shelters,
                   morie_datasets_vancouver_property_use_inspection_districts,
                   morie_datasets_vancouver_fire_halls)) {
    expect_equal(nrow(fn(offline = TRUE, max_features = 2L)), 2L)
  }
})

# ========================================== Phase 3EEE3 deeper coverage

test_that("morie_datasets_vancouver_community_centres reads 27-row fixture", {
  df <- morie_datasets_vancouver_community_centres(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 27L)
  expect_true("name" %in% names(df))
  expect_true("address" %in% names(df))
})

test_that("morie_datasets_vancouver_community_food_markets reads 91-row fixture", {
  df <- morie_datasets_vancouver_community_food_markets(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 91L)
  expect_true("marketname_location_host" %in% names(df))
})

test_that("morie_datasets_vancouver_disability_parking reads 100-row fixture", {
  df <- morie_datasets_vancouver_disability_parking(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 100L)
  expect_true("spaces" %in% names(df))
  # Vancouver lat/lon range
  expect_true(all(df$lon > -124 & df$lon < -122, na.rm = TRUE))
})

test_that("morie_datasets_vancouver_public_art reads 100-row sample", {
  df <- morie_datasets_vancouver_public_art(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 100L)
  for (col in c("title_of_work", "type", "neighbourhood",
                "yearofinstallation"))
    expect_true(col %in% names(df),
                 info = sprintf("missing %s", col))
})

test_that("max_features cap honoured on all 4 new Vancouver wrappers", {
  for (fn in list(morie_datasets_vancouver_community_centres,
                   morie_datasets_vancouver_community_food_markets,
                   morie_datasets_vancouver_disability_parking,
                   morie_datasets_vancouver_public_art)) {
    expect_equal(nrow(fn(offline = TRUE, max_features = 5L)), 5L)
  }
})
