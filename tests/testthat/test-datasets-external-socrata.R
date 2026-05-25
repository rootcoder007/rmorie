# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3LL: external Socrata feeds -- City of Chicago Crime +
# NYC NYPD Stop, Question and Frisk (2022/2023/2024). Offline fixtures
# + mocked live-mode dispatch via .morie_dataset_socrata_fetch.

# ========================================================== Chicago Crime

test_that("morie_datasets_chicago_crime(offline=TRUE) reads bundled 22-col fixture", {
  df <- suppressWarnings(morie_datasets_chicago_crime(offline = TRUE))
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 22L)  # +location composite col in 3PP+
  expect_true("location" %in% names(df))
  expect_true(nrow(df) > 0L)
  for (col in c("id", "case_number", "date", "primary_type",
                "description", "arrest", "domestic", "year",
                "latitude", "longitude"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("^JZ-SYNTH-", df$case_number)))
})

test_that("morie_datasets_chicago_crime(offline=TRUE) honours year + max_features", {
  df <- suppressWarnings(
    morie_datasets_chicago_crime(offline = TRUE, year = 2024L,
                                   max_features = 2L))
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) <= 2L)
  if (nrow(df) > 0L) expect_true(all(df$year == 2024L))
})

test_that("morie_datasets_chicago_crime(offline=FALSE) dispatches via mocked Socrata fetch", {
  stub <- data.frame(id = c(1L, 2L),
                      case_number = c("LIVE-CHI-1", "LIVE-CHI-2"),
                      primary_type = c("THEFT", "BATTERY"),
                      year = c(2024L, 2024L))
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url, "data\\.cityofchicago\\.org/resource/ijzp-q8t2\\.json$")
      expect_equal(where, "year=2024")
      expect_equal(max_features, 50L)
      stub
    },
    .package = "morie")
  out <- morie_datasets_chicago_crime(year = 2024L,
                                        max_features = 50L,
                                        offline = FALSE)
  expect_equal(out$case_number, c("LIVE-CHI-1", "LIVE-CHI-2"))
})

test_that("morie_datasets_chicago_crime(offline=FALSE) sends NULL where when year omitted", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_null(where)
      data.frame(id = 1L)
    },
    .package = "morie")
  out <- morie_datasets_chicago_crime(offline = FALSE)
  expect_s3_class(out, "data.frame")
})

# ========================================================== NYC SQF

test_that("morie_datasets_nyc_stop_and_frisk(offline=TRUE) reads bundled core schema", {
  df <- suppressWarnings(morie_datasets_nyc_stop_and_frisk(offline = TRUE))
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  for (col in c("STOP_FRISK_ID", "STOP_FRISK_DATE",
                "STOP_LOCATION_PRECINCT", "SUSPECT_RACE_DESCRIPTION",
                "SUSPECT_AGE_GROUP", "SUSPECT_SEX", "FRISKED_FLAG",
                "ARREST_MADE_FLAG"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("^SQF-SYNTH-", df$STOP_FRISK_ID)))
})

test_that("morie_datasets_nyc_stop_and_frisk(offline=TRUE) honours max_features", {
  df <- suppressWarnings(
    morie_datasets_nyc_stop_and_frisk(offline = TRUE,
                                        max_features = 2L))
  expect_true(nrow(df) <= 2L)
})

test_that("morie_datasets_nyc_stop_and_frisk(offline=FALSE) routes by year to correct resource_url", {
  reps <- list(
    list(year = 2022L, expect_url = "e4yi-bvqr"),
    list(year = 2023L, expect_url = "rbed-zzin"),
    list(year = 2024L, expect_url = "7v9w-k82r"))
  for (R in reps) {
    out <- testthat::with_mocked_bindings(
      .morie_dataset_socrata_fetch = function(url, where = NULL,
                                                max_features = NULL,
                                                app_token = NULL,
                                                paginate = FALSE,
                                                page_size = 1000L,
                                                max_pages = 200L) {
        expect_match(url, R$expect_url)
        data.frame(STOP_FRISK_ID = "LIVE", YEAR2 = R$year)
      },
      .package = "morie",
      code = morie_datasets_nyc_stop_and_frisk(year = R$year,
                                                  offline = FALSE))
    expect_s3_class(out, "data.frame")
    expect_equal(out$YEAR2, R$year)
  }
})

test_that("morie_datasets_nyc_stop_and_frisk(offline=FALSE) defaults to most-recent year", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      # 2024 is the most-recent registered year -> 7v9w-k82r.
      expect_match(url, "7v9w-k82r")
      data.frame(STOP_FRISK_ID = "DEFAULT")
    },
    .package = "morie")
  out <- morie_datasets_nyc_stop_and_frisk(offline = FALSE)
  expect_equal(out$STOP_FRISK_ID, "DEFAULT")
})

test_that("morie_datasets_nyc_stop_and_frisk(offline=FALSE) errors on unknown year", {
  expect_error(morie_datasets_nyc_stop_and_frisk(year = 2019L,
                                                   offline = FALSE),
               regexp = "no built-in NYC OpenData resource for year")
})

# ========================================================== discovery helper

test_that("morie_datasets_external_socrata_layers returns the full 6-row registry (3PP)", {
  reg <- morie_datasets_external_socrata_layers()
  expect_s3_class(reg, "data.frame")
  expect_equal(nrow(reg), 11L)  # +chicago_wards + community_areas + iucr_codes (3UU)
  expect_setequal(names(reg),
                  c("dataset_key", "label", "portal",
                    "resource_url", "fixture"))
  expect_setequal(reg$dataset_key,
                  c("chicago_crime", "chicago_arrests",
                    "chicago_neighborhoods",
                    "chicago_police_beats",
                    "chicago_police_districts",
                    "chicago_wards",  # 3UU
                    "chicago_community_areas",  # 3UU
                    "chicago_iucr_codes",  # 3UU
                    "nyc_sqf_2024", "nyc_sqf_2023", "nyc_sqf_2022"))
  expect_setequal(unique(reg$portal),
                  c("data.cityofchicago.org",
                    "data.cityofnewyork.us"))
})

test_that("morie_datasets_external_socrata_layers Chicago entry points at the canonical resource URL", {
  reg <- morie_datasets_external_socrata_layers()
  chi <- reg[reg$dataset_key == "chicago_crime", ]
  expect_equal(chi$resource_url,
               "https://data.cityofchicago.org/resource/ijzp-q8t2.json")
})

# ========================================================== integration

test_that("offline-mode is the default for all three wrappers (no accidental network)", {
  # Offline default means a bare call returns the fixture, not an
  # httr2 error. This is the safer default established post-3EE.
  expect_s3_class(suppressWarnings(morie_datasets_chicago_crime()),
                  "data.frame")
  expect_s3_class(suppressWarnings(morie_datasets_nyc_stop_and_frisk()),
                  "data.frame")
  expect_s3_class(morie_datasets_chicago_neighborhoods(),
                  "data.frame")
})

# ========================================================== Chicago Neighborhoods (3MM)

test_that("morie_datasets_chicago_neighborhoods(offline=TRUE) reads the bundled 98-row attribute layer", {
  df <- morie_datasets_chicago_neighborhoods(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 98L)
  expect_setequal(names(df),
                  c("pri_neigh", "sec_neigh", "shape_area",
                    "shape_len"))
  # Real Chicago neighbourhood names (canonical 98 Office-of-Tourism
  # primary-name list).
  for (nm in c("Albany Park", "Andersonville", "Beverly",
                "Loop", "Lincoln Park", "Hyde Park",
                "Bridgeport", "Bucktown"))
    expect_true(nm %in% df$pri_neigh)
})

test_that("morie_datasets_chicago_neighborhoods(offline=TRUE) honours max_features", {
  df <- morie_datasets_chicago_neighborhoods(offline = TRUE,
                                               max_features = 5L)
  expect_equal(nrow(df), 5L)
})

test_that("morie_datasets_chicago_neighborhoods(offline=FALSE) dispatches via mocked Socrata fetch (attributes only)", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      # Default geometry = FALSE -> URL carries $select query string.
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/y6yq-dbs2\\.json\\?\\$select=pri_neigh,sec_neigh,shape_area,shape_len$")
      data.frame(pri_neigh = "MOCK", sec_neigh = "MOCK",
                  shape_area = 1.0, shape_len = 1.0)
    },
    .package = "morie")
  out <- morie_datasets_chicago_neighborhoods(offline = FALSE)
  expect_equal(out$pri_neigh, "MOCK")
})

test_that("morie_datasets_chicago_neighborhoods(offline=FALSE, geometry=TRUE) requests the_geom too", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      # geometry = TRUE -> no $select, server returns full schema
      # including the_geom MultiPolygon.
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/y6yq-dbs2\\.json$")
      data.frame(the_geom = "GEOM-STUB", pri_neigh = "MOCK",
                  sec_neigh = "MOCK", shape_area = 1.0,
                  shape_len = 1.0,
                  stringsAsFactors = FALSE)
    },
    .package = "morie")
  out <- morie_datasets_chicago_neighborhoods(offline = FALSE,
                                                geometry = TRUE)
  expect_true("the_geom" %in% names(out))
})

test_that("morie_datasets_chicago_neighborhoods honours resource_id override", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url, "override-res-id\\.json")
      data.frame(pri_neigh = "OVERRIDE")
    },
    .package = "morie")
  out <- morie_datasets_chicago_neighborhoods(
    offline = FALSE,
    resource_id = "override-res-id")
  expect_equal(out$pri_neigh, "OVERRIDE")
})

test_that("morie_datasets_external_socrata_layers includes chicago_neighborhoods", {
  reg <- morie_datasets_external_socrata_layers()
  expect_equal(nrow(reg), 11L)  # +chicago_wards + community_areas + iucr_codes (3UU)
  expect_true("chicago_neighborhoods" %in% reg$dataset_key)
  chi_nh <- reg[reg$dataset_key == "chicago_neighborhoods", ]
  expect_equal(chi_nh$resource_url,
               "https://data.cityofchicago.org/resource/y6yq-dbs2.json")
  expect_equal(chi_nh$fixture, "chicago_neighborhoods.csv")
})
