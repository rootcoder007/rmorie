# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3EEE4: unified morie_datasets_load_by_key() dispatcher.

test_that("targeted-fixture dispatch hits VPD bundled sample (550 rows)", {
  df <- morie_datasets_load_by_key("vpd_crime")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 550L)
})

test_that("targeted-fixture dispatch hits NYPD arrests YTD (5 rows)", {
  df <- morie_datasets_load_by_key("nypd_arrests_ytd")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
})

test_that("targeted-fixture dispatch hits TPS PSDP layer (assault)", {
  df <- morie_datasets_load_by_key("assault")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
})

test_that("targeted-fixture dispatch hits MTL SIM interventions", {
  df <- morie_datasets_load_by_key(
    "interventions-service-securite-incendie-montreal")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 349L)
})

test_that("targeted-fixture dispatch hits TO ambulance stations", {
  df <- morie_datasets_load_by_key("ambulance-station-locations")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 46L)
})

test_that("targeted-fixture dispatch hits TO ASR misc", {
  df <- morie_datasets_load_by_key(
    "police-annual-statistical-report-miscellaneous-data")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 40L)
})

test_that("targeted-fixture dispatch hits Vancouver graffiti sample", {
  df <- morie_datasets_load_by_key("graffiti")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 100L)
})

test_that("targeted-fixture dispatch hits Vancouver fire halls", {
  df <- morie_datasets_load_by_key("fire-halls")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 20L)
})

test_that("targeted-fixture dispatch hits NYC borough boundaries", {
  df <- morie_datasets_load_by_key("borough")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
})

test_that("targeted-fixture dispatch hits NYC police precincts", {
  df <- morie_datasets_load_by_key("police_precinct")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 78L)
})

test_that("targeted-fixture dispatch hits NYC ZCTAs", {
  df <- morie_datasets_load_by_key("zcta")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 221L)
})

test_that("max_features cap is propagated", {
  df <- morie_datasets_load_by_key("vpd_crime", max_features = 7L)
  expect_equal(nrow(df), 7L)
})

test_that("unknown dataset_key raises clear error", {
  expect_error(
    morie_datasets_load_by_key("nonexistent-key-9999"),
    regexp = "unknown dataset_key")
})

test_that("3FFF1: MTL CKAN generic dispatch auto-resolves first CSV resource", {
  skip_on_cran()
  skip_if_offline("donnees.montreal.ca")
  df <- morie_datasets_load_by_key("communique-presse", max_features = 5L)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1L)
  expect_true(nrow(df) <= 5L)
})

test_that("3FFF1: TO CKAN generic dispatch auto-resolves first CSV resource", {
  skip_on_cran()
  skip_if_offline("ckan0.cf.opendata.inter.prod-toronto.ca")
  df <- morie_datasets_load_by_key(
    "police-annual-statistical-report-shooting-occurrences",
    max_features = 5L)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) >= 1L)
})

# ========================================== Phase 3FFF2 mode= + app_token

test_that("3FFF2: mode= argument signature accepts auto/soda2/soda3/odata", {
  args <- formals(morie_datasets_load_by_key)
  expect_true("mode" %in% names(args))
  expect_equal(eval(args$mode), c("auto", "soda2", "soda3", "odata"))
  expect_true("app_token" %in% names(args))
})

test_that("3FFF2: mode='auto' default is preserved (NYPD arrests offline)", {
  df <- morie_datasets_load_by_key("nypd_arrests_ytd")
  expect_equal(nrow(df), 5L)
})

test_that("3FFF2: mode='soda3' threads through to NYPD wrapper without error", {
  # Offline path -- mode is accepted but bundled fixture still wins.
  df <- morie_datasets_load_by_key("nypd_arrests_ytd", mode = "soda3")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
})

test_that("3FFF2: mode='soda3' + app_token routed to Socrata wrapper", {
  # Mock the SODA3 transport so we capture the mode + app_token threading.
  seen <- list()
  testthat::with_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            app_token = NULL,
                                            base_url = "https://data.cityofchicago.org",
                                            ...) {
      seen <<- list(view_id = view_id, soql = soql,
                    app_token = app_token, base_url = base_url)
      data.frame(arrest_key = "M-1")
    },
    .package = "morie",
    code = morie_datasets_load_by_key("nypd_arrests_ytd",
                                         offline = FALSE,
                                         mode = "soda3",
                                         app_token = "tok-9999"))
  expect_equal(seen$view_id, "uip8-fykc")
  expect_equal(seen$app_token, "tok-9999")
  expect_equal(seen$base_url, "https://data.cityofnewyork.us")
})

test_that("3FFF2: mode is ignored on non-Socrata sources (VPD bundled)", {
  # mode= must NOT break the dispatch for non-Socrata wrappers.
  df <- morie_datasets_load_by_key("vpd_crime", mode = "soda3")
  expect_equal(nrow(df), 550L)
})

# ========================================== Phase 3HHH5 collision handling

test_that("3HHH5: ambiguous dataset_key errors with helpful source= hint", {
  expect_error(
    morie_datasets_load_by_key("public-art"),
    regexp = "ambiguous.*toronto_opendata.*vancouver_opendata|ambiguous.*vancouver_opendata.*toronto_opendata")
})

test_that("3HHH5: source= disambiguates collision (Vancouver)", {
  # Vancouver public-art is an Opendatasoft id -- network call live mode.
  # Mock the underlying fetcher so the test stays offline.
  testthat::with_mocked_bindings(
    morie_datasets_vancouver_opendata_by_id = function(id, ...) {
      data.frame(dataset_id = id, source = "van-mock",
                  stringsAsFactors = FALSE)
    },
    .package = "morie",
    code = {
      df <- morie_datasets_load_by_key("public-art",
                                          source = "vancouver_opendata")
      expect_equal(df$dataset_id, "public-art")
    })
})

test_that("3HHH5: source= disambiguates collision (Toronto)", {
  testthat::with_mocked_bindings(
    morie_datasets_toronto_open_ckan_resource = function(resource_id,
                                                            limit, ...) {
      data.frame(id = resource_id, source = "tor-mock",
                  stringsAsFactors = FALSE)
    },
    .morie_ckan_resolve_first_csv = function(package_name, ckan_base) {
      sprintf("fake-resource-uuid-for-%s", package_name)
    },
    .package = "morie",
    code = {
      df <- morie_datasets_load_by_key("public-art",
                                          source = "toronto_opendata")
      expect_equal(df$id, "fake-resource-uuid-for-public-art")
    })
})

test_that("3HHH5: source= with unknown value errors with available list", {
  expect_error(
    morie_datasets_load_by_key("public-art",
                                  source = "atlantis_opendata"),
    regexp = "no entry for source.*atlantis_opendata.*Available")
})
