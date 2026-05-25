# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3UU: Chicago Wards + Community Areas + IUCR codes -- the 3
# foreign-key resolvers for chicago_crime (ijzp-q8t2). Wards +
# Community Areas are SODA3-only filtered views; IUCR is a SODA2
# base dictionary.

# =================================================== Chicago Wards (sp34-6z76)

test_that("morie_datasets_chicago_wards(offline=TRUE) reads bundled 50-row attribute fixture", {
  df <- morie_datasets_chicago_wards(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 50L)
  expect_setequal(names(df),
                  c("ward", "shape_leng", "shape_area"))
  # ward column is character to preserve leading zeros (none in
  # this dataset but the type discipline matters).
  expect_type(df$ward, "character")
  # All 50 ward numbers 1..50.
  expect_setequal(as.integer(df$ward), 1:50)
})

test_that("morie_datasets_chicago_wards(offline=TRUE) honours max_features", {
  df <- morie_datasets_chicago_wards(offline = TRUE,
                                       max_features = 5L)
  expect_equal(nrow(df), 5L)
})

test_that("morie_datasets_chicago_wards(offline=FALSE) routes through SODA3 with $select=ward,...", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            app_token = NULL,
                                            paginate = FALSE,
                                            page_size = 1000L,
                                            max_pages = 200L,
                                            max_features = NULL,
                                            base_url = "https://data.cityofchicago.org") {
      seen <<- list(view_id = view_id, soql = soql)
      data.frame(ward = "1", shape_leng = "1", shape_area = "1")
    },
    .package = "morie")
  morie_datasets_chicago_wards(offline = FALSE)
  expect_equal(seen$view_id, "sp34-6z76")
  expect_match(seen$soql,
               "^SELECT ward, shape_leng, shape_area ORDER BY ward$")
})

test_that("morie_datasets_chicago_wards(offline=FALSE, geometry=TRUE) drops the $select restriction", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            ...) {
      seen <<- list(soql = soql)
      data.frame(the_geom = "GEOM", ward = "1")
    },
    .package = "morie")
  morie_datasets_chicago_wards(offline = FALSE, geometry = TRUE)
  expect_match(seen$soql, "^SELECT \\* ORDER BY ward$")
})

test_that("morie_datasets_chicago_wards forwards paginate / page_size / app_token", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            app_token = NULL,
                                            paginate = FALSE,
                                            page_size = 1000L,
                                            max_pages = 200L,
                                            max_features = NULL,
                                            base_url = "https://data.cityofchicago.org") {
      seen <<- list(paginate = paginate, page_size = page_size,
                    app_token = app_token, max_features = max_features)
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_wards(offline = FALSE,
                                 paginate = TRUE,
                                 page_size = 50L,
                                 app_token = "tok-w",
                                 max_features = 25L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 50L)
  expect_equal(seen$app_token, "tok-w")
  expect_equal(seen$max_features, 25L)
})

# ====================================== Chicago Community Areas (cauq-8yn6)

test_that("morie_datasets_chicago_community_areas(offline=TRUE) reads bundled 77-row attribute fixture", {
  df <- morie_datasets_chicago_community_areas(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 77L)
  expect_setequal(names(df),
                  c("area_numbe", "community", "area_num_1",
                    "shape_area", "shape_len"))
  expect_type(df$area_numbe, "character")
  expect_type(df$area_num_1, "character")
  expect_setequal(as.integer(df$area_numbe), 1:77)
  # Spot-check canonical names (all UPPER CASE in the upstream feed).
  for (nm in c("ROGERS PARK", "WEST RIDGE", "UPTOWN",
                "LINCOLN PARK", "LOOP", "HYDE PARK",
                "EDGEWATER"))
    expect_true(nm %in% df$community)
})

test_that("morie_datasets_chicago_community_areas(offline=FALSE) routes through SODA3 with the canonical SELECT", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            ...) {
      seen <<- list(view_id = view_id, soql = soql)
      data.frame(community = "ROGERS PARK", area_numbe = "1")
    },
    .package = "morie")
  morie_datasets_chicago_community_areas(offline = FALSE)
  expect_equal(seen$view_id, "cauq-8yn6")
  expect_match(seen$soql,
               "^SELECT area_numbe, community, area_num_1, shape_area, shape_len ORDER BY area_numbe$")
})

test_that("morie_datasets_chicago_community_areas(offline=FALSE, geometry=TRUE) drops the $select restriction", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            ...) {
      seen <<- list(soql = soql)
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_community_areas(offline = FALSE,
                                           geometry = TRUE)
  expect_match(seen$soql, "^SELECT \\* ORDER BY area_numbe$")
})

# =================================================== IUCR codes (c7ck-438e)

test_that("morie_datasets_chicago_iucr_codes(offline=TRUE) reads bundled 410-row dictionary", {
  df <- morie_datasets_chicago_iucr_codes(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 410L)
  expect_setequal(names(df),
                  c("iucr", "primary_description",
                    "secondary_description", "index_code",
                    "active"))
  expect_type(df$iucr, "character")
  # Spot-check well-known IUCR codes.
  homicide <- df[df$iucr == "0110", ]
  if (nrow(homicide) == 0L) homicide <- df[df$iucr == "110", ]
  expect_true(nrow(homicide) >= 1L)
  expect_true(any(grepl("HOMICIDE",
                          homicide$primary_description,
                          ignore.case = TRUE)))
  # Index-code "I" rows should exist (FBI index crimes).
  expect_true("I" %in% df$index_code)
})

test_that("morie_datasets_chicago_iucr_codes(offline=TRUE) honours max_features", {
  df <- morie_datasets_chicago_iucr_codes(offline = TRUE,
                                            max_features = 25L)
  expect_equal(nrow(df), 25L)
})

test_that("morie_datasets_chicago_iucr_codes(offline=FALSE) routes through SODA2 fetch", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(url = url, max_features = max_features,
                    paginate = paginate)
      data.frame(iucr = "110",
                  primary_description = "HOMICIDE",
                  secondary_description = "FIRST DEGREE MURDER",
                  index_code = "I", active = TRUE)
    },
    .package = "morie")
  morie_datasets_chicago_iucr_codes(offline = FALSE,
                                      max_features = 50L,
                                      paginate = TRUE)
  expect_match(seen$url,
               "data\\.cityofchicago\\.org/resource/c7ck-438e\\.json$")
  expect_equal(seen$max_features, 50L)
  expect_true(isTRUE(seen$paginate))
})

test_that("morie_datasets_chicago_iucr_codes honours resource_id override", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, ...) {
      expect_match(url, "override-iucr-xyz\\.json$")
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_iucr_codes(
    offline = FALSE,
    resource_id = "override-iucr-xyz")
})

# =================================================== discovery helper

test_that("morie_datasets_external_socrata_layers registry grew to 11 in 3UU (3 new entries)", {
  reg <- morie_datasets_external_socrata_layers()
  expect_equal(nrow(reg), 11L)  # 8 (3PP+) + 3 (3UU)
  for (k in c("chicago_wards", "chicago_community_areas",
              "chicago_iucr_codes"))
    expect_true(k %in% reg$dataset_key,
                info = sprintf("registry missing 3UU entry %s", k))
  iucr <- reg[reg$dataset_key == "chicago_iucr_codes", ]
  expect_equal(iucr$resource_url,
               "https://data.cityofchicago.org/resource/c7ck-438e.json")
})

# =================================================== default offline behaviour

test_that("all 3 3UU loaders default to offline = TRUE", {
  expect_s3_class(morie_datasets_chicago_wards(),
                  "data.frame")
  expect_s3_class(morie_datasets_chicago_community_areas(),
                  "data.frame")
  expect_s3_class(morie_datasets_chicago_iucr_codes(),
                  "data.frame")
})

# =================================================== chicago_crime cross-ref alignment

test_that("chicago_crime's cross-ref block in @details was updated -- the 3UU wrappers are reachable", {
  # The chicago_crime docstring's @describe block names these 3
  # wrappers explicitly. Verify they exist + are callable.
  for (fn in list(morie_datasets_chicago_wards,
                  morie_datasets_chicago_community_areas,
                  morie_datasets_chicago_iucr_codes)) {
    expect_true(is.function(fn))
    df <- fn(offline = TRUE)
    expect_s3_class(df, "data.frame")
    expect_true(nrow(df) > 0L)
  }
})
