# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3VV+: chicago_crime mode=soda2/soda3 + resolved-joins
# analyzer. Verifies the dual-API routing + the 5-way join against
# the bundled boundary + dictionary fixtures.

# =================================================== chicago_crime mode arg

test_that("morie_datasets_chicago_crime(mode='soda2', offline=FALSE) routes through .morie_dataset_socrata_fetch", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(url = url, where = where)
      data.frame(id = 1L)
    },
    .package = "morie")
  morie_datasets_chicago_crime(year = 2024L, offline = FALSE,
                                  mode = "soda2")
  expect_match(seen$url, "ijzp-q8t2\\.json$")
  expect_equal(seen$where, "year=2024")
})

test_that("morie_datasets_chicago_crime(mode='soda3', offline=FALSE) routes through .morie_dataset_soda3_query", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            app_token = NULL,
                                            paginate = FALSE,
                                            page_size = 1000L,
                                            max_pages = 200L,
                                            max_features = NULL,
                                            base_url = "https://data.cityofchicago.org") {
      seen <<- list(view_id = view_id, soql = soql,
                    app_token = app_token, max_features = max_features)
      data.frame(id = 1L)
    },
    .package = "morie")
  morie_datasets_chicago_crime(year = 2024L, offline = FALSE,
                                  mode = "soda3",
                                  max_features = 100L,
                                  app_token = "tok-3vv")
  expect_equal(seen$view_id, "ijzp-q8t2")
  expect_match(seen$soql, "^SELECT \\* WHERE year=2024$")
  expect_equal(seen$app_token, "tok-3vv")
  expect_equal(seen$max_features, 100L)
})

test_that("morie_datasets_chicago_crime(mode='soda3') defaults to SELECT * when no year", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            ...) {
      seen <<- list(soql = soql)
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_crime(offline = FALSE, mode = "soda3")
  expect_equal(seen$soql, "SELECT *")
})

test_that("morie_datasets_chicago_crime defaults mode='soda2' (backward-compat)", {
  cpp_was_called <- FALSE
  soda3_was_called <- FALSE
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, ...) {
      cpp_was_called <<- TRUE
      data.frame()
    },
    .morie_dataset_soda3_query = function(view_id, ...) {
      soda3_was_called <<- TRUE
      data.frame()
    },
    .package = "morie")
  morie_datasets_chicago_crime(offline = FALSE)
  expect_true(cpp_was_called)
  expect_false(soda3_was_called)
})

test_that("morie_datasets_chicago_crime rejects unknown mode", {
  expect_error(morie_datasets_chicago_crime(offline = FALSE,
                                              mode = "graphql"))
})

# =================================================== resolved-joins analyzer

test_that("morie_datasets_chicago_crime_resolved(offline=TRUE) joins all 5 resolvers by default", {
  df <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)  # crime fixture has 5 rows
  # Expect the canonical prefixed resolver columns.
  for (col in c("ward_shape_leng", "ward_shape_area",
                "community_community", "community_shape_area",
                "beat_sector", "beat_district",
                "district_dist_label",
                "iucr_primary_description",
                "iucr_secondary_description",
                "iucr_index_code", "iucr_active"))
    expect_true(col %in% names(df),
                info = sprintf("missing resolver col: %s", col))
})

test_that("morie_datasets_chicago_crime_resolved(resolvers='iucr') only joins iucr", {
  df <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE, resolvers = "iucr"))
  expect_true("iucr_primary_description" %in% names(df))
  expect_true("iucr_secondary_description" %in% names(df))
  expect_false("ward_shape_leng" %in% names(df))
  expect_false("community_community" %in% names(df))
  expect_false("beat_sector" %in% names(df))
  expect_false("district_dist_label" %in% names(df))
})

test_that("morie_datasets_chicago_crime_resolved(resolvers=c('ward','iucr')) joins those two only", {
  df <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE, resolvers = c("ward", "iucr")))
  expect_true("ward_shape_leng" %in% names(df))
  expect_true("iucr_primary_description" %in% names(df))
  expect_false("community_community" %in% names(df))
  expect_false("beat_sector" %in% names(df))
})

test_that("morie_datasets_chicago_crime_resolved correctly resolves IUCR codes against the dictionary", {
  df <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE, resolvers = "iucr"))
  # The synthetic chicago_crime fixture has 5 known IUCR codes;
  # each should resolve to a non-NA primary_description (joining
  # against the bundled 410-row IUCR dictionary).
  expect_true(all(!is.na(df$iucr_primary_description)),
              info = paste("unresolved IUCRs:",
                            paste(df$iucr[is.na(df$iucr_primary_description)],
                                   collapse = ", ")))
})

test_that("morie_datasets_chicago_crime_resolved beat-join drops the within-sector beat collision", {
  # The chicago_police_beats fixture carries both `beat_num` (the
  # 4-digit form crime$beat matches) AND a 1-digit `beat`. The
  # analyzer must drop the latter before renaming beat_num -> beat
  # so the merge by="beat" has a unique key. Regression-test for
  # the bug found during 3VV+ smoke.
  df <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE, resolvers = "beat"))
  expect_s3_class(df, "data.frame")
  expect_true("beat" %in% names(df))
  expect_true("beat_sector" %in% names(df))
  expect_true("beat_district" %in% names(df))
})

test_that("morie_datasets_chicago_crime_resolved row count is preserved (left-join semantics)", {
  base <- suppressWarnings(morie_datasets_chicago_crime(
    offline = TRUE))
  resolved <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE))
  expect_equal(nrow(resolved), nrow(base))
})

test_that("morie_datasets_chicago_crime_resolved forwards mode + paginate + app_token to chicago_crime", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_chicago_crime = function(year = NULL,
                                              max_features = NULL,
                                              offline = TRUE,
                                              mode = c("soda2", "soda3"),
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L,
                                              app_token = NULL) {
      mode <- match.arg(mode)
      seen <<- list(mode = mode, paginate = paginate,
                    app_token = app_token, max_features = max_features)
      data.frame(beat = "1234", district = "12", ward = "1",
                  community_area = "1", iucr = "110")
    },
    .package = "morie")
  out <- morie_datasets_chicago_crime_resolved(
    offline = FALSE,
    mode = "soda3",
    paginate = TRUE,
    app_token = "tok-resolved",
    max_features = 50L,
    resolvers = "iucr")
  expect_equal(seen$mode, "soda3")
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$app_token, "tok-resolved")
  expect_equal(seen$max_features, 50L)
  expect_true("iucr_primary_description" %in% names(out))
})

test_that("morie_datasets_chicago_crime_resolved rejects unknown resolver names", {
  expect_error(
    morie_datasets_chicago_crime_resolved(offline = TRUE,
                                            resolvers = "ghost"))
})

test_that("morie_datasets_chicago_crime_resolved iucr join resolves the SYNTHETIC 0820 / 0498 / 2024 codes correctly", {
  # Specific spot-check: the synthetic chicago_crime fixture's row 1
  # has iucr="0820" (theft). Verify the join lifts the canonical
  # primary_description.
  df <- suppressWarnings(morie_datasets_chicago_crime_resolved(
    offline = TRUE, resolvers = "iucr"))
  dict <- morie_datasets_chicago_iucr_codes(offline = TRUE)
  for (i in seq_len(nrow(df))) {
    expected <- dict$primary_description[dict$iucr == df$iucr[i]]
    if (length(expected) > 0L) {
      expect_equal(df$iucr_primary_description[i], expected,
                   info = sprintf("row %d iucr=%s", i, df$iucr[i]))
    }
  }
})
