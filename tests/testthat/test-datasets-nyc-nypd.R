# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3NN: NYC NYPD criminal-justice Socrata wrappers (8 datasets).
# Offline-mode fixtures + mocked live-mode dispatch via
# .morie_dataset_socrata_fetch.

# ============================================================== registry helper

test_that("morie_datasets_nyc_nypd_layers returns the full 8-row NYPD registry", {
  reg <- morie_datasets_nyc_nypd_layers()
  expect_s3_class(reg, "data.frame")
  expect_equal(nrow(reg), 8L)
  expect_setequal(names(reg),
                  c("dataset_key", "label", "resource_id",
                    "resource_url", "permalink",
                    "data_dictionary_url", "footnotes_url",
                    "fixture"))
  expected_keys <- c("nypd_arrests_historic", "nypd_arrests_ytd",
                      "nypd_complaint_historic",
                      "nypd_complaint_ytd",
                      "nypd_hate_crimes",
                      "nypd_uof_incidents",
                      "nypd_uof_subjects",
                      "nypd_vehicle_stops")
  expect_setequal(reg$dataset_key, expected_keys)
  # All 8 resource_ids are non-NA Socrata 4-4 UUIDs.
  expect_true(all(grepl("^[a-z0-9]{4}-[a-z0-9]{4}$", reg$resource_id)))
  # All resource_urls hit data.cityofnewyork.us.
  expect_true(all(grepl(
    "^https://data\\.cityofnewyork\\.us/resource/", reg$resource_url)))
  # All 8 datasets have a stable /d/<id> permalink.
  expect_true(all(grepl(
    "^https://data\\.cityofnewyork\\.us/d/[a-z0-9]{4}-[a-z0-9]{4}$",
    reg$permalink)))
})

test_that("nypd_arrests_ytd carries the canonical XLSX data dictionary + PDF footnotes URLs", {
  reg <- morie_datasets_nyc_nypd_layers()
  ytd <- reg[reg$dataset_key == "nypd_arrests_ytd", ]
  expect_false(is.na(ytd$data_dictionary_url))
  expect_match(ytd$data_dictionary_url,
               "NYPD_Arrest_YTD_DataDictionary\\.xlsx$")
  expect_false(is.na(ytd$footnotes_url))
  expect_match(ytd$footnotes_url,
               "NYPD_Arrest_Incident_Level_Data_Footnotes\\.pdf$")
})

test_that("the other 7 NYPD entries leave data_dictionary_url + footnotes_url as NA (lookup pending)", {
  reg <- morie_datasets_nyc_nypd_layers()
  pending <- reg[reg$dataset_key != "nypd_arrests_ytd", ]
  expect_true(all(is.na(pending$data_dictionary_url)))
  expect_true(all(is.na(pending$footnotes_url)))
})

# =================================================== per-loader offline schemas

test_that("morie_datasets_nyc_nypd_arrests_historic offline -> 19-col arrest schema", {
  df <- morie_datasets_nyc_nypd_arrests_historic(offline = TRUE)
  expect_equal(ncol(df), 19L)
  for (col in c("arrest_key", "arrest_date", "ofns_desc",
                "arrest_boro", "arrest_precinct", "age_group",
                "perp_sex", "perp_race"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("^NYPD-AH-SYNTH", df$arrest_key)))
})

test_that("morie_datasets_nyc_nypd_arrests_ytd offline -> 19-col YTD arrest schema", {
  df <- morie_datasets_nyc_nypd_arrests_ytd(offline = TRUE)
  expect_equal(ncol(df), 19L)
  expect_true("geocoded_column" %in% names(df))
  expect_true(all(grepl("^NYPD-YA-SYNTH", df$arrest_key)))
})

test_that("morie_datasets_nyc_nypd_complaint_historic offline -> 35-col schema", {
  df <- morie_datasets_nyc_nypd_complaint_historic(offline = TRUE)
  expect_equal(ncol(df), 35L)
  for (col in c("cmplnt_num", "cmplnt_fr_dt", "addr_pct_cd",
                "ofns_desc", "boro_nm", "susp_age_group",
                "susp_race", "susp_sex", "vic_age_group",
                "vic_race", "vic_sex"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_nyc_nypd_complaint_ytd offline -> 36-col schema", {
  df <- morie_datasets_nyc_nypd_complaint_ytd(offline = TRUE)
  expect_equal(ncol(df), 36L)
  # YTD variant adds 'geocoded_column' on top of historic's 35 + has
  # different column ordering.
  expect_true("geocoded_column" %in% names(df))
})

test_that("morie_datasets_nyc_nypd_hate_crimes offline -> 14-col bias schema", {
  df <- morie_datasets_nyc_nypd_hate_crimes(offline = TRUE)
  expect_equal(ncol(df), 14L)
  for (col in c("full_complaint_id", "complaint_year_number",
                "complaint_precinct_code", "patrol_borough_name",
                "bias_motive_description", "offense_category"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_nyc_nypd_uof_incidents offline -> 7-col incidents schema", {
  df <- morie_datasets_nyc_nypd_uof_incidents(offline = TRUE)
  expect_equal(ncol(df), 7L)
  for (col in c("tri_incident_number", "forcetype",
                "occurrence_date", "basisforencounter"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_nyc_nypd_uof_subjects offline -> 8-col subjects schema", {
  df <- morie_datasets_nyc_nypd_uof_subjects(offline = TRUE)
  expect_equal(ncol(df), 8L)
  for (col in c("tri_incident_number", "subject_injury_level",
                "subject_race", "subject_gender",
                "force_against_mos", "subject_used_force"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_nyc_nypd_vehicle_stops offline -> 19-col vehicle-stop schema", {
  df <- morie_datasets_nyc_nypd_vehicle_stops(offline = TRUE)
  expect_equal(ncol(df), 19L)
  for (col in c("evnt_key", "occur_dt", "veh_seized_flg",
                "arrest_made_flg", "summon_issued_flg",
                "race_desc"))
    expect_true(col %in% names(df))
})

# =================================================== max_features truncation

test_that("max_features truncation works across all 8 wrappers", {
  for (fn in list(morie_datasets_nyc_nypd_arrests_historic,
                  morie_datasets_nyc_nypd_arrests_ytd,
                  morie_datasets_nyc_nypd_complaint_historic,
                  morie_datasets_nyc_nypd_complaint_ytd,
                  morie_datasets_nyc_nypd_hate_crimes,
                  morie_datasets_nyc_nypd_uof_incidents,
                  morie_datasets_nyc_nypd_uof_subjects,
                  morie_datasets_nyc_nypd_vehicle_stops)) {
    df <- fn(offline = TRUE, max_features = 2L)
    expect_equal(nrow(df), 2L)
  }
})

# =================================================== mocked live-mode dispatch

test_that("each NYPD wrapper auto-resolves canonical resource_id from registry on offline=FALSE", {
  expected <- list(
    list(fn = morie_datasets_nyc_nypd_arrests_historic,   rid = "8h9b-rp9u"),
    list(fn = morie_datasets_nyc_nypd_arrests_ytd,        rid = "uip8-fykc"),
    list(fn = morie_datasets_nyc_nypd_complaint_historic, rid = "qgea-i56i"),
    list(fn = morie_datasets_nyc_nypd_complaint_ytd,      rid = "5uac-w243"),
    list(fn = morie_datasets_nyc_nypd_hate_crimes,        rid = "bqiq-cu78"),
    list(fn = morie_datasets_nyc_nypd_uof_incidents,      rid = "f4tj-796d"),
    list(fn = morie_datasets_nyc_nypd_uof_subjects,       rid = "dufe-vxb7"),
    list(fn = morie_datasets_nyc_nypd_vehicle_stops,      rid = "hn9i-dwpr"))
  for (R in expected) {
    out <- testthat::with_mocked_bindings(
      .morie_dataset_socrata_fetch = function(url, where = NULL,
                                                max_features = NULL,
                                                app_token = NULL,
                                                paginate = FALSE,
                                                page_size = 1000L,
                                                max_pages = 200L) {
        expect_match(url,
                     sprintf(
                       "data\\.cityofnewyork\\.us/resource/%s\\.json$",
                       R$rid))
        data.frame(stub = 1L)
      },
      .package = "morie",
      code = R$fn(offline = FALSE))
    expect_s3_class(out, "data.frame")
  }
})

test_that("year filter emits the right SoQL $where for date-column datasets", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_equal(where, "date_extract_y(arrest_date) = 2024")
      data.frame(stub = 1L)
    },
    .package = "morie")
  out <- morie_datasets_nyc_nypd_arrests_historic(year = 2024L,
                                                    offline = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("year filter for hate_crimes uses complaint_year_number (no date_extract)", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_equal(where, "complaint_year_number = 2023")
      data.frame(stub = 1L)
    },
    .package = "morie")
  out <- morie_datasets_nyc_nypd_hate_crimes(year = 2023L,
                                               offline = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("year filter is omitted for uof_subjects (no date column)", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_null(where)
      data.frame(stub = 1L)
    },
    .package = "morie")
  out <- morie_datasets_nyc_nypd_uof_subjects(year = 2024L,
                                                offline = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("resource_id override is honoured", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url, "override-nypd-xyz\\.json$")
      data.frame(stub = 1L)
    },
    .package = "morie")
  out <- morie_datasets_nyc_nypd_arrests_historic(
    offline = FALSE, resource_id = "override-nypd-xyz")
  expect_s3_class(out, "data.frame")
})

# =================================================== generic by_key factory

test_that("morie_datasets_nyc_nypd_by_key reads bundled fixture offline", {
  df <- morie_datasets_nyc_nypd_by_key("nypd_hate_crimes",
                                          offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true("bias_motive_description" %in% names(df))
})

test_that("morie_datasets_nyc_nypd_by_key dispatches to mocked Socrata on live", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url, "f4tj-796d\\.json$")
      data.frame(stub = 1L)
    },
    .package = "morie")
  out <- morie_datasets_nyc_nypd_by_key("nypd_uof_incidents",
                                          offline = FALSE)
  expect_s3_class(out, "data.frame")
})

test_that("morie_datasets_nyc_nypd_by_key errors on unknown key", {
  expect_error(
    morie_datasets_nyc_nypd_by_key("not-a-real-key",
                                     offline = TRUE),
    regexp = "unknown NYC NYPD dataset_key")
})
