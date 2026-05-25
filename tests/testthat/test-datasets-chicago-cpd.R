# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3PP: City of Chicago Open Data "Arrests" (dpt3-jri9) Socrata
# wrapper + Chicago PD historical "Public Arrest Data" (2014-2017)
# static-CSV loader. Mirrors the 3LL/3OO pattern -- offline-default
# bundled fixtures, mockable live-mode dispatch via
# .morie_dataset_socrata_fetch (dpt3-jri9) or .morie_dataset_http_text
# (CPD historical CSV).

# ================================================ Chicago Open Data Arrests

test_that("morie_datasets_chicago_arrests(offline=TRUE) reads bundled 24-col fixture", {
  df <- morie_datasets_chicago_arrests(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 24L)
  expect_true(nrow(df) >= 5L)
  for (col in c("cb_no", "case_number", "arrest_date", "race",
                "charge_1_statute", "charge_1_description",
                "charge_1_type", "charge_1_class",
                "charges_statute", "charges_description",
                "charges_type", "charges_class"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("^SYNTH-ARR-", df$cb_no)))
})

test_that("morie_datasets_chicago_arrests(offline=TRUE) honours year + max_features", {
  df_all <- morie_datasets_chicago_arrests(offline = TRUE)
  df_2024 <- morie_datasets_chicago_arrests(offline = TRUE,
                                              year = 2024L)
  expect_true(nrow(df_2024) <= nrow(df_all))
  expect_true(all(substr(df_2024$arrest_date, 1L, 4L) == "2024"))
  df_cap <- morie_datasets_chicago_arrests(offline = TRUE,
                                             max_features = 2L)
  expect_equal(nrow(df_cap), 2L)
})

test_that("morie_datasets_chicago_arrests(offline=FALSE) dispatches via mocked Socrata fetch with canonical UUID", {
  stub <- data.frame(cb_no = c("LIVE-1", "LIVE-2"),
                      case_number = c("LIVE-CASE-1", "LIVE-CASE-2"),
                      arrest_date = c("2024-12-01", "2024-12-02"),
                      race = c("BLACK", "WHITE"))
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/dpt3-jri9\\.json$")
      expect_equal(where, "date_extract_y(arrest_date) = 2024")
      expect_equal(max_features, 50L)
      stub
    },
    .package = "morie")
  out <- morie_datasets_chicago_arrests(year = 2024L,
                                          max_features = 50L,
                                          offline = FALSE)
  expect_equal(out$cb_no, c("LIVE-1", "LIVE-2"))
})

test_that("morie_datasets_chicago_arrests(offline=FALSE) honours alias resource_id", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/arrests\\.json$")
      data.frame(cb_no = "ALIAS")
    },
    .package = "morie")
  out <- morie_datasets_chicago_arrests(offline = FALSE,
                                          resource_id = "arrests")
  expect_equal(out$cb_no, "ALIAS")
})

test_that("morie_datasets_chicago_arrests forwards paginate + page_size", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size,
                    max_pages = max_pages)
      data.frame(cb_no = "STUB")
    },
    .package = "morie")
  morie_datasets_chicago_arrests(offline = FALSE,
                                   paginate = TRUE,
                                   page_size = 5000L,
                                   max_pages = 42L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 5000L)
  expect_equal(seen$max_pages, 42L)
})

test_that("morie_datasets_chicago_arrests(offline=FALSE) sends NULL where when year omitted", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_null(where)
      data.frame(cb_no = "NOFILT")
    },
    .package = "morie")
  out <- morie_datasets_chicago_arrests(offline = FALSE)
  expect_equal(out$cb_no, "NOFILT")
})

# ============================================= CPD historical (chicagopolice.org)

test_that("morie_datasets_cpd_public_arrests(offline=TRUE) reads bundled 10-col fixture", {
  df <- morie_datasets_cpd_public_arrests(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 10L)
  expect_setequal(names(df),
                  c("ARR_DISTRICT", "ARR_BEAT", "ARR_YEAR",
                    "ARR_MONTH", "RACE_CODE_CD", "FBI_CODE",
                    "STATUTE", "STAT_DESCR", "CHARGE_CLASS_CD",
                    "CHARGE_TYPE_CD"))
  expect_true(all(grepl("^SYNTH", df$STAT_DESCR)))
})

test_that("morie_datasets_cpd_public_arrests(offline=TRUE) honours max_features", {
  df <- morie_datasets_cpd_public_arrests(offline = TRUE,
                                            max_features = 2L)
  expect_equal(nrow(df), 2L)
})

test_that("morie_datasets_cpd_public_arrests(offline=FALSE) requires `url`", {
  expect_error(
    morie_datasets_cpd_public_arrests(offline = FALSE),
    regexp = "lookup pending")
})

test_that("morie_datasets_cpd_public_arrests(offline=FALSE) hits .morie_dataset_http_text with the provided URL", {
  seen_url <- NULL
  raw_csv <- paste0(
    "ARR_DISTRICT,ARR_BEAT,ARR_YEAR,ARR_MONTH,RACE_CODE_CD,",
    "FBI_CODE,STATUTE,STAT_DESCR,CHARGE_CLASS_CD,CHARGE_TYPE_CD\n",
    "10,1033,2014,3,BLK,18,720 ILCS 570.0/407-B-1,LIVE DESC,X,F\n")
  testthat::local_mocked_bindings(
    .morie_dataset_http_text = function(url, query = NULL) {
      seen_url <<- url
      raw_csv
    },
    .package = "morie")
  df <- morie_datasets_cpd_public_arrests(
    offline = FALSE,
    url = "https://www.chicagopolice.org/wp-content/uploads/PublicReleaseArrestDataUPDATE.csv")
  expect_equal(seen_url,
               "https://www.chicagopolice.org/wp-content/uploads/PublicReleaseArrestDataUPDATE.csv")
  expect_equal(nrow(df), 1L)
  expect_equal(df$STAT_DESCR, "LIVE DESC")
})

test_that("morie_datasets_cpd_public_arrests(offline=FALSE) honours max_features when live", {
  raw_csv <- paste0(
    "ARR_DISTRICT,ARR_BEAT,ARR_YEAR,ARR_MONTH,RACE_CODE_CD,",
    "FBI_CODE,STATUTE,STAT_DESCR,CHARGE_CLASS_CD,CHARGE_TYPE_CD\n",
    "10,1033,2014,3,BLK,18,S1,D1,X,F\n",
    "9,923,2015,7,WWH,WRT,S2,D2,Z,\n",
    "14,1424,2016,11,BLK,06,S3,D3,A,M\n")
  testthat::local_mocked_bindings(
    .morie_dataset_http_text = function(url, query = NULL) raw_csv,
    .package = "morie")
  df <- morie_datasets_cpd_public_arrests(
    offline = FALSE,
    url = "https://x.test/y.csv",
    max_features = 2L)
  expect_equal(nrow(df), 2L)
})

# ============================================= Discovery helper extension

test_that("morie_datasets_external_socrata_layers now includes chicago_arrests (8 rows total post-3PP+)", {
  reg <- morie_datasets_external_socrata_layers()
  expect_equal(nrow(reg), 11L)  # +chicago_wards + community_areas + iucr_codes (3UU)
  expect_true("chicago_arrests" %in% reg$dataset_key)
  arr <- reg[reg$dataset_key == "chicago_arrests", ]
  expect_equal(arr$resource_url,
               "https://data.cityofchicago.org/resource/dpt3-jri9.json")
  expect_equal(arr$fixture, "chicago_arrests_dpt3_jri9_sample.csv")
})

# ============================================= Default offline behaviour

test_that("both 3PP loaders default to offline = TRUE (safer, no accidental network)", {
  expect_s3_class(morie_datasets_chicago_arrests(), "data.frame")
  expect_s3_class(morie_datasets_cpd_public_arrests(), "data.frame")
})

# ====================================== Chicago Police Beats (3PP+, n9it-hstw)

test_that("morie_datasets_chicago_police_beats(offline=TRUE) reads bundled 4-col attribute fixture", {
  df <- morie_datasets_chicago_police_beats(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 4L)
  expect_setequal(names(df),
                  c("beat_num", "beat", "sector", "district"))
  # All-character (no numeric coercion losing leading zeros).
  expect_type(df$beat_num, "character")
  expect_type(df$district, "character")
  # Spot-check a known beat from CPD's published listing.
  expect_true("1713" %in% df$beat_num)
  expect_true("3100" %in% df$beat_num)
})

test_that("morie_datasets_chicago_police_beats(offline=TRUE) honours max_features", {
  df <- morie_datasets_chicago_police_beats(offline = TRUE,
                                              max_features = 3L)
  expect_equal(nrow(df), 3L)
})

test_that("morie_datasets_chicago_police_beats(offline=FALSE, geometry=FALSE) requests attribute subset via $select", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/n9it-hstw\\.json\\?\\$select=beat_num,beat,sector,district$")
      data.frame(beat_num = "1234", beat = "3",
                  sector = "2", district = "12")
    },
    .package = "morie")
  out <- morie_datasets_chicago_police_beats(offline = FALSE)
  expect_equal(out$beat_num, "1234")
})

test_that("morie_datasets_chicago_police_beats(offline=FALSE, geometry=TRUE) drops the $select clause", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/n9it-hstw\\.json$")
      data.frame(the_geom = "GEOM-STUB", beat_num = "1234",
                  beat = "3", sector = "2", district = "12",
                  stringsAsFactors = FALSE)
    },
    .package = "morie")
  out <- morie_datasets_chicago_police_beats(offline = FALSE,
                                               geometry = TRUE)
  expect_true("the_geom" %in% names(out))
})

test_that("morie_datasets_chicago_police_beats forwards paginate args", {
  seen <- list()
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      seen <<- list(paginate = paginate, page_size = page_size,
                    max_pages = max_pages)
      data.frame(beat_num = "STUB")
    },
    .package = "morie")
  morie_datasets_chicago_police_beats(offline = FALSE,
                                        paginate = TRUE,
                                        page_size = 277L,
                                        max_pages = 2L)
  expect_true(isTRUE(seen$paginate))
  expect_equal(seen$page_size, 277L)
  expect_equal(seen$max_pages, 2L)
})

# ================================ Chicago Police Districts (3PP+, 24zt-jpfn)

test_that("morie_datasets_chicago_police_districts(offline=TRUE) reads bundled 2-col attribute fixture", {
  df <- morie_datasets_chicago_police_districts(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 2L)
  expect_setequal(names(df), c("dist_num", "dist_label"))
  expect_type(df$dist_num, "character")
  expect_type(df$dist_label, "character")
  # The 22 active districts + the special "31" headquarters polygon.
  for (d in c("1", "2", "12", "17", "22", "31"))
    expect_true(d %in% df$dist_num)
})

test_that("morie_datasets_chicago_police_districts(offline=TRUE) honours max_features", {
  df <- morie_datasets_chicago_police_districts(offline = TRUE,
                                                  max_features = 5L)
  expect_equal(nrow(df), 5L)
})

test_that("morie_datasets_chicago_police_districts(offline=FALSE, geometry=FALSE) requests $select=dist_num,dist_label", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url,
                   "data\\.cityofchicago\\.org/resource/24zt-jpfn\\.json\\?\\$select=dist_num,dist_label$")
      data.frame(dist_num = "1", dist_label = "1ST")
    },
    .package = "morie")
  out <- morie_datasets_chicago_police_districts(offline = FALSE)
  expect_equal(out$dist_label, "1ST")
})

test_that("morie_datasets_chicago_police_districts honours resource_id override", {
  testthat::local_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL,
                                              max_features = NULL,
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
      expect_match(url, "override-pd-xyz\\.json")
      data.frame(dist_num = "0", dist_label = "OVERRIDE")
    },
    .package = "morie")
  out <- morie_datasets_chicago_police_districts(
    offline = FALSE,
    resource_id = "override-pd-xyz")
  expect_equal(out$dist_label, "OVERRIDE")
})

# ====================================== discovery helper extension

test_that("morie_datasets_external_socrata_layers includes the 2 new 3PP+ boundary layers", {
  reg <- morie_datasets_external_socrata_layers()
  expect_true("chicago_police_beats" %in% reg$dataset_key)
  expect_true("chicago_police_districts" %in% reg$dataset_key)
  beats <- reg[reg$dataset_key == "chicago_police_beats", ]
  expect_equal(beats$resource_url,
               "https://data.cityofchicago.org/resource/n9it-hstw.json")
  expect_equal(beats$fixture, "chicago_police_beats.csv")
  dists <- reg[reg$dataset_key == "chicago_police_districts", ]
  expect_equal(dists$resource_url,
               "https://data.cityofchicago.org/resource/24zt-jpfn.json")
  expect_equal(dists$fixture, "chicago_police_districts.csv")
})
