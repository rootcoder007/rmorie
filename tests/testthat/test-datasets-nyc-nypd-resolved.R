# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3AAA: NYC NYPD dual-mode (soda2/soda3) + resolved-joins
# analyzer + 2 NYC OpenData resolvers (police precincts + borough
# boundaries). Mirrors the chicago_crime_resolved pattern from 3VV+.

# =================================================== dual-mode NYPD wrappers

test_that("all NYPD wrappers + by_key now accept mode + app_token", {
  for (fn in list(morie_datasets_nyc_nypd_by_key,
                  morie_datasets_nyc_nypd_arrests_historic,
                  morie_datasets_nyc_nypd_arrests_ytd,
                  morie_datasets_nyc_nypd_complaint_historic,
                  morie_datasets_nyc_nypd_complaint_ytd,
                  morie_datasets_nyc_nypd_hate_crimes,
                  morie_datasets_nyc_nypd_uof_incidents,
                  morie_datasets_nyc_nypd_uof_subjects,
                  morie_datasets_nyc_nypd_vehicle_stops)) {
    args <- names(formals(fn))
    expect_true("mode" %in% args)
    expect_true("app_token" %in% args)
    expect_equal(eval(formals(fn)$mode), c("soda2", "soda3"))
  }
})

test_that("default mode='soda2' for NYPD wrappers routes through .morie_dataset_socrata_fetch", {
  soda2 <- FALSE; soda3 <- FALSE
  testthat::with_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, ...) {
      soda2 <<- TRUE
      data.frame()
    },
    .morie_dataset_soda3_query = function(view_id, ...) {
      soda3 <<- TRUE
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_nyc_nypd_arrests_ytd(offline = FALSE))
  expect_true(soda2)
  expect_false(soda3)
})

test_that("mode='soda3' routes through .morie_dataset_soda3_query with NYC base_url + app_token", {
  seen <- list()
  testthat::with_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            app_token = NULL,
                                            base_url = "https://data.cityofchicago.org",
                                            ...) {
      seen <<- list(view_id = view_id, soql = soql,
                    app_token = app_token, base_url = base_url)
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_nyc_nypd_arrests_historic(
      year = 2024L, offline = FALSE,
      mode = "soda3", app_token = "tok-nypd"))
  expect_equal(seen$view_id, "8h9b-rp9u")
  expect_equal(seen$soql, "SELECT * WHERE date_extract_y(arrest_date) = 2024")
  expect_equal(seen$app_token, "tok-nypd")
  expect_equal(seen$base_url, "https://data.cityofnewyork.us")
})

test_that("NYPD complaint hits cmplnt_fr_dt year filter under both modes", {
  # SODA2
  seen_w <- NULL
  testthat::with_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL, ...) {
      seen_w <<- where
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_nyc_nypd_complaint_historic(
      year = 2024L, offline = FALSE, mode = "soda2"))
  expect_equal(seen_w, "date_extract_y(cmplnt_fr_dt) = 2024")
  # SODA3
  seen_s <- NULL
  testthat::with_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *", ...) {
      seen_s <<- soql
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_nyc_nypd_complaint_historic(
      year = 2024L, offline = FALSE, mode = "soda3"))
  expect_equal(seen_s, "SELECT * WHERE date_extract_y(cmplnt_fr_dt) = 2024")
})

# =================================================== new resolver wrappers

test_that("morie_datasets_nyc_police_precincts(offline=TRUE) reads bundled 78-row fixture", {
  df <- morie_datasets_nyc_police_precincts(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 78L)
  expect_setequal(names(df),
                  c("precinct", "shape_leng", "shape_area"))
  expect_type(df$precinct, "character")
  # Spot-check a well-known precinct (1st Precinct in Lower Manhattan).
  expect_true("1" %in% df$precinct)
  expect_true("75" %in% df$precinct)  # 75th, East New York
})

test_that("morie_datasets_nyc_boroughs(offline=TRUE) reads bundled 5-row fixture", {
  df <- morie_datasets_nyc_boroughs(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
  expect_setequal(names(df),
                  c("borocode", "boroname",
                    "shape_area", "shape_leng"))
  expect_setequal(df$borocode, as.character(1:5))
  expect_setequal(df$boroname,
                  c("Manhattan", "Bronx", "Brooklyn",
                    "Queens", "Staten Island"))
})

test_that("morie_datasets_nyc_boroughs(mode='soda3') routes via SODA3 + the_geom-only stripping", {
  seen <- list()
  testthat::with_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                            base_url = "https://data.cityofchicago.org",
                                            ...) {
      seen <<- list(view_id = view_id, soql = soql,
                    base_url = base_url)
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_nyc_boroughs(offline = FALSE,
                                          mode = "soda3"))
  expect_equal(seen$view_id, "gthc-hcne")
  expect_equal(seen$soql,
                "SELECT borocode, boroname, shape_area, shape_leng")
  expect_equal(seen$base_url, "https://data.cityofnewyork.us")
})

# =================================================== borough crosswalk

test_that("morie_datasets_nyc_nypd_boro_crosswalk returns the 5-row 4-col map", {
  cw <- morie_datasets_nyc_nypd_boro_crosswalk()
  expect_equal(nrow(cw), 5L)
  expect_setequal(names(cw),
                  c("arrest_boro", "boro_nm",
                    "borocode", "boroname"))
  # NYPD's 1-letter codes B/Q/M/S/K all appear.
  expect_setequal(cw$arrest_boro, c("M", "B", "K", "Q", "S"))
  # Maps to the correct boroname.
  m <- cw[cw$arrest_boro == "M", ]
  expect_equal(m$boro_nm, "MANHATTAN")
  expect_equal(m$borocode, "1")
  expect_equal(m$boroname, "Manhattan")
})

# =================================================== resolved-joins analyzer

test_that("morie_datasets_nyc_nypd_resolved(offline=TRUE) joins boro + precinct for arrests_ytd", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)  # synthetic fixture has 5 rows
  # Borough resolver columns (prefixed boro_*).
  for (col in c("boro_borocode", "boro_boroname",
                "boro_boro_nm", "boro_shape_area",
                "boro_shape_leng"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
  # Precinct resolver columns (prefixed precinct_*).
  for (col in c("precinct_shape_leng", "precinct_shape_area"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
  # All arrests_ytd rows resolve their arrest_boro to a borough name.
  expect_true(all(!is.na(df$boro_boroname)))
  expect_true(all(df$boro_boroname %in%
                    c("Manhattan", "Bronx", "Brooklyn",
                      "Queens", "Staten Island")))
})

test_that("morie_datasets_nyc_nypd_resolved(resolvers='boro') only joins borough", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE,
                                            resolvers = "boro")
  expect_true("boro_boroname" %in% names(df))
  expect_false("precinct_shape_leng" %in% names(df))
})

test_that("morie_datasets_nyc_nypd_resolved(resolvers='precinct') only joins precinct", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE,
                                            resolvers = "precinct")
  expect_false("boro_boroname" %in% names(df))
  expect_true("precinct_shape_leng" %in% names(df))
})

test_that("morie_datasets_nyc_nypd_resolved correctly maps arrest_boro 1-letter -> boroname", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE,
                                            resolvers = "boro")
  cw <- morie_datasets_nyc_nypd_boro_crosswalk()
  for (i in seq_len(nrow(df))) {
    expected <- cw$boroname[cw$arrest_boro == df$arrest_boro[i]]
    if (length(expected) > 0L) {
      expect_equal(df$boro_boroname[i], expected,
                   info = sprintf("row %d arrest_boro=%s",
                                   i, df$arrest_boro[i]))
    }
  }
})

test_that("morie_datasets_nyc_nypd_resolved row count is preserved", {
  base <- morie_datasets_nyc_nypd_by_key("nypd_arrests_ytd",
                                            offline = TRUE)
  resolved <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                                  offline = TRUE)
  expect_equal(nrow(resolved), nrow(base))
})

test_that("morie_datasets_nyc_nypd_resolved forwards mode + app_token to by_key", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_nyc_nypd_by_key = function(dataset_key,
                                                year = NULL,
                                                max_features = NULL,
                                                offline = TRUE,
                                                resource_id = NULL,
                                                mode = c("soda2", "soda3"),
                                                paginate = FALSE,
                                                page_size = 1000L,
                                                max_pages = 200L,
                                                app_token = NULL) {
      mode <- match.arg(mode)
      seen <<- list(dataset_key = dataset_key, mode = mode,
                    app_token = app_token)
      data.frame(arrest_boro = "M", arrest_precinct = "1")
    },
    .package = "morie")
  morie_datasets_nyc_nypd_resolved(
    "nypd_arrests_ytd",
    offline = FALSE, mode = "soda3",
    app_token = "tok-resolved-nypd")
  expect_equal(seen$dataset_key, "nypd_arrests_ytd")
  expect_equal(seen$mode, "soda3")
  expect_equal(seen$app_token, "tok-resolved-nypd")
})

test_that("morie_datasets_nyc_nypd_resolved rejects unknown resolver names", {
  expect_error(
    morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                       offline = TRUE,
                                       resolvers = "alien"))
})

# =================================================== offense codes wrapper

test_that("morie_datasets_nyc_nypd_offense_codes returns 246-row 5-col dict", {
  od <- morie_datasets_nyc_nypd_offense_codes()
  expect_s3_class(od, "data.frame")
  expect_equal(nrow(od), 246L)
  expect_setequal(names(od),
                  c("ky_cd", "ofns_desc", "pd_cd",
                    "pd_desc", "law_cat_cd"))
  expect_type(od$ky_cd, "character")
  expect_type(od$pd_cd, "character")
  # Spot-check well-known codes.
  rape <- subset(od, ky_cd == "104")
  expect_true(nrow(rape) >= 1L)
  expect_true(all(rape$ofns_desc == "RAPE"))
  # max_features cap is honoured.
  capped <- morie_datasets_nyc_nypd_offense_codes(max_features = 10L)
  expect_equal(nrow(capped), 10L)
})

test_that("morie_datasets_nyc_nypd_offense_codes hits known (ky_cd, pd_cd) pairs", {
  od <- morie_datasets_nyc_nypd_offense_codes()
  # (105, 397) -> ROBBERY OPEN AREA UNCLASSIFIED, felony.
  hit <- subset(od, ky_cd == "105" & pd_cd == "397")
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$ofns_desc, "ROBBERY")
  expect_equal(hit$law_cat_cd, "F")
})

# =================================================== offense resolver

test_that("morie_datasets_nyc_nypd_resolved(resolvers='offense') joins on (ky_cd, pd_cd)", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE,
                                            resolvers = "offense")
  expect_true("offense_ofns_desc" %in% names(df))
  expect_true("offense_pd_desc" %in% names(df))
  expect_true("offense_law_cat_cd" %in% names(df))
  # All 5 fixture rows resolve (fixture was aligned with real
  # NYPD (ky_cd, pd_cd) pairs).
  expect_true(all(!is.na(df$offense_ofns_desc)))
  expect_true(all(!is.na(df$offense_pd_desc)))
  expect_true(all(!is.na(df$offense_law_cat_cd)))
  # Spot-check: (105, 397) -> "ROBBERY".
  row <- df[df$ky_cd == "105" & df$pd_cd == "397", ]
  expect_equal(row$offense_ofns_desc, "ROBBERY")
  expect_equal(row$offense_law_cat_cd, "F")
})

test_that("morie_datasets_nyc_nypd_resolved offense join also fires for default 3-resolver call", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE)
  for (col in c("offense_ofns_desc", "offense_pd_desc",
                "offense_law_cat_cd"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
})

test_that("morie_datasets_nyc_nypd_resolved offense join no-ops for datasets without ky_cd/pd_cd", {
  # UoF subjects + vehicle stops don't carry ky_cd/pd_cd; the
  # offense resolver must silently fall through, not error.
  for (key in c("nypd_uof_subjects", "nypd_vehicle_stops")) {
    df <- morie_datasets_nyc_nypd_resolved(key,
                                              offline = TRUE,
                                              resolvers = "offense")
    expect_false("offense_ofns_desc" %in% names(df),
                  info = sprintf("dataset=%s", key))
  }
})

test_that("morie_datasets_nyc_nypd_resolved offense resolver name is accepted", {
  expect_error(
    morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                        offline = TRUE,
                                        resolvers = "offense"),
    regexp = NA)
})

# =================================================== law_code resolver (3CCC1)

test_that("morie_datasets_nyc_nypd_law_books returns the statute book dict", {
  b <- morie_datasets_nyc_nypd_law_books()
  expect_s3_class(b, "data.frame")
  expect_setequal(names(b), c("book", "name", "jurisdiction"))
  expect_true(nrow(b) >= 22L)
  pl <- subset(b, book == "PL")
  expect_equal(pl$name, "Penal Law")
  expect_equal(pl$jurisdiction, "NYS")
  vtl <- subset(b, book == "VTL")
  expect_equal(vtl$name, "Vehicle and Traffic Law")
  ac <- subset(b, book == "AC")
  expect_equal(ac$jurisdiction, "NYC")
})

test_that("morie_parse_nypd_law_code splits prefix + section", {
  p <- morie_parse_nypd_law_code(c("PL 1601005", "VTL0511000",
                                     "AC 0019190", "ABC0064A00",
                                     NA, "", "garbage"))
  expect_equal(p$book,
                c("PL", "VTL", "AC", "ABC", NA, NA, NA))
  expect_equal(p$section,
                c("1601005", "0511000", "0019190", "0064A00",
                  NA, NA, NA))
})

test_that("law_code resolver adds 4 cols + joins book name", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE,
                                            resolvers = "law_code")
  for (col in c("law_book", "law_section",
                "law_book_name", "law_jurisdiction"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
  expect_true(all(df$law_book == "PL"))
  expect_true(all(df$law_book_name == "Penal Law"))
  expect_true(all(df$law_jurisdiction == "NYS"))
})

test_that("default 4-resolver call adds all law_code cols", {
  df <- morie_datasets_nyc_nypd_resolved("nypd_arrests_ytd",
                                            offline = TRUE)
  for (col in c("law_book", "law_section",
                "law_book_name", "law_jurisdiction"))
    expect_true(col %in% names(df),
                info = sprintf("missing %s", col))
})

test_that("law_code resolver silently no-ops when law_code absent", {
  # UoF subjects + vehicle stops don't carry law_code at all.
  for (key in c("nypd_uof_subjects", "nypd_vehicle_stops")) {
    df <- morie_datasets_nyc_nypd_resolved(key,
                                              offline = TRUE,
                                              resolvers = "law_code")
    expect_false("law_book_name" %in% names(df),
                  info = sprintf("dataset=%s", key))
  }
})
