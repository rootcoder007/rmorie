# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3CCC2: NYC multi-boundary loader bundle.

test_that("morie_datasets_nyc_school_districts(offline=TRUE) reads 33-row fixture", {
  df <- morie_datasets_nyc_school_districts(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 33L)
  expect_setequal(names(df),
                  c("schooldist", "shape_leng", "shape_area"))
  expect_type(df$schooldist, "character")
  # Manhattan -> CSD 1-6; Bronx -> 7-12; Brooklyn -> 13-23; Queens
  # -> 24-30; SI -> 31. Spot-check both ends.
  expect_true("1" %in% df$schooldist)
  expect_true("32" %in% df$schooldist)
})

test_that("morie_datasets_nyc_council_districts(offline=TRUE) reads 51-row fixture", {
  df <- morie_datasets_nyc_council_districts(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 51L)
  expect_setequal(names(df),
                  c("coundist", "shape_leng", "shape_area"))
  expect_type(df$coundist, "character")
  # Council districts run 1 through 51.
  expect_setequal(as.integer(df$coundist), 1:51)
})

test_that("morie_datasets_nyc_community_districts(offline=TRUE) reads 71-row fixture", {
  df <- morie_datasets_nyc_community_districts(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 71L)
  expect_setequal(names(df),
                  c("boro_cd", "shape_leng", "shape_area"))
  expect_type(df$boro_cd, "character")
})

test_that("morie_datasets_nyc_ntas_2020(offline=TRUE) reads 262-row fixture", {
  df <- morie_datasets_nyc_ntas_2020(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 262L)
  for (col in c("nta2020", "ntaname", "ntaabbrev", "ntatype",
                "cdta2020", "cdtaname", "borocode", "boroname",
                "countyfips", "shape_leng", "shape_area"))
    expect_true(col %in% names(df),
                 info = sprintf("missing %s", col))
  expect_setequal(df$boroname,
                  c("Manhattan", "Bronx", "Brooklyn",
                    "Queens", "Staten Island"))
  # NTA abbreviations range 2-10 chars (NTA naming is not fixed-width).
  expect_true(all(nchar(df$ntaabbrev) >= 2L &
                     nchar(df$ntaabbrev) <= 12L))
})

test_that("morie_datasets_nyc_zctas(offline=TRUE) reads 221-row ZCTA fixture", {
  df <- morie_datasets_nyc_zctas(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 221L)
  expect_setequal(names(df),
                  c("zcta5", "arealand", "areawater",
                    "centlat", "centlon",
                    "intptlat", "intptlon"))
  expect_type(df$zcta5, "character")
  # All ZCTAs are 5-digit ZIP codes.
  expect_true(all(nchar(df$zcta5) == 5L))
  # Manhattan ZIPs start with 100xx; SI with 103xx; Bronx with 104xx;
  # Queens with 11xxx; Brooklyn with 112xx.
  expect_true(any(grepl("^100", df$zcta5)))
  expect_true(any(grepl("^112", df$zcta5)))
})

test_that("max_features cap is honoured on all 5 loaders", {
  for (fn in list(morie_datasets_nyc_school_districts,
                   morie_datasets_nyc_council_districts,
                   morie_datasets_nyc_community_districts,
                   morie_datasets_nyc_ntas_2020,
                   morie_datasets_nyc_zctas)) {
    expect_equal(nrow(fn(offline = TRUE, max_features = 3L)), 3L)
  }
})

test_that("morie_datasets_nyc_boundaries_catalog lists 7 boundary types", {
  cat_df <- morie_datasets_nyc_boundaries_catalog()
  expect_s3_class(cat_df, "data.frame")
  expect_equal(nrow(cat_df), 7L)
  expect_setequal(names(cat_df),
                  c("boundary", "loader", "soda_id", "n_rows",
                    "join_key", "row_key_joinable_to_nypd"))
  # Only borough + precinct are row-key joinable to NYPD CJ data
  # (district boundaries need a spatial join).
  expect_setequal(cat_df$boundary[cat_df$row_key_joinable_to_nypd],
                  c("borough", "police_precinct"))
  # Every loader name exists as an exported morie function.
  for (lname in cat_df$loader)
    expect_true(exists(lname, mode = "function",
                         envir = asNamespace("morie")),
                 info = sprintf("missing loader: %s", lname))
})

test_that("catalog row counts match each loader's actual row count", {
  cat_df <- morie_datasets_nyc_boundaries_catalog()
  for (i in seq_len(nrow(cat_df))) {
    fn <- get(cat_df$loader[i], envir = asNamespace("morie"))
    actual <- nrow(fn(offline = TRUE))
    expect_equal(actual, cat_df$n_rows[i],
                  info = sprintf("loader: %s", cat_df$loader[i]))
  }
})
