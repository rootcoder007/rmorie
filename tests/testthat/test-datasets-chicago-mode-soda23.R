# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3VV++: dual-mode (soda2/soda3) for the 5 remaining Chicago
# Open Data wrappers. Verifies each wrapper accepts mode=, defaults
# to soda2 (backward-compat), routes to the correct helper based on
# mode, and forwards app_token under soda3.

# All 5 wrappers + their canonical hub_id-ish resource ids:
.chicago_wrappers <- list(
  list(name = "chicago_arrests",
       fn   = quote(morie_datasets_chicago_arrests),
       rid  = "dpt3-jri9"),
  list(name = "chicago_neighborhoods",
       fn   = quote(morie_datasets_chicago_neighborhoods),
       rid  = "y6yq-dbs2"),
  list(name = "chicago_police_beats",
       fn   = quote(morie_datasets_chicago_police_beats),
       rid  = "n9it-hstw"),
  list(name = "chicago_police_districts",
       fn   = quote(morie_datasets_chicago_police_districts),
       rid  = "24zt-jpfn"),
  list(name = "chicago_iucr_codes",
       fn   = quote(morie_datasets_chicago_iucr_codes),
       rid  = "c7ck-438e"))

# ====================================== signature checks

test_that("all 5 wrappers now accept mode + app_token args (3VV++)", {
  for (w in .chicago_wrappers) {
    f <- eval(w$fn)
    fn_args <- names(formals(f))
    expect_true("mode" %in% fn_args,
                info = sprintf("%s missing `mode`", w$name))
    expect_true("app_token" %in% fn_args,
                info = sprintf("%s missing `app_token`", w$name))
    # `mode` has the canonical default c("soda2","soda3").
    default_mode <- formals(f)$mode
    expect_equal(eval(default_mode), c("soda2", "soda3"),
                 info = sprintf("%s mode default wrong", w$name))
  }
})

test_that("all 5 wrappers reject unknown mode via match.arg", {
  for (w in .chicago_wrappers) {
    f <- eval(w$fn)
    expect_error(f(offline = FALSE, mode = "graphql"),
                 info = sprintf("%s did not reject", w$name))
  }
})

# ====================================== default mode=soda2 (backward-compat)

test_that("default mode='soda2' routes through .morie_dataset_socrata_fetch for every wrapper", {
  for (w in .chicago_wrappers) {
    soda2_called <- FALSE
    soda3_called <- FALSE
    testthat::with_mocked_bindings(
      .morie_dataset_socrata_fetch = function(url, ...) {
        soda2_called <<- TRUE
        data.frame()
      },
      .morie_dataset_soda3_query = function(view_id, ...) {
        soda3_called <<- TRUE
        data.frame()
      },
      .package = "morie",
      code = eval(w$fn)(offline = FALSE))
    expect_true(soda2_called,
                info = sprintf("%s default did not call SODA2", w$name))
    expect_false(soda3_called,
                 info = sprintf("%s default called SODA3 (wrong)", w$name))
  }
})

# ====================================== mode='soda3' routes to soda3_query

test_that("mode='soda3' routes through .morie_dataset_soda3_query for every wrapper", {
  for (w in .chicago_wrappers) {
    seen <- list()
    testthat::with_mocked_bindings(
      .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                              app_token = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L,
                                              max_features = NULL,
                                              base_url = "https://data.cityofchicago.org") {
        seen <<- list(view_id = view_id, soql = soql,
                      app_token = app_token)
        data.frame()
      },
      .package = "morie",
      code = eval(w$fn)(offline = FALSE, mode = "soda3",
                          app_token = "tok-3vv2"))
    expect_equal(seen$view_id, w$rid,
                 info = sprintf("%s view_id wrong", w$name))
    expect_match(seen$soql, "^SELECT",
                 info = sprintf("%s soql malformed", w$name))
    expect_equal(seen$app_token, "tok-3vv2",
                 info = sprintf("%s app_token not forwarded", w$name))
  }
})

# ====================================== geometry arg interacts with mode (boundary wrappers)

test_that("chicago_neighborhoods + beats + districts with mode='soda3' + geometry=FALSE emit narrow SELECT", {
  reps <- list(
    list(fn = morie_datasets_chicago_neighborhoods,
         expect_cols = "pri_neigh, sec_neigh, shape_area, shape_len"),
    list(fn = morie_datasets_chicago_police_beats,
         expect_cols = "beat_num, beat, sector, district"),
    list(fn = morie_datasets_chicago_police_districts,
         expect_cols = "dist_num, dist_label"))
  for (R in reps) {
    seen_soql <- NULL
    testthat::with_mocked_bindings(
      .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                              ...) {
        seen_soql <<- soql
        data.frame()
      },
      .package = "morie",
      code = R$fn(offline = FALSE, mode = "soda3", geometry = FALSE))
    expect_equal(seen_soql,
                 sprintf("SELECT %s", R$expect_cols))
  }
})

test_that("chicago_neighborhoods + beats + districts with mode='soda3' + geometry=TRUE emit SELECT *", {
  for (fn in list(morie_datasets_chicago_neighborhoods,
                  morie_datasets_chicago_police_beats,
                  morie_datasets_chicago_police_districts)) {
    seen_soql <- NULL
    testthat::with_mocked_bindings(
      .morie_dataset_soda3_query = function(view_id, soql = "SELECT *",
                                              ...) {
        seen_soql <<- soql
        data.frame()
      },
      .package = "morie",
      code = fn(offline = FALSE, mode = "soda3", geometry = TRUE))
    expect_equal(seen_soql, "SELECT *")
  }
})

# ====================================== chicago_arrests year filter under both modes

test_that("chicago_arrests(year=N) emits date_extract_y filter in BOTH modes", {
  # SODA2: year goes into $where
  seen_where <- NULL
  testthat::with_mocked_bindings(
    .morie_dataset_socrata_fetch = function(url, where = NULL, ...) {
      seen_where <<- where
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_chicago_arrests(year = 2024L, offline = FALSE,
                                            mode = "soda2"))
  expect_equal(seen_where, "date_extract_y(arrest_date) = 2024")

  # SODA3: year goes into the SoQL WHERE clause
  seen_soql <- NULL
  testthat::with_mocked_bindings(
    .morie_dataset_soda3_query = function(view_id, soql = "SELECT *", ...) {
      seen_soql <<- soql
      data.frame()
    },
    .package = "morie",
    code = morie_datasets_chicago_arrests(year = 2024L, offline = FALSE,
                                            mode = "soda3"))
  expect_equal(seen_soql, "SELECT * WHERE date_extract_y(arrest_date) = 2024")
})

# ====================================== offline mode unchanged

test_that("offline=TRUE is unaffected by mode arg (mode is live-only)", {
  for (w in .chicago_wrappers) {
    df_s2 <- suppressWarnings(eval(w$fn)(offline = TRUE, mode = "soda2"))
    df_s3 <- suppressWarnings(eval(w$fn)(offline = TRUE, mode = "soda3"))
    expect_equal(nrow(df_s2), nrow(df_s3),
                 info = sprintf("%s offline mode arg drifted", w$name))
    expect_setequal(names(df_s2), names(df_s3))
  }
})
