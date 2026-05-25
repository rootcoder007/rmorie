# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_all_analyze.R: temporal / spatial / offence /
# concentration rollups, master driver, 13 convenience aliases, and
# the cross-dataset orchestrator.

set.seed(1L)

.mk_tps_full <- function(n = 200L, seed = 1L) {
  set.seed(seed)
  data.frame(
    OCC_YEAR   = sample(2018:2024, n, replace = TRUE),
    OCC_MONTH  = sample(1:12, n, replace = TRUE),
    OCC_DOW    = sample(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                        n, replace = TRUE),
    OCC_HOUR   = sample(0:23, n, replace = TRUE),
    DIVISION   = sample(paste0("D", 11:55), n, replace = TRUE),
    HOOD_158   = sample(letters[1:20], n, replace = TRUE),
    PREMISES_TYPE = sample(c("House","Apt","Street"), n, replace = TRUE),
    LOCATION_TYPE = sample(c("Bar","Park","Road"), n, replace = TRUE),
    OFFENCE     = sample(c("Assault","Theft"), n, replace = TRUE),
    UCR_CODE    = sample(c("1430","2120"), n, replace = TRUE),
    LAT_WGS84   = 43.6 + runif(n, 0, 0.2),
    LONG_WGS84  = -79.4 + runif(n, 0, 0.2),
    stringsAsFactors = FALSE
  )
}

test_that("morie_tps_temporal_summary builds rollups", {
  df <- .mk_tps_full(n = 100L)
  rr <- morie_tps_temporal_summary(df, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_result")
  expect_true("Total incidents" %in% names(rr$summary_lines))
  expect_true(length(rr$tables) >= 1L)
})

test_that("morie_tps_temporal_summary tolerates missing year col", {
  df <- data.frame(OFFENCE = c("A","B"))
  rr <- morie_tps_temporal_summary(df, ds_name = "NoYear")
  expect_s3_class(rr, "morie_tps_result")
})

test_that("morie_tps_spatial_summary captures lat/lon range", {
  df <- .mk_tps_full(n = 80L)
  rr <- morie_tps_spatial_summary(df, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_result")
  expect_true("Latitude range" %in% names(rr$summary_lines))
})

test_that("morie_tps_spatial_summary handles missing lat/lon", {
  df <- data.frame(DIVISION = c("a","b"))
  rr <- morie_tps_spatial_summary(df, ds_name = "NoLatLon")
  expect_s3_class(rr, "morie_tps_result")
})

test_that("morie_tps_offence_summary surfaces offence table", {
  df <- .mk_tps_full(n = 80L)
  rr <- morie_tps_offence_summary(df, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_result")
  expect_true(length(rr$tables) >= 1L)
})

test_that("morie_tps_offence_summary works when columns absent", {
  rr <- morie_tps_offence_summary(data.frame(x = 1:3), ds_name = "Empty")
  expect_s3_class(rr, "morie_tps_result")
})

test_that("morie_tps_gini_concentration: bounds + edge cases", {
  expect_equal(morie_tps_gini_concentration(rep(1, 5)), 0)
  expect_true(morie_tps_gini_concentration(c(0,0,0,100)) > 0.5)
  expect_true(is.na(morie_tps_gini_concentration(numeric(0))))
  expect_equal(morie_tps_gini_concentration(c(0,0,0)), 0)
})

test_that("morie_tps_neighbourhood_concentration computes Gini + top-K", {
  df <- .mk_tps_full(n = 150L)
  rr <- morie_tps_neighbourhood_concentration(df, ds_name = "Synth")
  expect_s3_class(rr, "morie_tps_result")
  expect_true("Gini coefficient (concentration)" %in% names(rr$summary_lines))
})

test_that("morie_tps_neighbourhood_concentration warns when HOOD_158 absent", {
  df <- data.frame(x = 1:5)
  rr <- morie_tps_neighbourhood_concentration(df, ds_name = "NoHood")
  expect_true(any(grepl("HOOD_158", rr$warnings)))
})

test_that("morie_tps_crime_compare runs over named list of dfs", {
  set.seed(1L)
  dfs <- list(
    Assault = .mk_tps_full(80L, seed = 2L),
    Robbery = .mk_tps_full(40L, seed = 3L)
  )
  rr <- morie_tps_crime_compare(dfs)
  expect_s3_class(rr, "morie_tps_result")
  expect_true(length(rr$tables) >= 1L)
})

test_that("morie_tps_crime_compare validates input", {
  expect_error(morie_tps_crime_compare(list()))
  expect_error(morie_tps_crime_compare(list(.mk_tps_full(10L))))
})

test_that("morie_tps_analyze_one bundles 4 sub-analyses", {
  df <- .mk_tps_full(60L)
  rr <- morie_tps_analyze_one(df, name = "Synth")
  expect_s3_class(rr, "morie_tps_result")
  expect_true(!is.null(rr$temporal))
  expect_true(!is.null(rr$spatial))
  expect_true(!is.null(rr$offences))
  expect_true(!is.null(rr$concentration))
})

test_that("convenience aliases dispatch to morie_tps_analyze_one", {
  df <- .mk_tps_full(40L)
  rr <- morie_tps_analyze_assault(df)
  expect_s3_class(rr, "morie_tps_result")
  expect_match(rr$title, "Assault")
  expect_s3_class(morie_tps_analyze_autotheft(df), "morie_tps_result")
  expect_s3_class(morie_tps_analyze_bicycletheft(df), "morie_tps_result")
  expect_s3_class(morie_tps_analyze_breakandenter(df), "morie_tps_result")
  expect_s3_class(morie_tps_analyze_communitysafetyindicators(df),
                  "morie_tps_result")
  expect_s3_class(morie_tps_analyze_hatecrimes(df), "morie_tps_result")
  expect_s3_class(morie_tps_analyze_homicides(df), "morie_tps_result")
  expect_s3_class(morie_tps_analyze_intimatepartnerandfamilyviolence(df),
                  "morie_tps_result")
  expect_s3_class(morie_tps_analyze_neighbourhoodcrimerates(df),
                  "morie_tps_result")
  expect_s3_class(morie_tps_analyze_robbery(df), "morie_tps_result")
  expect_s3_class(morie_tps_analyze_shootingandfirearmdiscarges(df),
                  "morie_tps_result")
  expect_s3_class(morie_tps_analyze_theftfrommovingvehicle(df),
                  "morie_tps_result")
  expect_s3_class(morie_tps_analyze_theftover(df), "morie_tps_result")
})

test_that("morie_tps_analyze_all returns per-dataset + cross-compare", {
  set.seed(1L)
  dfs <- list(
    Assault = .mk_tps_full(40L, seed = 2L),
    Robbery = .mk_tps_full(30L, seed = 3L)
  )
  rr <- morie_tps_analyze_all(dfs)
  expect_true("Assault" %in% names(rr))
  expect_true("Robbery" %in% names(rr))
  expect_true("__cross_compare__" %in% names(rr))
})

test_that("morie_tps_analyze_all writes per-dataset text dumps", {
  set.seed(1L)
  td <- tempfile("tps-dump-"); dir.create(td)
  dfs <- list(Assault = .mk_tps_full(20L, seed = 2L))
  rr <- morie_tps_analyze_all(dfs, out_dir = td)
  expect_true(file.exists(file.path(td, "tps_Assault.txt")))
  unlink(td, recursive = TRUE)
})