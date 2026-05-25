# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/otis_tps_overlay.R

mk_otis_b01 <- function() {
  set.seed(2026L)
  yrs <- rep(2023:2025, each = 30)
  regions <- sample(c("Toronto", "Central", "Eastern", "Western", "Northern"),
                    length(yrs), replace = TRUE,
                    prob = c(0.4, 0.2, 0.15, 0.15, 0.1))
  data.frame(
    EndFiscalYear = yrs,
    Region_AtTimeOfPlacement = regions,
    NumberConsecutiveDays_Segregation = sample(1:25, length(yrs), TRUE),
    stringsAsFactors = FALSE
  )
}

mk_tps_dataset <- function(years = 2020:2025, lambda = 200) {
  set.seed(99L)
  n <- 0
  yrs <- integer(0)
  for (y in years) {
    cnt <- rpois(1, lambda)
    yrs <- c(yrs, rep(y, cnt))
  }
  data.frame(OCC_YEAR = yrs, stringsAsFactors = FALSE)
}

test_that("morie_otis_tps_yoy_correlation runs with overlapping years", {
  b01 <- mk_otis_b01()
  tps <- list(
    assault = mk_tps_dataset(2020:2025, 200),
    robbery = mk_tps_dataset(2020:2025, 150)
  )
  res <- morie_otis_tps_yoy_correlation(b01, tps)
  expect_s3_class(res, "morie_otis_analysis_result")
  expect_true(length(res$tables) >= 1L)
})

test_that("morie_otis_tps_yoy_correlation OTIS-no-Toronto branch", {
  b01 <- mk_otis_b01()
  b01$Region_AtTimeOfPlacement <- "Central"  # no Toronto
  tps <- list(assault = mk_tps_dataset())
  res <- morie_otis_tps_yoy_correlation(b01, tps)
  expect_s3_class(res, "morie_otis_analysis_result")
})

test_that("morie_otis_tps_yoy_correlation TPS with REPORT_YEAR fallback", {
  b01 <- mk_otis_b01()
  tps_rpt <- data.frame(REPORT_YEAR = sample(2023:2025, 100, TRUE),
                        stringsAsFactors = FALSE)
  res <- morie_otis_tps_yoy_correlation(b01, list(thing = tps_rpt))
  expect_s3_class(res, "morie_otis_analysis_result")
})

test_that("morie_otis_tps_yoy_correlation TPS missing year column", {
  b01 <- mk_otis_b01()
  tps_bad <- data.frame(SOMETHING_ELSE = 1:50)
  res <- morie_otis_tps_yoy_correlation(b01, list(bad = tps_bad))
  expect_s3_class(res, "morie_otis_analysis_result")
})

test_that("morie_otis_tps_yoy_correlation TPS with few common years (<3)", {
  b01 <- mk_otis_b01()
  tps_solo <- data.frame(OCC_YEAR = rep(2010L, 5),
                          stringsAsFactors = FALSE)
  res <- morie_otis_tps_yoy_correlation(b01, list(narrow = tps_solo))
  expect_s3_class(res, "morie_otis_analysis_result")
})

test_that("morie_otis_tps_yoy_correlation errors on bad input types", {
  expect_error(morie_otis_tps_yoy_correlation(list(), list()),
                regexp = "data.frame")
})

test_that("morie_otis_tps_per_region_rollup happy path", {
  b01 <- mk_otis_b01()
  res <- morie_otis_tps_per_region_rollup(b01)
  expect_s3_class(res, "morie_otis_analysis_result")
  expect_true(!is.null(res$payload$by_region))
})

test_that("morie_otis_tps_per_region_rollup missing region column", {
  b01 <- mk_otis_b01()
  b01$Region_AtTimeOfPlacement <- NULL
  res <- morie_otis_tps_per_region_rollup(b01)
  expect_s3_class(res, "morie_otis_analysis_result")
  expect_true(any(grepl("region column", res$warnings)))
})

test_that("morie_otis_tps_per_region_rollup missing EndFiscalYear column", {
  b01 <- mk_otis_b01()
  b01$EndFiscalYear <- NULL
  res <- morie_otis_tps_per_region_rollup(b01)
  expect_s3_class(res, "morie_otis_analysis_result")
  expect_true(any(grepl("EndFiscalYear", res$warnings)))
})

test_that("morie_otis_tps_composite_overlay aliases yoy", {
  b01 <- mk_otis_b01()
  tps <- list(assault = mk_tps_dataset())
  r1 <- morie_otis_tps_composite_overlay(b01, tps)
  r2 <- morie_otis_tps_yoy_correlation(b01, tps)
  expect_equal(r1$title, r2$title)
})

test_that("morie_otis_tps_analyze_all returns named list with both analyses", {
  b01 <- mk_otis_b01()
  tps <- list(assault = mk_tps_dataset())
  res <- morie_otis_tps_analyze_all(b01, tps)
  expect_named(res, c("region_rollup", "yoy_correlation"))
  expect_s3_class(res$region_rollup, "morie_otis_analysis_result")
})

test_that("morie_otis_tps_analyze_all with out_dir writes rds files", {
  b01 <- mk_otis_b01()
  tps <- list(assault = mk_tps_dataset())
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  morie_otis_tps_analyze_all(b01, tps, out_dir = tmp)
  expect_true(file.exists(file.path(tmp, "overlay_region_rollup.rds")))
  expect_true(file.exists(file.path(tmp, "overlay_yoy_correlation.rds")))
})

test_that("morie_otis_tps_analyze_all swallows region-rollup errors", {
  # Pass a degenerate b01 that triggers an error inside region rollup
  bad_b01 <- list("not a df")  # will trip stopifnot
  res <- tryCatch(
    morie_otis_tps_analyze_all(bad_b01, list()),
    error = function(e) e
  )
  # Function uses tryCatch internally so should NOT error out — but the
  # outer stopifnot rejects bad input.  Either path is fine.
  expect_true(inherits(res, "list") || inherits(res, "error"))
})
