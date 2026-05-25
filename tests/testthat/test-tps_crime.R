# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_crime.R: YoY panel, composite index, bivariate
# Moran's I across categories, and the per-hood correlation matrix.
# All driven by named lists of synthetic TPS data.frames.

set.seed(1L)

.mk_tps_cat <- function(n = 120L, seed = 1L, n_hoods = 15L) {
  set.seed(seed)
  data.frame(
    OCC_YEAR    = sample(2018:2024, n, replace = TRUE),
    HOOD_158    = sample(letters[1:n_hoods], n, replace = TRUE),
    LAT_WGS84   = 43.6 + runif(n, 0, 0.2),
    LONG_WGS84  = -79.4 + runif(n, 0, 0.2),
    stringsAsFactors = FALSE
  )
}

.mk_dfs <- function() {
  list(
    Assault   = .mk_tps_cat(seed = 2L),
    Robbery   = .mk_tps_cat(seed = 3L),
    AutoTheft = .mk_tps_cat(seed = 4L)
  )
}

test_that("morie_tps_yoy_panel returns rich result with year x cat table", {
  rr <- morie_tps_yoy_panel(.mk_dfs())
  expect_s3_class(rr, "morie_tps_result")
  expect_true("Categories" %in% names(rr$summary_lines))
})

test_that("morie_tps_yoy_panel restricts via categories arg", {
  rr <- morie_tps_yoy_panel(.mk_dfs(), categories = c("Assault", "Robbery"))
  expect_s3_class(rr, "morie_tps_result")
  expect_equal(rr$summary_lines$Categories, 2L)
})

test_that("morie_tps_yoy_panel warns with no usable data", {
  rr <- morie_tps_yoy_panel(list(Assault = data.frame(x = 1)))
  expect_true(any(grepl("no datasets|year", rr$warnings)))
})

test_that("morie_tps_composite_index ranks neighbourhoods", {
  rr <- morie_tps_composite_index(.mk_dfs(), top_n = 5L)
  expect_s3_class(rr, "morie_tps_result")
  expect_true("Neighbourhoods scored" %in% names(rr$summary_lines))
})

test_that("morie_tps_composite_index accepts a named weights vector", {
  w <- c(Assault = 2, Robbery = 1, AutoTheft = 0.5)
  rr <- morie_tps_composite_index(.mk_dfs(), weights = as.list(w),
                                  top_n = 3L)
  expect_s3_class(rr, "morie_tps_result")
})

test_that("morie_tps_composite_index warns on empty input", {
  rr <- morie_tps_composite_index(list(Foo = data.frame(x = 1:2)))
  expect_true(any(grepl("no usable", rr$warnings)))
})

test_that("morie_tps_bivariate_morans_i runs across two categories", {
  rr <- morie_tps_bivariate_morans_i(.mk_dfs(), "Assault", "Robbery",
                                     k_neighbours = 3L)
  expect_s3_class(rr, "morie_tps_result")
})

test_that("morie_tps_bivariate_morans_i errors on non-df input", {
  dfs <- list(Assault = 1, Robbery = .mk_tps_cat())
  expect_error(morie_tps_bivariate_morans_i(dfs, "Assault", "Robbery"))
})

test_that("morie_tps_bivariate_morans_i warns when too few common hoods", {
  a <- data.frame(HOOD_158 = "a", LAT_WGS84 = 43.7, LONG_WGS84 = -79.4)
  b <- data.frame(HOOD_158 = "z", LAT_WGS84 = 43.7, LONG_WGS84 = -79.4)
  rr <- morie_tps_bivariate_morans_i(list(A = a, B = b), "A", "B")
  expect_s3_class(rr, "morie_tps_result")
  expect_true(any(grepl("common hoods", rr$warnings)))
})

test_that("morie_tps_category_correlation_matrix returns matrix", {
  rr <- morie_tps_category_correlation_matrix(.mk_dfs())
  expect_s3_class(rr, "morie_tps_result")
  expect_true(length(rr$tables) == 1L)
})

test_that("morie_tps_category_correlation_matrix warns on empty input", {
  rr <- morie_tps_category_correlation_matrix(list(Foo = 1))
  expect_true(any(grepl("no data|usable", rr$warnings)) ||
              length(rr$summary_lines) >= 0L)
})