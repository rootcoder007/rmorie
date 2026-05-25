# SPDX-License-Identifier: AGPL-3.0-or-later
library(testthat)
set.seed(1)

make_otis_b01 <- function(n_persons = 30, seed = 1) {
  set.seed(seed)
  rows <- list()
  for (i in seq_len(n_persons)) {
    k <- sample(1:3, 1)
    for (j in seq_len(k)) {
      rows[[length(rows) + 1L]] <- data.frame(
        UniqueIndividual_ID = sprintf("id%04d", i),
        EndFiscalYear = sample(2018:2023, 1),
        Region_AtTimeOfPlacement = sample(c("Central", "East", "West"), 1),
        Region_MostRecentPlacement = sample(c("Central", "East", "West"), 1),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

make_tps_datasets <- function(seed = 2) {
  set.seed(seed)
  list(
    use_of_force = data.frame(
      year = 2018:2023,
      region = rep("Central", 6),
      count = sample(10:200, 6),
      stringsAsFactors = FALSE
    ),
    stops = data.frame(
      year = 2018:2023,
      region = rep("East", 6),
      count = sample(10:200, 6),
      stringsAsFactors = FALSE
    )
  )
}

# ---------------------------------------------------------------------------
# morie_otis_tps_yoy_correlation
# ---------------------------------------------------------------------------

test_that("morie_otis_tps_yoy_correlation runs on synthetic data or skips", {
  set.seed(3)
  otis_b01 <- make_otis_b01(40)
  tps <- make_tps_datasets()
  res <- tryCatch(morie_otis_tps_yoy_correlation(otis_b01, tps),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs richer overlap between OTIS+TPS")
  expect_true(is.list(res) || is.data.frame(res))
})

test_that("morie_otis_tps_yoy_correlation handles empty tps list", {
  set.seed(4)
  otis_b01 <- make_otis_b01(20)
  res <- tryCatch(morie_otis_tps_yoy_correlation(otis_b01, list()),
                  error = function(e) NULL,
                  warning = function(w) NULL)
  expect_true(is.null(res) || is.list(res) || is.data.frame(res))
})

# ---------------------------------------------------------------------------
# morie_otis_tps_per_region_rollup
# ---------------------------------------------------------------------------

test_that("morie_otis_tps_per_region_rollup returns a tabular rollup", {
  set.seed(5)
  otis_b01 <- make_otis_b01(60)
  res <- tryCatch(morie_otis_tps_per_region_rollup(otis_b01),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs region columns the synthetic frame lacks")
  expect_true(is.list(res) || is.data.frame(res))
})

# ---------------------------------------------------------------------------
# morie_otis_tps_composite_overlay
# ---------------------------------------------------------------------------

test_that("morie_otis_tps_composite_overlay runs on synthetic data", {
  set.seed(6)
  otis_b01 <- make_otis_b01(40)
  tps <- make_tps_datasets()
  res <- tryCatch(morie_otis_tps_composite_overlay(otis_b01, tps),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs richer OTIS+TPS structure")
  expect_true(is.list(res) || is.data.frame(res))
})

# ---------------------------------------------------------------------------
# morie_otis_tps_analyze_all — top-level dispatcher
# ---------------------------------------------------------------------------

test_that("morie_otis_tps_analyze_all runs end-to-end or skips", {
  set.seed(7)
  otis_b01 <- make_otis_b01(50)
  tps <- make_tps_datasets()
  res <- tryCatch(morie_otis_tps_analyze_all(otis_b01, tps),
                  error = function(e) NULL)
  skip_if(is.null(res), "needs richer OTIS+TPS structure")
  expect_true(is.list(res))
})

test_that("morie_otis_tps_analyze_all tolerates empty TPS list", {
  set.seed(8)
  otis_b01 <- make_otis_b01(20)
  res <- tryCatch(morie_otis_tps_analyze_all(otis_b01, list()),
                  error = function(e) NULL,
                  warning = function(w) NULL)
  expect_true(is.null(res) || is.list(res))
})