# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Unit tests for R/tps_csi.R: CSI weight tables, weight lookup, per-year
# CSI, per-neighbourhood CSI, and the orchestrator over a named list of
# TPS data.frames.

set.seed(1L)

test_that("MORIE_TPS_TOTAL_CSI_WEIGHTS exposes the 9 CSI categories", {
  w <- MORIE_TPS_TOTAL_CSI_WEIGHTS()
  expect_type(w, "double")
  expect_true(length(w) >= 9L)
})

test_that("MORIE_TPS_VIOLENT_CSI_WEIGHTS returns numeric vector", {
  w <- MORIE_TPS_VIOLENT_CSI_WEIGHTS()
  expect_type(w, "double")
})

test_that("MORIE_TPS_TORONTO_POPULATION_BY_YEAR returns year keys", {
  pop <- MORIE_TPS_TORONTO_POPULATION_BY_YEAR()
  expect_true(length(pop) > 0L)
  expect_true(all(grepl("^[0-9]{4}$", names(pop))))
})

test_that("MORIE_TPS_CSI_CATEGORIES enumerates the 9 keys", {
  cats <- MORIE_TPS_CSI_CATEGORIES()
  expect_type(cats, "character")
  expect_true(length(cats) >= 9L)
})

test_that("morie_tps_csi_weight returns numeric scalar", {
  expect_true(is.numeric(morie_tps_csi_weight("Assault")))
  expect_true(is.numeric(morie_tps_csi_weight("Homicides",
                                              variant = "violent")))
})

test_that("morie_tps_csi_weight returns 0 for unknown category", {
  expect_equal(morie_tps_csi_weight("NoSuchCategory"), 0.0)
})

test_that("morie_tps_csi_weight respects override weights", {
  expect_equal(morie_tps_csi_weight("Assault",
                                    weights = list(Assault = 42)), 42)
  expect_equal(morie_tps_csi_weight("Other",
                                    weights = list(X = 1)), 0)
})

test_that("morie_tps_csi_weight validates variant", {
  expect_error(morie_tps_csi_weight("Assault", variant = "nope"))
})

test_that("morie_tps_csi_per_year handles long data.frame input", {
  cats <- MORIE_TPS_CSI_CATEGORIES()
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- as.integer(utils::head(pop_yrs, 3L))
  long <- expand.grid(year = yrs, category = cats[1:3],
                      stringsAsFactors = FALSE)
  long$count <- sample.int(50L, nrow(long), replace = TRUE)
  out <- morie_tps_csi_per_year(long)
  expect_s3_class(out, "data.frame")
  expect_true("csi_per_capita" %in% names(out))
})

test_that("morie_tps_csi_per_year handles nested-list input", {
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- utils::head(pop_yrs, 2L)
  cats <- MORIE_TPS_CSI_CATEGORIES()
  nested <- list()
  for (y in yrs) {
    nested[[y]] <- list()
    for (c in cats[1:2]) nested[[y]][[c]] <- 5L
  }
  out <- morie_tps_csi_per_year(nested)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that("morie_tps_csi_per_year rebases when anchor year set", {
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- as.integer(utils::head(pop_yrs, 3L))
  cats <- MORIE_TPS_CSI_CATEGORIES()
  long <- expand.grid(year = yrs, category = cats[1:2],
                      stringsAsFactors = FALSE)
  long$count <- 10L
  out <- morie_tps_csi_per_year(long, rebase_to_year = yrs[1],
                                 rebase_to_value = 100)
  expect_true("csi_index" %in% names(out))
  anchor_row <- out[out$year == yrs[1], ]
  expect_equal(anchor_row$csi_index, 100)
})

test_that("morie_tps_csi_per_year errors on bad rebase year", {
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- as.integer(utils::head(pop_yrs, 2L))
  long <- data.frame(year = yrs[1], category = "Assault", count = 5L)
  expect_error(morie_tps_csi_per_year(long, rebase_to_year = 9999L))
})

test_that("morie_tps_csi_per_year errors on malformed input", {
  expect_error(morie_tps_csi_per_year(data.frame(yr = 1, cat = "A", n = 1)))
  expect_error(morie_tps_csi_per_year(42))
})

test_that("morie_tps_csi_per_neighbourhood works with long data.frame", {
  cats <- MORIE_TPS_CSI_CATEGORIES()
  long <- expand.grid(HOOD_158 = c("a","b","c"),
                      category = cats[1:3],
                      stringsAsFactors = FALSE)
  long$count <- sample.int(20L, nrow(long), replace = TRUE)
  out <- morie_tps_csi_per_neighbourhood(long)
  expect_s3_class(out, "data.frame")
  expect_true("raw_weighted_sum" %in% names(out))
})

test_that("morie_tps_csi_per_neighbourhood handles nested list", {
  cats <- MORIE_TPS_CSI_CATEGORIES()
  nested <- list(a = list(), b = list())
  for (c in cats[1:2]) { nested$a[[c]] <- 4L; nested$b[[c]] <- 7L }
  out <- morie_tps_csi_per_neighbourhood(nested)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
})

test_that("morie_tps_analyze_csi_from_dataframes builds rich result", {
  set.seed(1L)
  cats <- MORIE_TPS_CSI_CATEGORIES()
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- as.integer(utils::head(pop_yrs, 4L))
  mk <- function() data.frame(
    OCC_YEAR = sample(yrs, 80L, replace = TRUE),
    HOOD_158 = sample(1:20, 80L, replace = TRUE)
  )
  dfs <- setNames(lapply(cats[1:3], function(c) mk()), cats[1:3])
  rr <- morie_tps_analyze_csi_from_dataframes(dfs)
  expect_s3_class(rr, "morie_tps_result")
  expect_true(!is.null(rr$payload$by_year))
  expect_true(!is.null(rr$payload$by_hood))
})

test_that("analyze_csi_from_dataframes ignores unknown categories", {
  set.seed(1L)
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- as.integer(utils::head(pop_yrs, 3L))
  dfs <- list(
    UnknownThing = data.frame(OCC_YEAR = yrs, HOOD_158 = 1)
  )
  rr <- morie_tps_analyze_csi_from_dataframes(dfs)
  expect_s3_class(rr, "morie_tps_result")
})

test_that("analyze_csi_from_dataframes accepts violent variant", {
  set.seed(1L)
  pop_yrs <- names(MORIE_TPS_TORONTO_POPULATION_BY_YEAR())
  yrs <- as.integer(utils::head(pop_yrs, 3L))
  cats <- MORIE_TPS_CSI_CATEGORIES()
  dfs <- setNames(list(data.frame(OCC_YEAR = yrs, HOOD_158 = 1)),
                  cats[1])
  rr <- morie_tps_analyze_csi_from_dataframes(dfs, variant = "violent")
  expect_s3_class(rr, "morie_tps_result")
})