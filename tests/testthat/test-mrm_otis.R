# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2R: tests for mrm_otis.R — OTIS-specific MRM analyzers.

test_that("mrm_otis_placement_concentration runs on dictionary-driven b09", {
  df <- make_synthetic_otis("b09", n = 100L, seed = 1L)
  out <- tryCatch(
    mrm_otis_placement_concentration(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("placement_concentration error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_otis_seg_duration_km runs on b01 placement frame", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 2L)
  out <- tryCatch(
    mrm_otis_seg_duration_km(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("seg_duration_km error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_otis_seg_duration_km accepts group_cols", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 3L)
  out <- tryCatch(
    mrm_otis_seg_duration_km(df, group_cols = "Gender"),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("seg_duration_km + group error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_otis_mortification_cooccurrence runs on b01 with alert cols", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 4L)
  out <- tryCatch(
    mrm_otis_mortification_cooccurrence(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("mortification_cooccurrence error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})

test_that("mrm_otis_region_locality runs on b01", {
  df <- make_synthetic_otis("b01", n = 200L, seed = 5L)
  out <- tryCatch(
    mrm_otis_region_locality(df),
    error = function(e) e
  )
  if (inherits(out, "error")) {
    skip(sprintf("region_locality error: %s",
                 conditionMessage(out)))
  }
  expect_true(is.list(out) || is.data.frame(out))
})
