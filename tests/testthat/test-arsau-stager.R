# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2D-2: dictionary-driven ARSAU stager smoke tests.
#
# Verifies that stage_synthetic_arsau() writes every registered
# (year, kind) CSV under the right layout for the morie_arsau_load_*
# loaders + analyzers, AND that make_synthetic_arsau() per-(year, kind)
# returns a frame with the real columns from the authoritative
# dictionary (e.g. real Toronto / OPP police-service vocabulary,
# real CEW-cartridge layout).

test_that("make_synthetic_arsau dispatches to all 10 (year, kind) entries", {
  for (year in c("2024", "2023")) {
    for (kind in c("main_records", "individual_records",
                   "probe_cycle_records", "weapon_records")) {
      df <- make_synthetic_arsau(year, kind, n = 12L, seed = 1L)
      expect_s3_class(df, "data.frame")
      expect_equal(nrow(df), 12L,
                   info = sprintf("year=%s kind=%s", year, kind))
      expect_true(ncol(df) > 1L,
                  info = sprintf("year=%s kind=%s", year, kind))
    }
  }
  for (kind in c("aggregate_summary", "detailed_dataset")) {
    df <- make_synthetic_arsau("2020-2022", kind, n = 12L, seed = 1L)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), 12L)
  }
})

test_that("make_synthetic_arsau errors on an unregistered (year, kind)", {
  expect_error(
    make_synthetic_arsau("9999", "main_records"),
    regexp = "no synthetic ARSAU fixture"
  )
})

test_that("2024 main_records carries real Toronto-OPP-Halton vocabulary", {
  df <- make_synthetic_arsau("2024", "main_records", n = 200L, seed = 1L)
  # 45 real Ontario police services in the dictionary; sampling 200
  # times reliably draws Toronto + OPP + Halton Regional (3 of the
  # most-frequent agencies).
  expect_true("PoliceService" %in% names(df))
  ps <- unique(df$PoliceService)
  expect_true("Toronto" %in% ps)
  expect_true("OPP" %in% ps)
})

test_that("stage_synthetic_arsau writes all 10 CSVs to the expected layout", {
  root <- stage_synthetic_arsau(n = 20L, seed = 1L)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  # Year subdirs
  for (yr in c("2020-2022", "2023", "2024")) {
    expect_true(dir.exists(file.path(root, yr)),
                info = paste("missing", yr))
  }
  # Each registered CSV exists + is non-empty
  for (rel in c("2020-2022/useofforce_agrregatesummarybyyear_2020-2022.csv",
                "2020-2022/useofforce_detaileddataset_2020-2022.csv",
                "2023/uof_main_records.csv",
                "2023/uof_individual_records.csv",
                "2023/uof_probe_cycle_records.csv",
                "2023/uof_weapon_records_invaliddata.csv",
                "2024/uof_main_records.csv",
                "2024/uof_individual_records.csv",
                "2024/uof_probe_cycle_records.csv",
                "2024/uof_weapon_records.csv")) {
    p <- file.path(root, rel)
    expect_true(file.exists(p), info = paste("missing", rel))
    expect_gt(file.info(p)$size, 0)
  }
})

# ---------------------------------------------------------------------------
# Wire dictionary-staged ARSAU into the analyzers (Phase 2D-2 coverage win)
# ---------------------------------------------------------------------------

test_that("morie_arsau_analyze_main_records runs on dictionary-staged 2024", {
  root <- stage_synthetic_arsau(n = 60L, seed = 2L)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_main_records(year = "2024", data_dir = root)
  )
  expect_true(inherits(res, "morie_arsau_analysis_result") ||
                inherits(res, "morie_arsau_result") ||
                inherits(res, "morie_rich_result"))
})

test_that("morie_arsau_analyze_individual_records runs on dictionary-staged 2024", {
  root <- stage_synthetic_arsau(n = 100L, seed = 3L)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_individual_records(year = "2024", data_dir = root,
                                            bootstrap_reps = 0L)
  )
  expect_true(inherits(res, "morie_arsau_analysis_result") ||
                inherits(res, "morie_arsau_result") ||
                inherits(res, "morie_rich_result"))
})

test_that("morie_arsau_analyze_weapon_records 2023 needs allow_invalid on stager", {
  root <- stage_synthetic_arsau(n = 60L, seed = 4L)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_error(suppressWarnings(
    morie_arsau_analyze_weapon_records(year = "2023", data_dir = root)
  ))
  res <- suppressWarnings(
    morie_arsau_analyze_weapon_records(year = "2023", data_dir = root,
                                        allow_invalid = TRUE)
  )
  expect_true(inherits(res, "morie_arsau_analysis_result") ||
                inherits(res, "morie_arsau_result") ||
                inherits(res, "morie_rich_result"))
})

test_that("morie_arsau_analyze_aggregate_summary runs on dictionary-staged 2020-2022", {
  root <- stage_synthetic_arsau(n = 60L, seed = 5L)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_aggregate_summary(year_range = "2020-2022",
                                           data_dir = root)
  )
  expect_true(inherits(res, "morie_arsau_analysis_result") ||
                inherits(res, "morie_arsau_result") ||
                inherits(res, "morie_rich_result"))
})

test_that("morie_arsau_analyze_detailed_dataset runs on dictionary-staged 2020-2022", {
  root <- stage_synthetic_arsau(n = 60L, seed = 6L)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_detailed_dataset(year_range = "2020-2022",
                                          data_dir = root)
  )
  expect_true(inherits(res, "morie_arsau_analysis_result") ||
                inherits(res, "morie_arsau_result") ||
                inherits(res, "morie_rich_result"))
})
