# SPDX-License-Identifier: AGPL-3.0-or-later
# Coverage tests for R/arsau_analyze.R
#
# These tests stage in-memory CSV fixtures into a temporary
# MORIE_ARSAU_DIR so the analyzers actually execute (rather than
# skipping). Each test sets data_dir explicitly to avoid touching the
# real ARSAU mirror.

set.seed(2026L)

.write_main_records_csv <- function(year, expected_cols, n_rows = 30L) {
  cols <- list(
    PoliceService = sample(c("Toronto", "OPP", "Halton"), n_rows, TRUE),
    IncidentType = sample(c("Use of Force", "Discharge"), n_rows, TRUE),
    PoliceServiceType = sample(c("Municipal", "OPP"), n_rows, TRUE),
    OPP_PoliceService_Region = sample(c("Central", "West"), n_rows, TRUE)
  )
  # Pad to expected_cols with filler columns
  while (length(cols) < expected_cols) {
    cols[[paste0("filler_", length(cols))]] <- runif(n_rows)
  }
  as.data.frame(cols, stringsAsFactors = FALSE)
}

.write_individual_records_csv <- function(year, expected_cols, n_rows = 40L) {
  cols <- list(
    BatchFileName = sample(c("f1", "f2"), n_rows, TRUE),
    Indiv_Index = seq_len(n_rows),
    Race = sample(c("White", "Black", "Indigenous", "Asian"),
                  n_rows, TRUE),
    Gender = sample(c("M", "F"), n_rows, TRUE),
    AgeCategory = sample(c("Adult", "Youth"), n_rows, TRUE),
    `IndivInjuries_PhysicalInjuries` =
      sample(c("Yes", "No"), n_rows, TRUE)
  )
  while (length(cols) < expected_cols) {
    cols[[paste0("filler_", length(cols))]] <- runif(n_rows)
  }
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

.write_probe_cycle_csv <- function(year, expected_cols, n_rows = 25L) {
  cols <- list(
    BatchFileName = sample(c("f1", "f2"), n_rows, TRUE),
    Indiv_Index = seq_len(n_rows),
    CEW_CartridgeProbe_CartridgeProbeCycles_Cyc =
      sample(c("1,2,3", "1,2", "1", "", "5,5"), n_rows, TRUE)
  )
  while (length(cols) < expected_cols) {
    cols[[paste0("filler_", length(cols))]] <- runif(n_rows)
  }
  as.data.frame(cols, stringsAsFactors = FALSE)
}

.write_weapon_csv <- function(year, expected_cols, n_rows = 30L) {
  cols <- list(
    BatchFileName = sample(c("f1", "f2"), n_rows, TRUE),
    Indiv_Index = seq_len(n_rows),
    Weapon = sample(c("CEW", "OC Spray", "Baton", "Firearm"), n_rows, TRUE),
    Location = sample(c("Indoor", "Outdoor"), n_rows, TRUE)
  )
  while (length(cols) < expected_cols) {
    cols[[paste0("filler_", length(cols))]] <- runif(n_rows)
  }
  as.data.frame(cols, stringsAsFactors = FALSE)
}

.write_aggregate_csv <- function(expected_cols, n_rows = 20L) {
  cols <- list(
    SECTION = sample(c("Force", "Discharge"), n_rows, TRUE),
    CATEGORY = sample(c("OC", "CEW", "Baton"), n_rows, TRUE),
    `UNITS OF MEASURE` = sample(c("count", "%"), n_rows, TRUE),
    YEAR_2020 = sample(1:50, n_rows, TRUE),
    YEAR_2021 = sample(1:50, n_rows, TRUE),
    YEAR_2022 = sample(1:50, n_rows, TRUE)
  )
  while (length(cols) < expected_cols) {
    cols[[paste0("filler_", length(cols))]] <- runif(n_rows)
  }
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

.stage_arsau_dir <- function() {
  root <- tempfile("arsau_stage_")
  reg <- morie_arsau_registry_df()
  for (i in seq_len(nrow(reg))) {
    r <- reg[i, ]
    sub <- file.path(root, r$year_or_range)
    dir.create(sub, recursive = TRUE, showWarnings = FALSE)
    target <- file.path(sub, r$csv_filename)
    df <- switch(r$kind,
      main_records = .write_main_records_csv(r$year_or_range, r$expected_cols),
      individual_records = .write_individual_records_csv(r$year_or_range, r$expected_cols),
      probe_cycle_records = .write_probe_cycle_csv(r$year_or_range, r$expected_cols),
      weapon_records = .write_weapon_csv(r$year_or_range, r$expected_cols),
      aggregate_summary = .write_aggregate_csv(r$expected_cols),
      detailed_dataset = .write_aggregate_csv(r$expected_cols)
    )
    utils::write.csv(df, target, row.names = FALSE)
  }
  root
}

.arsau_is_result <- function(x) {
  inherits(x, "morie_arsau_analysis_result") ||
    inherits(x, "morie_arsau_result") ||
    inherits(x, "morie_rich_result")
}

test_that("morie_arsau_analyze_main_records runs end-to-end", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_main_records(year = "2024", data_dir = root)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_individual_records runs end-to-end", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_individual_records(year = "2024", data_dir = root,
                                            bootstrap_reps = 0L)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_individual_records with bootstrap reps", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_individual_records(year = "2024", data_dir = root,
                                            bootstrap_reps = 5L)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_probe_cycle_records runs end-to-end", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_probe_cycle_records(year = "2024", data_dir = root)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_weapon_records 2024 path", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_weapon_records(year = "2024", data_dir = root)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_weapon_records 2023 requires allow_invalid", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  expect_error(
    suppressWarnings(
      morie_arsau_analyze_weapon_records(year = "2023", data_dir = root)
    )
  )
  # With allow_invalid = TRUE, it should run
  res <- suppressWarnings(
    morie_arsau_analyze_weapon_records(year = "2023", data_dir = root,
                                        allow_invalid = TRUE)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_aggregate_summary runs end-to-end", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_aggregate_summary(year_range = "2020-2022",
                                           data_dir = root)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_detailed_dataset runs end-to-end", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_detailed_dataset(year_range = "2020-2022",
                                          data_dir = root)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_individual_records French language", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_individual_records(year = "2024", data_dir = root,
                                            language = "fr",
                                            bootstrap_reps = 0L)
  )
  expect_true(.arsau_is_result(res))
})

test_that("morie_arsau_analyze_main_records 2023 also works", {
  root <- .stage_arsau_dir()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  res <- suppressWarnings(
    morie_arsau_analyze_main_records(year = "2023", data_dir = root)
  )
  expect_true(.arsau_is_result(res))
})
