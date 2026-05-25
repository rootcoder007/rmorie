# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Coverage tests for R/mrm_arsau.R — the per-record-type ARSAU
# analyzers.  Uses the same synthetic fixture builder as test-arsau.R.

.make_fixture_root_mrm <- function() {
  root <- file.path(tempdir(check = TRUE),
                     paste0("arsau-mrm-fx-", as.integer(Sys.time()), "-",
                            sample.int(1e6, 1)))
  if (dir.exists(root)) unlink(root, recursive = TRUE)
  dir.create(root, recursive = TRUE)
  for (yr in c("2020-2022", "2023", "2024")) {
    dir.create(file.path(root, yr))
  }
  set.seed(1)

  # 2020-2022 / aggregate_summary
  write.csv(data.frame(
    SECTION = c("REPORT_SCOPE", "REPORT_SCOPE", "OTHER"),
    CATEGORY = c("1 to 3 Subjects - Individual Reports", "X", "Y"),
    `UNITS OF MEASURE` = c("Number of Reports", "Number of Reports", "Number"),
    YEAR_2020 = c(100, 50, 25),
    YEAR_2021 = c(120, 55, 30),
    YEAR_2022 = c(110, 60, 28),
    check.names = FALSE
  ), file.path(root, "2020-2022", "useofforce_agrregatesummarybyyear_2020-2022.csv"),
  row.names = FALSE)

  # 2020-2022 / detailed
  write.csv(data.frame(
    REPORTING_YEAR = sample(2020:2022, 300, replace = TRUE),
    POLICE_SERVICE = sample(c("Toronto", "OPP", "Ottawa", "Hamilton", "York", "Halton", "Peel"),
                              300, replace = TRUE),
    ASSIGNMENT_TYPE = sample(c("Patrol", "Drugs", "Other"), 300, replace = TRUE),
    REPORT_TYPE = sample(c("Individual", "Team"), 300, replace = TRUE)
  ), file.path(root, "2020-2022", "useofforce_detaileddataset_2020-2022.csv"),
  row.names = FALSE)

  # 2023 / main
  write.csv(data.frame(
    IncidentYear = rep(2023L, 80),
    PoliceService = sample(c("Toronto", "OPP", "Halton", "York", "Peel"), 80, replace = TRUE),
    PoliceServiceType = sample(c("Municipal", "Provincial"), 80, replace = TRUE),
    OPP_PoliceService_Region = sample(c("Central", "Eastern", "Western"), 80, replace = TRUE),
    IncidentType = sample(c("Arrest", "Traffic", "PC"), 80, replace = TRUE)
  ), file.path(root, "2023", "uof_main_records.csv"), row.names = FALSE)

  # 2023 / individual — trailing-space typo on outcome col
  df_indiv <- data.frame(
    BatchFileName = sprintf("BF-%03d", 1:120),
    Indiv_Index = 1:120,
    Race = sample(c("White", "Black", "Asian", "Indigenous"), 120, replace = TRUE),
    Gender = sample(c("Male", "Female"), 120, replace = TRUE),
    AgeCategory = sample(c("18 - 24", "25 - 34", "35 - 64"), 120, replace = TRUE),
    `IndivInjuries_PhysicalInjuries ` = sample(c("Yes", "No"), 120, replace = TRUE),
    check.names = FALSE
  )
  write.csv(df_indiv, file.path(root, "2023", "uof_individual_records.csv"), row.names = FALSE)

  # 2023 / probe + weapon
  write.csv(data.frame(
    BatchFileName = "BF-001",
    Indiv_Index = 1:10,
    CEW_CartridgeProbe_CartridgeProbeCycles_Cyc = c("Cyc1", "Cyc1,Cyc2", "", NA,
                                                        "Cyc1,Cyc2,Cyc3", "Cyc1",
                                                        "Cyc1,Cyc2", "", "Cyc1", "Cyc1,Cyc2")
  ), file.path(root, "2023", "uof_probe_cycle_records.csv"), row.names = FALSE)
  write.csv(data.frame(
    BatchFileName = sprintf("BF-%03d", 1:50),
    Indiv_Index = 1:50,
    Weapon = sample(c("Firearm", "Taser", "Baton"), 50, replace = TRUE),
    Location = sample(c("Holster", "Hand"), 50, replace = TRUE)
  ), file.path(root, "2023", "uof_weapon_records_invaliddata.csv"), row.names = FALSE)

  # 2024 / main + individual + probe + weapon
  write.csv(data.frame(
    IncidentYear = rep(2024L, 100),
    PoliceService = sample(c("Toronto", "OPP", "York", "Halton", "Ottawa"), 100, replace = TRUE),
    PoliceServiceType = sample(c("Municipal", "Provincial"), 100, replace = TRUE),
    OPP_PoliceService_Region = sample(c("Central", "Northern"), 100, replace = TRUE),
    IncidentType = sample(c("Arrest", "PC"), 100, replace = TRUE)
  ), file.path(root, "2024", "uof_main_records.csv"), row.names = FALSE)
  write.csv(data.frame(
    BatchFileName = sprintf("BF-%03d", 1:120),
    Indiv_Index = 1:120,
    Race = sample(c("White", "Black", "Asian"), 120, replace = TRUE),
    Gender = sample(c("Male", "Female"), 120, replace = TRUE),
    AgeCategory = sample(c("18 - 24", "25 - 34", "35 - 64"), 120, replace = TRUE),
    IndivInjuries_PhysicalInjuries = sample(c("Yes", "No"), 120, replace = TRUE)
  ), file.path(root, "2024", "uof_individual_records.csv"), row.names = FALSE)
  write.csv(data.frame(
    BatchFileName = "BF-001",
    Indiv_Index = 1:8,
    CEW_CartridgeProbe_CartridgeProbeCycles_Cyc = c("Cyc1", "Cyc1,Cyc2", "", "Cyc1",
                                                       "Cyc1,Cyc2,Cyc3", "Cyc1", "", "Cyc1")
  ), file.path(root, "2024", "uof_probe_cycle_records.csv"), row.names = FALSE)
  write.csv(data.frame(
    BatchFileName = sprintf("BF-%03d", 1:60),
    Indiv_Index = 1:60,
    Weapon = sample(c("Firearm", "Taser", "Baton"), 60, replace = TRUE),
    Location = sample(c("Holster", "Hand"), 60, replace = TRUE),
    Indiv_Weapon_Index = 1:60
  ), file.path(root, "2024", "uof_weapon_records.csv"), row.names = FALSE)

  root
}


# ── analyze_main_records ───────────────────────────────────────────

test_that("analyze_main_records produces all expected sub-results", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_main_records(2024, data_dir = fx)
  expect_true("force_concentration" %in% names(r))
  expect_true("incident_type_x_force" %in% names(r))
  expect_true("data_quality" %in% names(r))
  expect_equal(r$kind, "main_records")
  expect_true(is.finite(r$force_concentration$gini))
  unlink(fx, recursive = TRUE)
})


# ── analyze_individual_records ─────────────────────────────────────

test_that("analyze_individual_records 2024 — all three disparity sub-results", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_individual_records(2024, data_dir = fx)
  expect_true("disparity_by_race" %in% names(r))
  expect_true("disparity_by_gender" %in% names(r))
  expect_true("disparity_by_age" %in% names(r))
  expect_true("data_quality" %in% names(r))
  unlink(fx, recursive = TRUE)
})

test_that("analyze_individual_records 2023 finds trailing-space outcome col", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_individual_records(2023, data_dir = fx)
  expect_true("disparity_by_race" %in% names(r))
  expect_true(is.finite(r$disparity_by_race$baseline_rate))
  unlink(fx, recursive = TRUE)
})

test_that("analyze_individual_records skips disparity when outcome col absent", {
  # Build a fixture WITHOUT the outcome column
  fx <- file.path(tempdir(check = TRUE),
                   paste0("arsau-noout-", as.integer(Sys.time())))
  if (dir.exists(fx)) unlink(fx, recursive = TRUE)
  dir.create(file.path(fx, "2024"), recursive = TRUE)
  write.csv(data.frame(
    BatchFileName = "BF-001",
    Indiv_Index = 1:5,
    Race = c("A", "B", "A", "B", "A"),
    Gender = c("M", "F", "M", "F", "M")
  ), file.path(fx, "2024", "uof_individual_records.csv"), row.names = FALSE)
  r <- morie_arsau_analyze_individual_records(2024, data_dir = fx)
  expect_false("disparity_by_race" %in% names(r))
  expect_true("data_quality" %in% names(r))
  unlink(fx, recursive = TRUE)
})


# ── analyze_probe_cycle_records ────────────────────────────────────

test_that("analyze_probe_cycle_records parses cycle counts", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_probe_cycle_records(2024, data_dir = fx)
  expect_true("cycle_distribution" %in% names(r))
  expect_true(is.finite(r$cycle_distribution$mean_cycles))
  expect_true("data_quality" %in% names(r))
  unlink(fx, recursive = TRUE)
})


# ── analyze_weapon_records ─────────────────────────────────────────

test_that("analyze_weapon_records 2024 with both columns present", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_weapon_records(2024, data_dir = fx)
  expect_true("weapon_x_location" %in% names(r))
  expect_true("weapon_frequencies" %in% names(r))
  expect_true(is.finite(r$weapon_x_location$cramers_v))
  unlink(fx, recursive = TRUE)
})

test_that("analyze_weapon_records 2023 invalid + extra_interpretation paragraph", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_weapon_records(2023, allow_invalid = TRUE, data_dir = fx)
  expect_false(r$is_valid)
  expect_match(r$interpretation, "invalid")
  unlink(fx, recursive = TRUE)
})


# ── analyze_aggregate_summary ──────────────────────────────────────

test_that("analyze_aggregate_summary computes YoY headline series", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_aggregate_summary("2020-2022", data_dir = fx)
  expect_true("yoy_change_headline" %in% names(r))
  expect_equal(r$yoy_change_headline$years, c(2020L, 2021L, 2022L))
  expect_equal(r$yoy_change_headline$counts, c(100L, 120L, 110L))
  unlink(fx, recursive = TRUE)
})


# ── analyze_detailed_dataset ───────────────────────────────────────

test_that("analyze_detailed_dataset chains all 4 sub-results", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_detailed_dataset("2020-2022", data_dir = fx)
  expect_true("force_concentration" %in% names(r))
  expect_true("assignment_x_force" %in% names(r))
  expect_true("yoy_change" %in% names(r))
  expect_true("data_quality" %in% names(r))
  expect_true(is.finite(r$force_concentration$gini))
  unlink(fx, recursive = TRUE)
})


# ── print method ───────────────────────────────────────────────────

test_that("print.morie_arsau_analysis_result emits readable output", {
  fx <- .make_fixture_root_mrm()
  r <- morie_arsau_analyze_main_records(2024, data_dir = fx)
  out <- capture.output(print(r))
  expect_true(any(grepl("ARSAU main_records", out)))
  unlink(fx, recursive = TRUE)
})


# ── invalid-2023 guard fires through the analyzer too ──────────────

test_that("analyze_weapon_records 2023 without allow_invalid still errors", {
  fx <- .make_fixture_root_mrm()
  expect_error(
    morie_arsau_analyze_weapon_records(2023, data_dir = fx),
    "flagged invalid"
  )
  unlink(fx, recursive = TRUE)
})
