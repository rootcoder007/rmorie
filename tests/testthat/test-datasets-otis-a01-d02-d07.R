# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3HH: OTIS a01 Restrictive Confinement + d02-d07 Deaths-in-
# Custody wrappers. Lookup-pending live mode (CKAN resource_ids not
# yet wired in; offline fixtures fully exercised + live errors with
# clear "lookup pending" message; resource_id= override works).

# ====================================================== a01 restrictive confinement

test_that("morie_datasets_otis_a01_restrictive_confinement(offline=TRUE) reads 10-col schema", {
  df <- morie_datasets_otis_a01_restrictive_confinement(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
  expect_equal(ncol(df), 10L)
  for (col in c("EndFiscalYear", "UniqueIndividual_ID",
                "Region_AtTimeOfPlacement",
                "Region_MostRecentPlacement", "Gender",
                "Age_Category", "MentalHealth_Alert",
                "SuicideRisk_Alert", "SuicideWatch_Alert",
                "Number_Of_Placements"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("-SYNTH-", df$UniqueIndividual_ID)))
})

test_that("morie_datasets_otis_a01_restrictive_confinement(offline=FALSE) auto-resolves resource_id from registry", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      # The registry now wires a01 to this canonical CKAN id.
      expect_equal(resource_id, "5a0c5804-a055-4031-9743-73f556e43bb4")
      data.frame(EndFiscalYear = 2024L,
                  UniqueIndividual_ID = "LIVE-A01-REG")
    },
    .package = "morie")
  out <- morie_datasets_otis_a01_restrictive_confinement(offline = FALSE)
  expect_equal(out$UniqueIndividual_ID, "LIVE-A01-REG")
})

test_that("morie_datasets_otis_a01_restrictive_confinement honours explicit resource_id override", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      # Caller-supplied resource_id wins over the registry's wired id.
      expect_equal(resource_id, "explicit-a01-override-id")
      data.frame(EndFiscalYear = 2024L,
                  UniqueIndividual_ID = "LIVE-A01-OVERRIDE")
    },
    .package = "morie")
  out <- morie_datasets_otis_a01_restrictive_confinement(
    offline = FALSE, resource_id = "explicit-a01-override-id")
  expect_equal(out$UniqueIndividual_ID, "LIVE-A01-OVERRIDE")
})

# ====================================================== d02-d05 (3-col schema)

test_that("morie_datasets_otis_d02_deaths_by_gender(offline=TRUE) reads 3-col schema", {
  df <- morie_datasets_otis_d02_deaths_by_gender(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_setequal(names(df),
                  c("Year", "Gender", "Number_CustodialDeaths"))
})

test_that("morie_datasets_otis_d03_deaths_by_race(offline=TRUE) reads 3-col schema", {
  df <- morie_datasets_otis_d03_deaths_by_race(offline = TRUE)
  expect_setequal(names(df),
                  c("Year", "Race", "Number_CustodialDeaths"))
})

test_that("morie_datasets_otis_d04_deaths_by_religion(offline=TRUE) reads 3-col schema", {
  df <- morie_datasets_otis_d04_deaths_by_religion(offline = TRUE)
  expect_setequal(names(df),
                  c("Year", "Religion", "Number_CustodialDeaths"))
})

test_that("morie_datasets_otis_d05_deaths_by_age_category(offline=TRUE) reads 3-col schema", {
  df <- morie_datasets_otis_d05_deaths_by_age_category(offline = TRUE)
  expect_setequal(names(df),
                  c("Year", "Age_Category", "Number_CustodialDeaths"))
})

# ====================================================== d06-d07 (4-col schema)

test_that("morie_datasets_otis_d06_cause_by_alert(offline=TRUE) reads 4-col cause-by-alert schema", {
  df <- morie_datasets_otis_d06_cause_by_alert(offline = TRUE)
  expect_setequal(names(df),
                  c("Year", "Alert_Type", "MedicalCauseOfDeath",
                    "Number_CustodialDeaths"))
})

test_that("morie_datasets_otis_d07_alerts_by_housing_unit(offline=TRUE) reads 4-col alerts-by-housing schema", {
  df <- morie_datasets_otis_d07_alerts_by_housing_unit(offline = TRUE)
  expect_setequal(names(df),
                  c("Year", "Alert_Type", "Housing_Type",
                    "Number_CustodialDeaths"))
})

# ====================================================== auto-resolve via registry

test_that("every d02-d07 + a01 wrapper auto-resolves canonical resource_id from registry", {
  # Map: wrapper -> expected wired CKAN resource_id (post-3KK).
  expected <- list(
    list(fn = morie_datasets_otis_a01_restrictive_confinement,
         rid = "5a0c5804-a055-4031-9743-73f556e43bb4"),
    list(fn = morie_datasets_otis_d02_deaths_by_gender,
         rid = "9de64ab4-0860-499d-8303-014bff5ec412"),
    list(fn = morie_datasets_otis_d03_deaths_by_race,
         rid = "3aaec288-2ab9-4850-851d-e40a69517df5"),
    list(fn = morie_datasets_otis_d04_deaths_by_religion,
         rid = "46437725-4ba6-454b-b4e1-0c05402c84ca"),
    list(fn = morie_datasets_otis_d05_deaths_by_age_category,
         rid = "45820ef9-e23a-4b4b-800a-c13c99dd5b0a"),
    list(fn = morie_datasets_otis_d06_cause_by_alert,
         rid = "cc9dd090-25fe-45b1-b6b0-ae3409fa133b"),
    list(fn = morie_datasets_otis_d07_alerts_by_housing_unit,
         rid = "6bb46038-5f50-4908-8c14-fdf31a4d3d98"))
  for (R in expected) {
    out <- testthat::with_mocked_bindings(
      .morie_ontario_ckan_dump_csv = function(resource_id,
                                                limit = 200000L) {
        expect_equal(resource_id, R$rid)
        data.frame(Year = 2024L, Number_CustodialDeaths = 1L)
      },
      .package = "morie",
      code = R$fn(offline = FALSE))
    expect_s3_class(out, "data.frame")
  }
})

test_that("every d02-d07 wrapper honours explicit resource_id override", {
  reps <- list(
    list(fn = morie_datasets_otis_d02_deaths_by_gender,        rid = "override-d02"),
    list(fn = morie_datasets_otis_d03_deaths_by_race,          rid = "override-d03"),
    list(fn = morie_datasets_otis_d04_deaths_by_religion,      rid = "override-d04"),
    list(fn = morie_datasets_otis_d05_deaths_by_age_category,  rid = "override-d05"),
    list(fn = morie_datasets_otis_d06_cause_by_alert,          rid = "override-d06"),
    list(fn = morie_datasets_otis_d07_alerts_by_housing_unit,  rid = "override-d07"))
  for (R in reps) {
    out <- testthat::with_mocked_bindings(
      .morie_ontario_ckan_dump_csv = function(resource_id,
                                                limit = 200000L) {
        expect_equal(resource_id, R$rid)
        data.frame(Year = 2024L, Number_CustodialDeaths = 1L)
      },
      .package = "morie",
      code = R$fn(offline = FALSE, resource_id = R$rid))
    expect_s3_class(out, "data.frame")
  }
})

# ====================================================== registry integration

test_that("morie_datasets_ontario_ckan_layers includes a01 + d02-d07 with wired resource_ids", {
  reg <- morie_datasets_ontario_ckan_layers()
  expected_otis_keys <- c("otis_d01_deaths_in_custody",
                           "otis_a01_restrictive_confinement",
                           "otis_d02_deaths_by_gender",
                           "otis_d03_deaths_by_race",
                           "otis_d04_deaths_by_religion",
                           "otis_d05_deaths_by_age_category",
                           "otis_d06_cause_by_alert",
                           "otis_d07_alerts_by_housing_unit")
  for (k in expected_otis_keys)
    expect_true(k %in% reg$dataset_key)
  # Post-3KK: all eight a01 + d01-d07 entries are wired (no NA).
  subset <- reg[reg$dataset_key %in% expected_otis_keys, ]
  expect_false(any(is.na(subset$resource_id)))
  # And the resource_ids look like canonical CKAN UUIDs.
  expect_true(all(grepl("^[0-9a-f]{8}-[0-9a-f]{4}", subset$resource_id)))
})

test_that("morie_datasets_ontario_ckan_by_key('otis_d03_deaths_by_race', offline=TRUE) reads bundled fixture", {
  df <- morie_datasets_ontario_ckan_by_key(
    "otis_d03_deaths_by_race", offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true("Race" %in% names(df))
})

test_that("morie_datasets_ontario_ckan_by_key honours resource_id override on lookup-pending entries", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "live-id-for-d06")
      data.frame(Year = 2024L, Alert_Type = "LIVE")
    },
    .package = "morie")
  out <- morie_datasets_ontario_ckan_by_key(
    "otis_d06_cause_by_alert",
    offline = FALSE, resource_id = "live-id-for-d06")
  expect_equal(out$Alert_Type, "LIVE")
})
