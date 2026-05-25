# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3GG: Ontario CKAN portal expansion -- 5 new ARSAU UoF
# loaders (individual / probe_cycle / weapon / aggregate / detailed)
# + consolidated registry + generic by_key factory.

# ====================================== morie_datasets_arsau_uof_individual_records

test_that("morie_datasets_arsau_uof_individual_records(offline=TRUE) reads 112-col schema", {
  df <- morie_datasets_arsau_uof_individual_records(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 5L)
  # Full 112-col upstream schema.
  expect_equal(ncol(df), 112L)
  for (col in c("BatchFileName", "Indiv_Index", "Race",
                "AgeCategory", "Gender", "DifficultyPerceiving"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("^SYNTHETIC-FIXTURE-IND", df$BatchFileName)))
})

test_that("morie_datasets_arsau_uof_individual_records(offline=FALSE) dispatches via mocked CKAN", {
  stub_df <- data.frame(BatchFileName = "LIVE-IND-1",
                         Indiv_Index = 1L, Race = "Black")
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id,
                   "690d4c5e-095e-49a0-bbab-b7fc680f3c6b")
      stub_df
    },
    .package = "morie")
  out <- morie_datasets_arsau_uof_individual_records(year = "2024",
                                                       offline = FALSE)
  expect_equal(out$BatchFileName, "LIVE-IND-1")
})

# ====================================== morie_datasets_arsau_uof_probe_cycle_records

test_that("morie_datasets_arsau_uof_probe_cycle_records(offline=TRUE) reads 3-col schema", {
  df <- morie_datasets_arsau_uof_probe_cycle_records(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 3L)
  expect_true("CEW_CartridgeProbe_CartridgeProbeCycles_Cyc" %in% names(df))
})

test_that("morie_datasets_arsau_uof_probe_cycle_records(offline=FALSE) dispatches with 2023 resource_id", {
  stub <- data.frame(BatchFileName = "LIVE-PRB", Indiv_Index = 1L,
                      CEW_CartridgeProbe_CartridgeProbeCycles_Cyc = 1L)
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "339b9e63-9521-44a6-8719-c2cb9aa39a8a")
      stub
    },
    .package = "morie")
  out <- morie_datasets_arsau_uof_probe_cycle_records(year = "2023",
                                                       offline = FALSE)
  expect_equal(out$BatchFileName, "LIVE-PRB")
})

# ====================================== morie_datasets_arsau_uof_weapon_records

test_that("morie_datasets_arsau_uof_weapon_records(offline=TRUE) reads 5-col schema", {
  df <- morie_datasets_arsau_uof_weapon_records(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 5L)
  for (col in c("BatchFileName", "Indiv_Index", "Weapon",
                "Location", "Indiv_Weapon_Index"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_arsau_uof_weapon_records errors on 2023 (INVALID per ministry)", {
  expect_error(morie_datasets_arsau_uof_weapon_records(year = "2023",
                                                         offline = FALSE),
               regexp = "NULL or marked INVALID")
})

# ====================================== morie_datasets_arsau_aggregate_summary

test_that("morie_datasets_arsau_aggregate_summary(offline=TRUE) reads 6-col 5-year schema", {
  df <- morie_datasets_arsau_aggregate_summary(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 6L)
  for (col in c("SECTION", "CATEGORY", "UNITS OF MEASURE",
                "YEAR_2020", "YEAR_2021", "YEAR_2022"))
    expect_true(col %in% names(df))
})

# ====================================== morie_datasets_arsau_detailed_dataset

test_that("morie_datasets_arsau_detailed_dataset(offline=TRUE) reads 167-col 5-year schema", {
  df <- morie_datasets_arsau_detailed_dataset(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 167L)
  for (col in c("REPORTING_YEAR", "RECORD_ID", "POLICE_SERVICE",
                "REPORT_TYPE"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_arsau_detailed_dataset(offline=FALSE) dispatches with 5-year resource_id", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "2150ac23-4e55-474a-b61f-81baf6850851")
      data.frame(REPORTING_YEAR = 2021L,
                  RECORD_ID = "LIVE-DET")
    },
    .package = "morie")
  out <- morie_datasets_arsau_detailed_dataset(offline = FALSE)
  expect_equal(out$RECORD_ID, "LIVE-DET")
})

# =============================================== consolidated registry helper

test_that("morie_datasets_ontario_ckan_layers returns the full registry as a tidy df", {
  reg <- morie_datasets_ontario_ckan_layers()
  expect_s3_class(reg, "data.frame")
  expect_setequal(names(reg),
                  c("dataset_key", "label", "resource_id",
                    "family", "year", "fixture"))
  expect_true(nrow(reg) >= 10L)
  # All ARSAU resource_ids look like CKAN UUIDs.
  arsau <- reg[reg$family == "arsau", ]
  expect_true(nrow(arsau) >= 9L)
  expect_true(all(grepl("^[0-9a-f]{8}-[0-9a-f]{4}", arsau$resource_id)))
  # OTIS d01 present.
  expect_true("otis_d01_deaths_in_custody" %in% reg$dataset_key)
})

test_that("morie_datasets_ontario_ckan_layers carries year-tagged ARSAU entries", {
  reg <- morie_datasets_ontario_ckan_layers()
  expect_true(any(reg$year == "2023" & reg$family == "arsau"))
  expect_true(any(reg$year == "2024" & reg$family == "arsau"))
  expect_true(any(reg$year == "2020-2022" & reg$family == "arsau"))
})

# =============================================== generic by_key factory

test_that("morie_datasets_ontario_ckan_by_key reads a known offline dataset", {
  df <- morie_datasets_ontario_ckan_by_key(
    "arsau_uof_main_records_2024", offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true("BatchFileName" %in% names(df))
})

test_that("morie_datasets_ontario_ckan_by_key dispatches to live CKAN via mock", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "89e3b63f-5679-4fa4-b98a-fdd2dc486f29")
      data.frame(Year = 2024L, UniqueIndividual_ID = "LIVE-DC-1")
    },
    .package = "morie")
  out <- morie_datasets_ontario_ckan_by_key("otis_d01_deaths_in_custody",
                                              offline = FALSE)
  expect_equal(out$UniqueIndividual_ID, "LIVE-DC-1")
})

test_that("morie_datasets_ontario_ckan_by_key errors on unknown key", {
  expect_error(
    morie_datasets_ontario_ckan_by_key("not-a-real-key",
                                         offline = TRUE),
    regexp = "unknown Ontario CKAN dataset_key")
})

test_that("morie_datasets_ontario_ckan_by_key honours resource_id override on live mode", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "override-xyz")
      data.frame(x = 1L)
    },
    .package = "morie")
  out <- morie_datasets_ontario_ckan_by_key(
    "otis_d01_deaths_in_custody",
    offline = FALSE,
    resource_id = "override-xyz")
  expect_equal(out$x, 1L)
})
