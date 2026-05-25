# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3EE: bundled fixtures + mocked-network tests for the three
# highest-stakes upstream feeds:
#   * Ontario ARSAU UoF main_records  (CKAN datastore dump)
#   * Ontario OTIS d01 Deaths-in-Custody  (CKAN datastore dump)
#   * TPS PSDP Mental Health Act Apprehensions  (ArcGIS FeatureServer)

# ====================================================== ARSAU UoF main_records

test_that("morie_datasets_arsau_uof_main_records(offline=TRUE) reads canonical schema", {
  df <- morie_datasets_arsau_uof_main_records(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  # Canonical schema markers from the upstream feed.
  for (col in c("IncidentYear", "BatchFileName", "PoliceService",
                "IncidentNumber", "Date", "IncidentType",
                "InteractionType", "NumberIndiv"))
    expect_true(col %in% names(df))
  # Bundled fixture is stamped synthetic.
  expect_true(all(grepl("^SYNTHETIC-FIXTURE", df$BatchFileName)))
})

test_that("morie_datasets_arsau_uof_main_records(offline=FALSE) dispatches via mocked CKAN", {
  stub_df <- data.frame(
    IncidentYear = c(2024L, 2024L),
    BatchFileName = c("LIVE-001", "LIVE-002"),
    PoliceService = c("Toronto Police Service", "OPP"),
    IncidentNumber = c("TPS-LIVE-001", "OPP-LIVE-002"),
    stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      # The 2024 resource id should be passed.
      expect_equal(resource_id, "ea9dc29c-b4f1-4426-b1f2-974ce995aca1")
      stub_df
    },
    .package = "morie")
  out <- morie_datasets_arsau_uof_main_records(year = "2024",
                                                 offline = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$BatchFileName[1], "LIVE-001")
})

test_that("morie_datasets_arsau_uof_main_records honours explicit resource_id override", {
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "override-id-xyz")
      data.frame(IncidentYear = 2099L, BatchFileName = "OVERRIDE")
    },
    .package = "morie")
  out <- morie_datasets_arsau_uof_main_records(year = "2024",
                                                 offline = FALSE,
                                                 resource_id = "override-id-xyz")
  expect_equal(out$BatchFileName, "OVERRIDE")
})

test_that("morie_datasets_arsau_uof_main_records errors on unknown year (no resource_id)", {
  expect_error(morie_datasets_arsau_uof_main_records(year = "2099",
                                                       offline = FALSE),
               regexp = "no canonical Ontario CKAN resource_id")
})

# ====================================================== OTIS d01 Deaths in Custody

test_that("morie_datasets_otis_d01_deaths_in_custody(offline=TRUE) reads canonical 6-col schema", {
  df <- morie_datasets_otis_d01_deaths_in_custody(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  expect_setequal(names(df), c("Year", "UniqueIndividual_ID",
                                "Region_AtTimeOfDeath",
                                "HousingUnit_Type",
                                "MedicalCauseofDeath",
                                "MeansofDeath"))
  expect_true(all(grepl("-SYNTH-", df$UniqueIndividual_ID)))
})

test_that("morie_datasets_otis_d01_deaths_in_custody(offline=FALSE) dispatches via mocked CKAN", {
  stub_df <- data.frame(
    Year = c(2024L, 2024L),
    UniqueIndividual_ID = c("2024-LIVE-001", "2024-LIVE-002"),
    Region_AtTimeOfDeath = c("Central", "Eastern"),
    HousingUnit_Type = c("General Population", "Segregation"),
    MedicalCauseofDeath = c("Natural", "Other"),
    MeansofDeath = c("Cardiovascular", "To Be Determined"),
    stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    .morie_ontario_ckan_dump_csv = function(resource_id, limit = 200000L) {
      expect_equal(resource_id, "89e3b63f-5679-4fa4-b98a-fdd2dc486f29")
      stub_df
    },
    .package = "morie")
  out <- morie_datasets_otis_d01_deaths_in_custody(offline = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$Region_AtTimeOfDeath, c("Central", "Eastern"))
})

# ==================================================== TPS MHA Apprehensions (PSDP)

test_that("morie_datasets_tps_mha_apprehensions(offline=TRUE) reads canonical TPS PSDP schema", {
  df <- morie_datasets_tps_mha_apprehensions(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  # TPS PSDP canonical column markers PLUS the MHA-specific column.
  for (col in c("OBJECTID", "EVENT_UNIQUE_ID", "OCC_YEAR",
                "DIVISION", "LOCATION_TYPE", "APPREHENSION_TYPE",
                "HOOD_158", "HOOD_140", "LONG_WGS84", "LAT_WGS84"))
    expect_true(col %in% names(df))
})

test_that("morie_datasets_tps_mha_apprehensions(offline=TRUE) carries BOTH hood schemas", {
  df <- morie_datasets_tps_mha_apprehensions(offline = TRUE)
  # Both columns present -> the schema mismatch assertion warns.
  expect_warning(morie_tps_assert_hood_version(df, expected = "158"),
                 regexp = "BOTH HOOD_158 .* and HOOD_140")
})

test_that("morie_datasets_tps_mha_apprehensions(offline=TRUE) honours year + max_features", {
  df <- morie_datasets_tps_mha_apprehensions(offline = TRUE,
                                               year = 2024L,
                                               max_features = 5L)
  expect_s3_class(df, "data.frame")
  expect_true(all(df$OCC_YEAR == 2024L))
  expect_true(nrow(df) <= 5L)
})

test_that("morie_datasets_tps_mha_apprehensions(offline=FALSE) routes through TPS Hub hub_id 333c4e1c... (3TT+)", {
  # 3TT+: MHA live dispatch was migrated from
  # .morie_tps_psdp_feature_query (direct FeatureServer URL) to
  # morie_datasets_tps_arcgis_hub_by_id with hub_id 333c4e1c...
  stub_df <- data.frame(
    OBJECTID = c(1L, 2L),
    EVENT_UNIQUE_ID = c("LIVE-MHA-001", "LIVE-MHA-002"),
    OCC_YEAR = c(2024L, 2024L),
    APPREHENSION_TYPE = c("MHA Section 16", "MHA Section 17"))
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                     format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
      expect_equal(hub_id, "333c4e1c96314741a83425045b6a7642")
      expect_equal(where, "OCC_YEAR = 2024")
      expect_equal(format, "json")
      stub_df
    },
    .package = "morie")
  out <- morie_datasets_tps_mha_apprehensions(year = 2024L,
                                                offline = FALSE)
  expect_equal(nrow(out), 2L)
  expect_equal(out$APPREHENSION_TYPE,
               c("MHA Section 16", "MHA Section 17"))
})

test_that("morie_datasets_tps_mha_apprehensions(offline=FALSE) (3TT+) defaults to 1=1 when year is NULL", {
  testthat::local_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                     format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
      expect_equal(where, "1=1")
      expect_equal(hub_id, "333c4e1c96314741a83425045b6a7642")
      data.frame(OBJECTID = 1L)
    },
    .package = "morie")
  out <- morie_datasets_tps_mha_apprehensions(offline = FALSE)
  expect_equal(nrow(out), 1L)
})

test_that("morie_datasets_tps_mha_apprehensions(offline=FALSE) honours explicit layer_url override", {
  testthat::local_mocked_bindings(
    .morie_tps_psdp_feature_query = function(layer_url, where,
                                                max_features = NULL,
                                                return_geometry = FALSE) {
      expect_equal(layer_url, "https://example/override/Layer/0")
      data.frame(OBJECTID = 99L)
    },
    .package = "morie")
  out <- morie_datasets_tps_mha_apprehensions(
    offline = FALSE,
    layer_url = "https://example/override/Layer/0")
  expect_equal(out$OBJECTID, 99L)
})

# =============================================== TPS MHA + HOOD equivalency interop

test_that("morie_datasets_tps_mha_apprehensions integrates with morie_tps_add_hood_140_from_158", {
  df <- morie_datasets_tps_mha_apprehensions(offline = TRUE)
  out <- morie_tps_add_hood_140_from_158(df, col_in = "HOOD_158")
  expect_true("HOOD_140_equiv" %in% names(out))
  # 158-168 in the fixture (row 2) -> historical parent 140-075.
  row2 <- out[out$HOOD_158 == "168", ]
  expect_equal(row2$HOOD_140_equiv, "075")
})
