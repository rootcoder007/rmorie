# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3FF: TPS Public Safety Data Portal (PSDP) 11-layer factory.
#
# Covers R/datasets_tps_psdp.R end-to-end against the 11 bundled
# per-layer synthetic fixtures + mocked-ArcGIS live-mode dispatch.

# ============================================================== registry helper

test_that("morie_tps_psdp_layers returns the full 11-layer registry (post-3TT+ includes hub_id)", {
  reg <- morie_tps_psdp_layers()
  expect_s3_class(reg, "data.frame")
  expect_equal(nrow(reg), 11L)
  expect_setequal(names(reg),
                  c("layer_key", "label", "arcgis_url", "fixture",
                    "hub_id"))  # +hub_id in 3TT+
  for (k in c("assault", "autotheft", "bicycletheft",
              "breakandenter", "hatecrimes", "homicides",
              "intimate_partner_family_violence", "robbery",
              "shooting_firearm_discharges",
              "theft_from_motor_vehicle", "theft_over"))
    expect_true(k %in% reg$layer_key)
  # Every ArcGIS URL is on the TPS S9th0jAJ7bqgIRjw FeatureServer host.
  expect_true(all(grepl(
    "^https://services\\.arcgis\\.com/S9th0jAJ7bqgIRjw/", reg$arcgis_url)))
  # 3TT+: all 11 hub_ids are 32-char hex GUIDs.
  expect_true(all(grepl("^[a-f0-9]{32}$", reg$hub_id)))
})

# ====================================================== offline-mode fixtures

test_that("all 11 TPS PSDP layers load offline and carry both HOOD schemas", {
  layers <- list(
    list(fn = morie_datasets_tps_assault,                      key = "assault"),
    list(fn = morie_datasets_tps_autotheft,                    key = "autotheft"),
    list(fn = morie_datasets_tps_bicycletheft,                 key = "bicycletheft"),
    list(fn = morie_datasets_tps_breakandenter,                key = "breakandenter"),
    list(fn = morie_datasets_tps_hatecrimes,                   key = "hatecrimes"),
    list(fn = morie_datasets_tps_homicides,                    key = "homicides"),
    list(fn = morie_datasets_tps_intimate_partner_family_violence,
         key = "intimate_partner_family_violence"),
    list(fn = morie_datasets_tps_robbery,                      key = "robbery"),
    list(fn = morie_datasets_tps_shooting_firearm_discharges,
         key = "shooting_firearm_discharges"),
    list(fn = morie_datasets_tps_theft_from_motor_vehicle,
         key = "theft_from_motor_vehicle"),
    list(fn = morie_datasets_tps_theft_over,                   key = "theft_over"))
  for (L in layers) {
    df <- L$fn(offline = TRUE)
    expect_s3_class(df, "data.frame")
    expect_true(nrow(df) > 0L)
    # Every layer carries BOTH hood schemas + canonical TPS naming.
    expect_true("HOOD_158" %in% names(df))
    expect_true("HOOD_140" %in% names(df))
  }
})

# ==================================================== per-cluster schema markers

test_that("Cluster A (standard 31-col crime) fixtures carry OFFENCE + CSI_CATEGORY", {
  for (fn in list(morie_datasets_tps_assault,
                  morie_datasets_tps_autotheft,
                  morie_datasets_tps_breakandenter,
                  morie_datasets_tps_robbery,
                  morie_datasets_tps_theft_from_motor_vehicle,
                  morie_datasets_tps_theft_over)) {
    df <- fn(offline = TRUE)
    expect_true("OFFENCE" %in% names(df))
    expect_true("CSI_CATEGORY" %in% names(df))
    expect_true("UCR_CODE" %in% names(df))
  }
})

test_that("BicycleTheft fixture carries PRIMARY_OFFENCE + BIKE_* schema", {
  df <- morie_datasets_tps_bicycletheft(offline = TRUE)
  expect_true("PRIMARY_OFFENCE" %in% names(df))
  for (col in c("BIKE_MAKE", "BIKE_MODEL", "BIKE_TYPE", "BIKE_COLOUR",
                "BIKE_COST", "STATUS"))
    expect_true(col %in% names(df))
})

test_that("HateCrimes fixture carries BIAS columns + ARREST_MADE", {
  df <- morie_datasets_tps_hatecrimes(offline = TRUE)
  for (col in c("AGE_BIAS", "RACE_BIAS", "ETHNICITY_BIAS",
                "RELIGION_BIAS", "SEXUAL_ORIENTATION_BIAS",
                "GENDER_BIAS", "PRIMARY_OFFENCE", "ARREST_MADE"))
    expect_true(col %in% names(df))
  # HateCrimes uses OCCURRENCE_YEAR / REPORTED_YEAR (not OCC_YEAR).
  expect_true("OCCURRENCE_YEAR" %in% names(df))
})

test_that("Homicides fixture carries HOMICIDE_TYPE", {
  df <- morie_datasets_tps_homicides(offline = TRUE)
  expect_true("HOMICIDE_TYPE" %in% names(df))
})

test_that("IPFV fixture carries FAMILY_VIOLENCE_FLAG/RELATION + COUNT + INDEX", {
  df <- morie_datasets_tps_intimate_partner_family_violence(offline = TRUE)
  for (col in c("INDEX", "HISTORICAL", "FAMILY_VIOLENCE_FLAG",
                "FAMILY_VIOLENCE_RELATION", "COUNT"))
    expect_true(col %in% names(df))
})

test_that("Shooting/Firearm Discharges fixture carries DEATH + INJURIES + EVENT_TYPE", {
  df <- morie_datasets_tps_shooting_firearm_discharges(offline = TRUE)
  for (col in c("DEATH", "INJURIES", "EVENT_TYPE", "OCC_TIME_RANGE"))
    expect_true(col %in% names(df))
})

# =============================================== year + max_features filters

test_that("offline year filter honours OCC_YEAR / OCCURRENCE_YEAR / REPORT_YEAR", {
  # OCC_YEAR layer: assault.
  df <- morie_datasets_tps_assault(offline = TRUE, year = 2024L)
  expect_true(all(df$OCC_YEAR == 2024L))
  # OCCURRENCE_YEAR layer: hatecrimes.
  df2 <- morie_datasets_tps_hatecrimes(offline = TRUE, year = 2024L)
  expect_true(all(df2$OCCURRENCE_YEAR == 2024L))
  # REPORT_YEAR layer: ipfv.
  df3 <- morie_datasets_tps_intimate_partner_family_violence(
    offline = TRUE, year = 2024L)
  expect_true(all(df3$REPORT_YEAR == 2024L))
})

test_that("offline max_features truncates to first N rows", {
  df <- morie_datasets_tps_assault(offline = TRUE, max_features = 2L)
  expect_equal(nrow(df), 2L)
})

# ====================================================== mocked ArcGIS dispatch

test_that("offline=FALSE (3TT+) routes through morie_datasets_tps_arcgis_hub_by_id with the canonical hub_id for every layer", {
  # 3TT+: the live dispatch was migrated from
  # .morie_tps_psdp_feature_query (direct FeatureServer URL) to
  # morie_datasets_tps_arcgis_hub_by_id (routed through the TPS Hub
  # catalog by hub_id). One spot-check per cluster representative.
  reps <- list(
    list(fn = morie_datasets_tps_assault,
         hub_id = "b4d0398d37eb4aa184065ed625ddb922"),
    list(fn = morie_datasets_tps_bicycletheft,
         hub_id = "a89d10d5e28444ceb0c8d1d4c0ee39cc"),
    list(fn = morie_datasets_tps_hatecrimes,
         hub_id = "3dc9a8fae28b42c7aaf8fc62c7fbfdaa"),
    list(fn = morie_datasets_tps_homicides,
         hub_id = "d96bf5b67c1c49879f354dad51cf81f9"),
    list(fn = morie_datasets_tps_intimate_partner_family_violence,
         hub_id = "724113c886ee4df2b917dcc047f82d26"),
    list(fn = morie_datasets_tps_shooting_firearm_discharges,
         hub_id = "64ddeca12da34403869968ec725e23c4"))
  for (R in reps) {
    captured <- local({
      stub <- data.frame(OBJECTID = 1L)
      testthat::with_mocked_bindings(
        morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                         format = "json",
                                                         where = "1=1",
                                                         max_features = NULL,
                                                         layer_idx = 0L,
                                                         offline = TRUE,
                                                         dest = NULL) {
          expect_equal(hub_id, R$hub_id)
          expect_equal(format, "json")
          expect_equal(where, "OCC_YEAR = 2024")
          expect_equal(layer_idx, 0L)
          stub
        },
        .package = "morie",
        code = R$fn(year = 2024L, offline = FALSE))
    })
    expect_s3_class(captured, "data.frame")
    expect_equal(nrow(captured), 1L)
  }
})

test_that("offline=FALSE honours layer_url override", {
  captured <- testthat::with_mocked_bindings(
    .morie_tps_psdp_feature_query = function(layer_url, where,
                                                max_features = NULL,
                                                return_geometry = FALSE) {
      expect_equal(layer_url, "https://example/override/X/0")
      data.frame(OBJECTID = 99L)
    },
    .package = "morie",
    code = morie_datasets_tps_assault(
      offline = FALSE,
      layer_url = "https://example/override/X/0"))
  expect_equal(captured$OBJECTID, 99L)
})

test_that("offline=FALSE (3TT+) defaults to where = '1=1' when year is NULL", {
  testthat::with_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                     format = "json",
                                                     where = "1=1",
                                                     max_features = NULL,
                                                     layer_idx = 0L,
                                                     offline = TRUE,
                                                     dest = NULL) {
      expect_equal(where, "1=1")
      expect_equal(hub_id, "d0e1e98de5f945faa2fe635dee3f4062")  # robbery
      data.frame(OBJECTID = 1L)
    },
    .package = "morie",
    code = morie_datasets_tps_robbery(offline = FALSE))
})

# ====================================================== error path

test_that(".morie_tps_psdp_dispatch errors on unknown layer_key", {
  expect_error(
    morie:::.morie_tps_psdp_dispatch("nonexistent", NULL, NULL,
                                       TRUE, NULL),
    regexp = "unknown TPS PSDP layer_key")
})

# ================================================== HOOD equivalency interop

test_that("TPS PSDP layers integrate with morie_tps_add_hood_140_from_158", {
  # Use the assault fixture which has the canonical HOOD_158 column.
  df <- morie_datasets_tps_assault(offline = TRUE)
  out <- morie_tps_add_hood_140_from_158(df, col_in = "HOOD_158")
  expect_true("HOOD_140_equiv" %in% names(out))
  # Row 2 of the fixture has HOOD_158 = "168" Downtown Yonge East
  # which is a child of 140-075 Church-Yonge Corridor (clean cake-cut).
  row_168 <- out[out$HOOD_158 == "168", ]
  expect_equal(row_168$HOOD_140_equiv, "075")
  # Row 4 has HOOD_158 = "163" Fort York-Liberty Village which is a
  # child of 140-082 Niagara.
  row_163 <- out[out$HOOD_158 == "163", ]
  expect_equal(row_163$HOOD_140_equiv, "082")
})

test_that("TPS PSDP layers integrate with morie_tps_assert_hood_version", {
  df <- morie_datasets_tps_homicides(offline = TRUE)
  # Both HOOD schemas present -> assert warns (silent-mix footgun).
  expect_warning(morie_tps_assert_hood_version(df, expected = "158"),
                 regexp = "BOTH HOOD_158 .* and HOOD_140")
})
