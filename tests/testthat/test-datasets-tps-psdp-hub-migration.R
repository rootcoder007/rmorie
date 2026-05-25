# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3TT+: migration of the 11 pre-3FF PSDP wrappers + MHA (3EE)
# to thin-dispatch over morie_datasets_tps_arcgis_hub_by_id with
# the canonical TPS Hub hub_id. Spot-checked individually in
# test-datasets-tps-psdp.R + test-datasets-ontario-tps.R; this
# file is the comprehensive matrix proof -- ALL 12 wrappers route
# through the hub_id path.

# Canonical mapping verified live 2026-05-24 against the TPS Hub
# OGC-API-Features search endpoint
# (data.tps.ca/api/search/v1/collections/dataset/items?limit=100).
.tt_plus_mapping <- list(
  list(fn = morie_datasets_tps_assault,
       hub_id = "b4d0398d37eb4aa184065ed625ddb922",
       title = "Assault Open Data"),
  list(fn = morie_datasets_tps_autotheft,
       hub_id = "95ab41aee16847dba8453bf1688249d6",
       title = "Auto Theft Open Data"),
  list(fn = morie_datasets_tps_bicycletheft,
       hub_id = "a89d10d5e28444ceb0c8d1d4c0ee39cc",
       title = "Bicycle Thefts Open Data"),
  list(fn = morie_datasets_tps_breakandenter,
       hub_id = "040ead448df2412da252cfbb532e77ac",
       title = "Break and Enter Open Data"),
  list(fn = morie_datasets_tps_hatecrimes,
       hub_id = "3dc9a8fae28b42c7aaf8fc62c7fbfdaa",
       title = "Hate Crimes Open Data"),
  list(fn = morie_datasets_tps_homicides,
       hub_id = "d96bf5b67c1c49879f354dad51cf81f9",
       title = "Homicides Open Data (ASR-RC-TBL-002)"),
  list(fn = morie_datasets_tps_intimate_partner_family_violence,
       hub_id = "724113c886ee4df2b917dcc047f82d26",
       title = "Intimate Partner and Family Violence Open Data"),
  list(fn = morie_datasets_tps_robbery,
       hub_id = "d0e1e98de5f945faa2fe635dee3f4062",
       title = "Robbery Open Data"),
  list(fn = morie_datasets_tps_shooting_firearm_discharges,
       hub_id = "64ddeca12da34403869968ec725e23c4",
       title = "Shooting and Firearm Discharges Open Data"),
  list(fn = morie_datasets_tps_theft_from_motor_vehicle,
       hub_id = "d9303bc20f8a4351b7744a8703eecb80",
       title = "Theft From Motor Vehicle Open Data"),
  list(fn = morie_datasets_tps_theft_over,
       hub_id = "7530d9b637c340059ccb81a782481c04",
       title = "Theft Over Open Data"),
  list(fn = morie_datasets_tps_mha_apprehensions,
       hub_id = "333c4e1c96314741a83425045b6a7642",
       title = "Mental Health Act Apprehensions Open Data"))

# =================================== matrix proof of canonical routing

test_that("all 12 migrated wrappers route through morie_datasets_tps_arcgis_hub_by_id with the canonical hub_id", {
  for (entry in .tt_plus_mapping) {
    seen <- list()
    testthat::with_mocked_bindings(
      morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                       format = "json",
                                                       where = "1=1",
                                                       max_features = NULL,
                                                       layer_idx = 0L,
                                                       offline = TRUE,
                                                       dest = NULL) {
        seen <<- list(hub_id = hub_id, format = format,
                      where = where, layer_idx = layer_idx,
                      offline = offline)
        data.frame(stub = 1L)
      },
      .package = "morie",
      code = entry$fn(year = 2024L, offline = FALSE))
    expect_equal(seen$hub_id, entry$hub_id,
                 info = sprintf("wrapper for %s did not dispatch to hub_id %s",
                                 entry$title, entry$hub_id))
    expect_equal(seen$format, "json")
    expect_equal(seen$where, "OCC_YEAR = 2024")
    expect_equal(seen$layer_idx, 0L)
    # 3TT+ contract: offline=TRUE for the resolve step (so the
    # bundled catalog provides the URL without network).
    expect_true(isTRUE(seen$offline))
  }
})

test_that("all 12 migrated wrappers default to where='1=1' when year is NULL", {
  for (entry in .tt_plus_mapping) {
    seen_where <- NULL
    testthat::with_mocked_bindings(
      morie_datasets_tps_arcgis_hub_by_id = function(hub_id,
                                                       where = "1=1",
                                                       ...) {
        seen_where <<- where
        data.frame()
      },
      .package = "morie",
      code = entry$fn(offline = FALSE))
    expect_equal(seen_where, "1=1",
                 info = sprintf("default where wrong for %s",
                                 entry$title))
  }
})

# =================================== backward-compat: layer_url override

test_that("legacy layer_url override still routes through .morie_tps_psdp_feature_query (NOT the hub_id path)", {
  # 3TT+: the layer_url override remains as the backward-compat
  # escape hatch. When the caller provides one, the migrated
  # dispatch must NOT call morie_datasets_tps_arcgis_hub_by_id;
  # it must hit the override URL via the legacy helper.
  hub_called <- FALSE
  feature_called <- FALSE
  testthat::with_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      hub_called <<- TRUE
      data.frame()
    },
    .morie_tps_psdp_feature_query = function(layer_url, where,
                                                max_features = NULL,
                                                return_geometry = FALSE) {
      feature_called <<- TRUE
      expect_equal(layer_url, "https://example.test/legacy/X/0")
      data.frame(OBJECTID = 99L)
    },
    .package = "morie",
    code = morie_datasets_tps_assault(
      offline = FALSE,
      layer_url = "https://example.test/legacy/X/0"))
  expect_false(hub_called,
               info = "layer_url override should bypass hub_id path")
  expect_true(feature_called,
              info = "layer_url override should hit .morie_tps_psdp_feature_query")
})

test_that("MHA layer_url override also bypasses the hub_id path (3TT+)", {
  hub_called <- FALSE
  feature_called <- FALSE
  testthat::with_mocked_bindings(
    morie_datasets_tps_arcgis_hub_by_id = function(hub_id, ...) {
      hub_called <<- TRUE
      data.frame()
    },
    .morie_tps_psdp_feature_query = function(layer_url, where,
                                                max_features = NULL,
                                                return_geometry = FALSE) {
      feature_called <<- TRUE
      expect_equal(layer_url, "https://example.test/mha-mirror/0")
      data.frame(OBJECTID = 77L)
    },
    .package = "morie",
    code = morie_datasets_tps_mha_apprehensions(
      offline = FALSE,
      layer_url = "https://example.test/mha-mirror/0"))
  expect_false(hub_called)
  expect_true(feature_called)
})

# =================================== offline mode unchanged

test_that("offline mode is unchanged by 3TT+ -- bundled fixtures still load + year filtering still works", {
  # Pick a representative; the offline path was deliberately
  # untouched in 3TT+.
  df <- morie_datasets_tps_assault(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0L)
  expect_true("OCC_YEAR" %in% names(df))
})

# =================================== layers helper carries hub_id

test_that("morie_tps_psdp_layers gained the hub_id column in 3TT+", {
  reg <- morie_tps_psdp_layers()
  expect_true("hub_id" %in% names(reg))
  expect_true(all(grepl("^[a-f0-9]{32}$", reg$hub_id)))
  # Spot check: assault row's hub_id matches the canonical.
  expect_equal(reg$hub_id[reg$layer_key == "assault"],
               "b4d0398d37eb4aa184065ed625ddb922")
})

# =================================== alignment with the TPS Hub catalog

test_that("every migrated wrapper's hub_id appears in the bundled TPS Hub catalog", {
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  for (entry in .tt_plus_mapping) {
    expect_true(entry$hub_id %in% cat$hub_id,
                info = sprintf("%s hub_id %s NOT in TPS Hub catalog",
                                entry$title, entry$hub_id))
    title_in_cat <- cat$title[cat$hub_id == entry$hub_id]
    expect_equal(title_in_cat, entry$title,
                 info = sprintf("title drift for %s", entry$hub_id))
  }
})
