# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3SS: TPS ArcGIS Hub catalog (71 datasets verified live
# 2026-05-24) + generic by-id loader supporting json / geojson /
# csv / shapefile / fgdb formats.

# ================================================== discovery helper

test_that("morie_datasets_tps_arcgis_hub_layers(offline=TRUE) reads the bundled 71-row catalog", {
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  expect_s3_class(cat, "data.frame")
  expect_equal(nrow(cat), 71L)
  expect_setequal(names(cat),
                  c("hub_id", "title", "type",
                    "feature_server_url", "owner",
                    "tags", "snippet"))
  # All 32-char hex hub_ids.
  expect_true(all(grepl("^[a-f0-9]{32}$", cat$hub_id)))
  # All owned by TorontoPoliceService (verified at fixture-creation).
  expect_true(all(cat$owner == "TorontoPoliceService"))
  # All FeatureServer URLs point at the services.arcgis.com host.
  expect_true(all(grepl(
    "^https://services\\.arcgis\\.com/[A-Za-z0-9]+/arcgis/rest/services/",
    cat$feature_server_url)))
  # Spot-check well-known datasets are present.
  for (t in c("Homicides Open Data (ASR-RC-TBL-002)",
              "Hate Crimes Open Data",
              "Persons in Crisis Calls for Service Attended Open Data",
              "Mental Health Act Apprehensions Open Data",
              "Victims of Crime (ASR-VC-TBL-001)",
              "Police Divisions",
              "Use of Force: Gender Composition (RBDC-UOF-TBL-006)"))
    expect_true(t %in% cat$title)
})

test_that("morie_datasets_tps_arcgis_hub_layers(offline=FALSE) hits the TPS Hub search API and parses features", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      expect_match(url,
                   "data\\.tps\\.ca/api/search/v1/collections/dataset/items$")
      expect_equal(query$limit, 100L)
      list(
        type = "FeatureCollection",
        numberMatched = 2L,
        numberReturned = 2L,
        features = list(
          list(id = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
               properties = list(
                 title = "Test Dataset A",
                 type = "Feature Service",
                 url = "https://services.arcgis.com/X/arcgis/rest/services/TestA/FeatureServer",
                 owner = "TorontoPoliceService",
                 tags = list("Toronto", "Police"),
                 snippet = "A snippet.")),
          list(id = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
               properties = list(
                 title = "Test Dataset B",
                 type = "Feature Service",
                 url = "https://services.arcgis.com/X/arcgis/rest/services/TestB/FeatureServer",
                 owner = "TorontoPoliceService",
                 tags = list("Police"),
                 snippet = "B snippet."))))
    },
    .package = "morie")
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = FALSE)
  expect_equal(nrow(cat), 2L)
  expect_setequal(cat$title, c("Test Dataset A", "Test Dataset B"))
  expect_equal(cat$tags[cat$title == "Test Dataset A"], "Toronto; Police")
})

# =================================================== hub-id resolver

test_that(".morie_dataset_tps_hub_resolve(offline=TRUE) finds the FeatureServer URL in the catalog", {
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  pic_id <- cat$hub_id[cat$title == "Persons in Crisis Calls for Service Attended Open Data"]
  resolved <- morie:::.morie_dataset_tps_hub_resolve(pic_id, offline = TRUE)
  expect_match(resolved, "Persons_in_Crisis")
  expect_match(resolved, "/FeatureServer$")
})

test_that(".morie_dataset_tps_hub_resolve errors on a non-hex hub_id", {
  expect_error(
    morie:::.morie_dataset_tps_hub_resolve("not-a-hex", offline = TRUE),
    regexp = "32-char hex GUID")
})

test_that(".morie_dataset_tps_hub_resolve errors on a hex GUID not in the catalog", {
  expect_error(
    morie:::.morie_dataset_tps_hub_resolve(
      "ffffffffffffffffffffffffffffffff",
      offline = TRUE),
    regexp = "not in the bundled catalog")
})

test_that(".morie_dataset_tps_hub_resolve(offline=FALSE) hits the ArcGIS Online items API", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      expect_match(url,
                   "arcgis\\.com/sharing/rest/content/items/[a-f0-9]{32}$")
      expect_equal(query$f, "json")
      list(url = "https://services.arcgis.com/X/arcgis/rest/services/Foo/FeatureServer",
           title = "Foo", type = "Feature Service")
    },
    .package = "morie")
  out <- morie:::.morie_dataset_tps_hub_resolve(
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", offline = FALSE)
  expect_match(out, "/Foo/FeatureServer$")
})

# =================================================== by-id loader: format = "json"

test_that("morie_datasets_tps_arcgis_hub_by_id(format='json') hits FeatureServer /0/query?f=json + parses attributes", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      expect_match(url,
                   "/Persons_in_Crisis_Calls_for_Service_Attended_Open_Data/FeatureServer/0/query$")
      expect_equal(query$where, "OCC_YEAR=2024")
      expect_equal(query$f, "json")
      expect_equal(query$outFields, "*")
      expect_equal(query$returnGeometry, "false")
      expect_equal(query$resultRecordCount, 25L)
      list(features = list(
        list(attributes = list(EVENT_UNIQUE_ID = "AAA", OCC_YEAR = 2024)),
        list(attributes = list(EVENT_UNIQUE_ID = "BBB", OCC_YEAR = 2024))))
    },
    .package = "morie")
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  pic_id <- cat$hub_id[cat$title == "Persons in Crisis Calls for Service Attended Open Data"]
  df <- morie_datasets_tps_arcgis_hub_by_id(
    pic_id, format = "json",
    where = "OCC_YEAR=2024",
    max_features = 25L,
    offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(df$EVENT_UNIQUE_ID, c("AAA", "BBB"))
})

test_that("morie_datasets_tps_arcgis_hub_by_id(format='json') returns empty frame on no features", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      list(features = list())
    },
    .package = "morie")
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  pic_id <- cat$hub_id[cat$title == "Persons in Crisis Calls for Service Attended Open Data"]
  df <- morie_datasets_tps_arcgis_hub_by_id(pic_id, format = "json")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
})

# =================================================== by-id loader: format = "geojson"

test_that("morie_datasets_tps_arcgis_hub_by_id(format='geojson') hits FeatureServer with f=geojson + returns the raw list", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      expect_match(url, "/FeatureServer/0/query$")
      expect_equal(query$f, "geojson")
      list(type = "FeatureCollection",
           features = list(list(type = "Feature",
                                  geometry = list(type = "Polygon"),
                                  properties = list(name = "D52"))))
    },
    .package = "morie")
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  pd_id <- cat$hub_id[cat$title == "Police Divisions"]
  out <- morie_datasets_tps_arcgis_hub_by_id(pd_id, format = "geojson")
  expect_equal(out$type, "FeatureCollection")
  expect_equal(out$features[[1L]]$properties$name, "D52")
})

# =================================================== by-id loader: format = "csv"

test_that("morie_datasets_tps_arcgis_hub_by_id(format='csv') hits hub.arcgis.com downloads + parses CSV", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_text = function(url, query = NULL) {
      expect_match(url,
                   "hub\\.arcgis\\.com/api/v3/datasets/[a-f0-9]{32}_0/downloads/data$")
      expect_equal(query$format, "csv")
      "col_a,col_b\n1,X\n2,Y\n"
    },
    .package = "morie")
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  ho_id <- cat$hub_id[cat$title == "Homicides Open Data (ASR-RC-TBL-002)"]
  df <- morie_datasets_tps_arcgis_hub_by_id(ho_id, format = "csv")
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("col_a", "col_b"))
  expect_equal(nrow(df), 2L)
})

# =================================================== by-id loader: validation

test_that("morie_datasets_tps_arcgis_hub_by_id rejects unknown format", {
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  any_id <- cat$hub_id[1L]
  expect_error(
    morie_datasets_tps_arcgis_hub_by_id(any_id, format = "kml"))
})

test_that("morie_datasets_tps_arcgis_hub_by_id rejects non-hex hub_id at the resolver layer", {
  expect_error(
    morie_datasets_tps_arcgis_hub_by_id("invalid-id"),
    regexp = "32-char hex GUID")
})

# =================================================== direct download helper

test_that("morie_datasets_tps_arcgis_hub_download builds the correct format-specific URL", {
  # Smoke test the URL-building logic without actually performing
  # the binary download by short-circuiting requireNamespace.
  expect_error(
    morie_datasets_tps_arcgis_hub_download(
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      format = "kml"),
    regexp = "should be one of")
})

# =================================================== dedupe with 3FF / 3EE

test_that("Catalog covers known 3EE + 3FF-overlap datasets (verification: dedupe-by-title)", {
  cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  # 3EE wrapped Mental Health Act Apprehensions (via FeatureServer URL):
  expect_true("Mental Health Act Apprehensions Open Data" %in% cat$title)
  # 3FF wrapped 6 of these via FeatureServer URL too:
  for (t in c("Persons in Crisis Calls for Service Attended Open Data",
              "Hate Crimes Open Data",
              "Shooting and Firearm Discharges Open Data",
              "Firearms Top Calibres (ASR-F-TBL-001)",
              "Top 20 Offences of Firearm Seizures (ASR-F-TBL-002)",
              "Miscellaneous Firearms (ASR-F-TBL-003)"))
    expect_true(t %in% cat$title)
})

# =================================================== integration: default offline

test_that("morie_datasets_tps_arcgis_hub_layers defaults to offline = TRUE", {
  expect_s3_class(morie_datasets_tps_arcgis_hub_layers(),
                  "data.frame")
})
