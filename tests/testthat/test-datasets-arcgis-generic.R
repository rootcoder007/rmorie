# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3SS+: portal-agnostic ArcGIS Online item helpers (resolve
# any item id, not just TPS Hub catalog entries) + named wrapper
# for ZonesofToronto_Neighbourhoods (EsriCanadaEducation,
# af06159170914808983959df6163fc86).

# ============================================== morie_datasets_arcgis_item_metadata

test_that("morie_datasets_arcgis_item_metadata returns a 1-row data.frame with the canonical columns", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      expect_match(url,
                   "arcgis\\.com/sharing/rest/content/items/af06159170914808983959df6163fc86$")
      expect_equal(query$f, "json")
      list(title = "Toronto Zoning per Neighbourhood",
           type = "Feature Service",
           url = "https://services.arcgis.com/As5CFN3ThbQpy8Ph/arcgis/rest/services/ZonesofToronto_Neighbourhoods/FeatureServer",
           owner = "EsriCanadaEducation",
           tags = list("Toronto", "Zoning", "Neighbourhoods"),
           snippet = "Summary of the Zones of Toronto Neighbourhoods")
    },
    .package = "morie")
  m <- morie_datasets_arcgis_item_metadata(
    "af06159170914808983959df6163fc86")
  expect_s3_class(m, "data.frame")
  expect_equal(nrow(m), 1L)
  expect_setequal(names(m),
                  c("hub_id", "title", "type",
                    "feature_server_url", "owner",
                    "tags", "snippet"))
  expect_equal(m$title, "Toronto Zoning per Neighbourhood")
  expect_equal(m$owner, "EsriCanadaEducation")
  expect_equal(m$tags, "Toronto; Zoning; Neighbourhoods")
})

test_that("morie_datasets_arcgis_item_metadata rejects non-hex item_id", {
  expect_error(
    morie_datasets_arcgis_item_metadata("invalid-shape"),
    regexp = "32-char hex GUID")
})

# ============================================== morie_datasets_arcgis_item_by_id

test_that("morie_datasets_arcgis_item_by_id(format='json') resolves item -> FeatureServer URL then queries layer 0", {
  call_count <- 0L
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        # First call: resolve via ArcGIS Online items API.
        expect_match(url,
                     "arcgis\\.com/sharing/rest/content/items/")
        return(list(url = "https://services.arcgis.com/X/arcgis/rest/services/Foo/FeatureServer",
                    title = "Foo", type = "Feature Service"))
      }
      # Second call: FeatureServer /0/query.
      expect_match(url, "/Foo/FeatureServer/0/query$")
      expect_equal(query$where, "1=1")
      list(features = list(
        list(attributes = list(id = 1L, name = "A")),
        list(attributes = list(id = 2L, name = "B"))))
    },
    .package = "morie")
  df <- morie_datasets_arcgis_item_by_id(
    "af06159170914808983959df6163fc86",
    format = "json")
  expect_equal(call_count, 2L)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(df$name, c("A", "B"))
})

test_that("morie_datasets_arcgis_item_by_id(format='geojson') hits FeatureServer with f=geojson", {
  call_count <- 0L
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        return(list(url = "https://services.arcgis.com/X/arcgis/rest/services/Foo/FeatureServer"))
      }
      expect_match(url, "/FeatureServer/2/query$")
      expect_equal(query$f, "geojson")
      list(type = "FeatureCollection",
           features = list(list(type = "Feature")))
    },
    .package = "morie")
  out <- morie_datasets_arcgis_item_by_id(
    "af06159170914808983959df6163fc86",
    format = "geojson",
    layer_idx = 2L)
  expect_equal(out$type, "FeatureCollection")
})

test_that("morie_datasets_arcgis_item_by_id(format='csv') hits hub.arcgis.com downloads after resolve", {
  testthat::local_mocked_bindings(
    .morie_dataset_http_json = function(url, query = NULL) {
      list(url = "https://services.arcgis.com/X/arcgis/rest/services/Foo/FeatureServer")
    },
    .morie_dataset_http_text = function(url, query = NULL) {
      expect_match(url,
                   "hub\\.arcgis\\.com/api/v3/datasets/[a-f0-9]{32}_0/downloads/data$")
      expect_equal(query$format, "csv")
      "a,b\n1,X\n2,Y\n"
    },
    .package = "morie")
  df <- morie_datasets_arcgis_item_by_id(
    "af06159170914808983959df6163fc86",
    format = "csv")
  expect_equal(nrow(df), 2L)
  expect_equal(names(df), c("a", "b"))
})

test_that("morie_datasets_arcgis_item_by_id rejects unknown format", {
  expect_error(
    morie_datasets_arcgis_item_by_id(
      "af06159170914808983959df6163fc86", format = "kml"),
    regexp = "should be one of")
})

test_that("morie_datasets_arcgis_item_by_id propagates non-hex item_id error from the resolver", {
  expect_error(
    morie_datasets_arcgis_item_by_id("not-a-hex"),
    regexp = "32-char hex GUID")
})

# ============================================== Toronto Zoning per Neighbourhood

test_that("morie_datasets_toronto_zoning_per_neighbourhood(offline=TRUE, layer='neighbourhoods') reads the 39-col fixture", {
  df <- morie_datasets_toronto_zoning_per_neighbourhood(offline = TRUE)
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 39L)
  expect_equal(nrow(df), 5L)
  for (col in c("Neighbourhood", "Total_Population", "Pop_Males",
                "Pop_Females", "Pop0_4years", "Pop85yearsandover",
                "Seniors55andover", "Seniors65andover",
                "Child0_14", "Youth15_24",
                "HomeLanguageCategory",
                "Language_Chinese", "Language_Italian",
                "Language_Korean", "Language_Persian",
                "Language_Portuguese", "Language_Russian",
                "Language_Spanish", "Language_Tagalog",
                "Language_Tamil", "Language_Urdu"))
    expect_true(col %in% names(df))
  expect_true(all(grepl("^SYNTH ", df$Neighbourhood)))
})

test_that("morie_datasets_toronto_zoning_per_neighbourhood(offline=TRUE, layer='zoning_stats') reads the 4-col fixture", {
  df <- morie_datasets_toronto_zoning_per_neighbourhood(
    offline = TRUE, layer = "zoning_stats")
  expect_s3_class(df, "data.frame")
  expect_equal(ncol(df), 4L)
  expect_setequal(names(df),
                  c("OBJECTID", "ZoneDesc",
                    "Neighbourhood_Name", "SUM_Area"))
  expect_true("Commercial" %in% df$ZoneDesc)
})

test_that("morie_datasets_toronto_zoning_per_neighbourhood(offline=TRUE) honours max_features", {
  df <- morie_datasets_toronto_zoning_per_neighbourhood(
    offline = TRUE, max_features = 2L)
  expect_equal(nrow(df), 2L)
})

test_that("morie_datasets_toronto_zoning_per_neighbourhood rejects unknown layer", {
  expect_error(
    morie_datasets_toronto_zoning_per_neighbourhood(layer = "ghost"))
})

test_that("morie_datasets_toronto_zoning_per_neighbourhood(offline=FALSE) routes through morie_datasets_arcgis_item_by_id with the canonical item_id + layer_idx", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_arcgis_item_by_id = function(item_id,
                                                  format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  dest = NULL) {
      seen <<- list(item_id = item_id, format = format,
                    layer_idx = layer_idx, where = where,
                    max_features = max_features)
      data.frame(stub = 1L)
    },
    .package = "morie")
  morie_datasets_toronto_zoning_per_neighbourhood(
    offline = FALSE, layer = "neighbourhoods",
    format = "json", max_features = 10L)
  expect_equal(seen$item_id, "af06159170914808983959df6163fc86")
  expect_equal(seen$layer_idx, 0L)
  expect_equal(seen$format, "json")
  expect_equal(seen$max_features, 10L)
})

test_that("morie_datasets_toronto_zoning_per_neighbourhood(offline=FALSE, layer='zoning_stats') routes to layer_idx=1", {
  seen <- list()
  testthat::local_mocked_bindings(
    morie_datasets_arcgis_item_by_id = function(item_id,
                                                  format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  dest = NULL) {
      seen <<- list(item_id = item_id, layer_idx = layer_idx)
      data.frame()
    },
    .package = "morie")
  morie_datasets_toronto_zoning_per_neighbourhood(
    offline = FALSE, layer = "zoning_stats")
  expect_equal(seen$layer_idx, 1L)
})

test_that("morie_datasets_toronto_zoning_per_neighbourhood defaults to offline = TRUE", {
  expect_s3_class(morie_datasets_toronto_zoning_per_neighbourhood(),
                  "data.frame")
})
