# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3CCC4: Cross-portal dataset catalog.

test_that("morie_dataset_portal_catalog() returns unified schema", {
  d <- morie_dataset_portal_catalog()
  expect_s3_class(d, "data.frame")
  expect_setequal(names(d),
                  c("dataset_key", "source", "id", "api_modes",
                    "loader", "dict_url", "n_rows_bundled"))
  expect_true(nrow(d) >= 300L)
  # Required cols are non-NA on every row.
  expect_true(all(!is.na(d$dataset_key) & nzchar(d$dataset_key)))
  expect_true(all(!is.na(d$source) & nzchar(d$source)))
  expect_true(all(!is.na(d$id) & nzchar(d$id)))
  expect_true(all(!is.na(d$api_modes) & nzchar(d$api_modes)))
  expect_true(all(!is.na(d$loader) & nzchar(d$loader)))
})

test_that("catalog covers all 14 expected portals", {
  d <- morie_dataset_portal_catalog()
  expect_setequal(unique(d$source),
                  c("chicago", "nyc_nypd", "nyc_opendata",
                    "tps_arcgis_hub", "tps_psdp",
                    "ontario_ckan", "vancouver_opendata",
                    "vpd_geodash", "statcan_ccjs",
                    "montreal_opendata", "toronto_opendata",
                    "calgary_opendata", "edmonton_opendata",
                    "ottawa_opendata"))
})

test_that("per-source row counts match expected (post-3GGG bulk)", {
  d <- morie_dataset_portal_catalog()
  src_n <- table(d$source)
  expect_equal(as.integer(src_n["nyc_nypd"]), 8L)
  expect_equal(as.integer(src_n["tps_psdp"]), 11L)
  expect_equal(as.integer(src_n["tps_arcgis_hub"]), 71L)
  expect_equal(as.integer(src_n["vancouver_opendata"]), 190L)
  expect_true(as.integer(src_n["ontario_ckan"]) >= 30L)
  # Post-3GGG bulk catalogs (these grew substantially):
  expect_true(as.integer(src_n["nyc_opendata"]) >= 2000L)
  expect_true(as.integer(src_n["chicago"]) >= 1500L)
  expect_true(as.integer(src_n["toronto_opendata"]) >= 500L)
  expect_true(as.integer(src_n["calgary_opendata"]) >= 900L)
  expect_true(as.integer(src_n["edmonton_opendata"]) >= 2000L)
  expect_true(as.integer(src_n["ottawa_opendata"]) >= 280L)
  # 3HHH1: MTL bulk grew from 23 (justice subset) to 401 (full).
  expect_true(as.integer(src_n["montreal_opendata"]) >= 400L)
})

test_that("api_modes column reflects portal protocol", {
  d <- morie_dataset_portal_catalog()
  # Socrata endpoints (NYC NYPD + Chicago + NYC OpenData boundaries)
  # all carry soda2 + soda3 + odata.
  socrata <- d[d$source %in% c("nyc_nypd", "chicago",
                                 "nyc_opendata"), ]
  expect_true(all(grepl("soda2", socrata$api_modes)))
  expect_true(all(grepl("soda3", socrata$api_modes)))
  expect_true(all(grepl("odata", socrata$api_modes)))
  # TPS endpoints carry arcgis_rest + arcgis_hub.
  tps <- d[d$source %in% c("tps_psdp", "tps_arcgis_hub"), ]
  expect_true(all(grepl("arcgis", tps$api_modes)))
  # Ontario CKAN + Montreal CKAN + Toronto CKAN.
  expect_true(all(d$api_modes[d$source == "ontario_ckan"] == "ckan"))
  expect_true(all(d$api_modes[d$source == "montreal_opendata"] == "ckan"))
  expect_true(all(d$api_modes[d$source == "toronto_opendata"] == "ckan"))
  # Calgary + Edmonton are Socrata.
  expect_true(all(grepl("soda2", d$api_modes[d$source == "calgary_opendata"])))
  expect_true(all(grepl("soda2", d$api_modes[d$source == "edmonton_opendata"])))
  # Ottawa is ArcGIS Hub.
  expect_true(all(grepl("arcgis", d$api_modes[d$source == "ottawa_opendata"])))
  # Vancouver = Opendatasoft v2.1.
  expect_true(all(d$api_modes[d$source == "vancouver_opendata"] ==
                     "opendatasoft_v21"))
  # VPD GeoDASH = manual download.
  expect_true(all(d$api_modes[d$source == "vpd_geodash"] ==
                     "manual_download"))
  # StatCan = statcan_wds.
  expect_true(all(d$api_modes[d$source == "statcan_ccjs"] ==
                     "statcan_wds"))
})

test_that("portal= filter returns subset", {
  d_all <- morie_dataset_portal_catalog()
  for (p in c("chicago", "nyc_nypd", "nyc_opendata",
              "tps_arcgis_hub", "tps_psdp", "ontario_ckan",
              "vancouver_opendata", "vpd_geodash",
              "montreal_opendata", "toronto_opendata",
              "calgary_opendata", "edmonton_opendata",
              "ottawa_opendata")) {
    filt <- morie_dataset_portal_catalog(portal = p)
    expect_true(all(filt$source == p),
                 info = sprintf("portal=%s", p))
    expect_equal(nrow(filt),
                  sum(d_all$source == p),
                  info = sprintf("portal=%s row mismatch", p))
  }
})

test_that("invalid portal raises error", {
  expect_error(morie_dataset_portal_catalog(portal = "atlantis"))
})

test_that("at least one NYPD dataset surfaces its dict_url", {
  d <- morie_dataset_portal_catalog(portal = "nyc_nypd")
  ytd <- d[d$dataset_key == "nypd_arrests_ytd", ]
  expect_equal(nrow(ytd), 1L)
  expect_true(!is.na(ytd$dict_url))
  expect_true(grepl("DataDictionary", ytd$dict_url))
})

test_that("morie_datasets_vancouver_opendata_layers reads 190-row catalog", {
  v <- morie_datasets_vancouver_opendata_layers(offline = TRUE)
  expect_s3_class(v, "data.frame")
  expect_equal(nrow(v), 190L)
  expect_setequal(names(v),
                  c("dataset_id", "title", "publisher",
                    "records_count"))
  expect_type(v$dataset_id, "character")
})

test_that("morie_datasets_vancouver_opendata_by_id signature accepts both formats", {
  fa <- formals(morie_datasets_vancouver_opendata_by_id)
  expect_true("dataset_id" %in% names(fa))
  expect_equal(eval(fa$format), c("json", "csv"))
})
