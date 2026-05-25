# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3CCC4: Cross-portal dataset catalog.
#
# Unifies the per-portal registries into a single canonical view
# so callers can ask "what datasets ship with morie?" across:
#
#   * NYC NYPD CJ datasets (Socrata, .MORIE_NYC_NYPD_REGISTRY)
#   * External Socrata datasets (Chicago + NYPD SQF,
#     morie_datasets_external_socrata_layers())
#   * TPS PSDP layers (ArcGIS, morie_tps_psdp_layers())
#   * TPS ArcGIS Hub catalog (71 items,
#     morie_datasets_tps_arcgis_hub_layers())
#   * Ontario CKAN (OTIS family, morie_datasets_ontario_ckan_layers())
#   * NYC OpenData boundary loaders (7 fixtures incl. borough +
#     precinct, morie_datasets_nyc_boundaries_catalog())
#   * Vancouver Open Data civic catalog (Opendatasoft v2.1, 190
#     datasets, morie_datasets_vancouver_opendata_layers())
#
# Each entry surfaces a uniform schema:
#
#   dataset_key | source        | id          | api_modes
#               | loader        | dict_url    | n_rows_bundled

#' Unified cross-portal catalog of every morie-shippable dataset
#'
#' Phase 3CCC4. Single-view aggregator over morie's per-portal
#' registries. Returns one row per (portal, dataset) pair with a
#' uniform schema so callers can answer questions like "which TPS
#' datasets does morie ship?" or "what fixtures are bundled for
#' Chicago?" without knowing each portal's registry shape.
#'
#' API mode column legend (`api_modes`):
#' \itemize{
#'   \item \strong{soda2}        -- Socrata SODA2 `/resource/<id>.json` (open).
#'   \item \strong{soda2_csv}    -- Socrata SODA2 `/resource/<id>.csv` (open).
#'   \item \strong{soda2_geojson}-- Socrata SODA2 `/resource/<id>.geojson` (open).
#'   \item \strong{soda3}        -- Socrata SODA3
#'     `/api/v3/views/<id>/query.json` or `.csv` (requires
#'     authentication; see Socrata support ref below).
#'   \item \strong{odata}        -- Socrata OData v4
#'     `/api/odata/v4/<id>` (open, but `$filter` is broken on many
#'     Socrata datasets due to Edm typing -- prefer soda3 for filters).
#'   \item \strong{arcgis_rest}  -- ArcGIS FeatureServer REST query
#'     endpoint (open).
#'   \item \strong{arcgis_hub}   -- TPS Hub OGC-API-Features (open).
#'   \item \strong{ckan}         -- CKAN datastore (open).
#'   \item \strong{opendatasoft_v21} -- Opendatasoft Explore API v2.1
#'     `/api/explore/v2.1/catalog/datasets/<id>/records` (open).
#'   \item \strong{manual_download} -- Data is gated behind manual
#'     T&C acceptance via a publisher web UI; no automation API.
#'     Caller downloads manually and passes the local path to the
#'     morie loader. Used by VPD GeoDASH.
#'   \item \strong{statcan_wds} -- Statistics Canada Web Data Service
#'     REST API (open; POST endpoints for metadata + vector data).
#' }
#'
#' @note SODA3 (`/api/v3/views/<id>/query.{json,csv}`) requires
#'   authentication per Socrata's API documentation
#'   (\url{https://support.socrata.com/hc/en-us/articles/34730618169623-SODA3-API}).
#'   morie's `mode = "soda3"` accepts an `app_token` argument on
#'   every wrapper that supports it.
#'
#' @param portal Optional character filter: `"chicago"`, `"nyc_nypd"`,
#'   `"nyc_opendata"`, `"tps_arcgis_hub"`, `"tps_psdp"`,
#'   `"ontario_ckan"`, `"vancouver_opendata"`, `"vpd_geodash"`,
#'   `"statcan_ccjs"`, `"montreal_opendata"`, `"toronto_opendata"`,
#'   `"calgary_opendata"`, `"edmonton_opendata"`, `"ottawa_opendata"`.
#'   `NULL` (default) returns all portals.
#' @return A `data.frame` with one row per dataset. Columns:
#'   `dataset_key`, `source`, `id`, `api_modes`, `loader`,
#'   `dict_url`, `n_rows_bundled`.
#' @examples
#' cat_df <- morie_dataset_portal_catalog()
#' nrow(cat_df)
#' table(cat_df$source)
#'
#' tps <- morie_dataset_portal_catalog(portal = "tps_arcgis_hub")
#' head(tps$dataset_key)
# Session-scoped cache for the full ~9k-row catalog. Rebuilding scans
# 14 portal registries (~2.8s on a modern laptop); memoizing keeps
# subsequent morie_datasets_load_by_key() lookups instant. Cleared
# via morie_dataset_portal_catalog_clear_cache().
#' @noRd
.morie_portal_catalog_env <- new.env(parent = emptyenv())

#' Clear the session-scoped portal-catalog cache
#'
#' Forces the next [morie_dataset_portal_catalog()] call to rebuild
#' from the per-portal registries. Useful after editing or extending
#' a registry in an interactive session.
#'
#' @return Invisibly `NULL`.
#' @export
morie_dataset_portal_catalog_clear_cache <- function() {
  rm(list = ls(.morie_portal_catalog_env),
     envir = .morie_portal_catalog_env)
  invisible(NULL)
}

#' Build the cross-portal dataset catalog
#'
#' Aggregates every per-portal registry (Chicago, NYC NYPD, NYC
#' OpenData, TPS ArcGIS Hub, TPS PSDP, Ontario CKAN, Vancouver,
#' VPD GeoDASH, Statistics Canada CCJS, Montreal, Toronto, Calgary,
#' Edmonton, Ottawa) into a single tidy `data.frame` for cross-portal
#' discovery and tooling. Caches the result in a session-local
#' environment so repeated calls are O(1); call
#' [morie_dataset_portal_catalog_clear_cache()] to force a rebuild
#' after editing a registry in an interactive session.
#'
#' @param portal Optional character filter restricting output to a
#'   single portal. `NULL` (default) returns every dataset across all
#'   registries.
#' @return A `data.frame` with columns `dataset_key`, `source`,
#'   `id`, `api_modes`, `loader`, `dict_url`, `n_rows_bundled`.
#' @examples
#' cat_df <- morie_dataset_portal_catalog()
#' nrow(cat_df)
#' table(cat_df$source)
#' @seealso [morie_dataset_portal_catalog_clear_cache()],
#'   [morie_datasets_load_by_key()], [morie_datasets_browse()]
#' @export
morie_dataset_portal_catalog <- function(portal = NULL) {
  cached <- .morie_portal_catalog_env$full
  if (!is.null(cached)) {
    out <- cached
    if (!is.null(portal)) {
      portal <- match.arg(portal,
                          choices = c("chicago", "nyc_nypd", "nyc_opendata",
                                      "tps_arcgis_hub", "tps_psdp",
                                      "ontario_ckan", "vancouver_opendata",
                                      "vpd_geodash", "statcan_ccjs",
                                      "montreal_opendata", "toronto_opendata",
                                      "calgary_opendata", "edmonton_opendata",
                                      "ottawa_opendata"))
      out <- out[out$source == portal, , drop = FALSE]
      rownames(out) <- NULL
    }
    return(out)
  }
  rows <- list()
  push <- function(r) rows[[length(rows) + 1L]] <<- r

  socrata_modes <- "soda2,soda2_csv,soda2_geojson,soda3,odata"

  # --- NYC NYPD CJ (9 datasets) ---------------------------------
  reg <- .MORIE_NYC_NYPD_REGISTRY
  for (k in names(reg)) {
    e <- reg[[k]]
    push(data.frame(
      dataset_key = k,
      source = "nyc_nypd",
      id = e$resource_id,
      api_modes = socrata_modes,
      loader = sprintf("morie_datasets_%s", k),
      dict_url = if (!is.null(e$data_dictionary_url) &&
                       !is.na(e$data_dictionary_url))
                    e$data_dictionary_url else NA_character_,
      n_rows_bundled = .morie_portal_fixture_rows(e$fixture),
      stringsAsFactors = FALSE))
  }

  # --- NYC OpenData BULK (3GGG1, 2851 entities) ----------------
  nyc_bulk <- morie_datasets_nyc_opendata_bulk_layers(offline = TRUE)
  for (i in seq_len(nrow(nyc_bulk))) {
    push(data.frame(
      dataset_key = nyc_bulk$soda_id[i],
      source = "nyc_opendata",
      id = nyc_bulk$soda_id[i],
      api_modes = "soda2,soda2_csv,soda2_geojson,soda3,odata",
      loader = "morie_datasets_nyc_socrata_by_id",
      dict_url = sprintf("https://data.cityofnewyork.us/d/%s",
                          nyc_bulk$soda_id[i]),
      n_rows_bundled = NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- Chicago Open Data BULK (3GGG2, 1856 entities) -----------
  chi_bulk <- morie_datasets_chicago_opendata_bulk_layers(offline = TRUE)
  for (i in seq_len(nrow(chi_bulk))) {
    push(data.frame(
      dataset_key = chi_bulk$soda_id[i],
      source = "chicago",
      id = chi_bulk$soda_id[i],
      api_modes = "soda2,soda2_csv,soda2_geojson,soda3,odata",
      loader = "morie_datasets_chicago_socrata_by_id",
      dict_url = sprintf("https://data.cityofchicago.org/d/%s",
                          chi_bulk$soda_id[i]),
      n_rows_bundled = NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- External Socrata (Chicago + NYPD SQF) --------------------
  ext <- morie_datasets_external_socrata_layers()
  for (i in seq_len(nrow(ext))) {
    rid <- sub(".*resource/([^.]+)\\.json.*", "\\1",
                ext$resource_url[i])
    src <- if (grepl("cityofchicago", ext$portal[i])) "chicago"
           else if (grepl("cityofnewyork", ext$portal[i])) "nyc_opendata"
           else ext$portal[i]
    push(data.frame(
      dataset_key = ext$dataset_key[i],
      source = src,
      id = rid,
      api_modes = socrata_modes,
      loader = sprintf("morie_datasets_%s", ext$dataset_key[i]),
      dict_url = NA_character_,
      n_rows_bundled = .morie_portal_fixture_rows(ext$fixture[i]),
      stringsAsFactors = FALSE))
  }

  # --- TPS PSDP (11 layers) -------------------------------------
  psdp <- morie_tps_psdp_layers()
  for (i in seq_len(nrow(psdp))) {
    push(data.frame(
      dataset_key = psdp$layer_key[i],
      source = "tps_psdp",
      id = psdp$hub_id[i],
      api_modes = "arcgis_rest,arcgis_hub",
      loader = sprintf("morie_datasets_tps_%s", psdp$layer_key[i]),
      dict_url = NA_character_,
      n_rows_bundled = .morie_portal_fixture_rows(psdp$fixture[i]),
      stringsAsFactors = FALSE))
  }

  # --- TPS ArcGIS Hub (71 catalog items, by hub_id) -------------
  hub <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
  for (i in seq_len(nrow(hub))) {
    push(data.frame(
      dataset_key = sprintf("hub:%s", hub$hub_id[i]),
      source = "tps_arcgis_hub",
      id = hub$hub_id[i],
      api_modes = "arcgis_rest,arcgis_hub",
      loader = "morie_datasets_tps_arcgis_hub_by_id",
      dict_url = NA_character_,
      n_rows_bundled = NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- Ontario CKAN (OTIS family) -------------------------------
  ock <- morie_datasets_ontario_ckan_layers()
  for (i in seq_len(nrow(ock))) {
    push(data.frame(
      dataset_key = ock$dataset_key[i],
      source = "ontario_ckan",
      id = ock$resource_id[i],
      api_modes = "ckan",
      loader = "morie_datasets_ontario_ckan_by_key",
      dict_url = NA_character_,
      n_rows_bundled = .morie_portal_fixture_rows(ock$fixture[i]),
      stringsAsFactors = FALSE))
  }

  # --- NYC OpenData boundary loaders (7 fixtures from 3CCC2) ---
  bnd <- morie_datasets_nyc_boundaries_catalog()
  for (i in seq_len(nrow(bnd))) {
    push(data.frame(
      dataset_key = bnd$boundary[i],
      source = "nyc_opendata",
      id = bnd$soda_id[i],
      api_modes = socrata_modes,
      loader = bnd$loader[i],
      dict_url = NA_character_,
      n_rows_bundled = bnd$n_rows[i],
      stringsAsFactors = FALSE))
  }

  # --- Vancouver Open Data BULK (3HHH2, 190 datasets) ---------
  van_bulk <- morie_datasets_vancouver_opendata_bulk_layers(offline = TRUE)
  for (i in seq_len(nrow(van_bulk))) {
    lk <- van_bulk$dataset_id[i]
    push(data.frame(
      dataset_key = lk, source = "vancouver_opendata", id = lk,
      api_modes = "opendatasoft_v21",
      loader = "morie_datasets_vancouver_opendata_by_id",
      dict_url = sprintf("https://opendata.vancouver.ca/explore/dataset/%s",
                          lk),
      n_rows_bundled = NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- Calgary Open Data BULK (3GGG4, 933 entities) -----------
  cal_bulk <- morie_datasets_calgary_opendata_bulk_layers(offline = TRUE)
  cal_n_map <- c("78gh-n26t" = 200L, "bdez-pds9" = 200L,
                  "cqsb-2hhg" = 43L)
  cal_loader_map <- c("78gh-n26t" = "morie_datasets_calgary_community_crime_stats",
                       "bdez-pds9" = "morie_datasets_calgary_fire_response_calls",
                       "cqsb-2hhg" = "morie_datasets_calgary_fire_stations")
  for (i in seq_len(nrow(cal_bulk))) {
    lk <- cal_bulk$soda_id[i]
    push(data.frame(
      dataset_key = lk, source = "calgary_opendata", id = lk,
      api_modes = "soda2,soda2_csv,soda2_geojson,soda3,odata",
      loader = unname(if (lk %in% names(cal_loader_map))
                         cal_loader_map[[lk]]
                       else "morie_datasets_calgary_socrata_by_id"),
      dict_url = sprintf("https://data.calgary.ca/d/%s", lk),
      n_rows_bundled = unname(if (lk %in% names(cal_n_map))
                                 cal_n_map[[lk]] else NA_integer_),
      stringsAsFactors = FALSE))
  }

  # --- Edmonton Open Data BULK (3GGG, 2027 entities) ----------
  edm_bulk <- morie_datasets_edmonton_opendata_bulk_layers(offline = TRUE)
  edm_n_map <- c("e7aq-scxv" = 10L, "b4y7-zhnz" = 31L)
  edm_loader_map <- c("e7aq-scxv" = "morie_datasets_edmonton_police_stations",
                       "b4y7-zhnz" = "morie_datasets_edmonton_fire_stations")
  for (i in seq_len(nrow(edm_bulk))) {
    lk <- edm_bulk$soda_id[i]
    push(data.frame(
      dataset_key = lk, source = "edmonton_opendata", id = lk,
      api_modes = "soda2,soda2_csv,soda2_geojson,soda3,odata",
      loader = unname(if (lk %in% names(edm_loader_map))
                         edm_loader_map[[lk]]
                       else "morie_datasets_edmonton_socrata_by_id"),
      dict_url = sprintf("https://data.edmonton.ca/d/%s", lk),
      n_rows_bundled = unname(if (lk %in% names(edm_n_map))
                                 edm_n_map[[lk]] else NA_integer_),
      stringsAsFactors = FALSE))
  }

  # --- Ottawa Open Data BULK (3GGG5, 287 datasets) ------------
  ott_bulk <- morie_datasets_ottawa_opendata_bulk_layers(offline = TRUE)
  for (i in seq_len(nrow(ott_bulk))) {
    push(data.frame(
      dataset_key = ott_bulk$hub_id[i],
      source = "ottawa_opendata", id = ott_bulk$hub_id[i],
      api_modes = "arcgis_rest,arcgis_hub",
      loader = "morie_datasets_tps_arcgis_hub_by_id",
      dict_url = sprintf("https://open.ottawa.ca/datasets/%s",
                          ott_bulk$hub_id[i]),
      n_rows_bundled = NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- Toronto Open Data BULK (3GGG3, 540 packages) -----------
  tor_bulk <- morie_datasets_toronto_opendata_bulk_layers(offline = TRUE)
  tor_loader_map <- c(
    "ambulance-station-locations" = "morie_datasets_toronto_ambulance_stations",
    "police-annual-statistical-report-miscellaneous-data" = "morie_datasets_toronto_asr_miscellaneous")
  tor_n_map <- c(
    "ambulance-station-locations" = 46L,
    "police-annual-statistical-report-miscellaneous-data" = 40L)
  for (i in seq_len(nrow(tor_bulk))) {
    lk <- tor_bulk$package_name[i]
    push(data.frame(
      dataset_key = lk, source = "toronto_opendata", id = lk,
      api_modes = "ckan",
      loader = unname(if (lk %in% names(tor_loader_map))
                         tor_loader_map[[lk]]
                       else "morie_datasets_toronto_open_ckan_resource"),
      dict_url = sprintf("https://open.toronto.ca/dataset/%s", lk),
      n_rows_bundled = unname(if (lk %in% names(tor_n_map))
                                 tor_n_map[[lk]] else NA_integer_),
      stringsAsFactors = FALSE))
  }

  # --- Montreal Open Data BULK (3HHH1, 401 packages) ----------
  mtl_bulk <- morie_datasets_montreal_opendata_bulk_layers(offline = TRUE)
  for (i in seq_len(nrow(mtl_bulk))) {
    lk <- mtl_bulk$package_name[i]
    push(data.frame(
      dataset_key = lk, source = "montreal_opendata", id = lk,
      api_modes = "ckan",
      loader = if (lk == "interventions-service-securite-incendie-montreal")
                "morie_datasets_montreal_sim_interventions"
              else "morie_datasets_montreal_ckan_resource",
      dict_url = sprintf("https://donnees.montreal.ca/dataset/%s", lk),
      n_rows_bundled = if (lk == "interventions-service-securite-incendie-montreal")
                         349L else NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- StatCan CCJS WDS REST cubes (10 curated) ----------------
  sc <- morie_datasets_statcan_ccjs_cubes()
  for (i in seq_len(nrow(sc))) {
    push(data.frame(
      dataset_key = sprintf("statcan_%d", sc$product_id[i]),
      source = "statcan_ccjs",
      id = as.character(sc$product_id[i]),
      api_modes = "statcan_wds",
      loader = "morie_datasets_statcan_cube_metadata",
      dict_url = sprintf("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=%d",
                          sc$product_id[i]),
      n_rows_bundled = NA_integer_,
      stringsAsFactors = FALSE))
  }

  # --- VPD GeoDASH (manual download, sample bundled) -----------
  push(data.frame(
    dataset_key = "vpd_crime",
    source = "vpd_geodash",
    id = "crimedata_csv_AllNeighbourhoods_AllYears",
    api_modes = "manual_download",
    loader = "morie_datasets_vpd_crime",
    dict_url = "https://geodash.vpd.ca/opendata/",
    n_rows_bundled = .morie_portal_fixture_rows("vpd_crime_sample.csv"),
    stringsAsFactors = FALSE))

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Memoize before subsetting so subsequent calls (with or without
  # a `portal` filter) all benefit.
  .morie_portal_catalog_env$full <- out

  if (!is.null(portal)) {
    portal <- match.arg(portal,
                         choices = c("chicago", "nyc_nypd",
                                      "nyc_opendata",
                                      "tps_arcgis_hub", "tps_psdp",
                                      "ontario_ckan",
                                      "vancouver_opendata",
                                      "vpd_geodash",
                                      "statcan_ccjs",
                                      "montreal_opendata",
                                      "toronto_opendata",
                                      "calgary_opendata",
                                      "edmonton_opendata",
                                      "ottawa_opendata"))
    out <- out[out$source == portal, , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.morie_portal_fixture_rows <- function(fname) {
  if (is.null(fname) || is.na(fname) || !nzchar(fname))
    return(NA_integer_)
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path)) return(NA_integer_)
  # Lightweight row count via tally of newlines minus the header.
  n_lines <- length(readLines(path, warn = FALSE))
  max(0L, n_lines - 1L)
}
