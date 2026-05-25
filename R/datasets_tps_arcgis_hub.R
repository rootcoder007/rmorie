# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Toronto Police Service ArcGIS Hub catalog (data.tps.ca).
#
# The TPS Hub publishes 71 canonical datasets (verified live via
# `https://data.tps.ca/api/search/v1/collections/dataset/items?limit=100`
# returning `numberMatched: 71`, all owned by TorontoPoliceService).
# Every dataset is exposed as an ArcGIS "Feature Service" reachable
# at a stable FeatureServer URL on `services.arcgis.com/S9th0jAJ7bqgIRjw`.
#
# Each Hub item resolves through three identifiers:
#
#   * `hub_id`             -- 32-char hex GUID used in
#                              data.tps.ca/maps/<hub_id> URLs and the
#                              ArcGIS Online items API
#                              (`/sharing/rest/content/items/<id>?f=json`).
#
#   * `feature_server_url` -- the underlying ArcGIS Feature Service
#                              endpoint
#                              (`services.arcgis.com/.../FeatureServer`).
#                              `/0/query?where=1=1&outFields=*&f=json` for
#                              tabular data; `?f=geojson` for GeoJSON.
#
#   * `Hub download URL`   -- on-the-fly CSV / GeoJSON / Shapefile /
#                              File Geodatabase exports at
#                              `https://hub.arcgis.com/api/v3/datasets/<hub_id>_0/downloads/data?format=<fmt>`.
#
# 3SS bundles the full 71-row catalog as a CSV fixture so discovery
# works offline. By-id loading dispatches to FeatureServer (JSON /
# GeoJSON) or the Hub downloads API (CSV / Shapefile / FGDB) based on
# `format`.

# ---------------------------------------------------------------------------
# Catalog discovery
# ---------------------------------------------------------------------------

#' List the Toronto Police Service ArcGIS Hub datasets wrapped by morie
#'
#' Sibling discovery helper to [morie_datasets_external_socrata_layers()]
#' and [morie_datasets_ontario_ckan_layers()], covering all 71 datasets
#' currently published on `data.tps.ca`.
#'
#' Verified live against the canonical TPS Hub catalog API
#' (`https://data.tps.ca/api/search/v1/collections/dataset/items?limit=100`)
#' on 2026-05-24 -- returned `numberMatched: 71`, all owned by
#' `TorontoPoliceService`.
#'
#' Coverage spans nine families:
#'
#' \describe{
#'   \item{MCI Open Data}{Assault, Auto Theft, Bicycle Thefts,
#'     Break and Enter, Hate Crimes, Homicides, Intimate Partner +
#'     Family Violence, Robbery, Shooting + Firearm Discharges,
#'     Theft From Motor Vehicle, Theft Over.}
#'   \item{Use of Force (RBDC-UOF)}{Six per-axis breakdown tables
#'     plus the Types-and-Perceived-Weapons crosstab.}
#'   \item{Annual Statistical Report (ASR)}{Administrative,
#'     Calls for Service, Enforcement, Firearms, Misc., Personnel,
#'     Public Complaints, Reported Crimes, Regulated Interactions,
#'     Search of Persons, Traffic, Victims of Crime.}
#'   \item{Killed and Seriously Injured (KSI)}{Main + per-mode
#'     (Automobile / Cyclist / Fatals / Motorcyclist / Passenger /
#'     Pedestrian).}
#'   \item{Budget}{Annual figures 2020 through 2026 plus
#'     `Budget_by_Command`.}
#'   \item{Personnel + Staffing}{ASR-PB family + `Staffing_by_Command`.}
#'   \item{Historical FIRS}{Annual files 2008 through 2013.}
#'   \item{Geographic boundaries}{Facilities, Patrol Zone,
#'     Police Divisions.}
#'   \item{Other}{Community Safety Indicators, Mental Health Act
#'     Apprehensions, Neighbourhood Crime Rates, Persons in Crisis
#'     Calls for Service Attended.}
#' }
#'
#' @param offline If `TRUE` (default), read the bundled 71-row
#'   catalog fixture (`inst/extdata/tps_arcgis_hub_catalog.csv`).
#'   Live mode hits the TPS Hub search API.
#' @return A `data.frame` with columns `hub_id`, `title`, `type`,
#'   `feature_server_url`, `owner`, `tags`, `snippet`.
#' @references TPS Public Safety Data Portal,
#'   \url{https://data.tps.ca/search?collection=dataset}.
#' @examples
#' cat <- morie_datasets_tps_arcgis_hub_layers()
#' nrow(cat)        # 71
#' head(cat$title)
#' @export
morie_datasets_tps_arcgis_hub_layers <- function(offline = TRUE) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "tps_arcgis_hub_catalog.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled TPS ArcGIS Hub catalog fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    return(df)
  }
  # Live mode -- hit the TPS Hub OGC-API-Features search endpoint.
  body <- .morie_dataset_http_json(
    "https://data.tps.ca/api/search/v1/collections/dataset/items",
    query = list(limit = 100L))
  feats <- body$features
  if (is.null(feats) || length(feats) == 0L) return(data.frame())
  rows <- lapply(feats, function(f) {
    p <- f$properties
    data.frame(
      hub_id = f$id,
      title  = p$title %||% "",
      type   = p$type  %||% "Feature Service",
      feature_server_url = p$url %||% "",
      owner  = p$owner %||% "TorontoPoliceService",
      tags   = paste(unlist(p$tags) %||% character(), collapse = "; "),
      snippet = substr(p$snippet %||% "", 1L, 300L),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[order(out$title), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Hub-id -> FeatureServer URL resolver
# ---------------------------------------------------------------------------

#' Resolve a TPS Hub item id to its underlying ArcGIS FeatureServer URL.
#' Offline mode looks up the cached catalog; live mode hits the ArcGIS
#' Online items API (`/sharing/rest/content/items/<id>?f=json`).
#' @keywords internal
#' @noRd
.morie_dataset_tps_hub_resolve <- function(hub_id, offline = TRUE) {
  if (!grepl("^[a-f0-9]{32}$", hub_id)) {
    stop(sprintf(paste0("morie TPS hub_id must be a 32-char hex GUID; ",
                         "got '%s'"), hub_id), call. = FALSE)
  }
  if (isTRUE(offline)) {
    cat <- morie_datasets_tps_arcgis_hub_layers(offline = TRUE)
    hit <- cat[cat$hub_id == hub_id, , drop = FALSE]
    if (nrow(hit) == 0L) {
      stop(sprintf(paste0(
        "morie TPS hub_id '%s' not in the bundled catalog ",
        "(71 datasets verified 2026-05-24). Pass offline = FALSE ",
        "to resolve via the live ArcGIS Online items API."),
        hub_id), call. = FALSE)
    }
    return(hit$feature_server_url[[1L]])
  }
  body <- .morie_dataset_http_json(
    sprintf("https://www.arcgis.com/sharing/rest/content/items/%s",
            hub_id),
    query = list(f = "json"))
  if (is.null(body$url) || !nzchar(body$url)) {
    stop(sprintf(paste0(
      "morie TPS hub_id '%s' did not return a FeatureServer url ",
      "from the ArcGIS Online items API."), hub_id), call. = FALSE)
  }
  body$url
}

# ---------------------------------------------------------------------------
# Generic by-id loader (multi-format aware)
# ---------------------------------------------------------------------------

.MORIE_TPS_HUB_FORMATS <- c("json", "geojson", "csv",
                             "shapefile", "fgdb")

#' Generic TPS ArcGIS Hub dataset loader by hub_id
#'
#' Single entry point for the 71 datasets listed by
#' [morie_datasets_tps_arcgis_hub_layers()]. Supports five export
#' formats:
#'
#' \describe{
#'   \item{`"json"` (default)}{Hits the FeatureServer
#'     `/0/query?where=...&outFields=*&f=json` endpoint and parses
#'     attributes into a tidy `data.frame`. Same path the existing
#'     TPS PSDP loaders (3FF) use; honours an arbitrary `where`
#'     clause and `max_features` cap.}
#'   \item{`"geojson"`}{Hits `?f=geojson` and returns the raw GeoJSON
#'     as a parsed R list. Caller can pass to `sf::st_read()`.}
#'   \item{`"csv"`}{Hits the Hub CSV exporter at
#'     `hub.arcgis.com/api/v3/datasets/<hub_id>_0/downloads/data?format=csv`
#'     and parses the returned CSV into a `data.frame`.}
#'   \item{`"shapefile"` / `"fgdb"`}{Downloads the binary archive
#'     (Esri Shapefile zip / File Geodatabase zip) to `dest`
#'     (default `tempfile()`) and returns the file path. Caller can
#'     `sf::st_read()` the unzipped contents.}
#' }
#'
#' For boundary layers (Police Divisions, Patrol Zone, Facilities)
#' you'll typically want `format = "geojson"` to get the polygon
#' geometry. For tabular outputs (UoF tables, KSI counts, ASR
#' tables, budgets) `format = "json"` is sufficient and lightest.
#'
#' @param hub_id 32-char hex GUID. See
#'   [morie_datasets_tps_arcgis_hub_layers()] for the canonical 71.
#' @param format One of `"json"` (default), `"geojson"`, `"csv"`,
#'   `"shapefile"`, `"fgdb"`.
#' @param where SoQL-style `WHERE` filter passed to FeatureServer
#'   `?where=` (default `"1=1"`).
#' @param max_features Optional cap on returned rows. Passed as
#'   `resultRecordCount` to FeatureServer; ignored for binary formats.
#' @param layer_idx Integer index of the FeatureServer layer to pull
#'   (default `0L`, the first layer).
#' @param offline Logical; if `TRUE`, the hub_id is resolved via the
#'   bundled catalog (no network needed for the resolution step).
#'   Default `TRUE` -- you can run this against the 71 catalog
#'   entries without network. Live data fetches always hit the
#'   network regardless of this argument; "offline" here only
#'   affects the hub_id -> FeatureServer URL lookup.
#' @param dest Optional path for binary downloads
#'   (`format %in% c("shapefile", "fgdb")`); defaults to `tempfile()`.
#' @return A `data.frame` (json / csv), a parsed GeoJSON list, or a
#'   file path (binary).
#' @export
morie_datasets_tps_arcgis_hub_by_id <- function(hub_id,
                                                  format = "json",
                                                  where = "1=1",
                                                  max_features = NULL,
                                                  layer_idx = 0L,
                                                  offline = TRUE,
                                                  dest = NULL) {
  format <- match.arg(format, choices = .MORIE_TPS_HUB_FORMATS)
  fs_url <- .morie_dataset_tps_hub_resolve(hub_id, offline = offline)
  if (format == "json") {
    layer_url <- sprintf("%s/%d/query", fs_url, as.integer(layer_idx))
    query <- list(where = where, outFields = "*", f = "json",
                  returnGeometry = "false")
    if (!is.null(max_features)) {
      query$resultRecordCount <- as.integer(max_features)
    }
    body <- .morie_dataset_http_json(layer_url, query = query)
    feats <- body$features
    if (is.null(feats) || length(feats) == 0L) return(data.frame())
    attrs <- lapply(feats, function(f) f$attributes)
    return(.morie_dataset_records_to_df(attrs))
  }
  if (format == "geojson") {
    layer_url <- sprintf("%s/%d/query", fs_url, as.integer(layer_idx))
    query <- list(where = where, outFields = "*", f = "geojson")
    if (!is.null(max_features)) {
      query$resultRecordCount <- as.integer(max_features)
    }
    return(.morie_dataset_http_json(layer_url, query = query))
  }
  if (format == "csv") {
    csv_url <- sprintf(paste0(
      "https://hub.arcgis.com/api/v3/datasets/%s_%d/",
      "downloads/data"),
      hub_id, as.integer(layer_idx))
    raw <- .morie_dataset_http_text(csv_url,
                                     query = list(format = "csv"))
    return(utils::read.csv(text = raw, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  # Binary formats: download to dest.
  fmt_token <- switch(format,
                      shapefile = "shp",
                      fgdb      = "fgdb")
  bin_url <- sprintf(paste0(
    "https://hub.arcgis.com/api/v3/datasets/%s_%d/",
    "downloads/data?format=%s"),
    hub_id, as.integer(layer_idx), fmt_token)
  if (is.null(dest)) {
    suffix <- switch(format, shapefile = ".shp.zip", fgdb = ".fgdb.zip")
    dest <- tempfile(fileext = suffix)
  }
  # 3XX: routes through .morie_dataset_http_bytes (libcurl-backed
  # via morie::http::get_bytes from 3VV, with httr2 fallback) so
  # the binary payload survives without NUL truncation across the
  # whole chain.
  bytes <- .morie_dataset_http_bytes(bin_url)
  writeBin(bytes, dest)
  dest
}

#' Direct multi-format downloader (binary or text) for a TPS Hub item
#'
#' Thin wrapper that just hits the Hub downloads endpoint without the
#' FeatureServer `/query` round-trip. Use this when you want the
#' canonical CSV / GeoJSON / Shapefile / FGDB exactly as the Hub UI
#' serves them (including any column-name and projection differences
#' that on-the-fly exports introduce vs the FeatureServer source).
#'
#' @param hub_id 32-char hex GUID.
#' @param format One of `"csv"`, `"geojson"`, `"shapefile"`, `"fgdb"`.
#' @param layer_idx Integer layer index (default `0L`).
#' @param dest Optional destination path; defaults to `tempfile()`.
#' @return Path to the downloaded file.
#' @export
morie_datasets_tps_arcgis_hub_download <- function(hub_id,
                                                     format = "csv",
                                                     layer_idx = 0L,
                                                     dest = NULL) {
  format <- match.arg(format,
                       choices = c("csv", "geojson",
                                    "shapefile", "fgdb"))
  fmt_token <- switch(format,
                      csv = "csv",
                      geojson = "geojson",
                      shapefile = "shp",
                      fgdb = "fgdb")
  suffix <- switch(format,
                   csv = ".csv",
                   geojson = ".geojson",
                   shapefile = ".shp.zip",
                   fgdb = ".fgdb.zip")
  url <- sprintf(paste0(
    "https://hub.arcgis.com/api/v3/datasets/%s_%d/",
    "downloads/data?format=%s"),
    hub_id, as.integer(layer_idx), fmt_token)
  if (is.null(dest)) dest <- tempfile(fileext = suffix)
  # 3XX: libcurl-backed + httr2 fallback.
  bytes <- .morie_dataset_http_bytes(url)
  writeBin(bytes, dest)
  dest
}

# ---------------------------------------------------------------------------
# Generic ArcGIS Online item resolution + by-id loading (portal-agnostic).
# 3SS+ generalisation: the resolve + fetch logic works for ANY ArcGIS
# Online item id, not just TPS Hub catalog entries. Useful when you
# already know the GUID and don't need (or want) to round-trip through
# a portal-specific catalog. Examples: EsriCanadaEducation's "Toronto
# Zoning per Neighbourhood" (af06159170914808983959df6163fc86), City
# of Toronto Open Data org items, any per-user Esri Online dataset.
# ---------------------------------------------------------------------------

#' Resolve any ArcGIS Online item id to its FeatureServer URL +
#' canonical metadata.
#'
#' Lightweight discovery helper -- one network call to the ArcGIS
#' Online items API (`/sharing/rest/content/items/<item_id>?f=json`),
#' returns a single-row data.frame with the same columns the TPS Hub
#' catalog (\code{\link{morie_datasets_tps_arcgis_hub_layers}})
#' returns: `hub_id`, `title`, `type`, `feature_server_url`, `owner`,
#' `tags`, `snippet`. Use this when the item is NOT in the bundled
#' TPS catalog (any non-TorontoPoliceService item).
#'
#' @param item_id 32-char hex GUID for an ArcGIS Online item.
#' @return A `data.frame` with one row.
#' @examples
#' # Vee's Toronto Zoning per Neighbourhood discovery
#' # m <- morie_datasets_arcgis_item_metadata(
#' #   "af06159170914808983959df6163fc86")
#' # m$title  #> "Toronto Zoning per Neighbourhood"
#' @export
morie_datasets_arcgis_item_metadata <- function(item_id) {
  if (!grepl("^[a-f0-9]{32}$", item_id)) {
    stop(sprintf(paste0("morie ArcGIS item_id must be a 32-char hex ",
                         "GUID; got '%s'"), item_id), call. = FALSE)
  }
  body <- .morie_dataset_http_json(
    sprintf("https://www.arcgis.com/sharing/rest/content/items/%s",
            item_id),
    query = list(f = "json"))
  data.frame(
    hub_id = item_id,
    title  = body$title %||% "",
    type   = body$type  %||% "",
    feature_server_url = body$url %||% "",
    owner  = body$owner %||% "",
    tags   = paste(unlist(body$tags) %||% character(), collapse = "; "),
    snippet = substr(body$snippet %||% "", 1L, 300L),
    stringsAsFactors = FALSE)
}

#' Generic by-id loader for any ArcGIS Online Feature Service item.
#'
#' Portal-agnostic sibling to
#' [morie_datasets_tps_arcgis_hub_by_id()]. Works for ANY ArcGIS
#' Online item GUID (not just TPS Hub catalog entries). Same five
#' format paths (json / geojson / csv / shapefile / fgdb).
#'
#' The hub_id is ALWAYS resolved live (via the items API) because
#' there's no bundled catalog for non-TPS items. If you find
#' yourself calling this against the same item repeatedly, consider
#' adding a named wrapper (e.g. the shipped
#' \code{\link{morie_datasets_toronto_zoning_per_neighbourhood}}
#' wraps EsriCanadaEducation's `af06159170914808983959df6163fc86`
#' with bundled fixtures for offline use).
#'
#' @param item_id 32-char hex GUID.
#' @param format One of `"json"` (default), `"geojson"`, `"csv"`,
#'   `"shapefile"`, `"fgdb"`.
#' @param where Optional SoQL-style WHERE for the FeatureServer
#'   query. Default `"1=1"`.
#' @param max_features Optional row cap.
#' @param layer_idx Integer layer index (default `0L`).
#' @param dest Optional destination path for binary downloads.
#' @return A `data.frame` (json / csv), parsed GeoJSON list, or
#'   file path (binary).
#' @export
morie_datasets_arcgis_item_by_id <- function(item_id,
                                               format = "json",
                                               where = "1=1",
                                               max_features = NULL,
                                               layer_idx = 0L,
                                               dest = NULL) {
  format <- match.arg(format, choices = .MORIE_TPS_HUB_FORMATS)
  # Always live-resolve -- there's no portal-agnostic offline catalog.
  fs_url <- .morie_dataset_tps_hub_resolve(item_id, offline = FALSE)
  if (format == "json") {
    layer_url <- sprintf("%s/%d/query", fs_url, as.integer(layer_idx))
    query <- list(where = where, outFields = "*", f = "json",
                  returnGeometry = "false")
    if (!is.null(max_features)) {
      query$resultRecordCount <- as.integer(max_features)
    }
    body <- .morie_dataset_http_json(layer_url, query = query)
    feats <- body$features
    if (is.null(feats) || length(feats) == 0L) return(data.frame())
    return(.morie_dataset_records_to_df(lapply(feats, function(f) f$attributes)))
  }
  if (format == "geojson") {
    layer_url <- sprintf("%s/%d/query", fs_url, as.integer(layer_idx))
    query <- list(where = where, outFields = "*", f = "geojson")
    if (!is.null(max_features)) {
      query$resultRecordCount <- as.integer(max_features)
    }
    return(.morie_dataset_http_json(layer_url, query = query))
  }
  if (format == "csv") {
    csv_url <- sprintf(paste0(
      "https://hub.arcgis.com/api/v3/datasets/%s_%d/",
      "downloads/data"),
      item_id, as.integer(layer_idx))
    raw <- .morie_dataset_http_text(csv_url,
                                     query = list(format = "csv"))
    return(utils::read.csv(text = raw, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  fmt_token <- switch(format, shapefile = "shp", fgdb = "fgdb")
  bin_url <- sprintf(paste0(
    "https://hub.arcgis.com/api/v3/datasets/%s_%d/",
    "downloads/data?format=%s"),
    item_id, as.integer(layer_idx), fmt_token)
  if (is.null(dest)) {
    suffix <- switch(format, shapefile = ".shp.zip", fgdb = ".fgdb.zip")
    dest <- tempfile(fileext = suffix)
  }
  # 3XX: libcurl-backed + httr2 fallback.
  bytes <- .morie_dataset_http_bytes(bin_url)
  writeBin(bytes, dest)
  dest
}

# ---------------------------------------------------------------------------
# Toronto Zoning per Neighbourhood (EsriCanadaEducation,
# af06159170914808983959df6163fc86) -- 3SS+ named wrapper.
# ---------------------------------------------------------------------------

.MORIE_TORONTO_ZONING_ITEM_ID <- "af06159170914808983959df6163fc86"

#' Toronto Zoning per Neighbourhood (EsriCanadaEducation)
#'
#' Wraps EsriCanadaEducation's ArcGIS Online Feature Service
#' `ZonesofToronto_Neighbourhoods`
#' (item id `af06159170914808983959df6163fc86`; FeatureServer at
#' `services.arcgis.com/As5CFN3ThbQpy8Ph/.../ZonesofToronto_Neighbourhoods/FeatureServer`).
#' Two layers in the service:
#'
#' \describe{
#'   \item{`layer = "neighbourhoods"` (FeatureServer layer 0)}{Polygon
#'     boundaries for Toronto neighbourhoods with a 39-column
#'     demographic schema -- total population, sex split, 18 age
#'     brackets (0-4 through 85+), senior + youth + child aggregates,
#'     and 10 specific language counts (Chinese, Italian, Korean,
#'     Persian, Portuguese, Russian, Spanish, Tagalog, Tamil, Urdu)
#'     plus a `HomeLanguageCategory` total.}
#'   \item{`layer = "zoning_stats"` (FeatureServer table 1)}{Per-
#'     neighbourhood zoning-area stats -- 4 columns (`OBJECTID`,
#'     `ZoneDesc`, `Neighbourhood_Name`, `SUM_Area`). Many rows per
#'     neighbourhood, one per `ZoneDesc` (Commercial, Residential,
#'     Industrial, etc.).}
#' }
#'
#' Offline mode reads bundled 5-row synthetic fixtures
#' (`toronto_zoning_neighbourhoods_sample.csv` /
#' `toronto_zoning_stats_sample.csv`) -- SYNTH-stamped, not
#' attributable to actual Toronto neighbourhoods. Live mode hits
#' the FeatureServer via the 3SS+ generic
#' [morie_datasets_arcgis_item_by_id()] resolver.
#'
#' @param layer One of `"neighbourhoods"` (default, polygon
#'   demographics) or `"zoning_stats"` (per-zone area table).
#' @param format One of `"json"` (default), `"geojson"`, `"csv"`,
#'   `"shapefile"`, `"fgdb"`. Only honoured when `offline = FALSE`.
#' @param where Optional FeatureServer WHERE filter (live mode).
#' @param max_features Optional row cap.
#' @param offline Logical; if `TRUE` (default), read the bundled
#'   synthetic fixture.
#' @return A `data.frame` (json / csv / offline), parsed GeoJSON
#'   list, or file path (binary).
#' @references Esri Canada Education -- ArcGIS Online item
#'   `af06159170914808983959df6163fc86`.
#' @examples
#' df <- morie_datasets_toronto_zoning_per_neighbourhood(offline = TRUE)
#' head(df[, c("Neighbourhood", "Total_Population", "Seniors65andover")])
#' @export
morie_datasets_toronto_zoning_per_neighbourhood <- function(
    layer = c("neighbourhoods", "zoning_stats"),
    format = "json",
    where = "1=1",
    max_features = NULL,
    offline = TRUE) {
  layer <- match.arg(layer)
  layer_idx <- if (layer == "neighbourhoods") 0L else 1L
  if (isTRUE(offline)) {
    fixture <- if (layer == "neighbourhoods") {
      "toronto_zoning_neighbourhoods_sample.csv"
    } else {
      "toronto_zoning_stats_sample.csv"
    }
    path <- system.file("extdata", fixture, package = "morie")
    if (!nzchar(path)) {
      stop(sprintf("bundled Toronto Zoning fixture %s missing",
                   fixture), call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  morie_datasets_arcgis_item_by_id(
    .MORIE_TORONTO_ZONING_ITEM_ID,
    format = format,
    where = where,
    max_features = max_features,
    layer_idx = layer_idx)
}
