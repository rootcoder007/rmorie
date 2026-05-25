# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Toronto Police Service open-data client (R port).
#
# TPS publishes its open-data feeds through an ArcGIS Hub at
# https://data.torontopolice.on.ca/. Every dataset is backed by an
# ArcGIS FeatureServer with a stable REST API that returns JSON via
# `<layer>/query?f=json`.
#
# Mirrors `morie.ingest.tps` in Python:
#
#   * `.MORIE_TPS_LAYER_REGISTRY` - canonical layer URLs
#   * `morie_ingest_tps_layers()`        - registry as data.frame
#   * `morie_ingest_tps_feature_layer()` - paged query -> data.frame
#
# HTTP: routes via .morie_dataset_http_text (3YY -> libcurl C++ backend with httr2 fallback). JSON: jsonlite::fromJSON(simplifyVector=FALSE).
# Geometry: the source layers are stored in WGS-1984 Web Mercator
# (auxiliary sphere); we force `outSR=4326` so `geom_x`/`geom_y`
# come back as longitude/latitude, not metres.

.MORIE_TPS_DEFAULT_UA <- "morie/r (+https://hadesllm.com)"
.MORIE_TPS_DEFAULT_TIMEOUT <- 60

# Canonical TPS open-data layer endpoints (verified 2026-05-13).
# Stable IDs but underlying service URLs can rotate under ArcGIS Hub
# reorganisation; fall back to `morie_ingest_tps_layers()` to inspect.
.MORIE_TPS_LAYER_REGISTRY <- list(
  `major-crime` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Major_Crime_Indicators_Open_Data/FeatureServer/0"
  ),
  `shooting-firearms` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Shooting_and_Firearm_Discharges_Open_Data/FeatureServer/0"
  ),
  homicide = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Homicides_Open_Data/FeatureServer/0"
  ),
  robbery = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Robbery_Open_Data/FeatureServer/0"
  ),
  assault = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Assault_Open_Data/FeatureServer/0"
  ),
  `auto-theft` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Auto_Theft_Open_Data/FeatureServer/0"
  ),
  `break-and-enter` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Break_and_Enter_Open_Data/FeatureServer/0"
  ),
  `theft-over` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Theft_Over_Open_Data/FeatureServer/0"
  ),
  `bicycle-thefts` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Bicycle_Thefts_Open_Data/FeatureServer/0"
  )
)

#' Built-in TPS open-data layer registry
#'
#' Returns the canonical Toronto Police Service open-data ArcGIS
#' FeatureServer layer URLs morie ships with as a flat data.frame.
#' Useful for discovery and for the CLI \code{--list} surface.
#'
#' @return A base R \code{data.frame} with columns \code{name},
#'   \code{url}.
#' @examples
#' morie_ingest_tps_layers()
#' @export
morie_ingest_tps_layers <- function() {
  data.frame(
    name = names(.MORIE_TPS_LAYER_REGISTRY),
    url  = unlist(.MORIE_TPS_LAYER_REGISTRY, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

# Internal: a single ArcGIS FeatureServer /query call.
.morie_tps_arcgis_query <- function(layer_url,
                                    where = "1=1",
                                    out_fields = "*",
                                    return_geometry = FALSE,
                                    result_offset = 0L,
                                    result_record_count = 2000L,
                                    user_agent =
                                      .MORIE_TPS_DEFAULT_UA,
                                    timeout =
                                      .MORIE_TPS_DEFAULT_TIMEOUT) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package 'httr2' is required for morie_ingest_tps_*(). ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }
  params <- list(
    where = where,
    outFields = out_fields,
    returnGeometry = if (isTRUE(return_geometry)) "true" else "false",
    outSR = 4326L,
    resultOffset = as.integer(result_offset),
    resultRecordCount = as.integer(result_record_count),
    f = "json"
  )
  # 3YY: route through .morie_dataset_http_text + jsonlite for
  # simplifyVector = FALSE semantics (downstream expects list-of-records).
  body <- tryCatch(
    .morie_dataset_http_text(paste0(layer_url, "/query"),
                              query = params,
                              timeout_s = as.integer(timeout)),
    error = function(e) {
      stop("morie TPS layer query failed (", layer_url, "): ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  if (!is.null(payload$error)) {
    stop("morie TPS layer query error: ",
      paste(utils::capture.output(str(payload$error)), collapse = " "),
      call. = FALSE
    )
  }
  payload
}

# Internal: bind one ArcGIS feature payload's attribute rows into a
# data.frame, optionally splicing in geom_x / geom_y.
.morie_tps_features_to_rows <- function(features, return_geometry) {
  if (length(features) == 0L) {
    return(list())
  }
  lapply(features, function(f) {
    attrs <- f$attributes
    if (is.null(attrs)) attrs <- list()
    if (isTRUE(return_geometry) && !is.null(f$geometry)) {
      attrs$geom_x <- f$geometry$x
      attrs$geom_y <- f$geometry$y
    }
    attrs
  })
}

#' Fetch every feature from a TPS ArcGIS FeatureServer layer
#'
#' ArcGIS FeatureServer queries cap at the layer's server-side
#' \code{maxRecordCount} (2,000 for the TPS layers).  This function
#' pages through transparently using \code{resultOffset} and the
#' \code{exceededTransferLimit} flag, emitting one data.frame.
#'
#' @param layer_url Full URL to a FeatureServer layer, e.g. one of
#'   the entries in \code{\link{morie_ingest_tps_layers}}.
#' @param where ArcGIS WHERE clause.  Default \code{"1=1"} fetches
#'   everything.  Examples: \code{"OCC_YEAR = 2024"},
#'   \code{"OCC_YEAR BETWEEN 2020 AND 2025"}.
#' @param out_fields Comma-separated attribute list, or \code{"*"}.
#' @param return_geometry If \code{TRUE}, includes \code{geom_x} /
#'   \code{geom_y} columns (longitude / latitude in EPSG:4326).
#' @param page_size Records per request; clamped server-side to 2,000
#'   for TPS layers.
#' @param max_features Optional hard cap on total returned rows.
#' @param user_agent,timeout Standard request knobs.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
#' df <- morie_ingest_tps_feature_layer(
#'   morie_ingest_tps_layers()$url[
#'     morie_ingest_tps_layers()$name == "major-crime"
#'   ],
#'   where = "OCC_YEAR >= 2023",
#'   max_features = 5000L
#' )
#' nrow(df)
#' }
#' @export
morie_ingest_tps_feature_layer <- function(
    layer_url,
    where = "1=1",
    out_fields = "*",
    return_geometry = FALSE,
    page_size = 2000L,
    max_features = NULL,
    user_agent = .MORIE_TPS_DEFAULT_UA,
    timeout = .MORIE_TPS_DEFAULT_TIMEOUT) {
  if (!is.character(layer_url) || length(layer_url) != 1L ||
    !nzchar(layer_url)) {
    stop("`layer_url` must be a single non-empty URL.", call. = FALSE)
  }
  page_size <- as.integer(page_size)
  rows <- list()
  offset <- 0L
  repeat {
    payload <- .morie_tps_arcgis_query(
      layer_url,
      where = where,
      out_fields = out_fields,
      return_geometry = return_geometry,
      result_offset = offset,
      result_record_count = page_size,
      user_agent = user_agent,
      timeout = timeout
    )
    features <- payload$features
    if (is.null(features)) features <- list()
    if (length(features) == 0L) break
    rows <- c(
      rows,
      .morie_tps_features_to_rows(features, return_geometry)
    )
    if (!is.null(max_features) && length(rows) >= max_features) {
      rows <- rows[seq_len(max_features)]
      break
    }
    if (!isTRUE(payload$exceededTransferLimit)) break
    offset <- offset + length(features)
  }
  if (length(rows) == 0L) {
    stop(
      "morie_ingest_tps_feature_layer: zero features for where=",
      paste0("'", where, "'"), ".",
      call. = FALSE
    )
  }
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  do.call(rbind, lapply(rows, function(r) {
    vals <- lapply(cols, function(k) {
      v <- r[[k]]
      if (is.null(v)) NA else if (is.list(v)) {
        paste(unlist(v, use.names = FALSE), collapse = ";")
      } else {
        v
      }
    })
    names(vals) <- cols
    as.data.frame(vals, stringsAsFactors = FALSE)
  }))
}

#' Fetch a TPS open-data layer by short name
#'
#' Convenience wrapper around
#' \code{\link{morie_ingest_tps_feature_layer}} that takes a
#' registry short-name (e.g. \code{"major-crime"},
#' \code{"shooting-firearms"}) instead of a raw FeatureServer URL.
#'
#' @param layer Short name from \code{\link{morie_ingest_tps_layers}}.
#' @param year Optional shortcut for \code{OCC_YEAR = <year>} when
#'   \code{where} is \code{NULL}.
#' @param where Raw ArcGIS WHERE clause (overrides \code{year}).
#' @param return_geometry Include longitude/latitude columns.
#' @param max_features Optional hard cap on rows.
#' @param ... Forwarded to \code{\link{morie_ingest_tps_feature_layer}}.
#' @return A base R \code{data.frame}.
#' @export
morie_ingest_tps_fetch <- function(layer,
                                   year = NULL,
                                   where = NULL,
                                   return_geometry = FALSE,
                                   max_features = NULL,
                                   ...) {
  if (!is.character(layer) || length(layer) != 1L || !nzchar(layer)) {
    stop("`layer` must be a single non-empty registry name.", call. = FALSE)
  }
  url <- .MORIE_TPS_LAYER_REGISTRY[[layer]]
  if (is.null(url)) {
    stop(
      "Unknown TPS layer '", layer, "'. Try one of: ",
      paste(names(.MORIE_TPS_LAYER_REGISTRY), collapse = ", "),
      call. = FALSE
    )
  }
  clause <- if (!is.null(where) && nzchar(where)) {
    where
  } else if (!is.null(year)) {
    yr <- suppressWarnings(as.integer(year))
    if (is.na(yr)) {
      stop("`year` must be coercible to integer.", call. = FALSE)
    }
    sprintf("OCC_YEAR = %d", yr)
  } else {
    "1=1"
  }
  morie_ingest_tps_feature_layer(
    layer_url = url,
    where = clause,
    return_geometry = return_geometry,
    max_features = max_features,
    ...
  )
}
