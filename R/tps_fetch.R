# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie/tps_fetch.R -- Toronto Police Service ArcGIS REST fetcher.
#
# Mirrors the Python `morie.tps_fetch` module. Pages through the
# ArcGIS REST `/query` endpoint for a category's FeatureServer
# layer, accumulates GeoJSON features, and writes a tidy CSV into
# the user-chosen cache directory. Uses `httr2` for the HTTP loop;
# all I/O is gated behind a `requireNamespace` check so the package
# can still be loaded on a network-free CRAN build.

#' Per-category ArcGIS REST FeatureServer layer URLs (constant)
#'
#' Each layer's `/query` endpoint is appended at request time.
#' Folded into the `morie_tps_layer_urls()` Rd via `@rdname` to
#' avoid the case-insensitive filesystem collision between
#' `MORIE_TPS_LAYER_URLS.Rd` and `morie_tps_layer_urls.Rd`.
#'
#' @rdname morie_tps_layer_urls
#' @export
MORIE_TPS_LAYER_URLS <- c(
  Assault = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Assault_Open_Data/FeatureServer/0"),
  AutoTheft = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Auto_Theft_Open_Data/FeatureServer/0"),
  BicycleTheft = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Bicycle_Thefts_Open_Data/FeatureServer/0"),
  BreakAndEnter = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Break_and_Enter_Open_Data/FeatureServer/0"),
  Homicides = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Homicides_Open_Data_ASR_RC_TBL_002/FeatureServer/0"),
  Robbery = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Robbery_Open_Data/FeatureServer/0"),
  ShootingAndFirearmDiscarges = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Shooting_and_Firearm_Discharges_Open_Data/",
    "FeatureServer/0"),
  TheftFromMV = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Theft_From_Motor_Vehicle_Open_Data/FeatureServer/0"),
  TheftOver = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/",
    "services/Theft_Over_Open_Data/FeatureServer/0")
)


#' List TPS categories known to the fetcher.
#'
#' @return Character vector of category names, sorted.
#'
#' @export
morie_tps_list_categories <- function() {
  sort(names(MORIE_TPS_LAYER_URLS))
}


# Internal: one ArcGIS REST /query GET. Returns the parsed GeoJSON
# list (raises on HTTP / JSON failure).
.morie_tps_fetch_arcgis_query <- function(base_url, where, offset,
                                    max_records = 2000L,
                                    timeout = 120) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "morie_tps_fetch_category() needs the `httr2` package. ",
      "Install with install.packages('httr2').",
      call. = FALSE
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "morie_tps_fetch_category() needs the `jsonlite` package. ",
      "Install with install.packages('jsonlite').",
      call. = FALSE
    )
  }
  req <- httr2::request(paste0(base_url, "/query"))
  req <- httr2::req_url_query(
    req,
    where = where,
    outFields = "*",
    returnGeometry = "true",
    f = "geojson",
    resultRecordCount = as.integer(max_records),
    resultOffset = as.integer(offset)
  )
  req <- httr2::req_timeout(req, timeout)
  resp <- httr2::req_perform(req)
  jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = FALSE
  )
}


#' Fetch one TPS category as a CSV, paging until exhausted.
#'
#' Walks the ArcGIS REST `/query` endpoint for the category's
#' FeatureServer layer, accumulates all features in memory, and
#' writes a single CSV file to `cache_dir`. Returns the CSV path.
#'
#' @param category One of [morie_tps_list_categories()].
#' @param cache_dir Directory to write the CSV into. Defaults to
#'   `tempdir()` (CRAN-safe; the Python default of
#'   `~/.cache/morie/tps` requires `R_user_dir` opt-in in R).
#' @param where ArcGIS SQL `where` clause (default `"1=1"`).
#' @param overwrite If `FALSE` and the output exists, return it
#'   without re-downloading.
#' @param max_records_per_page Pagination size (server caps at 2000).
#'
#' @return Path to the written CSV file.
#'
#' @export
morie_tps_fetch_category <- function(category,
                                     cache_dir = NULL,
                                     where = "1=1",
                                     overwrite = FALSE,
                                     max_records_per_page = 2000L) {
  stopifnot(is.character(category), length(category) == 1L)
  if (!(category %in% names(MORIE_TPS_LAYER_URLS))) {
    stop(sprintf(
      "Unknown TPS category %s. Known: %s",
      sQuote(category),
      paste(morie_tps_list_categories(), collapse = ", ")
    ), call. = FALSE)
  }
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "morie", "tps")
  }
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  out_path <- file.path(cache_dir, sprintf("tps_%s.csv", category))
  if (file.exists(out_path) && !overwrite) {
    return(out_path)
  }

  base <- MORIE_TPS_LAYER_URLS[[category]]
  offset <- 0L
  rows <- list()
  fieldnames <- NULL

  repeat {
    page <- .morie_tps_fetch_arcgis_query(
      base, where = where, offset = offset,
      max_records = max_records_per_page
    )
    feats <- page$features
    if (is.null(feats) || length(feats) == 0L) break
    for (f in feats) {
      props <- f$properties
      if (is.null(props)) props <- list()
      geom <- f$geometry
      if (!is.null(geom) && identical(geom$type, "Point") &&
          !is.null(geom$coordinates)) {
        if (is.null(props$LONG_WGS84)) {
          props$LONG_WGS84 <- geom$coordinates[[1]]
        }
        if (is.null(props$LAT_WGS84)) {
          props$LAT_WGS84 <- geom$coordinates[[2]]
        }
      }
      if (is.null(fieldnames)) fieldnames <- names(props)
      rows[[length(rows) + 1L]] <- props
    }
    if (length(feats) < max_records_per_page) break
    offset <- offset + length(feats)
  }

  if (length(rows) == 0L) {
    stop(sprintf("No features returned for %s", sQuote(category)),
         call. = FALSE)
  }
  if (is.null(fieldnames)) fieldnames <- names(rows[[1L]])

  # Normalise list-of-lists into a data.frame (NULL -> NA per column).
  mat <- lapply(fieldnames, function(fn) {
    vapply(rows, function(r) {
      v <- r[[fn]]
      if (is.null(v)) NA_character_ else as.character(v)
    }, character(1))
  })
  df <- as.data.frame(stats::setNames(mat, fieldnames),
                      stringsAsFactors = FALSE,
                      check.names = FALSE)
  utils::write.csv(df, out_path, row.names = FALSE)
  out_path
}


#' Fetch a TPS category and return it as a `data.frame`.
#'
#' Thin wrapper over [morie_tps_fetch_category()]: writes the CSV
#' then reads it back. Mirrors the Python `fetch_tps_dataframe`
#' convenience used as a `DATASET_CATALOG` fetcher.
#'
#' @param category One of [morie_tps_list_categories()].
#' @param ... Passed through to [morie_tps_fetch_category()].
#'
#' @return A `data.frame`.
#'
#' @export
morie_tps_fetch_dataframe <- function(category, ...) {
  p <- morie_tps_fetch_category(category, ...)
  utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
}
