# SPDX-License-Identifier: AGPL-3.0-or-later
#' Bundled reference data samples and dataset fetchers
#'
#' MORIE ships a small set of reference CSVs in `inst/extdata/` so that
#' the `mrm_otis_*()` and `mrm_tps_*()` callables can be exercised
#' without any network call. For full datasets, the on-demand fetchers
#' pull from the original public sources:
#'
#' * OTIS: data.ontario.ca CKAN package `data-on-inmates-in-ontario`.
#'   Resource IDs are baked into `morie_dataset_catalog()`; use
#'   `morie_load_dataset("otisb01")` (etc.) which calls the existing
#'   CKAN fetcher.
#' * TPS: Toronto Police Open Data ArcGIS REST. Use
#'   `morie_fetch_tps(category = "Assault")`.
#' * SIU: Ontario SIU Director's Reports site. Use
#'   `morie_fetch_siu()` which parses the public reports site on
#'   demand (per-user, since redistribution of the parsed corpus is
#'   not clearly licensed).
#'
#' @return The on-demand fetchers (\code{morie_fetch_tps()},
#'   \code{morie_fetch_siu()}) return the file path to the downloaded or
#'   cached CSV; \code{morie_load_dataset()} returns the loaded
#'   \code{data.frame}.
#' @examples
#' if (FALSE) {
#'   b01 <- morie_load_dataset("otisb01")
#'   head(b01)
#' }
#' @name mrm_samples
NULL


#' Load a bundled MORIE reference sample by name
#'
#' Returns a small CSV that ships with the package, suitable for
#' running examples and tests of the `mrm_*()` callables without any
#' network or external data dependency.
#'
#' @param name One of `"otis_b01"`, `"otis_b09"`, `"otis_c11"`,
#'   `"tps_assault"`.
#' @return A data.frame.
#' @export
#' @examples
#' b01 <- morie_sample("otis_b01")
#' head(b01)
morie_sample <- function(name = c("otis_b01", "otis_b09", "otis_c11", "tps_assault")) {
  name <- match.arg(name)
  files <- c(
    otis_b01 = "otis_b01_sample.csv",
    otis_b09 = "otis_b09_sample.csv",
    otis_c11 = "otis_c11_sample.csv",
    tps_assault = "tps_assault_sample.csv"
  )
  path <- system.file("extdata", files[[name]], package = "morie")
  if (path == "" || !file.exists(path)) {
    stop("Sample file not found in installed morie/extdata. Reinstall the package.")
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# TPS ArcGIS REST fetcher
# ---------------------------------------------------------------------------

#' TPS ArcGIS layer URLs known to MORIE
#'
#' @return Named character vector mapping TPS category names to ArcGIS
#'   FeatureServer layer roots.
#' @examples
#' urls <- morie_tps_layer_urls()
#' names(urls) # categories: Assault, AutoTheft, Homicide, ...
#' length(urls) # number of layers
#' @export
morie_tps_layer_urls <- function() {
  c(
    Assault =
      "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/Assault_Open_Data/FeatureServer/0",
    AutoTheft =
      "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/Auto_Theft_Open_Data/FeatureServer/0",
    BicycleTheft =
      "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/Bicycle_Thefts_Open_Data/FeatureServer/0",
    BreakAndEnter =
      "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/Break_and_Enter_Open_Data/FeatureServer/0",
    Homicides =
      paste0("https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/",
        "rest/services/Homicides_Open_Data_ASR_RC_TBL_002/FeatureServer/0"),
    Robbery =
      "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/Robbery_Open_Data/FeatureServer/0",
    ShootingAndFirearmDiscarges =
      paste0("https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/",
        "rest/services/Shooting_and_Firearm_Discharges_Open_Data/FeatureServer/0"),
    TheftFromMV =
      paste0("https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/",
        "rest/services/Theft_From_Motor_Vehicle_Open_Data/FeatureServer/0"),
    TheftOver =
      "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/Theft_Over_Open_Data/FeatureServer/0"
  )
}


#' Fetch a TPS category from the Toronto Police ArcGIS REST endpoint
#'
#' Pages through the ArcGIS `/query` endpoint and writes a tidy CSV to
#' the morie cache directory. Calls back to a cached file on subsequent
#' calls unless `overwrite = TRUE`.
#'
#' @param category One of `names(morie_tps_layer_urls())`.
#' @param cache_dir Directory for the CSV. Defaults to a
#'   session-scoped subdirectory of `tempdir()` that R cleans up
#'   automatically. For persistent caching pass
#'   `cache_dir = morie_cache_dir("tps")`; see
#'   [morie_cache_dir] and [morie_cache_clear].
#' @param where ArcGIS SQL where clause (default `"1=1"`).
#' @param overwrite Logical; if `FALSE` and the CSV exists, return its
#'   path without re-downloading.
#' @param max_per_page ArcGIS page size (default `2000`; server caps).
#' @return Path to the CSV.
#' @examples
#' \dontrun{
#' # Network: fetches major-crime indicators from the Toronto Police
#' # ArcGIS open-data layer.
#' csv <- morie_fetch_tps(
#'   category = "Assault",
#'   cache_dir = tempdir(),
#'   where = "OCC_YEAR = 2024"
#' )
#' tps <- utils::read.csv(csv)
#' nrow(tps)
#' }
#' @export
morie_fetch_tps <- function(
  category,
  cache_dir = file.path(tempdir(), "morie", "tps"),
  where = "1=1",
  overwrite = FALSE,
  max_per_page = 2000L
) {
  urls <- morie_tps_layer_urls()
  if (!category %in% names(urls)) {
    stop(
      "Unknown TPS category. Known: ",
      paste(names(urls), collapse = ", ")
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite required for morie_fetch_tps().")
  }
  cache_dir <- path.expand(cache_dir)
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  out <- file.path(cache_dir, paste0("tps_", category, ".csv"))
  if (file.exists(out) && !overwrite) {
    return(out)
  }

  base <- urls[[category]]
  offset <- 0L
  rows <- list()
  repeat {
    url <- sprintf(
      "%s/query?where=%s&outFields=*&returnGeometry=true&f=geojson&resultRecordCount=%d&resultOffset=%d",
      base, utils::URLencode(where, reserved = TRUE), max_per_page, offset
    )
    page <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(page)) {
      # Abort loudly rather than silently caching a partial download:
      # a transient failure mid-paging must not be written to disk and
      # then returned as if it were the complete layer.
      stop(
        "morie_fetch_tps(): TPS ArcGIS request failed at offset ",
        offset, " for category '", category, "'."
      )
    }
    feats <- page$features
    if (length(feats) == 0L) break
    for (f in feats) {
      r <- f$properties
      if (!is.null(f$geometry) && identical(f$geometry$type, "Point")) {
        r$LONG_WGS84 <- f$geometry$coordinates[[1]]
        r$LAT_WGS84 <- f$geometry$coordinates[[2]]
      }
      rows[[length(rows) + 1L]] <- r
    }
    offset <- offset + length(feats)
    # Page on the server's exceededTransferLimit flag, NOT on a short
    # page: a layer whose server-side maxRecordCount is below
    # max_per_page returns short pages on every call, so breaking on a
    # short page would silently truncate the download to page one.
    if (!isTRUE(page$exceededTransferLimit)) break
  }
  if (length(rows) == 0L) stop("No features returned for ", category)
  df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  utils::write.csv(df, out, row.names = FALSE)
  out
}


# morie_fetch_siu() now lives in R/siu.R -- it drives the all-C/C++
# SIU parser (src/siu_parser.cpp) instead of the Python module.
