# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie/tps_io.R -- multi-format readers for TPS open-data exports.
#
# Each TPS category on disk has up to 9 sibling format exports:
#
#   CSV               -- utils::read.csv (canonical fast path)
#   Excel             -- readxl::read_excel
#   GeoJSON           -- sf::st_read (if available) else jsonlite scan
#   FeatureCollection -- ESRI JSON; same as GeoJSON path
#   KML / KMZ         -- sf::st_read (KML driver) when available
#   GeoPackage        -- sf::st_read
#   SQLiteGeodatabase -- sf::st_read
#   Shapefile         -- sf::st_read
#   FileGeoDatabase   -- sf::st_read (OpenFileGDB driver) when available
#
# Where the Python implementation hand-rolled WKB / KML / ESRI-JSON
# decoders, this R port leans on the `sf` package whenever it is
# installed; for CSV and Excel it uses base / `readxl` directly. Any
# format whose required package is missing raises `NotYetPorted` so
# the caller gets a clear "install sf" / "install readxl" path.

#' Format names that `morie_tps_load()` knows how to dispatch.
#' @export
MORIE_TPS_SUPPORTED_FORMATS <- c(
  "csv", "excel",
  "geojson", "featurecollection",
  "kml",
  "geopackage", "sqlitegeodatabase",
  "shapefile", "filegeodatabase"
)


.morie_tps_io_category_dir <- function(name, fmt_subdir) {
  canonical <- .morie_tps_canonical(name)
  base <- morie_tps_data_dir()
  file.path(base, canonical, fmt_subdir)
}


.morie_tps_io_pick_one <- function(d, exts) {
  if (!dir.exists(d)) {
    stop(sprintf("no matching file in %s (exts: %s)",
                 d, paste(exts, collapse = ", ")),
         call. = FALSE)
  }
  for (ext in exts) {
    cands <- list.files(
      d,
      pattern = paste0("\\.", ext, "$"),
      full.names = TRUE,
      ignore.case = TRUE
    )
    if (length(cands)) return(cands[[1L]])
  }
  stop(sprintf("no matching file in %s (exts: %s)",
               d, paste(exts, collapse = ", ")),
       call. = FALSE)
}


.morie_tps_apply_nrows <- function(df, nrows) {
  if (is.null(nrows)) return(df)
  n <- min(nrow(df), as.integer(nrows))
  df[seq_len(n), , drop = FALSE]
}


# ── CSV / Excel ───────────────────────────────────────────────────


.morie_tps_read_csv <- function(name, nrows) {
  p <- .morie_tps_io_pick_one(
    .morie_tps_io_category_dir(name, "CSV"), "csv")
  args <- list(file = p, stringsAsFactors = FALSE,
               check.names = FALSE)
  if (!is.null(nrows)) args$nrows <- as.integer(nrows)
  do.call(utils::read.csv, args)
}


.morie_tps_read_excel <- function(name, nrows) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "TPS Excel reading needs the `readxl` package. ",
      "Install with install.packages('readxl').",
      call. = FALSE
    )
  }
  p <- .morie_tps_io_pick_one(
    .morie_tps_io_category_dir(name, "Excel"), c("xlsx", "xls"))
  df <- as.data.frame(
    readxl::read_excel(
      p,
      n_max = if (is.null(nrows)) Inf else as.integer(nrows)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  df
}


# ── GeoJSON / FeatureCollection ───────────────────────────────────


.morie_tps_read_geojson <- function(name, nrows) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "TPS GeoJSON reading needs the `sf` package. ",
      "Install with install.packages('sf').",
      call. = FALSE
    )
  }
  p <- .morie_tps_io_pick_one(
    .morie_tps_io_category_dir(name, "GeoJSON"),
    c("geojson", "json"))
  df <- as.data.frame(
    sf::st_read(p, quiet = TRUE),
    stringsAsFactors = FALSE
  )
  .morie_tps_apply_nrows(df, nrows)
}


.morie_tps_read_featurecollection <- function(name, nrows) {
  # ESRI FeatureCollection exports are .txt with JSON inside;
  # the sf GeoJSON driver handles them when given the right path.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "TPS FeatureCollection reading needs the `sf` package. ",
      "Install with install.packages('sf').",
      call. = FALSE
    )
  }
  p <- .morie_tps_io_pick_one(
    .morie_tps_io_category_dir(name, "FeatureCollection"),
    c("txt", "json", "geojson"))
  # Coerce to a GeoJSON virtual path: copy under .geojson if .txt.
  if (tolower(tools::file_ext(p)) == "txt") {
    tmp <- tempfile(fileext = ".geojson")
    file.copy(p, tmp, overwrite = TRUE)
    p <- tmp
  }
  df <- as.data.frame(
    sf::st_read(p, quiet = TRUE),
    stringsAsFactors = FALSE
  )
  .morie_tps_apply_nrows(df, nrows)
}


# ── KML / KMZ ─────────────────────────────────────────────────────


.morie_tps_read_kml <- function(name, nrows) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "TPS KML/KMZ reading needs the `sf` package (KML driver). ",
      "Install with install.packages('sf').",
      call. = FALSE
    )
  }
  d <- .morie_tps_io_category_dir(name, "KML")
  if (!dir.exists(d)) {
    stop(sprintf("no KML/KMZ in %s", d), call. = FALSE)
  }
  cands <- c(
    list.files(d, pattern = "\\.kmz$", full.names = TRUE,
               ignore.case = TRUE),
    list.files(d, pattern = "\\.kml$", full.names = TRUE,
               ignore.case = TRUE)
  )
  if (!length(cands)) {
    stop(sprintf("no KML/KMZ in %s", d), call. = FALSE)
  }
  p <- cands[[1L]]
  if (tolower(tools::file_ext(p)) == "kmz") {
    # Extract inner .kml.
    td <- tempfile()
    dir.create(td)
    utils::unzip(p, exdir = td)
    inner <- list.files(td, pattern = "\\.kml$",
                        full.names = TRUE, recursive = TRUE)
    if (!length(inner)) {
      stop(sprintf("no .kml inside %s", p), call. = FALSE)
    }
    p <- inner[[1L]]
  }
  df <- as.data.frame(
    sf::st_read(p, quiet = TRUE),
    stringsAsFactors = FALSE
  )
  .morie_tps_apply_nrows(df, nrows)
}


# ── GeoPackage / SQLiteGeodatabase ────────────────────────────────


.morie_tps_read_sf_path <- function(p, nrows) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "This TPS format reader needs the `sf` package. ",
      "Install with install.packages('sf').",
      call. = FALSE
    )
  }
  df <- as.data.frame(
    sf::st_read(p, quiet = TRUE),
    stringsAsFactors = FALSE
  )
  .morie_tps_apply_nrows(df, nrows)
}


.morie_tps_read_geopackage <- function(name, nrows) {
  p <- .morie_tps_io_pick_one(
    .morie_tps_io_category_dir(name, "GeoPackage"), "gpkg")
  .morie_tps_read_sf_path(p, nrows)
}


.morie_tps_read_sqlite_geodatabase <- function(name, nrows) {
  p <- .morie_tps_io_pick_one(
    .morie_tps_io_category_dir(name, "SQLiteGeodatabase"),
    "geodatabase")
  .morie_tps_read_sf_path(p, nrows)
}


# ── Shapefile / FileGeoDatabase ───────────────────────────────────


.morie_tps_read_shapefile <- function(name, nrows) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "TPS Shapefile reading needs the `sf` package. ",
      "Install with install.packages('sf').",
      call. = FALSE
    )
  }
  d <- .morie_tps_io_category_dir(name, "Shapefile")
  # Prefer an unpacked .shp; fall back to extracting from a .zip.
  shp <- list.files(d, pattern = "\\.shp$", full.names = TRUE,
                    ignore.case = TRUE)
  if (!length(shp)) {
    zips <- list.files(d, pattern = "\\.zip$", full.names = TRUE,
                       ignore.case = TRUE)
    if (!length(zips)) {
      stop(sprintf("no shapefile (.shp/.zip) in %s", d),
           call. = FALSE)
    }
    td <- tempfile()
    dir.create(td)
    utils::unzip(zips[[1L]], exdir = td)
    shp <- list.files(td, pattern = "\\.shp$",
                      full.names = TRUE, recursive = TRUE,
                      ignore.case = TRUE)
    if (!length(shp)) {
      stop(sprintf("no .shp inside %s", zips[[1L]]),
           call. = FALSE)
    }
  }
  .morie_tps_read_sf_path(shp[[1L]], nrows)
}


.morie_tps_read_filegeodatabase <- function(name, nrows) {
  # GDAL OpenFileGDB driver via sf::st_read works on the directory.
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "TPS FileGeoDatabase reading needs the `sf` package with the ",
      "OpenFileGDB driver. Install with install.packages('sf').",
      call. = FALSE
    )
  }
  d <- .morie_tps_io_category_dir(name, "FileGeoDatabase")
  # A .gdb is itself a directory; locate it.
  gdbs <- list.dirs(d, recursive = FALSE)
  gdb <- gdbs[grepl("\\.gdb$", gdbs, ignore.case = TRUE)]
  if (!length(gdb)) {
    # Maybe a zip first.
    zips <- list.files(d, pattern = "\\.zip$", full.names = TRUE,
                       ignore.case = TRUE)
    if (!length(zips)) {
      stop("NotYetPorted: FileGeoDatabase requires an unpacked .gdb",
           call. = FALSE)
    }
    td <- tempfile()
    dir.create(td)
    utils::unzip(zips[[1L]], exdir = td)
    gdbs2 <- list.dirs(td, recursive = TRUE)
    gdb <- gdbs2[grepl("\\.gdb$", gdbs2, ignore.case = TRUE)]
    if (!length(gdb)) {
      stop("NotYetPorted: no .gdb in archive", call. = FALSE)
    }
  }
  .morie_tps_read_sf_path(gdb[[1L]], nrows)
}


# ── Master dispatcher ─────────────────────────────────────────────


.MORIE_TPS_DISPATCH <- list(
  csv               = ".morie_tps_read_csv",
  excel             = ".morie_tps_read_excel",
  geojson           = ".morie_tps_read_geojson",
  featurecollection = ".morie_tps_read_featurecollection",
  kml               = ".morie_tps_read_kml",
  geopackage        = ".morie_tps_read_geopackage",
  sqlitegeodatabase = ".morie_tps_read_sqlite_geodatabase",
  shapefile         = ".morie_tps_read_shapefile",
  filegeodatabase   = ".morie_tps_read_filegeodatabase"
)


#' Load TPS dataset `name` in the given `format`.
#'
#' Mirror of Python's `morie.tps_io.load_tps`. `csv` and `excel`
#' work with base R / `readxl`; all spatial formats (`geojson`,
#' `featurecollection`, `kml`, `geopackage`, `sqlitegeodatabase`,
#' `shapefile`, `filegeodatabase`) are gated behind
#' `requireNamespace("sf")` and surface a clean install message if
#' the dependency is missing.
#'
#' @param name TPS category. Case-insensitive.
#' @param format One of [MORIE_TPS_SUPPORTED_FORMATS].
#' @param nrows Optional integer cap on rows.
#'
#' @return A `data.frame` (spatial readers return the dropped-sf
#'   data frame; geometry column is preserved as an `sfc`).
#'
#' @export
morie_tps_load <- function(name, format = "csv", nrows = NULL) {
  fmt <- tolower(format)
  if (!(fmt %in% names(.MORIE_TPS_DISPATCH))) {
    stop(sprintf(
      "unknown format %s; valid: %s",
      sQuote(format),
      paste(names(.MORIE_TPS_DISPATCH), collapse = ", ")
    ), call. = FALSE)
  }
  fn_name <- .MORIE_TPS_DISPATCH[[fmt]]
  fn <- get(fn_name, mode = "function",
            envir = asNamespace("morie"),
            inherits = FALSE)
  fn(name, nrows)
}


#' Map TPS format name -> path of the file that would be loaded.
#'
#' Formats whose sibling directory or file is not present on disk
#' are omitted from the returned named character vector. Use this
#' to discover which formats a given category actually exports.
#'
#' @param name TPS category. Case-insensitive.
#'
#' @return Named character vector (`format` -> file path).
#'
#' @export
morie_tps_list_formats <- function(name) {
  canonical <- tryCatch(
    .morie_tps_canonical(name),
    error = function(e) NULL
  )
  if (is.null(canonical)) return(character(0))
  base <- file.path(morie_tps_data_dir(), canonical)
  fmt_dirs <- list(
    csv               = list("CSV", c("csv")),
    excel             = list("Excel", c("xlsx", "xls")),
    geojson           = list("GeoJSON", c("geojson", "json")),
    featurecollection = list("FeatureCollection",
                             c("txt", "json", "geojson")),
    kml               = list("KML", c("kmz", "kml")),
    geopackage        = list("GeoPackage", c("gpkg")),
    sqlitegeodatabase = list("SQLiteGeodatabase",
                             c("geodatabase")),
    shapefile         = list("Shapefile", c("zip", "shp")),
    filegeodatabase   = list("FileGeoDatabase", c("zip"))
  )
  out <- character(0)
  for (fmt in names(fmt_dirs)) {
    subdir <- fmt_dirs[[fmt]][[1L]]
    exts <- fmt_dirs[[fmt]][[2L]]
    d <- file.path(base, subdir)
    if (!dir.exists(d)) next
    for (ext in exts) {
      cands <- list.files(d, pattern = paste0("\\.", ext, "$"),
                          full.names = TRUE, ignore.case = TRUE)
      if (length(cands)) {
        out[[fmt]] <- cands[[1L]]
        break
      }
    }
  }
  out
}


#' List of formats this build can actually load.
#'
#' Always returns `csv`. `excel` requires `readxl`; spatial formats
#' require `sf`. If a needed namespace isn't installed, that format
#' is omitted from the returned vector.
#'
#' @return Character vector of available format names, sorted.
#'
#' @export
morie_tps_available_formats <- function() {
  out <- c("csv")
  if (requireNamespace("readxl", quietly = TRUE)) {
    out <- c(out, "excel")
  }
  if (requireNamespace("sf", quietly = TRUE)) {
    out <- c(out,
             "geojson", "featurecollection", "kml",
             "geopackage", "sqlitegeodatabase",
             "shapefile", "filegeodatabase")
  }
  sort(out)
}
