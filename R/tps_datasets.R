# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie/tps_datasets.R -- registry + CSV loader for Toronto Police
# Service public crime datasets.
#
# 13 categories sit at `data/datasets/TPS/<Category>/CSV/` in the
# repo data tree. Each category has Excel/Shapefile/GeoJSON/etc.
# sibling formats handled by `tps_io.R`. This module is the lightweight
# CSV path -- the loader most callers want.

#' Default project data directory for TPS open data.
#'
#' Resolves to `<repo>/data/datasets/TPS/` when `morie` is loaded out
#' of a source checkout. Users can override per-call via the `path`
#' argument of [morie_tps_load_dataset()].
#'
#' @export
morie_tps_data_dir <- function() {
  # Mirror Python's `Path(__file__).resolve().parents[5] / data/datasets/TPS`
  # but tolerant of the installed-package layout: prefer an env override.
  env <- Sys.getenv("MORIE_TPS_DATA_DIR", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  # Walk up from the package install (or source) directory.
  pkg_dir <- tryCatch(
    find.package("morie"),
    error = function(e) getwd()
  )
  candidate <- file.path(pkg_dir, "..", "..", "..", "..", "..",
                         "data", "datasets", "TPS")
  normalizePath(candidate, mustWork = FALSE)
}


#' Registry of TPS open-data categories.
#'
#' A named list of one-row metadata records keyed by canonical
#' category name. Each entry holds `description`, `primary_date`
#' (canonical date column name), and `has_geometry` (whether
#' LAT/LONG WGS84 columns are expected).
#'
#' @export
MORIE_TPS_REGISTRY <- list(
  Assault = list(
    description = "Reported assault incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  AutoTheft = list(
    description = "Reported auto-theft incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  BicycleTheft = list(
    description = "Reported bicycle thefts in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  BreakandEnter = list(
    description = "Reported break-and-enter incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  CommunitySafetyIndicators = list(
    description = "Toronto community-safety composite indicators",
    primary_date = "REPORT_DATE", has_geometry = TRUE),
  HateCrimes = list(
    description = "Reported hate-crime incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  Homicides = list(
    description = "Reported homicide incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  IntimatePartnerAndFamilyViolence = list(
    description = paste(
      "Reported intimate-partner and family violence in Toronto"
    ),
    primary_date = "OCC_DATE", has_geometry = TRUE),
  NeighbourhoodCrimeRates = list(
    description = paste(
      "Per-neighbourhood crime rates (annualised, by HOOD_158)"
    ),
    primary_date = "REPORT_YEAR", has_geometry = TRUE),
  Robbery = list(
    description = "Reported robbery incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE),
  ShootingAndFirearmDiscarges = list(
    description = paste(
      "Reported shooting and firearm-discharge incidents in Toronto"
    ),
    primary_date = "OCC_DATE", has_geometry = TRUE),
  TheftFromMovingVehicle = list(
    description = paste(
      "Reported theft-from-moving-vehicle incidents in Toronto"
    ),
    primary_date = "OCC_DATE", has_geometry = TRUE),
  TheftOver = list(
    description = "Reported theft-over-$5000 incidents in Toronto",
    primary_date = "OCC_DATE", has_geometry = TRUE)
)


# Tolerant column-name normalisation used by HateCrimes and a few
# other sensitivity-redacted feeds that use OCCURRENCE_* /
# REPORTED_* instead of the OCC_* / REPORT_* canonical schema.
.morie_tps_rename_map <- c(
  OCCURRENCE_DATE  = "OCC_DATE",
  OCCURRENCE_YEAR  = "OCC_YEAR",
  OCCURRENCE_MONTH = "OCC_MONTH",
  OCCURRENCE_DAY   = "OCC_DAY",
  OCCURRENCE_HOUR  = "OCC_HOUR",
  OCCURRENCE_DOW   = "OCC_DOW",
  OCCURRENCE_DOY   = "OCC_DOY",
  REPORTED_DATE    = "REPORT_DATE",
  REPORTED_YEAR    = "REPORT_YEAR"
)

.morie_tps_canonical <- function(name) {
  stopifnot(is.character(name), length(name) == 1L)
  keys <- names(MORIE_TPS_REGISTRY)
  hit <- keys[tolower(keys) == tolower(name)]
  if (length(hit) == 0L) {
    stop(sprintf(
      "unknown TPS dataset %s. valid: %s",
      sQuote(name),
      paste(sort(keys), collapse = ", ")
    ), call. = FALSE)
  }
  hit[[1L]]
}


#' Load one TPS dataset by category name (CSV thin path).
#'
#' `name` is case-insensitive. Pass `nrows = N` for a quick sample
#' while developing against the largest tables.
#'
#' For non-CSV sibling formats (Excel, GeoJSON, KML, GeoPackage,
#' Shapefile, etc.), use [morie_tps_load()] from `tps_io.R` instead.
#'
#' @param name Character scalar. One of `names(MORIE_TPS_REGISTRY)`,
#'   case-insensitive.
#' @param path Optional character scalar. Override the CSV file or
#'   directory to load from. If a directory, the first `*.csv` inside
#'   is picked. If `NULL`, the loader walks `morie_tps_data_dir()`.
#' @param csv_filename Optional filename inside the category's `CSV/`
#'   directory.
#' @param nrows Optional integer. Cap on rows to load.
#'
#' @return A `data.frame` (the CSV contents) with tolerant
#'   OCCURRENCE_* / REPORTED_* column renaming applied.
#'
#' @examples
#' \dontrun{
#' df <- morie_tps_load_dataset("Assault", nrows = 1000L)
#' }
#'
#' @export
morie_tps_load_dataset <- function(name,
                                   path = NULL,
                                   csv_filename = NULL,
                                   nrows = NULL) {
  canonical <- .morie_tps_canonical(name)
  if (!is.null(path)) {
    p <- path
    if (dir.exists(p)) {
      cands <- list.files(p, pattern = "\\.csv$", full.names = TRUE)
      if (!length(cands)) {
        stop(sprintf("no CSV in %s", p), call. = FALSE)
      }
      p <- cands[[1L]]
    }
  } else {
    base <- file.path(morie_tps_data_dir(), canonical, "CSV")
    if (!is.null(csv_filename)) {
      p <- file.path(base, csv_filename)
    } else {
      cands <- list.files(base, pattern = "\\.csv$", full.names = TRUE)
      if (!length(cands)) {
        stop(sprintf(
          paste("TPS %s CSV not found under %s.",
                "Verify data/datasets/TPS/<Category>/CSV/",
                "has the export."),
          canonical, base
        ), call. = FALSE)
      }
      p <- cands[[1L]]
    }
  }
  if (!file.exists(p)) {
    stop(sprintf("TPS %s CSV not found at %s", canonical, p),
         call. = FALSE)
  }
  read_args <- list(file = p, stringsAsFactors = FALSE,
                    check.names = FALSE)
  if (!is.null(nrows)) read_args$nrows <- as.integer(nrows)
  df <- do.call(utils::read.csv, read_args)
  cols <- colnames(df)
  for (src in names(.morie_tps_rename_map)) {
    dst <- .morie_tps_rename_map[[src]]
    if (src %in% cols && !(dst %in% cols)) {
      cols[cols == src] <- dst
    }
  }
  colnames(df) <- cols
  df
}


#' List all TPS datasets as a `data.frame`.
#'
#' Returns one row per registered category with columns `name`,
#' `description`, and `primary_date`.
#'
#' @return A `data.frame` sorted by `name`.
#'
#' @examples
#' morie_tps_list_datasets()
#'
#' @export
morie_tps_list_datasets <- function() {
  nms <- sort(names(MORIE_TPS_REGISTRY))
  do.call(rbind, lapply(nms, function(n) {
    r <- MORIE_TPS_REGISTRY[[n]]
    data.frame(
      name = n,
      description = r$description,
      primary_date = r$primary_date,
      stringsAsFactors = FALSE
    )
  }))
}
