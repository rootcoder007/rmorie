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
#' @return A logical scalar.
#' @examples
#' morie_tps_data_dir()
#' @export
morie_tps_data_dir <- function() {
  # Env override wins.
  env <- Sys.getenv("MORIE_TPS_DATA_DIR", unset = NA_character_)
  if (!is.na(env) && nzchar(env)) {
    return(normalizePath(env, mustWork = FALSE))
  }
  # Resolve from the project root (robust: here::here() / a DESCRIPTION or
  # pyproject.toml marker walk) rather than a fixed number of parent hops
  # from the install directory -- a hop count does not hold across install
  # layouts (the same class of bug the Python side carried).
  root <- tryCatch(.morie_project_root(), error = function(e) NA_character_)
  if (!is.na(root) && nzchar(root)) {
    return(normalizePath(file.path(root, "data", "datasets", "TPS"),
                         winslash = "/", mustWork = FALSE))
  }
  # Fallback: relative to the working directory (the analyst runs from the
  # data root). mustWork = FALSE so callers can file.exists()-check and
  # fall back to an explicit path.
  normalizePath(file.path("data", "datasets", "TPS"),
                winslash = "/", mustWork = FALSE)
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

# Internal: bundled-sample fallback for a fresh box with no local TPS
# cache. Looks for tps_<category>_sample.csv in rmorie, then in the
# rmoriedata companion package (the canonical data holder).
#' Internal helper: Morie Tps Sample Fallback
#' @noRd
.morie_tps_sample_fallback <- function(canonical, nrows = NULL) {
  key <- tolower(canonical)
  fnames <- c(sprintf("tps_%s_sample.csv", key),
              sprintf("tps_psdp_%s_sample.csv", key))
  smp <- ""
  for (fname in fnames) {
    smp <- system.file("extdata", fname, package = "rmorie")
    if (nzchar(smp)) break
    if (requireNamespace("rmoriedata", quietly = TRUE)) {
      smp <- system.file("extdata", fname, package = "rmoriedata")
      if (nzchar(smp)) break
    }
  }
  if (!nzchar(smp)) return(NULL)
  message(
    "morie_tps: no local TPS cache for '", canonical, "'; using the ",
    "bundled sample from ",
    if (grepl("rmoriedata", smp, fixed = TRUE)) "rmoriedata" else "rmorie",
    ". Fetch the full export with morie_tps_fetch_category()."
  )
  df <- utils::read.csv(smp, stringsAsFactors = FALSE, check.names = FALSE)
  .morie_tps_apply_nrows(df, nrows)
}

#' .morie_tps_canonical
#'
#' Part of the tps_datasets implementation; see the file header for the
#' source it follows.
#'
#' @param name See Usage.
#' @return The value of \code{[[}.
#' @export
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
#' \donttest{
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
        fb <- .morie_tps_sample_fallback(canonical, nrows)
        if (!is.null(fb)) return(fb)
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
