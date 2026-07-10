# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3GGG1-5: bulk catalog snapshots harvested live from each
# portal's catalog API, then included as inst/extdata CSVs for
# offline-first cross-portal discovery.
#
# Sizes (approximate, as of 2026-05-24):
#   NYC OpenData      -> 2851 entities (2395 datasets + 294 maps + 162 others)
#   Chicago Open Data -> 1856 entities
#   Toronto Open Data ->  540 packages (CKAN)
#   Calgary Open Data ->  933 entities (Socrata)
#   Edmonton Open Data-> 2027 entities (Socrata)
#   Ottawa Open Data  ->  287 datasets (ArcGIS Hub)
#
# Each loader returns a uniform 7-column data.frame:
#   id, title, type, description, updated_at, page_views_total, domain_category
# (CKAN/Hub variants drop columns that don't map cleanly.)

#' NYC OpenData bulk catalog (2851 entities)
#' @param offline If `TRUE` (default), reads included CSV.
#' @return Tabular catalog snapshot.
#' @export
morie_datasets_nyc_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("nyc_opendata_bulk_catalog.csv", offline)
}

#' Chicago Open Data bulk catalog (1856 entities)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_chicago_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("chicago_opendata_bulk_catalog.csv", offline)
}

#' Toronto Open Data bulk CKAN catalog (540 packages)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_toronto_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("toronto_opendata_bulk_catalog.csv", offline)
}

#' Calgary Open Data bulk catalog (933 entities)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_calgary_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("calgary_opendata_bulk_catalog.csv", offline)
}

#' Edmonton Open Data bulk catalog (2027 entities)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_edmonton_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("edmonton_opendata_bulk_catalog.csv", offline)
}

#' Ottawa Open Data bulk ArcGIS Hub catalog (287 datasets)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_ottawa_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("ottawa_opendata_bulk_catalog.csv", offline)
}

#' Montreal Open Data bulk CKAN catalog (401 packages, 3HHH1)
#'
#' Phase 3HHH1. Included snapshot of every CKAN package on
#' donnees.montreal.ca -- substantially broader than the 23-row
#' Loi/Justice/Securite subset from 3EEE1.
#'
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_montreal_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("montreal_opendata_bulk_catalog.csv", offline)
}

#' Vancouver Open Data bulk Opendatasoft v2.1 catalog (190 datasets, 3HHH2)
#'
#' Phase 3HHH2. Included snapshot of every dataset on
#' opendata.vancouver.ca with richer schema (publisher, theme,
#' license, records_count).
#'
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @return A \code{data.frame}.
#' @export
morie_datasets_vancouver_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("vancouver_opendata_bulk_catalog.csv", offline)
}

# ---------------------------------------------------------------------------
# Generic Socrata-by-id dispatchers for NYC + Chicago
# (mirror morie_datasets_{calgary,edmonton}_socrata_by_id from 3FFF3)
# ---------------------------------------------------------------------------

#' Fetch a NYC OpenData Socrata dataset by ID
#' @param soda_id 4-4 Socrata resource ID.
#' @param limit Page size.
#' @return A `data.frame` of records.
#' @export
morie_datasets_nyc_socrata_by_id <- function(soda_id,
                                               limit = 1000L) {
  url <- sprintf("https://data.cityofnewyork.us/resource/%s.json?$limit=%d",
                  soda_id, as.integer(limit))
  df <- .morie_dataset_http_json(url)
  for (j in rev(seq_along(df)))
    if (is.list(df[[j]])) df[[j]] <- NULL
  df
}

#' Fetch a Chicago Open Data Socrata dataset by ID
#' @rdname morie_datasets_nyc_socrata_by_id
#' @return A \code{data.frame} of the requested dataset (a 0-row typed frame when the data is unavailable offline).
#' @export
morie_datasets_chicago_socrata_by_id <- function(soda_id,
                                                   limit = 1000L) {
  url <- sprintf("https://data.cityofchicago.org/resource/%s.json?$limit=%d",
                  soda_id, as.integer(limit))
  df <- .morie_dataset_http_json(url)
  for (j in rev(seq_along(df)))
    if (is.list(df[[j]])) df[[j]] <- NULL
  df
}

#' Internal helper: Morie Bulk Fixture
#' @noRd
.morie_bulk_fixture <- function(fname, offline) {
  if (!isTRUE(offline)) {
    stop(sprintf(paste0(
      "Live re-harvest of '%s' is not implemented as a public API. ",
      "Use the bundled snapshot via offline = TRUE; ",
      "or call the underlying Socrata/CKAN/Hub catalog endpoint directly."),
      fname),
      call. = FALSE)
  }
  # Look in rmorie first (tiny CSVs ship here), then rmoriedata
  # (heavy bulk catalogs ship there). Return an empty data.frame on
  # miss so downstream consumers can cleanly concat zero-row frames.
  path <- system.file("extdata", fname, package = "rmorie")
  if (!nzchar(path) && requireNamespace("rmoriedata", quietly = TRUE)) {
    path <- system.file("extdata", fname, package = "rmoriedata")
  }
  if (!nzchar(path)) {
    warning(sprintf(
      "bulk catalog '%s' not bundled; returning empty data.frame. %s",
      fname,
      "Install the rmoriedata companion: remotes::install_github('rootcoder007/rmoriedata')"),
      call. = FALSE)
    return(data.frame())
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
