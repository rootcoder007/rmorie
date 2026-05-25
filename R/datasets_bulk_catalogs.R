# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3GGG1-5: bulk catalog snapshots harvested live from each
# portal's catalog API, then bundled as inst/extdata CSVs for
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
#' @param offline If `TRUE` (default), reads bundled CSV.
#' @return Tabular catalog snapshot.
#' @export
morie_datasets_nyc_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("nyc_opendata_bulk_catalog.csv", offline)
}

#' Chicago Open Data bulk catalog (1856 entities)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @export
morie_datasets_chicago_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("chicago_opendata_bulk_catalog.csv", offline)
}

#' Toronto Open Data bulk CKAN catalog (540 packages)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @export
morie_datasets_toronto_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("toronto_opendata_bulk_catalog.csv", offline)
}

#' Calgary Open Data bulk catalog (933 entities)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @export
morie_datasets_calgary_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("calgary_opendata_bulk_catalog.csv", offline)
}

#' Edmonton Open Data bulk catalog (2027 entities)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @export
morie_datasets_edmonton_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("edmonton_opendata_bulk_catalog.csv", offline)
}

#' Ottawa Open Data bulk ArcGIS Hub catalog (287 datasets)
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @export
morie_datasets_ottawa_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("ottawa_opendata_bulk_catalog.csv", offline)
}

#' Montreal Open Data bulk CKAN catalog (401 packages, 3HHH1)
#'
#' Phase 3HHH1. Bundled snapshot of every CKAN package on
#' donnees.montreal.ca -- substantially broader than the 23-row
#' Loi/Justice/Securite subset from 3EEE1.
#'
#' @rdname morie_datasets_nyc_opendata_bulk_layers
#' @export
morie_datasets_montreal_opendata_bulk_layers <- function(offline = TRUE) {
  .morie_bulk_fixture("montreal_opendata_bulk_catalog.csv", offline)
}

#' Vancouver Open Data bulk Opendatasoft v2.1 catalog (190 datasets, 3HHH2)
#'
#' Phase 3HHH2. Bundled snapshot of every dataset on
#' opendata.vancouver.ca with richer schema (publisher, theme,
#' license, records_count).
#'
#' @rdname morie_datasets_nyc_opendata_bulk_layers
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

.morie_bulk_fixture <- function(fname, offline) {
  if (!isTRUE(offline)) {
    stop(sprintf(paste0(
      "Live re-harvest of '%s' is not implemented as a public API. ",
      "Use the bundled snapshot via offline = TRUE; ",
      "or call the underlying Socrata/CKAN/Hub catalog endpoint directly."),
      fname),
      call. = FALSE)
  }
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path))
    stop(sprintf("bundled bulk catalog missing: %s", fname),
          call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
