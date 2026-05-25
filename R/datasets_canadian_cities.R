# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3FFF3: Calgary + Ottawa + Edmonton open data loaders.
#
# Three more Canadian municipal portals, each with a different
# tech stack:
#
#   * Calgary -- Socrata at data.calgary.ca
#     Catalog API: api.us.socrata.com/api/catalog/v1?domains=data.calgary.ca
#     Records:    /resource/<id>.json
#   * Edmonton -- Socrata at data.edmonton.ca
#   * Ottawa  -- ArcGIS Hub at open.ottawa.ca
#     Catalog API: /api/search/v1/collections/dataset/items
#     Records:    via the dataset's FeatureServer /query endpoint

.MORIE_CALGARY_BASE  <- "https://data.calgary.ca"
.MORIE_EDMONTON_BASE <- "https://data.edmonton.ca"
.MORIE_OTTAWA_HUB_BASE <- "https://open.ottawa.ca"

# ---------------------------------------------------------------------------
# Catalog loaders
# ---------------------------------------------------------------------------

#' Calgary Open Data crime-adjacent catalog
#'
#' Phase 3FFF3. Bundled snapshot of 157 City-of-Calgary Socrata
#' datasets matched on crime-adjacent keywords (crime, police,
#' fire, ambulance, traffic, incident, collision, bylaw, 311).
#'
#' @param offline If `TRUE` (default), reads the bundled CSV.
#' @return A `data.frame` with `soda_id`, `title`, `type`,
#'   `search_keyword`.
#' @export
morie_datasets_calgary_open_crime_adjacent_layers <- function(offline = TRUE) {
  .morie_canadian_cat_fixture("calgary_opendata_crime_adjacent_catalog.csv",
                                offline)
}

#' Edmonton Open Data crime-adjacent catalog
#' @rdname morie_datasets_calgary_open_crime_adjacent_layers
#' @export
morie_datasets_edmonton_open_crime_adjacent_layers <- function(offline = TRUE) {
  .morie_canadian_cat_fixture("edmonton_opendata_crime_adjacent_catalog.csv",
                                offline)
}

#' Ottawa Open Data (ArcGIS Hub) crime-adjacent catalog
#' @rdname morie_datasets_calgary_open_crime_adjacent_layers
#' @export
morie_datasets_ottawa_open_crime_adjacent_layers <- function(offline = TRUE) {
  .morie_canadian_cat_fixture("ottawa_opendata_crime_adjacent_catalog.csv",
                                offline)
}

.morie_canadian_cat_fixture <- function(fname, offline) {
  if (!isTRUE(offline)) {
    stop(sprintf(paste0(
      "Live mode for '%s' not implemented in 3FFF3. Use the bundled ",
      "snapshot via offline = TRUE or call the city's Socrata/ArcGIS Hub ",
      "search API directly."), fname),
      call. = FALSE)
  }
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path))
    stop(sprintf("bundled fixture missing: %s", fname), call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# ---------------------------------------------------------------------------
# Calgary fixtures + generic dispatcher
# ---------------------------------------------------------------------------

#' Calgary Community Crime Statistics (sample)
#'
#' Phase 3FFF3. Bundled 200-row sample of Calgary's per-community
#' per-month crime counts (Socrata id `78gh-n26t`). Covers all 8
#' canonical CPS categories.
#'
#' @param offline If `TRUE` (default), reads bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `community`, `category`, `crime_count`,
#'   `year`, `month`.
#' @export
morie_datasets_calgary_community_crime_stats <- function(offline = TRUE,
                                                            max_features = NULL) {
  df <- if (offline)
    .morie_canadian_fixture("calgary_community_crime_stats_sample.csv")
  else morie_datasets_calgary_socrata_by_id("78gh-n26t", limit = 500L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Calgary Fire Response Calls (sample)
#'
#' Phase 3FFF3. Bundled 200-row sample of Calgary fire response
#' calls (Socrata id `bdez-pds9`).
#'
#' @rdname morie_datasets_calgary_community_crime_stats
#' @export
morie_datasets_calgary_fire_response_calls <- function(offline = TRUE,
                                                         max_features = NULL) {
  df <- if (offline)
    .morie_canadian_fixture("calgary_fire_response_calls_sample.csv")
  else morie_datasets_calgary_socrata_by_id("bdez-pds9", limit = 200L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Calgary Fire Stations
#' @rdname morie_datasets_calgary_community_crime_stats
#' @export
morie_datasets_calgary_fire_stations <- function(offline = TRUE,
                                                   max_features = NULL) {
  df <- if (offline)
    .morie_canadian_fixture("calgary_fire_stations.csv")
  else morie_datasets_calgary_socrata_by_id("cqsb-2hhg", limit = 100L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Fetch a Calgary Open Data Socrata dataset by ID
#'
#' Phase 3FFF3. Generic SODA2 fetch wrapper for arbitrary Calgary
#' Socrata resources.
#'
#' @param soda_id 4-4 Socrata resource ID (e.g. `"78gh-n26t"`).
#' @param limit Page size (default 1000).
#' @return A `data.frame` of records.
#' @export
morie_datasets_calgary_socrata_by_id <- function(soda_id,
                                                   limit = 1000L) {
  url <- sprintf("%s/resource/%s.json?$limit=%d",
                  .MORIE_CALGARY_BASE, soda_id, as.integer(limit))
  df <- .morie_dataset_http_json(url)
  for (j in rev(seq_along(df)))
    if (is.list(df[[j]])) df[[j]] <- NULL
  df
}

# ---------------------------------------------------------------------------
# Edmonton fixtures + generic dispatcher
# ---------------------------------------------------------------------------

#' Edmonton Police Station locations
#'
#' Phase 3FFF3. Bundled 10-row fixture of Edmonton Police Service
#' station locations (Socrata id `e7aq-scxv`).
#'
#' @param offline If `TRUE` (default), reads bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `name`, `address`, `latitude`,
#'   `longitude`.
#' @export
morie_datasets_edmonton_police_stations <- function(offline = TRUE,
                                                      max_features = NULL) {
  df <- if (offline)
    .morie_canadian_fixture("edmonton_police_stations.csv")
  else morie_datasets_edmonton_socrata_by_id("e7aq-scxv", limit = 50L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Edmonton Fire Station locations
#' @rdname morie_datasets_edmonton_police_stations
#' @export
morie_datasets_edmonton_fire_stations <- function(offline = TRUE,
                                                    max_features = NULL) {
  df <- if (offline)
    .morie_canadian_fixture("edmonton_fire_stations.csv")
  else morie_datasets_edmonton_socrata_by_id("b4y7-zhnz", limit = 50L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Fetch an Edmonton Open Data Socrata dataset by ID
#' @rdname morie_datasets_calgary_socrata_by_id
#' @param soda_id 4-4 Socrata resource ID.
#' @param limit Page size (default 1000).
#' @export
morie_datasets_edmonton_socrata_by_id <- function(soda_id,
                                                    limit = 1000L) {
  url <- sprintf("%s/resource/%s.json?$limit=%d",
                  .MORIE_EDMONTON_BASE, soda_id, as.integer(limit))
  df <- .morie_dataset_http_json(url)
  for (j in rev(seq_along(df)))
    if (is.list(df[[j]])) df[[j]] <- NULL
  df
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.morie_canadian_fixture <- function(fname) {
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path))
    stop(sprintf("bundled fixture missing: %s", fname), call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}
