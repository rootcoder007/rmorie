# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3CCC4: Vancouver Open Data (Opendatasoft v2.1) loader.
#
# opendata.vancouver.ca runs Opendatasoft's Explore API v2.1 (a
# different stack than Socrata's SODA2/SODA3 or ArcGIS Hub). API
# shape:
#
#   * Catalog list:
#       /api/explore/v2.1/catalog/datasets?limit=100&offset=0
#   * Per-dataset records:
#       /api/explore/v2.1/catalog/datasets/<id>/records?limit=N
#   * Per-dataset exports (CSV/GeoJSON):
#       /api/explore/v2.1/catalog/datasets/<id>/exports/csv
#       /api/explore/v2.1/catalog/datasets/<id>/exports/geojson
#
# NOTE: as of 2026-05-24, opendata.vancouver.ca does NOT host VPD
# crime/incident data. VPD publishes that separately on the
# vpd-crimedata Open Data Portal at geodash.vpd.ca. This loader
# covers the City-of-Vancouver civic datasets (190 datasets:
# property, transit, parks, infrastructure, budgets, etc.) which
# are useful for context joins on the morie geographic side.

.MORIE_VANCOUVER_OPENDATA_BASE <-
  "https://opendata.vancouver.ca/api/explore/v2.1"

#' Vancouver Open Data full dataset catalog (Opendatasoft v2.1)
#'
#' Phase 3CCC4. Bundled snapshot of every City-of-Vancouver dataset
#' published on opendata.vancouver.ca (190 datasets as of
#' 2026-05-24). Each row identifies a dataset by its Opendatasoft
#' `dataset_id` slug (used as the URL path segment for records /
#' exports endpoints).
#'
#' @param offline If `TRUE` (default), reads the bundled CSV; if
#'   `FALSE`, paginates the live catalog endpoint.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `dataset_id`, `title`, `publisher`,
#'   `records_count`.
#' @references Opendatasoft Explore API v2.1,
#'   \url{https://opendata.vancouver.ca/api-console/explore/v2.1/}.
#' @examples
#' cat_df <- morie_datasets_vancouver_opendata_layers(offline = TRUE)
#' nrow(cat_df)  # 190
#' head(cat_df$title)
#' @export
morie_datasets_vancouver_opendata_layers <- function(offline = TRUE,
                                                       max_features = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "vancouver_opendata_catalog.csv",
                        package = "morie")
    if (!nzchar(path))
      stop("bundled Vancouver Open Data catalog fixture missing",
           call. = FALSE)
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
  } else {
    # Paginate via offset (Opendatasoft max limit=100/req).
    all_rows <- list()
    offset <- 0L
    repeat {
      url <- sprintf(paste0("%s/catalog/datasets?limit=100",
                              "&select=dataset_id,title,publisher,records_count",
                              "&offset=%d&order_by=dataset_id"),
                       .MORIE_VANCOUVER_OPENDATA_BASE, offset)
      r <- .morie_dataset_http_json(url)
      if (is.null(r$results) || length(r$results) == 0L) break
      all_rows[[length(all_rows) + 1L]] <- r$results
      offset <- offset + nrow(r$results)
      if (!is.null(r$total_count) && offset >= r$total_count) break
    }
    df <- if (length(all_rows)) do.call(rbind, all_rows) else data.frame()
  }
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Bundled Vancouver Open Data crime-adjacent civic datasets
#'
#' Phase 3DDD1. Five small fixtures harvested live from
#' opendata.vancouver.ca for offline reproducibility -- chosen to
#' surface neighbourhood-level civic context useful in carceral /
#' policing analysis even though VPD itself publishes crime data
#' separately (see [morie_datasets_vpd_crime()]).
#'
#' \tabular{lll}{
#'   \strong{Loader}      \tab \strong{Dataset slug}     \tab \strong{Rows} \cr
#'   `morie_datasets_vancouver_graffiti()` \tab `graffiti` \tab 100 (of 7683) \cr
#'   `morie_datasets_vancouver_noise_control_areas()` \tab `noise-control-areas` \tab 3 \cr
#'   `morie_datasets_vancouver_homeless_shelters()` \tab `homeless-shelter-locations` \tab 17 \cr
#'   `morie_datasets_vancouver_property_use_inspection_districts()` \tab `property-use-inspection-districts` \tab 23 \cr
#'   `morie_datasets_vancouver_fire_halls()` \tab `fire-halls` \tab 20 \cr
#' }
#'
#' All loaders accept the same `offline = TRUE` (default) /
#' `max_features` interface as the other morie dataset wrappers.
#'
#' @name vancouver_crime_adjacent
NULL

.morie_vancouver_fixture <- function(fname) {
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path))
    stop(sprintf("bundled Vancouver fixture missing: %s", fname),
         call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Vancouver graffiti incident records (sample)
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_graffiti <- function(offline = TRUE,
                                                max_features = NULL) {
  if (offline) {
    df <- .morie_vancouver_fixture("vancouver_graffiti_sample.csv")
  } else {
    df <- morie_datasets_vancouver_opendata_by_id("graffiti",
                                                    limit = 100L,
                                                    format = "json")
    if ("geo_point_2d" %in% names(df)) {
      df$lon <- df$geo_point_2d$lon
      df$lat <- df$geo_point_2d$lat
      df$geo_point_2d <- NULL
    }
    df$geom <- NULL
  }
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver noise control areas (bylaw zones)
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_noise_control_areas <- function(offline = TRUE,
                                                            max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_noise_control_areas.csv")
  else morie_datasets_vancouver_opendata_by_id("noise-control-areas",
                                                  limit = 10L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver homeless shelter locations
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_homeless_shelters <- function(offline = TRUE,
                                                         max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_homeless_shelters.csv")
  else morie_datasets_vancouver_opendata_by_id("homeless-shelter-locations",
                                                  limit = 50L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver property use inspection districts
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_property_use_inspection_districts <- function(
    offline = TRUE, max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture(
      "vancouver_property_use_inspection_districts.csv")
  else morie_datasets_vancouver_opendata_by_id(
    "property-use-inspection-districts", limit = 50L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver fire hall locations
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_fire_halls <- function(offline = TRUE,
                                                  max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_fire_halls.csv")
  else morie_datasets_vancouver_opendata_by_id("fire-halls",
                                                  limit = 50L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

# ---------------------------------------------------------------------------
# Phase 3EEE3 -- 4 more bundled Vancouver fixtures
# ---------------------------------------------------------------------------

#' Vancouver community centre locations
#'
#' Phase 3EEE3. Bundled 27-row snapshot of City-run community
#' centres. Useful as an "anchor institutions" overlay for analyses
#' of neighbourhood-level crime + social-service access.
#'
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_community_centres <- function(offline = TRUE,
                                                         max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_community_centres.csv")
  else morie_datasets_vancouver_opendata_by_id("community-centres",
                                                  limit = 50L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver community food markets and farmers markets
#'
#' Phase 3EEE3. Bundled 91-row snapshot of community + farmers
#' markets across Vancouver. Useful for food-access / quality-of-life
#' overlays.
#'
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_community_food_markets <- function(offline = TRUE,
                                                              max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_community_food_markets.csv")
  else morie_datasets_vancouver_opendata_by_id(
    "community-food-markets-and-farmers-markets", limit = 100L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver designated disability parking spaces
#'
#' Phase 3EEE3. Bundled 100-row sample of designated disability
#' parking locations across Vancouver (out of 159 total).
#'
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_disability_parking <- function(offline = TRUE,
                                                          max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_disability_parking.csv")
  else morie_datasets_vancouver_opendata_by_id("disability-parking",
                                                  limit = 100L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Vancouver public art registry
#'
#' Phase 3EEE3. Bundled 100-row sample of Vancouver's public art
#' registry (out of 747 total) -- artist, install year,
#' neighbourhood, primary material. Useful as a CPTED-style
#' "place-making" overlay variable.
#'
#' @rdname vancouver_crime_adjacent
#' @inheritParams morie_datasets_vancouver_opendata_layers
#' @export
morie_datasets_vancouver_public_art <- function(offline = TRUE,
                                                  max_features = NULL) {
  df <- if (offline)
    .morie_vancouver_fixture("vancouver_public_art.csv")
  else morie_datasets_vancouver_opendata_by_id("public-art",
                                                  limit = 100L)
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Fetch records from a Vancouver Open Data dataset by ID
#'
#' Phase 3CCC4. Hits the Opendatasoft v2.1 `/records` endpoint for
#' an arbitrary Vancouver dataset slug. Returns the `results` array
#' as a `data.frame`. For larger pulls, use `format = "csv"` to hit
#' the unrestricted `/exports/csv` endpoint instead.
#'
#' @param dataset_id Opendatasoft dataset slug (from
#'   [morie_datasets_vancouver_opendata_layers()]).
#' @param limit Page size (max 100 for `/records`).
#' @param format One of `"json"` (default, `/records` endpoint) or
#'   `"csv"` (`/exports/csv` endpoint, no row limit).
#' @return A `data.frame` of records.
#' @examples
#' \dontrun{
#' df <- morie_datasets_vancouver_opendata_by_id("non-market-housing",
#'                                                  limit = 50)
#' nrow(df)
#' }
#' @export
morie_datasets_vancouver_opendata_by_id <- function(dataset_id,
                                                      limit = 100L,
                                                      format = c("json", "csv")) {
  format <- match.arg(format)
  if (format == "json") {
    url <- sprintf("%s/catalog/datasets/%s/records?limit=%d",
                    .MORIE_VANCOUVER_OPENDATA_BASE,
                    dataset_id, as.integer(limit))
    r <- .morie_dataset_http_json(url)
    if (is.null(r$results)) return(data.frame())
    return(r$results)
  }
  # CSV export
  url <- sprintf("%s/catalog/datasets/%s/exports/csv",
                  .MORIE_VANCOUVER_OPENDATA_BASE, dataset_id)
  text <- .morie_dataset_http_text(url)
  utils::read.csv(text = text, sep = ";", stringsAsFactors = FALSE,
                  check.names = FALSE)
}
