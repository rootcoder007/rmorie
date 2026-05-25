# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3EEE2: Toronto Open Data (open.toronto.ca) CKAN loaders
# beyond the TPS-specific ArcGIS Hub layer.
#
# open.toronto.ca runs CKAN at the upstream:
#   Base URL: https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3
#
# Coverage focus: crime-adjacent civic context that TPS Hub doesn't
# publish -- ambulance / fire / 311 / by-law / parking / traffic
# collision datasets owned by other City departments.

.MORIE_TORONTO_CKAN_BASE <-
  "https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3"

#' Toronto Open Data crime-adjacent CKAN catalog
#'
#' Phase 3EEE2. Bundled snapshot of 208 City-of-Toronto CKAN
#' packages matched on crime-adjacent keywords (311, fire, police,
#' ambulance, parking, traffic collision, by-law, emergency, crime,
#' wellbeing). Each row identifies a package by its CKAN slug;
#' callers fetch records via
#' [morie_datasets_toronto_open_ckan_resource()] or visit the
#' open.toronto.ca dataset page.
#'
#' @param offline If `TRUE` (default), reads the bundled CSV.
#' @return A `data.frame` with `package_name`, `title`,
#'   `num_resources`, `metadata_modified`, `search_keyword`.
#' @export
morie_datasets_toronto_open_crime_adjacent_layers <- function(offline = TRUE) {
  if (isTRUE(offline)) {
    path <- system.file("extdata",
                         "toronto_opendata_crime_adjacent_catalog.csv",
                         package = "morie")
    if (!nzchar(path))
      stop("bundled Toronto crime-adjacent catalog missing", call. = FALSE)
    return(utils::read.csv(path, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  # Live mode: re-paginate the search keywords.
  kws <- c("311", "fire", "police", "ambulance", "parking",
            "traffic collision", "by-law", "emergency", "crime",
            "wellbeing")
  rows <- list()
  for (q in kws) {
    url <- sprintf("%s/action/package_search?q=%s&rows=50",
                    .MORIE_TORONTO_CKAN_BASE, utils::URLencode(q))
    r <- .morie_dataset_http_json(url)
    res <- r$result$results
    if (length(res) == 0) next
    rows[[length(rows) + 1L]] <- data.frame(
      package_name = res$name, title = res$title,
      num_resources = res$num_resources,
      metadata_modified = res$metadata_modified,
      search_keyword = q,
      stringsAsFactors = FALSE)
  }
  if (length(rows) == 0L) return(data.frame())
  out <- do.call(rbind, rows)
  out[!duplicated(out$package_name), ]
}

#' Toronto Ambulance station locations
#'
#' Phase 3EEE2. Bundled snapshot of `ambulance-station-locations`
#' (46 EMS stations across Toronto). Useful as a control overlay
#' for crime + EMS dispatch analyses.
#'
#' @param offline If `TRUE` (default), reads bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with full station address + EMS metadata.
#' @export
morie_datasets_toronto_ambulance_stations <- function(offline = TRUE,
                                                        max_features = NULL) {
  if (offline) {
    path <- system.file("extdata", "toronto_ambulance_stations.csv",
                        package = "morie")
    if (!nzchar(path))
      stop("bundled TO ambulance stations missing", call. = FALSE)
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
  } else {
    df <- morie_datasets_toronto_open_ckan_resource(
      "5e55700d-dc9e-4053-b530-f9c918ecf1df",
      limit = 100L)
  }
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' TPS Annual Statistical Report -- Miscellaneous data (aggregated)
#'
#' Phase 3EEE2. Bundled snapshot of
#' `police-annual-statistical-report-miscellaneous-data` -- 40 rows
#' of year x section x category x subtype aggregates covering hate
#' crime counts, IMPACT calls, and other Toronto Police aggregates
#' that aren't in the per-incident ArcGIS Hub layers.
#'
#' @param offline If `TRUE` (default), reads bundled CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with `YEAR`, `SECTION`, `CATEGORY`,
#'   `SUBTYPE`, `COUNT_`.
#' @export
morie_datasets_toronto_asr_miscellaneous <- function(offline = TRUE,
                                                       max_features = NULL) {
  if (offline) {
    path <- system.file("extdata", "toronto_asr_miscellaneous.csv",
                        package = "morie")
    if (!nzchar(path))
      stop("bundled TO ASR misc data missing", call. = FALSE)
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
  } else {
    df <- morie_datasets_toronto_open_ckan_resource(
      "7457b070-73e7-4621-94b2-f77a886d799d",
      limit = 100L)
  }
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' Fetch records from any open.toronto.ca CKAN resource
#'
#' Phase 3EEE2. Generic loader hitting CKAN's `datastore_search`
#' endpoint for an arbitrary Toronto package resource.
#'
#' @param resource_id CKAN resource UUID (from `package_show`).
#' @param limit Page size (max 32000 per CKAN; sane default 100).
#' @return A `data.frame` of records.
#' @export
morie_datasets_toronto_open_ckan_resource <- function(resource_id,
                                                        limit = 100L) {
  url <- sprintf("%s/action/datastore_search?resource_id=%s&limit=%d",
                  .MORIE_TORONTO_CKAN_BASE,
                  resource_id, as.integer(limit))
  r <- .morie_dataset_http_json(url)
  if (!isTRUE(r$success))
    stop("Toronto CKAN datastore_search failed", call. = FALSE)
  if (is.null(r$result$records) || length(r$result$records) == 0L)
    return(data.frame())
  r$result$records
}
