# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3EEE1: Montreal Open Data (donnees.montreal.ca) CKAN
# loader -- "Loi, justice et securite publique" group (23 datasets).
#
# Donnees Montreal runs CKAN 2.x at:
#   Base URL: https://donnees.montreal.ca/api/3/
#   Endpoints:
#     /action/group_show?id=loi-justice-securite-publique
#     /action/package_search?fq=groups:loi-justice-securite-publique
#     /action/package_show?id=<package>
#     /action/datastore_search?resource_id=<id> (when active)
#
# All datasets carry CC-BY 4.0 license and are bilingual EN/FR
# (titles + notes in French; column codes typically French
# abbreviations like NOM_ARROND, CASERNE, DESCRIPTION_GROUPE).
#
# Flagship: SIM (Service de securite incendie de Montreal)
# fire + EMS interventions (172,899 records, 2005-2026).
# Categories (DESCRIPTION_GROUPE):
#   * 1-REPOND          (first responder / EMS)
#   * SANS FEU          (non-fire incidents)
#   * Alarmes-incendies (fire alarms)
#   * AUTREFEU          (other fires)
#   * INCENDIE          (structural fires)
#   * FAU-ALER          (false alerts)
#   * NOUVEAU           (new categorization)

.MORIE_MONTREAL_CKAN_BASE <- "https://donnees.montreal.ca/api/3"

#' Donnees Montreal "Loi, justice et securite publique" catalog
#'
#' Phase 3EEE1. Bundled 23-row snapshot of every CKAN package in
#' the Law / Justice / Public Safety group on donnees.montreal.ca.
#' Includes the SIM fire/EMS interventions dataset, SPVM police
#' station boundaries, municipal regulations, traffic collisions,
#' and ~20 others.
#'
#' @param offline If `TRUE` (default), reads the bundled CSV; if
#'   `FALSE`, hits `/action/package_search` live.
#' @return A `data.frame` with `package_name`, `title`,
#'   `num_resources`, `metadata_modified`, `language`, `license`.
#' @examples
#' cat_df <- morie_datasets_montreal_justice_safety_layers()
#' nrow(cat_df)  # 23
#' head(cat_df$title)
#' @export
morie_datasets_montreal_justice_safety_layers <- function(offline = TRUE) {
  if (isTRUE(offline)) {
    path <- system.file("extdata",
                         "montreal_justice_safety_catalog.csv",
                         package = "morie")
    if (!nzchar(path))
      stop("bundled MTL justice/safety catalog missing", call. = FALSE)
    return(utils::read.csv(path, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  url <- paste0(.MORIE_MONTREAL_CKAN_BASE,
                "/action/package_search?fq=groups:loi-justice-securite-publique",
                "&rows=50")
  r <- .morie_dataset_http_json(url)
  if (!isTRUE(r$success)) stop("MTL CKAN package_search failed", call. = FALSE)
  res <- r$result$results
  data.frame(
    package_name = res$name,
    title = res$title,
    num_resources = res$num_resources,
    metadata_modified = res$metadata_modified,
    language = res$language %||% NA_character_,
    license = res$license_id %||% NA_character_,
    stringsAsFactors = FALSE)
}

#' SIM Montreal Fire Service intervention records (sample)
#'
#' Phase 3EEE1. Bundled stratified 349-row sample (50 rows per
#' DESCRIPTION_GROUPE category) of SIM (Service de securite
#' incendie de Montreal) interventions, drawn from the full
#' 172,899-row open feed for years 2005-2026.
#'
#' Three source modes:
#'
#' \describe{
#'   \item{`offline = TRUE` (default)}{Bundled 349-row sample for
#'     tests + intro examples.}
#'   \item{`csv_path = "..."`}{Reads a user-downloaded
#'     `donneesouvertes-interventions-sim.csv` (or yearly variant)
#'     from the CKAN resource link.}
#' }
#'
#' Columns (13):
#'   INCIDENT_NBR (per-year incident id), CREATION_DATE_TIME,
#'   INCIDENT_TYPE_DESC, DESCRIPTION_GROUPE, CASERNE (fire-hall
#'   number), NOM_VILLE, NOM_ARROND (arrondissement), DIVISION,
#'   NOMBRE_UNITES (vehicles deployed), MTM8_X, MTM8_Y (Quebec
#'   MTM zone 8 NAD83 / EPSG:32188), LONGITUDE, LATITUDE (WGS84,
#'   obfuscated to intersections per privacy policy).
#'
#' @param offline If `TRUE` (default), reads the bundled sample.
#' @param csv_path Optional path to a user-downloaded full CSV.
#' @param max_features Optional row cap.
#' @return A `data.frame` with the 13 SIM columns.
#' @references CKAN package
#'   `interventions-service-securite-incendie-montreal`,
#'   \url{https://donnees.montreal.ca/dataset/interventions-service-securite-incendie-montreal}.
#' @examples
#' df <- morie_datasets_montreal_sim_interventions(offline = TRUE)
#' nrow(df)              # 349
#' table(df$DESCRIPTION_GROUPE)
#' @export
morie_datasets_montreal_sim_interventions <- function(offline = TRUE,
                                                         csv_path = NULL,
                                                         max_features = NULL) {
  if (!is.null(csv_path)) {
    if (!file.exists(csv_path))
      stop(sprintf("SIM CSV not found: %s", csv_path), call. = FALSE)
    df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  } else if (offline) {
    path <- system.file("extdata",
                         "montreal_sim_interventions_sample.csv",
                         package = "morie")
    if (!nzchar(path))
      stop("bundled SIM interventions sample missing", call. = FALSE)
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    stop("MTL Open Data CKAN's SIM resource is large (170k+ rows). ",
         "Either set offline = TRUE (bundled sample) or download ",
         "from https://donnees.montreal.ca/dataset/interventions-service-securite-incendie-montreal ",
         "and pass csv_path = '...'.",
         call. = FALSE)
  }
  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

#' SIM intervention TYPE -> French description dictionary
#'
#' Phase 3EEE1. Bundled lookup table mapping the
#' `INCIDENT_TYPE_DESC` codes used in SIM interventions to their
#' canonical French descriptions (from the dataset's own
#' `type-interventions-descriptions20161122.csv` sidecar).
#'
#' @return A `data.frame` with `INCIDENT_TYPE_DESCRIPTION` +
#'   `Description`.
#' @examples
#' d <- morie_datasets_montreal_sim_intervention_types()
#' nrow(d)
#' head(d)
#' @export
morie_datasets_montreal_sim_intervention_types <- function() {
  path <- system.file("extdata",
                       "montreal_sim_intervention_types.csv",
                       package = "morie")
  if (!nzchar(path))
    stop("bundled SIM intervention types fixture missing",
         call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Fetch records from any donnees.montreal.ca CKAN datastore resource
#'
#' Phase 3EEE1. Generic loader that hits CKAN's `datastore_search`
#' endpoint for a given `resource_id`. Useful for any MTL package
#' beyond the bundled SIM sample.
#'
#' @param resource_id CKAN resource UUID (from `package_show`).
#' @param limit Page size (CKAN default 100, max varies by host).
#' @param filters Optional named list of `{column: value}` filters.
#' @return A `data.frame` of records.
#' @examples
#' \dontrun{
#' # Hypothetical SPVM station boundaries:
#' df <- morie_datasets_montreal_ckan_resource(
#'   resource_id = "abc-def-...",
#'   limit = 50)
#' }
#' @export
morie_datasets_montreal_ckan_resource <- function(resource_id,
                                                    limit = 100L,
                                                    filters = NULL) {
  qs <- list(resource_id = resource_id,
              limit = as.integer(limit))
  if (!is.null(filters))
    qs$filters <- jsonlite::toJSON(filters, auto_unbox = TRUE)
  url <- paste0(.MORIE_MONTREAL_CKAN_BASE, "/action/datastore_search")
  r <- .morie_dataset_http_json(url, query = qs)
  if (!isTRUE(r$success))
    stop("MTL CKAN datastore_search failed", call. = FALSE)
  if (is.null(r$result$records) || length(r$result$records) == 0L)
    return(data.frame())
  r$result$records
}
