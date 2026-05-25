# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Three high-level dataset loaders that mirror the canonical
# upstream feeds for our three highest-stakes data sources:
#
#   1. ARSAU UoF main_records  -- Ontario Police Use of Force,
#        Race-Based Data Strategy
#        https://data.ontario.ca/dataset/police-use-of-force-race-based-data
#        Resource:
#          ea9dc29c-b4f1-4426-b1f2-974ce995aca1
#        Direct CSV:
#          .../resource/ea9dc29c-b4f1-4426-b1f2-974ce995aca1/download/
#            uof_main_records.csv
#        Datastore dump JSON:
#          https://data.ontario.ca/datastore/dump/
#            ea9dc29c-b4f1-4426-b1f2-974ce995aca1?format=json
#
#   2. OTIS d01 Deaths-in-Custody  -- Ministry of the Solicitor General
#        Data on Inmates in Ontario
#        https://data.ontario.ca/dataset/data-on-inmates-in-ontario
#        Resource:
#          89e3b63f-5679-4fa4-b98a-fdd2dc486f29
#        Direct CSV:
#          .../resource/89e3b63f-5679-4fa4-b98a-fdd2dc486f29/download/
#            d01_deaths_in_custody_detailed_dataset.csv
#        Datastore dump JSON:
#          https://data.ontario.ca/datastore/dump/
#            89e3b63f-5679-4fa4-b98a-fdd2dc486f29?format=json
#
#   3. TPS Mental Health Act Apprehensions  -- TPS Public Safety
#        Data Portal (PSDP)
#        https://data.tps.ca/datasets/333c4e1c96314741a83425045b6a7642_0/
#          explore
#        Published as GeoPackage / Shapefile / GeoJSON / CSV / KML /
#        File Geodatabase / Feature Collection / Excel /
#        SQLite Geodatabase. We default to the ArcGIS REST
#        FeatureServer JSON for the live-mode path.
#
# Each loader supports offline = TRUE (read a small bundled synthetic
# fixture from inst/extdata/) and offline = FALSE (hit the live
# upstream via a mockable internal helper:
#
#   .morie_ontario_ckan_dump_csv()    -- for the two Ontario CKAN feeds
#   .morie_tps_psdp_feature_query()   -- for the TPS PSDP ArcGIS feed
#
# Tests stub these helpers via testthat::local_mocked_bindings(.package
# = "morie") so the suite never reaches the wire.

# ---------------------------------------------------------------------------
# Internal http helpers (mockable)
# ---------------------------------------------------------------------------

# Default Ontario CKAN portal endpoint (data.ontario.ca).
.MORIE_ONTARIO_CKAN_BASE <- "https://data.ontario.ca"

# Internal: GET the Ontario CKAN datastore_dump JSON endpoint for a
# given resource_id; return a parsed data.frame. Mock this in tests.
.morie_ontario_ckan_dump_csv <- function(resource_id, limit = 200000L) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Ontario CKAN dump fetch needs httr2 + jsonlite. ",
         "install.packages(c('httr2', 'jsonlite'))",
         call. = FALSE)
  }
  url <- sprintf("%s/datastore/dump/%s?format=json",
                 .MORIE_ONTARIO_CKAN_BASE, resource_id)
  # 3XX: routes through the shared libcurl backend (with httr2
  # fallback). 120-second timeout preserved.
  body <- .morie_dataset_http_json(url, timeout_s = 120L)
  # CKAN datastore_dump returns either {"records":[...]} or a bare array
  # depending on portal version; handle both.
  if (is.data.frame(body)) return(body)
  if (!is.null(body$records)) return(as.data.frame(body$records))
  if (!is.null(body$result$records))
    return(as.data.frame(body$result$records))
  stop("unrecognised Ontario CKAN datastore_dump payload shape",
       call. = FALSE)
}

# Default TPS Public Safety Data Portal ArcGIS host.
.MORIE_TPS_PSDP_BASE <- "https://services.arcgis.com"

# Internal: GET a TPS PSDP ArcGIS FeatureServer layer; return a
# data.frame of attributes. Mock this in tests.
.morie_tps_psdp_feature_query <- function(layer_url, where = "1=1",
                                            max_features = NULL,
                                            return_geometry = FALSE) {
  # Delegate to the existing TPS ingest helper so this stays a thin
  # wrapper that callers (and mocks) can target cleanly.
  if (exists(".morie_dataset_tps_fetch",
             envir = asNamespace("morie"), inherits = FALSE)) {
    return(.morie_dataset_tps_fetch(
      layer_url, where = where, max_features = max_features,
      return_geometry = return_geometry))
  }
  stop(".morie_dataset_tps_fetch unavailable; cannot dispatch TPS PSDP query",
       call. = FALSE)
}

# ---------------------------------------------------------------------------
# ARSAU UoF main_records
# ---------------------------------------------------------------------------

# Canonical Ontario CKAN resource ids for the ARSAU UoF main_records
# resource, by reporting year.
.MORIE_ARSAU_UOF_MAIN_RECORDS_RESOURCE_IDS <- list(
  "2023" = "94f303a2-963e-4fd1-958d-6681b310cb6d",
  "2024" = "ea9dc29c-b4f1-4426-b1f2-974ce995aca1")

#' Ontario Use-of-Force main records (one row per incident)
#'
#' Wraps the Ontario Police Use-of-Force Race-Based Data Strategy
#' resource. Offline mode reads a small bundled synthetic fixture
#' from `inst/extdata/arsau_uof_main_records_sample.csv` (5 rows in
#' the canonical 23-column subset of the 65-column upstream schema,
#' clearly stamped `SYNTHETIC-FIXTURE-XXX`). Live mode hits the
#' Ontario CKAN datastore-dump JSON endpoint for the requested
#' reporting year.
#'
#' @param year Reporting year (`"2023"` or `"2024"`). Honoured only
#'   when `offline = FALSE`.
#' @param offline If `TRUE` (default), read the bundled fixture. If
#'   `FALSE`, hit the live CKAN endpoint via httr2.
#' @param resource_id Optional CKAN resource id override.
#' @return A `data.frame`.
#' @references Ontario Open Data Catalogue, "Police Use of Force"
#'   (\url{https://data.ontario.ca/dataset/police-use-of-force-race-based-data});
#'   Open Government Licence -- Ontario.
#' @examples
#' df <- morie_datasets_arsau_uof_main_records(offline = TRUE)
#' head(df[, c("IncidentYear", "PoliceService", "IncidentType")])
#' @export
morie_datasets_arsau_uof_main_records <- function(year = "2024",
                                                    offline = TRUE,
                                                    resource_id = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "arsau_uof_main_records_sample.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled ARSAU UoF main_records fixture missing",
           call. = FALSE)
    }
    return(utils::read.csv(path, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  if (is.null(resource_id)) {
    resource_id <- .MORIE_ARSAU_UOF_MAIN_RECORDS_RESOURCE_IDS[[
      as.character(year)]]
    if (is.null(resource_id)) {
      stop(sprintf(paste0(
        "no canonical Ontario CKAN resource_id for ARSAU UoF ",
        "main_records year %s; pass resource_id= explicitly"),
        year), call. = FALSE)
    }
  }
  .morie_ontario_ckan_dump_csv(resource_id)
}

# ---------------------------------------------------------------------------
# OTIS d01 Deaths in Custody
# ---------------------------------------------------------------------------

.MORIE_OTIS_D01_RESOURCE_ID <- "89e3b63f-5679-4fa4-b98a-fdd2dc486f29"

#' OTIS Deaths-in-Custody detailed dataset (d01)
#'
#' Wraps the Ontario "Data on Inmates in Ontario" d01 resource.
#' Schema: `Year, UniqueIndividual_ID, Region_AtTimeOfDeath,
#' HousingUnit_Type, MedicalCauseofDeath, MeansofDeath`.
#'
#' @param offline If `TRUE` (default), read the bundled synthetic
#'   fixture from `inst/extdata/otis_d01_deaths_in_custody_sample.csv`
#'   (5 rows). If `FALSE`, hit the live CKAN datastore-dump JSON
#'   endpoint for resource id `89e3b63f-5679-4fa4-b98a-fdd2dc486f29`.
#' @param resource_id Optional CKAN resource id override.
#' @return A `data.frame`.
#' @references Ontario Open Data Catalogue, "Data on Inmates in
#'   Ontario"
#'   (\url{https://data.ontario.ca/dataset/data-on-inmates-in-ontario});
#'   Open Government Licence -- Ontario.
#' @examples
#' df <- morie_datasets_otis_d01_deaths_in_custody(offline = TRUE)
#' table(df$Region_AtTimeOfDeath)
#' @export
morie_datasets_otis_d01_deaths_in_custody <- function(offline = TRUE,
                                                       resource_id = NULL,
                                                       source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d01 Deaths in Custody",
    "otis_d01_deaths_in_custody_sample.csv",
    offline = offline,
    resource_id = resource_id %||% .MORIE_OTIS_D01_RESOURCE_ID,
    registry_key = "otis_d01_deaths_in_custody",
    source = source
  )
}

# ---------------------------------------------------------------------------
# TPS Mental Health Act Apprehensions (PSDP)
# ---------------------------------------------------------------------------

# Canonical ArcGIS FeatureServer URL for the TPS PSDP MHA layer
# (333c4e1c96314741a83425045b6a7642_0). Kept for backward compat
# with callers that override via `layer_url = ...`; the 3TT+
# canonical live path routes through the TPS Hub catalog hub_id
# .MORIE_TPS_PSDP_MHA_HUB_ID below.
.MORIE_TPS_PSDP_MHA_LAYER_URL <- paste0(
  "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
  "Mental_Health_Act_Apprehensions_Open_Data/FeatureServer/0")

# 3TT+ canonical TPS Hub item id for Mental Health Act Apprehensions.
# Verified live against the TPS Hub catalog API 2026-05-24.
.MORIE_TPS_PSDP_MHA_HUB_ID <- "333c4e1c96314741a83425045b6a7642"

#' TPS Mental Health Act Apprehensions (PSDP)
#'
#' Wraps the TPS Public Safety Data Portal "Mental Health Act
#' Apprehensions" layer (one row per police-attended MHA event).
#' Carries both HOOD_158 and HOOD_140 columns -- callers should pick
#' a version via [morie_tps_resolve_hood_col()] before downstream
#' analysis.
#'
#' @param year Optional reporting year filter (applies an
#'   `OCC_YEAR = <year>` WHERE clause when `offline = FALSE`).
#' @param max_features Optional cap on returned rows
#'   (`offline = FALSE` only).
#' @param offline If `TRUE` (default), read the bundled synthetic
#'   fixture from `inst/extdata/tps_mha_apprehensions_sample.csv`
#'   (5 rows in the canonical TPS PSDP 22-column schema). If
#'   `FALSE`, hit the TPS PSDP ArcGIS FeatureServer.
#' @param layer_url Optional ArcGIS layer URL override.
#' @return A `data.frame`.
#' @references TPS Public Safety Data Portal, "Mental Health Act
#'   Apprehensions Open Data"
#'   (\url{https://data.tps.ca/datasets/333c4e1c96314741a83425045b6a7642_0/explore}).
#' @examples
#' df <- morie_datasets_tps_mha_apprehensions(offline = TRUE)
#' table(df$APPREHENSION_TYPE)
#' @export
morie_datasets_tps_mha_apprehensions <- function(year = NULL,
                                                   max_features = NULL,
                                                   offline = TRUE,
                                                   layer_url = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "tps_mha_apprehensions_sample.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled TPS MHA fixture missing", call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(year) && "OCC_YEAR" %in% names(df)) {
      df <- df[df$OCC_YEAR == as.integer(year), , drop = FALSE]
      rownames(df) <- NULL
    }
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  where <- if (is.null(year)) "1=1" else sprintf("OCC_YEAR = %d",
                                                  as.integer(year))
  # Backward-compat escape hatch: if the caller passes a layer_url
  # override, hit it directly via the pre-3TT+ FeatureServer query
  # helper. Preserves the historical override behaviour.
  if (!is.null(layer_url)) {
    return(.morie_tps_psdp_feature_query(
      layer_url, where = where,
      max_features = max_features,
      return_geometry = FALSE))
  }
  # 3TT+ canonical path: route through the TPS Hub catalog by
  # hub_id. Single code path with the other 11 PSDP wrappers and
  # the 60 3TT bulk-generated wrappers.
  morie_datasets_tps_arcgis_hub_by_id(
    .MORIE_TPS_PSDP_MHA_HUB_ID,
    format = "json",
    where = where,
    max_features = max_features,
    layer_idx = 0L,
    offline = TRUE)  # offline=TRUE only means "resolve URL from
                      # the bundled catalog" -- the data query
                      # still hits the network.
}

# ---------------------------------------------------------------------------
# ARSAU UoF -- the four remaining year x record-type loaders
# ---------------------------------------------------------------------------
#
# Canonical Ontario CKAN resource ids by year x kind (extracted from
# the ARSAU sidecar JSONs the morie repo already ships). 2023|
# weapon_records is INVALID per the ministry's technical report and
# has no published resource; we leave the slot NA.

.MORIE_ARSAU_UOF_RESOURCE_IDS <- list(
  individual_records = list(
    "2023" = "133c73fa-9d8e-435e-8c6d-7d1e14d1e88d",
    "2024" = "690d4c5e-095e-49a0-bbab-b7fc680f3c6b"),
  probe_cycle_records = list(
    "2023" = "339b9e63-9521-44a6-8719-c2cb9aa39a8a",
    "2024" = "76875b6a-4352-4722-a3f6-997cc220dc4f"),
  weapon_records = list(
    "2023" = NA_character_,
    "2024" = "2c1ab494-d636-4c17-9699-3819112982a5"),
  # 5-year aggregate (2020-2022) -- single resource per kind.
  aggregate_summary  = list(
    "2020-2022" = "7560d405-444c-4340-95c4-f73849015501"),
  detailed_dataset   = list(
    "2020-2022" = "2150ac23-4e55-474a-b61f-81baf6850851"))

# Internal: shared offline+live dispatch for ARSAU UoF wrappers.
.morie_arsau_uof_dispatch <- function(kind, year, offline,
                                        resource_id, fixture_name) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", fixture_name, package = "morie")
    if (!nzchar(path)) {
      stop(sprintf("bundled ARSAU UoF fixture %s missing",
                   fixture_name), call. = FALSE)
    }
    return(utils::read.csv(path, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  if (is.null(resource_id)) {
    rid <- .MORIE_ARSAU_UOF_RESOURCE_IDS[[kind]][[as.character(year)]]
    if (is.null(rid) || is.na(rid)) {
      stop(sprintf(paste0(
        "no canonical Ontario CKAN resource_id for ARSAU UoF ",
        "%s year %s (NULL or marked INVALID); pass resource_id= ",
        "explicitly"), kind, year), call. = FALSE)
    }
    resource_id <- rid
  }
  .morie_ontario_ckan_dump_csv(resource_id)
}

#' Ontario Use-of-Force individual records (one row per
#' individual-in-incident)
#' @param year Reporting year (`"2023"` or `"2024"`).
#' @inheritParams morie_datasets_arsau_uof_main_records
#' @return A `data.frame`.
#' @export
morie_datasets_arsau_uof_individual_records <- function(year = "2024",
                                                          offline = TRUE,
                                                          resource_id = NULL) {
  .morie_arsau_uof_dispatch("individual_records", year, offline,
                              resource_id,
                              "arsau_uof_individual_records_sample.csv")
}

#' Ontario Use-of-Force probe-cycle records (one row per CEW cartridge
#' probe per individual-in-incident)
#' @inheritParams morie_datasets_arsau_uof_individual_records
#' @return A `data.frame`.
#' @export
morie_datasets_arsau_uof_probe_cycle_records <- function(year = "2024",
                                                           offline = TRUE,
                                                           resource_id = NULL) {
  .morie_arsau_uof_dispatch("probe_cycle_records", year, offline,
                              resource_id,
                              "arsau_uof_probe_cycle_records_sample.csv")
}

#' Ontario Use-of-Force weapon records (one row per weapon per
#' individual-in-incident)
#'
#' Year 2023 weapon_records is marked INVALID by the Ontario ministry's
#' technical report and has no published CKAN resource; passing
#' `year = "2023"` with `offline = FALSE` raises.
#'
#' @inheritParams morie_datasets_arsau_uof_individual_records
#' @return A `data.frame`.
#' @export
morie_datasets_arsau_uof_weapon_records <- function(year = "2024",
                                                      offline = TRUE,
                                                      resource_id = NULL) {
  .morie_arsau_uof_dispatch("weapon_records", year, offline,
                              resource_id,
                              "arsau_uof_weapon_records_sample.csv")
}

#' Ontario Use-of-Force aggregate summary (5-year 2020-2022, pre-RBDS
#' rollup)
#'
#' @param offline If `TRUE` (default), read the bundled synthetic
#'   fixture. If `FALSE`, hit Ontario CKAN.
#' @param resource_id Optional override.
#' @return A `data.frame`.
#' @export
morie_datasets_arsau_aggregate_summary <- function(offline = TRUE,
                                                     resource_id = NULL) {
  .morie_arsau_uof_dispatch(
    "aggregate_summary", "2020-2022", offline, resource_id,
    "arsau_uof_aggregate_summary_2020_2022_sample.csv")
}

#' Ontario Use-of-Force detailed dataset (5-year 2020-2022, pre-RBDS)
#' @inheritParams morie_datasets_arsau_aggregate_summary
#' @return A `data.frame`.
#' @export
morie_datasets_arsau_detailed_dataset <- function(offline = TRUE,
                                                    resource_id = NULL) {
  .morie_arsau_uof_dispatch(
    "detailed_dataset", "2020-2022", offline, resource_id,
    "arsau_uof_detailed_dataset_2020_2022_sample.csv")
}

# ---------------------------------------------------------------------------
# Consolidated Ontario CKAN registry (discovery)
# ---------------------------------------------------------------------------

.MORIE_ONTARIO_CKAN_REGISTRY <- list(
  arsau_uof_main_records_2023 = list(
    resource_id = "94f303a2-963e-4fd1-958d-6681b310cb6d",
    label = "ARSAU UoF main records (2023)",
    fixture = "arsau_uof_main_records_sample.csv",
    family = "arsau", year = "2023"),
  arsau_uof_main_records_2024 = list(
    resource_id = "ea9dc29c-b4f1-4426-b1f2-974ce995aca1",
    label = "ARSAU UoF main records (2024)",
    fixture = "arsau_uof_main_records_sample.csv",
    family = "arsau", year = "2024"),
  arsau_uof_individual_records_2023 = list(
    resource_id = "133c73fa-9d8e-435e-8c6d-7d1e14d1e88d",
    label = "ARSAU UoF individual records (2023)",
    fixture = "arsau_uof_individual_records_sample.csv",
    family = "arsau", year = "2023"),
  arsau_uof_individual_records_2024 = list(
    resource_id = "690d4c5e-095e-49a0-bbab-b7fc680f3c6b",
    label = "ARSAU UoF individual records (2024)",
    fixture = "arsau_uof_individual_records_sample.csv",
    family = "arsau", year = "2024"),
  arsau_uof_probe_cycle_records_2023 = list(
    resource_id = "339b9e63-9521-44a6-8719-c2cb9aa39a8a",
    label = "ARSAU UoF probe-cycle records (2023)",
    fixture = "arsau_uof_probe_cycle_records_sample.csv",
    family = "arsau", year = "2023"),
  arsau_uof_probe_cycle_records_2024 = list(
    resource_id = "76875b6a-4352-4722-a3f6-997cc220dc4f",
    label = "ARSAU UoF probe-cycle records (2024)",
    fixture = "arsau_uof_probe_cycle_records_sample.csv",
    family = "arsau", year = "2024"),
  arsau_uof_weapon_records_2024 = list(
    resource_id = "2c1ab494-d636-4c17-9699-3819112982a5",
    label = "ARSAU UoF weapon records (2024)",
    fixture = "arsau_uof_weapon_records_sample.csv",
    family = "arsau", year = "2024"),
  arsau_uof_aggregate_summary_2020_2022 = list(
    resource_id = "7560d405-444c-4340-95c4-f73849015501",
    label = "ARSAU UoF aggregate summary 2020-2022",
    fixture = "arsau_uof_aggregate_summary_2020_2022_sample.csv",
    family = "arsau", year = "2020-2022"),
  arsau_uof_detailed_dataset_2020_2022 = list(
    resource_id = "2150ac23-4e55-474a-b61f-81baf6850851",
    label = "ARSAU UoF detailed dataset 2020-2022",
    fixture = "arsau_uof_detailed_dataset_2020_2022_sample.csv",
    family = "arsau", year = "2020-2022"),
  otis_d01_deaths_in_custody = list(
    resource_id = "89e3b63f-5679-4fa4-b98a-fdd2dc486f29",
    label = "OTIS d01 Deaths-in-Custody detailed",
    fixture = "otis_d01_deaths_in_custody_sample.csv",
    family = "otis", year = "all"),
  # OTIS a01 + d02-d07 ship in the same dataset package but the CKAN
  # resource ids are not yet wired into this registry; offline mode
  # works against bundled fixtures, live mode requires the user pass
  # resource_id = ... explicitly. PRs welcome to fill these in.
  otis_a01_restrictive_confinement = list(
    resource_id = "5a0c5804-a055-4031-9743-73f556e43bb4",
    label = "OTIS a01 Restrictive Confinement detailed",
    fixture = "otis_a01_restrictive_confinement_sample.csv",
    family = "otis", year = "all"),
  otis_d02_deaths_by_gender = list(
    resource_id = "9de64ab4-0860-499d-8303-014bff5ec412",
    label = "OTIS d02 Deaths-in-Custody by gender",
    fixture = "otis_d02_deaths_by_gender_sample.csv",
    family = "otis", year = "all"),
  otis_d03_deaths_by_race = list(
    resource_id = "3aaec288-2ab9-4850-851d-e40a69517df5",
    label = "OTIS d03 Deaths-in-Custody by race",
    fixture = "otis_d03_deaths_by_race_sample.csv",
    family = "otis", year = "all"),
  otis_d04_deaths_by_religion = list(
    resource_id = "46437725-4ba6-454b-b4e1-0c05402c84ca",
    label = "OTIS d04 Deaths-in-Custody by religion",
    fixture = "otis_d04_deaths_by_religion_sample.csv",
    family = "otis", year = "all"),
  otis_d05_deaths_by_age_category = list(
    resource_id = "45820ef9-e23a-4b4b-800a-c13c99dd5b0a",
    label = "OTIS d05 Deaths-in-Custody by age category",
    fixture = "otis_d05_deaths_by_age_category_sample.csv",
    family = "otis", year = "all"),
  otis_d06_cause_by_alert = list(
    resource_id = "cc9dd090-25fe-45b1-b6b0-ae3409fa133b",
    label = "OTIS d06 Deaths-in-Custody cause-by-alert",
    fixture = "otis_d06_cause_by_alert_sample.csv",
    family = "otis", year = "all"),
  otis_d07_alerts_by_housing_unit = list(
    resource_id = "6bb46038-5f50-4908-8c14-fdf31a4d3d98",
    label = "OTIS d07 Deaths-in-Custody alerts by housing unit",
    fixture = "otis_d07_alerts_by_housing_unit_sample.csv",
    family = "otis", year = "all"),
  # OTIS b01-b09 Segregation family (lookup-pending live mode).
  otis_b01_segregation_detailed = list(
    resource_id = "406e6d90-d568-4553-8ca7-bc9f90e133b9",
    label = "OTIS b01 Segregation detailed",
    fixture = "otis_b01_segregation_detailed_sample.csv",
    family = "otis", year = "all"),
  otis_b02_segregation_total_days = list(
    resource_id = "84161f23-ee75-48b4-97df-3b19b8bbd745",
    label = "OTIS b02 Segregation total days",
    fixture = "otis_b02_segregation_total_days_sample.csv",
    family = "otis", year = "all"),
  otis_b03_seg_alerts_by_institution = list(
    resource_id = "ef45902a-b946-49fe-8c2f-f778e8357e1f",
    label = "OTIS b03 Segregation alerts by institution",
    fixture = "otis_b03_seg_alerts_by_institution_sample.csv",
    family = "otis", year = "all"),
  otis_b04_seg_consecutive_by_region = list(
    resource_id = "d76d8f65-6318-4a45-b4c7-5b9a2d985408",
    label = "OTIS b04 Segregation consecutive durations by region",
    fixture = "otis_b04_seg_consecutive_by_region_sample.csv",
    family = "otis", year = "all"),
  otis_b05_seg_consecutive_lengths = list(
    resource_id = "754e8cde-5c74-4c0a-9782-79767a2b26b0",
    label = "OTIS b05 Segregation consecutive lengths",
    fixture = "otis_b05_seg_consecutive_lengths_sample.csv",
    family = "otis", year = "all"),
  otis_b06_seg_reason_by_institution = list(
    resource_id = "af633c35-2f98-4ca6-8629-02aa8acd237a",
    label = "OTIS b06 Segregation reason by institution",
    fixture = "otis_b06_seg_reason_by_institution_sample.csv",
    family = "otis", year = "all"),
  otis_b07_seg_alerts_by_gender = list(
    resource_id = "38090dad-9f73-4a0b-8a7b-ca2477fc0030",
    label = "OTIS b07 Segregation alerts by gender",
    fixture = "otis_b07_seg_alerts_by_gender_sample.csv",
    family = "otis", year = "all"),
  otis_b08_seg_consecutive_by_institution = list(
    resource_id = "73c77cf2-faeb-4136-a897-ed4d4c19e240",
    label = "OTIS b08 Segregation consecutive durations by institution",
    fixture = "otis_b08_seg_consecutive_by_institution_sample.csv",
    family = "otis", year = "all"),
  otis_b09_seg_n_times = list(
    resource_id = "df24e943-d52b-43a8-a10e-a3cc906e26bb",
    label = "OTIS b09 Individuals by N times in segregation",
    fixture = "otis_b09_seg_n_times_sample.csv",
    family = "otis", year = "all"),
  # OTIS c01-c12 Individuals-in-Segregation+RC family (lookup-pending).
  otis_c01_individuals_total = list(
    resource_id = "81bc03cc-b3f6-4983-b717-11f85fa90330",
    label = "OTIS c01 Individuals total",
    fixture = "otis_c01_individuals_total_sample.csv",
    family = "otis", year = "all"),
  otis_c02_individuals_by_institution = list(
    resource_id = "cb4ed82f-c67a-430a-9cb5-5e5698a06ddf",
    label = "OTIS c02 Individuals by institution",
    fixture = "otis_c02_individuals_by_institution_sample.csv",
    family = "otis", year = "all"),
  otis_c03_individuals_race_by_gender = list(
    resource_id = "0532a199-3a4f-45b4-b79b-6db7920ff7f2",
    label = "OTIS c03 Individuals race by gender",
    fixture = "otis_c03_individuals_race_by_gender_sample.csv",
    family = "otis", year = "all"),
  otis_c04_individuals_race_by_region = list(
    resource_id = "b38f754f-9141-4ea3-a10c-a071473ed00a",
    label = "OTIS c04 Individuals race by region",
    fixture = "otis_c04_individuals_race_by_region_sample.csv",
    family = "otis", year = "all"),
  otis_c05_individuals_religion_by_region = list(
    resource_id = "c899bf66-bd7b-4305-8ccc-ae031e041df8",
    label = "OTIS c05 Individuals religion by region",
    fixture = "otis_c05_individuals_religion_by_region_sample.csv",
    family = "otis", year = "all"),
  otis_c06_individuals_age_by_region = list(
    resource_id = "4e4c91e9-ae29-4ab6-864e-bcda896c7882",
    label = "OTIS c06 Individuals age by region",
    fixture = "otis_c06_individuals_age_by_region_sample.csv",
    family = "otis", year = "all"),
  otis_c07_individuals_alerts = list(
    resource_id = "879cf325-a7a7-4d48-bc69-ea050d8a4d4e",
    label = "OTIS c07 Individuals alerts",
    fixture = "otis_c07_individuals_alerts_sample.csv",
    family = "otis", year = "all"),
  otis_c08_individuals_religion_by_gender = list(
    resource_id = "8fc09ce2-5097-4a94-af29-abcdcd5aa015",
    label = "OTIS c08 Individuals religion by gender",
    fixture = "otis_c08_individuals_religion_by_gender_sample.csv",
    family = "otis", year = "all"),
  otis_c09_individuals_age_by_gender = list(
    resource_id = "0e990da6-5427-453e-91ba-b6f24dee2ef2",
    label = "OTIS c09 Individuals age by gender",
    fixture = "otis_c09_individuals_age_by_gender_sample.csv",
    family = "otis", year = "all"),
  otis_c10_aggregate_durations_by_institution = list(
    resource_id = "eaf6d52a-210a-48ef-822a-294e9346c45c",
    label = "OTIS c10 Aggregate durations by institution",
    fixture = "otis_c10_aggregate_durations_by_institution_sample.csv",
    family = "otis", year = "all"),
  otis_c11_aggregate_lengths = list(
    resource_id = "9c7b74a5-53ad-4ef0-a7a6-97772cd01c55",
    label = "OTIS c11 Aggregate lengths",
    fixture = "otis_c11_aggregate_lengths_sample.csv",
    family = "otis", year = "all"),
  otis_c12_aggregate_durations_by_region = list(
    resource_id = "d7080653-69fc-4f38-8d83-709fe16ae465",
    label = "OTIS c12 Aggregate durations by region",
    fixture = "otis_c12_aggregate_durations_by_region_sample.csv",
    family = "otis", year = "all"))

#' List the Ontario CKAN datasets wrapped by morie
#'
#' Returns the consolidated registry of every Ontario Open Data feed
#' morie ships an offline-mode fixture + mocked live-mode dispatch for.
#' Pair with [morie_datasets_ontario_ckan_by_key()] for generic factory
#' access by `dataset_key`.
#'
#' Coverage as of this release:
#'   * ARSAU UoF: 2 main + 2 individual + 2 probe_cycle + 1 weapon (2024)
#'     + 2 5-year aggregates (aggregate_summary + detailed_dataset).
#'   * OTIS: d01 Deaths-in-Custody.
#'   * d02-d07 OTIS deaths variants + OTIS a01/b/c families: known but
#'     resource_ids not yet wired in (PRs welcome).
#'
#' @return A `data.frame` with columns `dataset_key`, `label`,
#'   `resource_id`, `family`, `year`, `fixture`.
#' @export
morie_datasets_ontario_ckan_layers <- function() {
  rows <- lapply(names(.MORIE_ONTARIO_CKAN_REGISTRY), function(k) {
    e <- .MORIE_ONTARIO_CKAN_REGISTRY[[k]]
    data.frame(dataset_key = k, label = e$label,
                resource_id = e$resource_id, family = e$family,
                year = e$year, fixture = e$fixture,
                stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# OTIS family expansion: a01 restrictive confinement + d02-d07 deaths
# ---------------------------------------------------------------------------
#
# These seven OTIS datasets ship in the Ontario "Data on Inmates in
# Ontario" CKAN package but the canonical resource_ids for d02-d07 +
# a01 are not yet wired into the morie registry. Offline mode (default)
# works against bundled synthetic fixtures; live mode (offline = FALSE)
# raises with a clear "lookup pending" message until the resource_id
# is supplied via the registry or the resource_id= override.

# Internal shared dispatch for the OTIS wrappers. Resource ids are
# auto-resolved from .MORIE_ONTARIO_CKAN_REGISTRY[[registry_key]] when
# the caller doesn't pass an explicit override; if the registry entry
# is also missing or NA the function errors with a clear message.
.morie_otis_lookup_pending_dispatch <- function(dataset_label, fixture,
                                                  offline, resource_id,
                                                  registry_key = NULL,
                                                  source = NULL) {
  # Derive `source` from the older `offline` arg for back-compat:
  # offline = TRUE  -> source = "bundled" (existing behaviour: read the
  #                    inst/extdata/*_sample.csv; error if missing)
  # offline = FALSE -> source = "live"    (existing behaviour: hit the
  #                    live Ontario CKAN endpoint; error on miss)
  # source = "auto"/"synthetic"/"empty" are new opt-in modes that
  # require the caller to pass `source =` explicitly.
  if (is.null(source) || identical(source, "")) {
    source <- if (isTRUE(offline)) "bundled" else "live"
  }
  source <- match.arg(
    source, c("auto", "live", "bundled", "synthetic", "empty"))
  # Resolve the synth id from the fixture filename: strip the
  # "otis_<id>_..._sample.csv" prefix down to just the <id>.
  short_id <- sub("^otis_([a-z]\\d+)_.*", "\\1",
                  sub("\\.csv$", "", fixture))
  bundled_name <- sub("\\.csv$", "", fixture)
  live_fn <- function() {
    rid <- resource_id
    if (is.null(rid) && !is.null(registry_key)) {
      entry <- .MORIE_ONTARIO_CKAN_REGISTRY[[registry_key]]
      if (!is.null(entry) && !is.na(entry$resource_id))
        rid <- entry$resource_id
    }
    if (is.null(rid) || is.na(rid)) {
      stop(sprintf(paste0(
        "Live mode for OTIS %s needs an explicit resource_id ",
        "(canonical Ontario CKAN id lookup pending in the morie ",
        "registry). Pass resource_id= or open a PR adding it to ",
        ".MORIE_ONTARIO_CKAN_REGISTRY."), dataset_label),
        call. = FALSE)
    }
    .morie_ontario_ckan_dump_csv(rid)
  }
  synth_fn <- function() morie_synth_otis(short_id, n = 30L)
  .morie_load_chain(
    source = source,
    live_fn = live_fn,
    bundled_name = bundled_name,
    synth_fn = synth_fn,
    columns = NULL
  )
}

#' OTIS a01 -- Restrictive Confinement (detailed per-individual)
#' @param offline If `TRUE` (default), read the bundled synthetic
#'   fixture. If `FALSE`, hit Ontario CKAN (`resource_id` required).
#' @param resource_id Optional CKAN resource id (required for live).
#' @return A `data.frame` with the canonical 10-col schema
#'   (`EndFiscalYear`, `UniqueIndividual_ID`,
#'   `Region_AtTimeOfPlacement`, `Region_MostRecentPlacement`,
#'   `Gender`, `Age_Category`, `MentalHealth_Alert`,
#'   `SuicideRisk_Alert`, `SuicideWatch_Alert`,
#'   `Number_Of_Placements`).
#' @export
morie_datasets_otis_a01_restrictive_confinement <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "a01 Restrictive Confinement",
    "otis_a01_restrictive_confinement_sample.csv",
    offline, resource_id, registry_key = "otis_a01_restrictive_confinement", source = source)
}

#' OTIS d02 -- Deaths in custody by gender
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_d02_deaths_by_gender <- function(offline = TRUE,
                                                       resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d02 Deaths-in-Custody by gender",
    "otis_d02_deaths_by_gender_sample.csv",
    offline, resource_id, registry_key = "otis_d02_deaths_by_gender", source = source)
}

#' OTIS d03 -- Deaths in custody by race
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_d03_deaths_by_race <- function(offline = TRUE,
                                                     resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d03 Deaths-in-Custody by race",
    "otis_d03_deaths_by_race_sample.csv",
    offline, resource_id, registry_key = "otis_d03_deaths_by_race", source = source)
}

#' OTIS d04 -- Deaths in custody by religion
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_d04_deaths_by_religion <- function(offline = TRUE,
                                                         resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d04 Deaths-in-Custody by religion",
    "otis_d04_deaths_by_religion_sample.csv",
    offline, resource_id, registry_key = "otis_d04_deaths_by_religion", source = source)
}

#' OTIS d05 -- Deaths in custody by age category
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_d05_deaths_by_age_category <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d05 Deaths-in-Custody by age category",
    "otis_d05_deaths_by_age_category_sample.csv",
    offline, resource_id, registry_key = "otis_d05_deaths_by_age_category", source = source)
}

#' OTIS d06 -- Deaths in custody by alert type x institution
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_d06_cause_by_alert <- function(offline = TRUE,
                                                     resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d06 Deaths-in-Custody cause-by-alert",
    "otis_d06_cause_by_alert_sample.csv",
    offline, resource_id, registry_key = "otis_d06_cause_by_alert", source = source)
}

#' OTIS d07 -- Deaths in custody alerts x housing unit
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_d07_alerts_by_housing_unit <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "d07 Deaths-in-Custody alerts by housing unit",
    "otis_d07_alerts_by_housing_unit_sample.csv",
    offline, resource_id, registry_key = "otis_d07_alerts_by_housing_unit", source = source)
}

# ---------------------------------------------------------------------------
# OTIS b01-b09 Segregation family (lookup-pending)
# ---------------------------------------------------------------------------

#' OTIS b01 -- Segregation detailed (per-individual episodes)
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @return A `data.frame` with the canonical 18-col schema.
#' @export
morie_datasets_otis_b01_segregation_detailed <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b01 Segregation detailed",
    "otis_b01_segregation_detailed_sample.csv", offline, resource_id, registry_key = "otis_b01_segregation_detailed", source = source)
}

#' OTIS b02 -- Segregation total days per individual
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b02_segregation_total_days <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b02 Segregation total days",
    "otis_b02_segregation_total_days_sample.csv", offline, resource_id, registry_key = "otis_b02_segregation_total_days", source = source)
}

#' OTIS b03 -- Segregation placements: alerts + hold by institution
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b03_seg_alerts_by_institution <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b03 Segregation alerts by institution",
    "otis_b03_seg_alerts_by_institution_sample.csv", offline, resource_id, registry_key = "otis_b03_seg_alerts_by_institution", source = source)
}

#' OTIS b04 -- Segregation consecutive durations by region
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b04_seg_consecutive_by_region <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b04 Segregation consecutive durations by region",
    "otis_b04_seg_consecutive_by_region_sample.csv", offline, resource_id, registry_key = "otis_b04_seg_consecutive_by_region", source = source)
}

#' OTIS b05 -- Segregation placements by consecutive-length bucket
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b05_seg_consecutive_lengths <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b05 Segregation consecutive lengths",
    "otis_b05_seg_consecutive_lengths_sample.csv", offline, resource_id, registry_key = "otis_b05_seg_consecutive_lengths", source = source)
}

#' OTIS b06 -- Segregation placements: reason for placement by institution
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b06_seg_reason_by_institution <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b06 Segregation reason by institution",
    "otis_b06_seg_reason_by_institution_sample.csv", offline, resource_id, registry_key = "otis_b06_seg_reason_by_institution", source = source)
}

#' OTIS b07 -- Segregation placements: alerts + hold by gender
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b07_seg_alerts_by_gender <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b07 Segregation alerts by gender",
    "otis_b07_seg_alerts_by_gender_sample.csv", offline, resource_id, registry_key = "otis_b07_seg_alerts_by_gender", source = source)
}

#' OTIS b08 -- Segregation consecutive durations by institution
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b08_seg_consecutive_by_institution <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b08 Segregation consecutive durations by institution",
    "otis_b08_seg_consecutive_by_institution_sample.csv",
    offline, resource_id, registry_key = "otis_b08_seg_consecutive_by_institution", source = source)
}

#' OTIS b09 -- Individuals in segregation by number of times placed
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_b09_seg_n_times <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "b09 Individuals by N times in segregation",
    "otis_b09_seg_n_times_sample.csv", offline, resource_id, registry_key = "otis_b09_seg_n_times", source = source)
}

# ---------------------------------------------------------------------------
# OTIS c01-c12 Individuals-in-Segregation+RC family (lookup-pending)
# ---------------------------------------------------------------------------

#' OTIS c01 -- Total individuals (in custody / restrictive confinement / segregation)
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c01_individuals_total <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c01 Individuals total",
    "otis_c01_individuals_total_sample.csv", offline, resource_id, registry_key = "otis_c01_individuals_total", source = source)
}

#' OTIS c02 -- Individuals by institution
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c02_individuals_by_institution <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c02 Individuals by institution",
    "otis_c02_individuals_by_institution_sample.csv",
    offline, resource_id, registry_key = "otis_c02_individuals_by_institution", source = source)
}

#' OTIS c03 -- Individuals by race x gender
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c03_individuals_race_by_gender <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c03 Individuals race by gender",
    "otis_c03_individuals_race_by_gender_sample.csv", offline, resource_id, registry_key = "otis_c03_individuals_race_by_gender", source = source)
}

#' OTIS c04 -- Individuals by race x region
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c04_individuals_race_by_region <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c04 Individuals race by region",
    "otis_c04_individuals_race_by_region_sample.csv", offline, resource_id, registry_key = "otis_c04_individuals_race_by_region", source = source)
}

#' OTIS c05 -- Individuals by religion x region
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c05_individuals_religion_by_region <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c05 Individuals religion by region",
    "otis_c05_individuals_religion_by_region_sample.csv",
    offline, resource_id, registry_key = "otis_c05_individuals_religion_by_region", source = source)
}

#' OTIS c06 -- Individuals by age category x region
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c06_individuals_age_by_region <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c06 Individuals age by region",
    "otis_c06_individuals_age_by_region_sample.csv", offline, resource_id, registry_key = "otis_c06_individuals_age_by_region", source = source)
}

#' OTIS c07 -- Individuals: alerts + hold flags
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c07_individuals_alerts <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c07 Individuals alerts",
    "otis_c07_individuals_alerts_sample.csv", offline, resource_id, registry_key = "otis_c07_individuals_alerts", source = source)
}

#' OTIS c08 -- Individuals by religion x gender
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c08_individuals_religion_by_gender <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c08 Individuals religion by gender",
    "otis_c08_individuals_religion_by_gender_sample.csv",
    offline, resource_id, registry_key = "otis_c08_individuals_religion_by_gender", source = source)
}

#' OTIS c09 -- Individuals by age category x gender
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c09_individuals_age_by_gender <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c09 Individuals age by gender",
    "otis_c09_individuals_age_by_gender_sample.csv", offline, resource_id, registry_key = "otis_c09_individuals_age_by_gender", source = source)
}

#' OTIS c10 -- Aggregate durations by institution
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c10_aggregate_durations_by_institution <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c10 Aggregate durations by institution",
    "otis_c10_aggregate_durations_by_institution_sample.csv",
    offline, resource_id, registry_key = "otis_c10_aggregate_durations_by_institution", source = source)
}

#' OTIS c11 -- Aggregate lengths
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c11_aggregate_lengths <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c11 Aggregate lengths",
    "otis_c11_aggregate_lengths_sample.csv", offline, resource_id, registry_key = "otis_c11_aggregate_lengths", source = source)
}

#' OTIS c12 -- Aggregate durations by region
#' @inheritParams morie_datasets_otis_a01_restrictive_confinement
#' @export
morie_datasets_otis_c12_aggregate_durations_by_region <- function(
  offline = TRUE, resource_id = NULL, source = NULL) {
  .morie_otis_lookup_pending_dispatch(
    "c12 Aggregate durations by region",
    "otis_c12_aggregate_durations_by_region_sample.csv",
    offline, resource_id, registry_key = "otis_c12_aggregate_durations_by_region", source = source)
}

#' Generic Ontario CKAN dataset loader (by registry key)
#'
#' @param dataset_key One of the keys in
#'   [morie_datasets_ontario_ckan_layers()] (e.g.
#'   `"arsau_uof_main_records_2024"`).
#' @param offline If `TRUE` (default), read the bundled synthetic
#'   fixture. If `FALSE`, hit the live Ontario CKAN datastore-dump
#'   endpoint.
#' @param resource_id Optional CKAN resource_id override.
#' @return A `data.frame`.
#' @export
morie_datasets_ontario_ckan_by_key <- function(dataset_key,
                                                  offline = TRUE,
                                                  resource_id = NULL) {
  if (!(dataset_key %in% names(.MORIE_ONTARIO_CKAN_REGISTRY))) {
    stop(sprintf(paste0(
      "unknown Ontario CKAN dataset_key '%s'. Available: %s"),
      dataset_key,
      paste(names(.MORIE_ONTARIO_CKAN_REGISTRY), collapse = ", ")),
      call. = FALSE)
  }
  entry <- .MORIE_ONTARIO_CKAN_REGISTRY[[dataset_key]]
  if (isTRUE(offline)) {
    path <- system.file("extdata", entry$fixture, package = "morie")
    if (!nzchar(path)) {
      stop(sprintf("bundled Ontario CKAN fixture %s missing",
                   entry$fixture), call. = FALSE)
    }
    return(utils::read.csv(path, stringsAsFactors = FALSE,
                            check.names = FALSE))
  }
  if (is.null(resource_id)) resource_id <- entry$resource_id
  .morie_ontario_ckan_dump_csv(resource_id)
}
