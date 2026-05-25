# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Ontario Use of Force in Correctional Institutions ("RIBD" /
# Restricted Information Bulletin Data) -- 12 CSV resources +
# bilingual data dictionary from the Ontario Open Data Catalogue:
#
#   https://data.ontario.ca/dataset/use-of-force-in-correctional-institutions
#
# Each loader follows the same offline-first pattern morie uses for
# OTIS / ARSAU UoF / TPS Hub data: a 50-row real CKAN slice bundled
# at inst/extdata/corrections_uof_<key>_sample.csv (default), with
# offline = FALSE hitting the live CKAN datastore_dump JSON endpoint
# for the canonical resource id.
#
# Data licence: Open Government Licence -- Ontario.
# Data dictionary: inst/extdata/corrections_uof_dictionary.json
#   (parsed from datadictionary_correctionsribd_en_fr20250822.xlsx).

# Canonical CKAN resource ids, keyed by morie short name.
.MORIE_CORRECTIONS_UOF_RESOURCE_IDS <- list(
  incidents               = "0cd1a330-b6e3-4120-96ce-90c750522064",
  inmate_incident         = "ee273b03-64df-48fc-ad6c-a0d1d35a9e27",
  staff_incident          = "befd3aba-9cab-430a-93f7-76cef36af250",
  incident_type           = "ff5b61e2-b73d-47d1-843c-0c6711acabcd",
  institution_summary     = "25ad22ed-5df7-42a0-94e0-a6652ceb9442",
  location_summary        = "b7299617-bc85-464a-8a3a-cf42c9f51c27",
  select_incident_summary = "c74a5c2f-8a55-488c-aa7a-b5b61fe92dad",
  inmate_participant      = "fc5cdb84-20f8-4ceb-94e5-c8d347b40021",
  indigenous              = "f6ed9442-12ab-4534-ac80-73bea6ac0094",
  ethnic_origin           = "3291665c-a77e-41a3-9549-94c6293320b5",
  race                    = "93f1f1f1-4347-4236-a8e3-7e2f1b0f8068",
  religion                = "a065f65f-8994-4242-9aa3-5a457445331f"
)

#' Canonical CKAN resource id table for the Corrections UoF dataset.
#'
#' Returns the 12 morie short-name -> CKAN resource-id map for the
#' Ontario "Use of Force in Correctional Institutions" dataset on
#' data.ontario.ca. Useful for catalog discovery + sanity tests.
#'
#' @return Named list of 12 CKAN resource ids.
#' @export
#' @examples
#' ids <- morie_corrections_uof_resource_ids()
#' length(ids)
#' names(ids)
morie_corrections_uof_resource_ids <- function() {
  .MORIE_CORRECTIONS_UOF_RESOURCE_IDS
}

#' Generic Corrections-UoF loader.
#'
#' Internal helper that every per-resource loader delegates to via
#' the shared \code{.morie_load_chain()} (live -> bundled ->
#' synthetic -> empty). Not exported.
#' @keywords internal
#' @noRd
.morie_corrections_uof_load <- function(key, offline = TRUE,
                                          resource_id = NULL,
                                          source = NULL) {
  if (is.null(source) || identical(source, "")) {
    source <- if (isTRUE(offline)) "bundled" else "live"
  }
  rid <- resource_id %||% .MORIE_CORRECTIONS_UOF_RESOURCE_IDS[[key]]
  bundled <- sprintf("corrections_uof_%s_sample", key)
  live_fn <- function() {
    if (is.null(rid) || is.na(rid)) {
      stop(sprintf(
        "no canonical CKAN resource_id for corrections-uof key %s",
        sQuote(key)), call. = FALSE)
    }
    .morie_ontario_ckan_dump_csv(rid)
  }
  synth_fn <- function() morie_synth_corrections_uof(key, n = 30L)
  .morie_load_chain(
    source = source,
    live_fn = live_fn,
    bundled_name = bundled,
    synth_fn = synth_fn,
    columns = NULL
  )
}

#' Use-of-force incidents (head dataset)
#' @param offline Logical; \code{TRUE} (default) reads the bundled
#'   real CKAN sample at
#'   \code{inst/extdata/corrections_uof_incidents_sample.csv}.
#'   \code{FALSE} hits the live CKAN endpoint.
#' @param resource_id Optional CKAN resource id override.
#' @param source One of \code{"auto"}, \code{"live"}, \code{"bundled"},
#'   \code{"synthetic"}, \code{"empty"}; takes precedence over
#'   \code{offline} when supplied.
#' @return \code{data.frame}.
#' @references
#'   \url{https://data.ontario.ca/dataset/use-of-force-in-correctional-institutions};
#'   Open Government Licence -- Ontario.
#' @export
morie_datasets_corrections_uof_incidents <- function(offline = TRUE,
                                                       resource_id = NULL,
                                                       source = NULL) {
  .morie_corrections_uof_load("incidents", offline, resource_id, source)
}

#' Inmate-to-incidents bridging table
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_inmate_incident <- function(offline = TRUE,
                                                             resource_id = NULL,
                                                             source = NULL) {
  .morie_corrections_uof_load("inmate_incident", offline, resource_id, source)
}

#' Staff-to-incidents bridging table
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_staff_incident <- function(offline = TRUE,
                                                            resource_id = NULL,
                                                            source = NULL) {
  .morie_corrections_uof_load("staff_incident", offline, resource_id, source)
}

#' Incident-type lookup
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_incident_type <- function(offline = TRUE,
                                                           resource_id = NULL,
                                                           source = NULL) {
  .morie_corrections_uof_load("incident_type", offline, resource_id, source)
}

#' Institution-level annual incident summary
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_institution_summary <- function(offline = TRUE,
                                                                 resource_id = NULL,
                                                                 source = NULL) {
  .morie_corrections_uof_load("institution_summary", offline, resource_id, source)
}

#' Location-of-incident annual summary
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_location_summary <- function(offline = TRUE,
                                                              resource_id = NULL,
                                                              source = NULL) {
  .morie_corrections_uof_load("location_summary", offline, resource_id, source)
}

#' Select-incident-type annual summary
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_select_incident_summary <- function(offline = TRUE,
                                                                     resource_id = NULL,
                                                                     source = NULL) {
  .morie_corrections_uof_load("select_incident_summary", offline,
                                resource_id, source)
}

#' Inmate-participant demographics (head)
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_inmate_participant <- function(offline = TRUE,
                                                                resource_id = NULL,
                                                                source = NULL) {
  .morie_corrections_uof_load("inmate_participant", offline, resource_id, source)
}

#' Inmate-participant Indigenous identity
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_indigenous <- function(offline = TRUE,
                                                       resource_id = NULL,
                                                       source = NULL) {
  .morie_corrections_uof_load("indigenous", offline, resource_id, source)
}

#' Inmate-participant ethnic origin
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_ethnic_origin <- function(offline = TRUE,
                                                           resource_id = NULL,
                                                           source = NULL) {
  .morie_corrections_uof_load("ethnic_origin", offline, resource_id, source)
}

#' Inmate-participant race
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_race <- function(offline = TRUE,
                                                  resource_id = NULL,
                                                  source = NULL) {
  .morie_corrections_uof_load("race", offline, resource_id, source)
}

#' Inmate-participant religion
#' @inheritParams morie_datasets_corrections_uof_incidents
#' @export
morie_datasets_corrections_uof_religion <- function(offline = TRUE,
                                                     resource_id = NULL,
                                                     source = NULL) {
  .morie_corrections_uof_load("religion", offline, resource_id, source)
}

# ---------------------------------------------------------------------------
# Synthetic generator
# ---------------------------------------------------------------------------

#' Build a synthetic Corrections-UoF data.frame for testing.
#'
#' Returns a small data.frame mirroring the column shape of the
#' published corrections-UoF resource for the given short \code{key}.
#' Schemas are derived from the bundled
#' \code{inst/extdata/corrections_uof_dictionary.json} (parsed from
#' \code{datadictionary_correctionsribd_en_fr20250822.xlsx}). Values
#' are uniformly drawn from the dictionary "Data Values" examples
#' or generic ranges; used only for offline-no-fixture fallback in
#' tests and demos. NOT a substitute for the real upstream data
#' published at \url{https://data.ontario.ca/dataset/use-of-force-in-correctional-institutions}.
#'
#' @param key Character; one of the 12 short names returned by
#'   \code{\link{morie_corrections_uof_resource_ids}}.
#' @param n Integer; number of synthetic rows. Default 30.
#' @param seed Integer; RNG seed. Default 1.
#' @return A \code{data.frame}.
#' @seealso \code{\link{morie_datasets_corrections_uof_incidents}} and
#'   the 11 sibling loaders.
#' @export
#' @examples
#' df <- morie_synth_corrections_uof("incidents", n = 10)
#' nrow(df)
morie_synth_corrections_uof <- function(key, n = 30L, seed = 1L) {
  set.seed(seed)
  n <- as.integer(n)
  # Pull the column names from the bundled real CKAN sample so the
  # synthetic frame matches exactly what the offline path returns.
  bundled_path <- system.file(
    "extdata",
    sprintf("corrections_uof_%s_sample.csv", key),
    package = "morie")
  if (!nzchar(bundled_path) || !file.exists(bundled_path)) {
    stop(sprintf("morie_synth_corrections_uof: no bundled sample for key %s",
                 sQuote(key)), call. = FALSE)
  }
  schema <- utils::read.csv(bundled_path, nrows = 5L,
                              stringsAsFactors = FALSE, check.names = FALSE)
  cols <- names(schema)
  draw_col <- function(col_name) {
    real <- schema[[col_name]]
    # Numeric? sample uniformly across the observed min..max.
    if (is.numeric(real) && length(stats::na.omit(real)) > 0L) {
      mn <- floor(min(real, na.rm = TRUE))
      mx <- ceiling(max(real, na.rm = TRUE))
      return(sample(seq.int(mn, max(mn + 1L, mx)), n, replace = TRUE))
    }
    # Otherwise: re-sample the observed character levels.
    pool <- unique(stats::na.omit(as.character(real)))
    if (length(pool) == 0L) return(rep(NA_character_, n))
    sample(pool, n, replace = TRUE)
  }
  out <- as.data.frame(
    stats::setNames(lapply(cols, draw_col), cols),
    stringsAsFactors = FALSE
  )
  out
}
