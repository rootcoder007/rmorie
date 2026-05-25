# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 3DDD2: Vancouver Police Department (VPD) crime data loader.
#
# CRITICAL ETHICAL + LEGAL NOTE:
#
# VPD publishes crime data through GeoDASH (geodash.vpd.ca/opendata)
# but downloads are gated behind manual T&C acceptance via the web
# UI. There is no public, automation-friendly API or stable URL
# pattern morie can hit directly. Users MUST:
#
#   1. Visit https://geodash.vpd.ca/opendata/
#   2. Accept VPD's legal terms and conditions on that page.
#   3. Download crimedata_csv_AllNeighbourhoods_AllYears.zip
#      (or a per-neighbourhood / per-year variant) themselves.
#   4. Pass the local zip path to morie_datasets_vpd_crime().
#
# morie ships a stratified 550-row sample (50/TYPE x 11 categories)
# for offline introspection + tests. The bundled
# vpd_legal_disclaimer.txt surfaces VPD's terms verbatim so callers
# can read them programmatically.
#
# Data columns (10):
#   TYPE, YEAR, MONTH, DAY, HOUR, MINUTE,
#   HUNDRED_BLOCK, NEIGHBOURHOOD, X, Y
#
# Coordinates: UTM Zone 10 N (NAD83 / EPSG:26910). X around
# 480000-500000 ; Y around 5450000-5470000 for City of Vancouver.
# For Person-against incidents the address is randomized to several
# blocks + offset to an intersection per VPD's privacy guarantee.

#' Read VPD's legal disclaimer (bundled verbatim from the zip)
#'
#' Phase 3DDD2. Returns the legal disclaimer text shipped with
#' VPD's open crime data download. Useful in headless or
#' programmatic workflows where the human reader of the GeoDASH
#' web UI is not the same as the script user.
#'
#' @return A character vector (one element per line).
#' @export
morie_datasets_vpd_legal_disclaimer <- function() {
  path <- system.file("extdata", "vpd_legal_disclaimer.txt",
                      package = "morie")
  if (!nzchar(path))
    stop("bundled VPD legal disclaimer missing", call. = FALSE)
  readLines(path, warn = FALSE)
}

#' Load Vancouver Police Department crime incident data
#'
#' Phase 3DDD2. Loads VPD's open crime incident records. Three
#' source modes:
#'
#' \describe{
#'   \item{`offline = TRUE` (default)}{Reads a bundled stratified
#'     550-row sample (50 rows per `TYPE` x 11 categories) covering
#'     years 2003-2026 and all 25 VPD-defined neighbourhoods.
#'     Intended for tests + intro examples -- NOT for analysis.}
#'   \item{`zip_path = "..."`}{Reads from a local copy of VPD's
#'     `crimedata_csv_AllNeighbourhoods_AllYears.zip` that the
#'     caller has downloaded themselves from
#'     \url{https://geodash.vpd.ca/opendata/} (after accepting
#'     VPD's terms + conditions there).}
#'   \item{`csv_path = "..."`}{Reads from a pre-extracted CSV
#'     (skip the zip if the caller already has the CSV on disk).}
#' }
#'
#' The bundled sample is open-licensed under VPD's GeoDASH terms;
#' the full feed requires manual T&C acceptance per VPD policy and
#' there is no automation-friendly API. See
#' [morie_datasets_vpd_legal_disclaimer()] for the full text.
#'
#' Columns (10): `TYPE`, `YEAR`, `MONTH`, `DAY`, `HOUR`, `MINUTE`,
#' `HUNDRED_BLOCK`, `NEIGHBOURHOOD`, `X`, `Y`. Coordinates are UTM
#' Zone 10 N (NAD83 / EPSG:26910). For Offence-Against-a-Person
#' incidents the location is deliberately randomized + offset per
#' VPD's privacy policy.
#'
#' Categories present in the sample:
#' \itemize{
#'   \item Break and Enter Commercial
#'   \item Break and Enter Residential/Other
#'   \item Homicide
#'   \item Mischief
#'   \item Offence Against a Person (aggregated)
#'   \item Other Theft
#'   \item Theft from Vehicle
#'   \item Theft of Bicycle
#'   \item Theft of Vehicle
#'   \item Vehicle Collision or Pedestrian Struck (with Fatality)
#'   \item Vehicle Collision or Pedestrian Struck (with Injury)
#' }
#'
#' @section Data quality + interpretation caveats (per VPD GeoDASH disclaimer):
#'
#' \itemize{
#'   \item \strong{Source}: extracted from the PRIME-BC Police Records
#'         Management System (RMS); filtered + aggregated to comply
#'         with the BC Freedom of Information & Protection of Privacy
#'         Act (BC FIPPA).
#'   \item \strong{`Offence Against a Person` is INTENTIONALLY aggregated}
#'         to reduce re-identification risk. It bundles robbery,
#'         assault (incl. sexual assault, domestic assault), and
#'         other violent incidents EXCEPT `Assaults Against Police`.
#'         Sub-categories are deliberately NOT exposed; do not
#'         attempt to disaggregate this column.
#'   \item \strong{`Other Theft` aggregates} shoplifting, theft of
#'         personal property (over / under $5000), mail theft, and
#'         utilities theft.
#'   \item \strong{Reporting method}: 'All Offence' + 'Founded'
#'         (incidents the investigating officer determined did
#'         occur). This is \strong{NOT comparable} to Statistics
#'         Canada's published numbers, which use 'UCR Survey'
#'         Most-Serious-Offence (MSO) scoring. Do not mix VPD
#'         GeoDASH totals with StatCan totals in the same
#'         denominator.
#'   \item \strong{Location precision is deliberately reduced}:
#'         person-crimes have their X/Y \strong{randomized to
#'         several blocks and offset to an intersection}; no
#'         time/street-name is provided. Property-crimes are
#'         provided at the \strong{hundred-block} level only. Never
#'         interpret a row's X/Y as the actual scene of the
#'         incident.
#'   \item \strong{Crime classification + file status may change
#'         retroactively} as investigations evolve. The dataset is
#'         a snapshot, not an archive of fact.
#'   \item \strong{Update schedule}: VPD refreshes the feed every
#'         Sunday morning. Cache locally for reproducible analysis.
#'   \item \strong{Not a calls-for-service log}: only incidents
#'         that passed the founded-categorization filter appear.
#'         Totals do \strong{not} reflect total calls or complaints
#'         made to the VPD.
#'   \item \strong{Liability disclaimer}: VPD / Vancouver Police
#'         Board / City of Vancouver assume no liability for any
#'         decision made from this data. morie surfaces it as-is.
#' }
#'
#' @param offline If `TRUE` (default) and `zip_path`/`csv_path` are
#'   `NULL`, reads the bundled 550-row sample.
#' @param zip_path Optional path to a user-downloaded
#'   `crimedata_csv_AllNeighbourhoods_AllYears.zip`. Mutually
#'   exclusive with `csv_path`.
#' @param csv_path Optional path to a pre-extracted CSV. Mutually
#'   exclusive with `zip_path`.
#' @param max_features Optional row cap.
#' @param accept_terms If reading from a user-supplied zip/csv,
#'   pass `TRUE` to silently acknowledge VPD's terms (else a
#'   warning surfaces them once per session).
#' @return A `data.frame` with 10 columns.
#' @references VPD GeoDASH Open Data,
#'   \url{https://geodash.vpd.ca/opendata/}.
#' @examples
#' df <- morie_datasets_vpd_crime(offline = TRUE)
#' nrow(df)              # 550
#' table(df$TYPE)
#' table(df$NEIGHBOURHOOD)
#' @export
morie_datasets_vpd_crime <- function(offline = TRUE,
                                       zip_path = NULL,
                                       csv_path = NULL,
                                       max_features = NULL,
                                       accept_terms = FALSE) {
  if (!is.null(zip_path) && !is.null(csv_path))
    stop("pass only one of zip_path / csv_path", call. = FALSE)

  if (!is.null(zip_path)) {
    if (!file.exists(zip_path))
      stop(sprintf("VPD zip not found: %s", zip_path), call. = FALSE)
    .morie_vpd_terms_warning(accept_terms)
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    res <- system2("unzip",
                    args = c("-p", shQuote(zip_path),
                              "crimedata_csv_AllNeighbourhoods_AllYears.csv"),
                    stdout = tmp)
    if (!file.exists(tmp) || file.size(tmp) == 0L)
      stop("VPD zip extract failed -- ensure unzip is installed",
           call. = FALSE)
    df <- utils::read.csv(tmp, stringsAsFactors = FALSE)
  } else if (!is.null(csv_path)) {
    if (!file.exists(csv_path))
      stop(sprintf("VPD CSV not found: %s", csv_path), call. = FALSE)
    .morie_vpd_terms_warning(accept_terms)
    df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  } else if (offline) {
    path <- system.file("extdata", "vpd_crime_sample.csv",
                        package = "morie")
    if (!nzchar(path))
      stop("bundled VPD crime sample missing", call. = FALSE)
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    stop("VPD provides no automation API. Either set offline = TRUE ",
         "(bundled sample) or download the zip manually from ",
         "https://geodash.vpd.ca/opendata/ and pass zip_path = '...'.",
         call. = FALSE)
  }

  if (!is.null(max_features))
    df <- utils::head(df, as.integer(max_features))
  df
}

.MORIE_VPD_TERMS_WARNED <- new.env(parent = emptyenv())

.morie_vpd_terms_warning <- function(accept_terms) {
  if (isTRUE(accept_terms)) return(invisible())
  if (isTRUE(.MORIE_VPD_TERMS_WARNED$warned)) return(invisible())
  warning(paste0(
    "Loading VPD crime data implies acceptance of VPD's open data ",
    "terms (see morie_datasets_vpd_legal_disclaimer()). Pass ",
    "accept_terms = TRUE to silence this warning."), call. = FALSE)
  .MORIE_VPD_TERMS_WARNED$warned <- TRUE
  invisible()
}
