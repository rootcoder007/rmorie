# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Toronto neighbourhood boundary-version awareness (HOOD_158 vs HOOD_140)
# plus Neighbourhood Improvement Area (NIA) loader and 158<->140 crosswalk.
#
# Why this module exists
# ----------------------
# The City of Toronto publishes TWO parallel neighbourhood schemas:
#
#   * 158 social-planning neighbourhoods (CURRENT, adopted 2022) -- the
#     column `HOOD_158` / `NEIGHBOURHOOD_158` on TPS PSDP feeds.
#   * 140 social-planning neighbourhoods (HISTORICAL, in force
#     ~2014-2021) -- the column `HOOD_140` / `NEIGHBOURHOOD_140`.
#
# The polygons are NOT identical: some 140s were split, some merged,
# and a handful were renamed. TPS crime feeds back-fill BOTH columns
# onto historical records via lat/lon re-geocoding, so a naive analysis
# silently mixes the two schemas across years.
#
# This module gives morie callers explicit version awareness:
#   * morie_to_neighbourhoods()         -- load polygons for 158 / 140 / NIA
#   * morie_tps_resolve_hood_col()      -- pick the right hood column
#   * morie_tps_assert_hood_version()   -- error / warn on version drift
#   * morie_tps_year_to_hood_version()  -- recommended schema per year
#   * morie_to_hood_crosswalk()         -- bundled 140<->158 mapping
#
# Upstream sources
# ----------------
#   City of Toronto Open Data (Open Government Licence -- Toronto):
#     - https://open.toronto.ca/dataset/neighbourhoods/
#     - https://open.toronto.ca/dataset/neighbourhood-improvement-areas/
#     - https://open.toronto.ca/dataset/neighbourhood-crime-rates/
#   CKAN datastore dump (live mode):
#     - https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3/action/
#         datastore_search?resource_id=<id>
#
# Bundled small synthetic fixtures live in inst/extdata/:
#   to_neighbourhoods_158.csv
#   to_neighbourhoods_140.csv
#   to_neighbourhood_improvement_areas.csv
#   to_hood_158_140_crosswalk.csv

#' Toronto neighbourhood boundary versions
#'
#' Wraps loading of the City of Toronto neighbourhood polygon layers
#' for the CURRENT 158-neighbourhood scheme (`HOOD_158`), the
#' HISTORICAL 140-neighbourhood scheme (`HOOD_140`), and the
#' Neighbourhood Improvement Area (NIA) layer. Also provides
#' version-resolution helpers so downstream analyses do not silently
#' mix the two schemes across years.
#'
#' @name morie_toronto_neighbourhoods
NULL

# Map a "version" string to the bundled fixture filename.
.morie_to_fixture_name <- function(version) {
  switch(version,
    "158" = "to_neighbourhoods_158.csv",
    "140" = "to_neighbourhoods_140.csv",
    "nia" = "to_neighbourhood_improvement_areas.csv",
    stop("unknown version: ", version, call. = FALSE))
}

# Read a bundled neighbourhood fixture from inst/extdata.
.morie_to_neighbourhoods_fixture <- function(version) {
  fname <- .morie_to_fixture_name(version)
  path <- system.file("extdata", fname, package = "morie")
  if (!nzchar(path)) {
    stop(sprintf(paste0(
      "Bundled fixture %s not found in the installed morie package. ",
      "Re-install morie, or pass offline = FALSE to fetch from ",
      "open.toronto.ca CKAN."), fname), call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

# Internal: live CKAN datastore_search fetcher. Mockable via
# testthat::local_mocked_bindings(.morie_to_ckan_dump_csv = ...).
.morie_to_ckan_dump_csv <- function(resource_id, limit = 100000L) {
  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop(paste0(
      "Open Toronto CKAN dump fetch needs httr2 + jsonlite. ",
      "install.packages(c('httr2', 'jsonlite'))"),
      call. = FALSE)
  }
  url <- sprintf(paste0(
    "https://ckan0.cf.opendata.inter.prod-toronto.ca/api/3/action/",
    "datastore_search?resource_id=%s&limit=%d"),
    resource_id, as.integer(limit))
  req <- httr2::request(url)
  req <- httr2::req_timeout(req, 60)
  resp <- httr2::req_perform(req)
  body <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                              simplifyVector = TRUE)
  recs <- body$result$records
  if (is.null(recs)) data.frame() else as.data.frame(recs)
}

# Default CKAN resource ids for the live mode (current as of 2026-05).
# Pass `resource_id =` to morie_to_neighbourhoods() to override.
.MORIE_TO_DEFAULT_RESOURCE_IDS <- list(
  "158" = "4def3f65-2a65-4a4f-83c4-867e4cca287a",
  "140" = "0c39c4d8-c75c-49ba-9b00-4a3c0c0b5d6e",
  "nia" = "fec0e35a-5da9-4ab3-b69e-66e5a98c8d99")

#' Load a Toronto neighbourhood polygon layer
#'
#' Returns a data.frame with the canonical City of Toronto Open Data
#' schema (`_id`, `AREA_ID`, `AREA_ATTR_ID`, `PARENT_AREA_ID`,
#' `AREA_SHORT_CODE`, `AREA_LONG_CODE`, `AREA_NAME`, `AREA_DESC`,
#' `CLASSIFICATION`, `CLASSIFICATION_CODE`, `OBJECTID`, `geometry`)
#' for the requested version.
#'
#' @param version One of `"158"` (current City scheme), `"140"`
#'   (historical 2014--2021 scheme), or `"nia"` (Neighbourhood
#'   Improvement Areas).
#' @param offline If `TRUE` (default), read the small bundled synthetic
#'   fixture from `inst/extdata/`. If `FALSE`, hit the live City of
#'   Toronto CKAN `datastore_search` endpoint via httr2.
#' @param resource_id Optional CKAN resource id override. Used only
#'   when `offline = FALSE`.
#' @return A `data.frame`.
#' @references City of Toronto Open Data, "Neighbourhoods" dataset
#'   (\url{https://open.toronto.ca/dataset/neighbourhoods/});
#'   "Neighbourhood Improvement Areas"
#'   (\url{https://open.toronto.ca/dataset/neighbourhood-improvement-areas/});
#'   licensed under the Open Government Licence -- Toronto.
#' @examples
#' df <- morie_to_neighbourhoods("158", offline = TRUE)
#' head(df[, c("AREA_SHORT_CODE", "AREA_NAME")])
#' @export
morie_to_neighbourhoods <- function(version = c("158", "140", "nia"),
                                     offline = TRUE,
                                     resource_id = NULL) {
  version <- match.arg(version)
  if (isTRUE(offline)) {
    return(.morie_to_neighbourhoods_fixture(version))
  }
  if (is.null(resource_id)) {
    resource_id <- .MORIE_TO_DEFAULT_RESOURCE_IDS[[version]]
  }
  .morie_to_ckan_dump_csv(resource_id)
}

# Column-name candidates for each version (TPS feeds use HOOD_*;
# Open Toronto layers use NEIGHBOURHOOD_*; some legacy exports use
# lower-case).
.MORIE_TPS_HOOD_VERSIONS <- list(
  "158" = c("HOOD_158", "hood_158", "NEIGHBOURHOOD_158",
            "neighbourhood_158"),
  "140" = c("HOOD_140", "hood_140", "NEIGHBOURHOOD_140",
            "neighbourhood_140"))

#' Resolve which HOOD_* column to use on a TPS crime data.frame
#'
#' Many TPS PSDP crime layers carry BOTH `HOOD_158` (current) and
#' `HOOD_140` (historical 2014--2021) columns. Pick the version your
#' analysis needs explicitly so the two schemes are not silently mixed
#' across years.
#'
#' @param df A TPS crime `data.frame`.
#' @param prefer Either `"158"` (current) or `"140"` (historical).
#' @param fallback If `TRUE` (default), accept the other version when
#'   the preferred one is absent (with a warning). If `FALSE`, return
#'   `NULL` when the preferred version is missing.
#' @return Character scalar (the chosen column name), or `NULL` if no
#'   suitable column is present.
#' @examples
#' df <- data.frame(OCC_YEAR = 2024L, HOOD_158 = "82", HOOD_140 = "82")
#' morie_tps_resolve_hood_col(df, prefer = "158")
#' @export
morie_tps_resolve_hood_col <- function(df, prefer = c("158", "140"),
                                        fallback = TRUE) {
  prefer <- match.arg(prefer)
  other  <- setdiff(c("158", "140"), prefer)
  for (cand in .MORIE_TPS_HOOD_VERSIONS[[prefer]]) {
    if (cand %in% names(df)) return(cand)
  }
  if (isTRUE(fallback)) {
    for (cand in .MORIE_TPS_HOOD_VERSIONS[[other]]) {
      if (cand %in% names(df)) {
        warning(sprintf(
          "preferred HOOD_%s not present; falling back to %s",
          prefer, cand), call. = FALSE)
        return(cand)
      }
    }
  }
  warning("no HOOD_158 / HOOD_140 column found in df", call. = FALSE)
  NULL
}

#' Assert the HOOD_* schema version of a TPS data.frame
#'
#' Errors when the expected schema's column is absent. Warns when
#' BOTH schemas are present (downstream code MAY accidentally use the
#' wrong one).
#'
#' @param df A TPS crime `data.frame`.
#' @param expected Either `"158"` or `"140"`.
#' @return Invisibly `TRUE` on success.
#' @export
morie_tps_assert_hood_version <- function(df,
                                            expected = c("158", "140")) {
  expected <- match.arg(expected)
  cands <- .MORIE_TPS_HOOD_VERSIONS[[expected]]
  if (length(intersect(cands, names(df))) == 0L) {
    stop(sprintf(
      "expected HOOD_%s schema but no %s column found in df",
      expected, paste(cands, collapse = " / ")),
      call. = FALSE)
  }
  other <- setdiff(c("158", "140"), expected)
  other_present <- intersect(.MORIE_TPS_HOOD_VERSIONS[[other]], names(df))
  if (length(other_present) > 0L) {
    warning(sprintf(paste0(
      "df carries BOTH HOOD_%s (expected) and HOOD_%s columns; pick ",
      "one explicitly via morie_tps_resolve_hood_col() to avoid silent ",
      "mismatches between the 158 (current) and 140 (historical) ",
      "neighbourhood schemes."),
      expected, other), call. = FALSE)
  }
  invisible(TRUE)
}

#' Recommended HOOD_* schema version for a given OCC_YEAR
#'
#' The City of Toronto adopted the 158-neighbourhood scheme in 2022.
#' Pre-2022 TPS crime records are most faithfully analysed in the
#' historical 140-neighbourhood scheme; 2022-onwards records align
#' with the 158-scheme. TPS often back-fills both columns onto the
#' same record via lat/lon re-geocoding, but the polygon boundaries
#' do not match.
#'
#' @param year Integer year (or vector of years).
#' @return Character vector of `"158"` / `"140"` recommendations,
#'   parallel to `year`.
#' @export
morie_tps_year_to_hood_version <- function(year) {
  y <- suppressWarnings(as.integer(year))
  ifelse(is.na(y), NA_character_, ifelse(y >= 2022L, "158", "140"))
}

#' Load the bundled BIDIRECTIONAL 158 <-> 140 neighbourhood crosswalk
#'
#' Returns the bundled `inst/extdata/to_hood_158_140_crosswalk.csv`,
#' computed from polygon intersection of the two upstream Open Toronto
#' GeoJSON layers (`Neighbourhoods - 4326.geojson` and
#' `Neighbourhoods - historical 140 - 4326.geojson`) reprojected to
#' EPSG:3347 (NAD83 Statistics Canada Lambert -- metric, accurate
#' areas). Seven columns:
#'
#' \describe{
#'   \item{hood_140}{3-char zero-padded historical short code}
#'   \item{name_140}{historical neighbourhood name (carries "(NN)" suffix)}
#'   \item{hood_158}{3-char zero-padded current short code}
#'   \item{name_158}{current neighbourhood name}
#'   \item{pct_140_in_158}{FORWARD: percent of the 140's area inside this
#'     158. Per-140 rows sum to 100. Used by
#'     [morie_tps_disaggregate_140_to_158()] as the cake-cutting
#'     weight under a uniform-density assumption.}
#'   \item{pct_158_in_140}{REVERSE: percent of the 158's area inside this
#'     140. Per-158 rows sum to 100. For pure cake-cuts every split
#'     child has `pct_158_in_140 == 100` (each 158 is entirely
#'     inside its parent 140), so
#'     [morie_tps_aggregate_158_to_140()] is mathematically EXACT
#'     (lossless sum) for the 1:1 + split cohort. Only the one
#'     split+merge sliver in the bundled OT data has a non-100
#'     reverse percent.}
#'   \item{relation}{"1:1" / "split" (one 140 -> N 158s) / "merge"
#'     (multiple 140s -> one 158) / "split+merge"}
#' }
#'
#' Empirical distribution on the bundled OT data:
#'
#'   * 123 1:1 rows (78\% of 140 hoods)            -- both percents == 100
#'   * 34 split rows (16 historical hoods)         -- pct_158_in_140 == 100
#'   * 1 merge                                     -- both percents == 100
#'   * 1 split+merge                               -- one sliver < 100
#'
#' @return A `data.frame` with the columns above. `hood_140` and
#'   `hood_158` are character (zero-padded to 3 chars).
#' @export
morie_to_hood_crosswalk <- function() {
  path <- system.file("extdata", "to_hood_158_140_crosswalk.csv",
                      package = "morie")
  if (!nzchar(path)) {
    stop(paste0(
      "Bundled 158<->140 crosswalk fixture missing from the installed ",
      "morie package."), call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                   colClasses = c(hood_140 = "character",
                                  hood_158 = "character"))
}

# Backwards-compatibility: the original equivalency joiners (added in
# the same phase) read `area_overlap_pct`; remap to the new
# `pct_140_in_158` so they keep working.
.morie_to_legacy_overlap_col <- function(cw) {
  if (!"area_overlap_pct" %in% names(cw)) {
    cw$area_overlap_pct <- cw$pct_140_in_158
  }
  cw
}

# Normalise a hood-code value to the 3-char zero-padded canonical form
# the crosswalk uses ("82" -> "082"; "0082" -> "082"; "Niagara" -> NA).
.morie_to_normalise_hood_code <- function(x) {
  s <- trimws(as.character(x))
  i <- suppressWarnings(as.integer(s))
  ifelse(is.na(i) | i < 0L | i > 999L, NA_character_,
         sprintf("%03d", i))
}

#' Add an equivalent HOOD_158 column to a HOOD_140-keyed data.frame
#'
#' Looks up each row's `HOOD_140` (or `hood_140` / `NEIGHBOURHOOD_140`
#' / `neighbourhood_140`) in the bundled crosswalk and writes the
#' PRIMARY-overlap 158 hood code into a new column (default name
#' `HOOD_158_equiv`).
#'
#' For 1:1 mappings the result is exact. For splits (1 historical
#' hood -> 2--4 current hoods) the 158 hood with the largest area
#' overlap wins; this is **lossy** -- analyses at the 158-level
#' should ideally re-aggregate from the per-incident lat/lon rather
#' than relying on the primary-overlap join.
#'
#' @param df A TPS crime `data.frame`.
#' @param col_in Name of the input HOOD_140 column. By default the
#'   first match from `c("HOOD_140", "hood_140", "NEIGHBOURHOOD_140",
#'   "neighbourhood_140")` present in `df`.
#' @param col_out Name of the new column to add. Default
#'   `"HOOD_158_equiv"`.
#' @param crosswalk Optional pre-loaded crosswalk; defaults to
#'   `morie_to_hood_crosswalk()`.
#' @return `df` with the equivalent-code column appended.
#' @examples
#' df <- data.frame(EVENT_ID = 1:3, HOOD_140 = c("082", "001", "075"))
#' morie_tps_add_hood_158_from_140(df)
#' @export
morie_tps_add_hood_158_from_140 <- function(df, col_in = NULL,
                                              col_out = "HOOD_158_equiv",
                                              crosswalk = NULL) {
  if (is.null(col_in)) col_in <- suppressWarnings(
    morie_tps_resolve_hood_col(df, prefer = "140", fallback = FALSE))
  if (is.null(col_in) || !(col_in %in% names(df))) {
    stop("no HOOD_140 column found in df", call. = FALSE)
  }
  if (is.null(crosswalk)) crosswalk <- morie_to_hood_crosswalk()
  cw <- .morie_to_legacy_overlap_col(crosswalk)
  # Per-140, keep only the primary-overlap 158 (highest pct).
  ord <- order(cw$hood_140, -cw$pct_140_in_158)
  primary <- cw[ord, ]
  primary <- primary[!duplicated(primary$hood_140), ]
  lookup <- setNames(primary$hood_158, primary$hood_140)
  key <- .morie_to_normalise_hood_code(df[[col_in]])
  df[[col_out]] <- unname(lookup[key])
  df
}

#' Add an equivalent HOOD_140 column to a HOOD_158-keyed data.frame
#'
#' The mirror of [morie_tps_add_hood_158_from_140()]. For 158-hoods
#' that did not exist in the 140-scheme (the newly-split children
#' like Etobicoke City Centre / Islington from 14-Islington-City-
#' Centre-West) the result is the historical parent 140-hood.
#'
#' @inheritParams morie_tps_add_hood_158_from_140
#' @param col_in Name of the input HOOD_158 column.
#' @param col_out Name of the new column. Default `"HOOD_140_equiv"`.
#' @return `df` with the equivalent-code column appended.
#' @export
morie_tps_add_hood_140_from_158 <- function(df, col_in = NULL,
                                              col_out = "HOOD_140_equiv",
                                              crosswalk = NULL) {
  if (is.null(col_in)) col_in <- suppressWarnings(
    morie_tps_resolve_hood_col(df, prefer = "158", fallback = FALSE))
  if (is.null(col_in) || !(col_in %in% names(df))) {
    stop("no HOOD_158 column found in df", call. = FALSE)
  }
  if (is.null(crosswalk)) crosswalk <- morie_to_hood_crosswalk()
  # Per-158, keep only the primary-overlap 140 (highest pct_158_in_140).
  # For pure splits all children have pct_158_in_140 == 100 so this is
  # the unique parent; for split+merge the dominant parent wins.
  ord <- order(crosswalk$hood_158, -crosswalk$pct_158_in_140)
  primary <- crosswalk[ord, ]
  primary <- primary[!duplicated(primary$hood_158), ]
  lookup <- setNames(primary$hood_140, primary$hood_158)
  key <- .morie_to_normalise_hood_code(df[[col_in]])
  df[[col_out]] <- unname(lookup[key])
  df
}

#' Disaggregate per-140 counts to per-158 counts (uniform-density)
#'
#' Cake-cutting in the FORWARD direction. Given a `data.frame` of per-
#' historical-hood counts (one row per `hood_140`, one or more numeric
#' count columns), splits each 140's count across its 158-children in
#' proportion to `pct_140_in_158 / 100`. For 1:1 hoods the count is
#' passed through unchanged. For splits the count is partitioned
#' (e.g. 140-75 Church-Yonge Corridor's 100 incidents become 59.24 in
#' 158-168 Downtown Yonge East and 40.76 in 158-167 Church-Wellesley).
#'
#' This assumes UNIFORM SPATIAL DENSITY of the underlying events
#' inside each 140-hood -- which is the best you can do without per-
#' incident lat/lon. If you have lat/lon, prefer re-binning from
#' points (via `sf::st_join` against
#' `morie_to_neighbourhoods("158", offline = FALSE)`) over this
#' uniform-density approximation.
#'
#' @param df A `data.frame` keyed on a 140-hood column.
#' @param hood_140_col Name of the 140-hood column. Default
#'   `"HOOD_140"`.
#' @param count_cols Character vector of numeric count columns to
#'   disaggregate. Default: every numeric column in `df` except
#'   `hood_140_col`.
#' @param crosswalk Optional pre-loaded crosswalk; defaults to
#'   `morie_to_hood_crosswalk()`.
#' @return A `data.frame` with columns `hood_158`, `name_158`,
#'   `hood_140`, all chosen `count_cols` (now per-158 fractional
#'   counts), and `pct_140_in_158` (the cake-cut weight applied).
#' @examples
#' df <- data.frame(HOOD_140 = c("075", "001"),
#'                  incidents = c(100, 42))
#' morie_tps_disaggregate_140_to_158(df)
#' @export
morie_tps_disaggregate_140_to_158 <- function(df,
                                                hood_140_col = "HOOD_140",
                                                count_cols = NULL,
                                                crosswalk = NULL) {
  if (!(hood_140_col %in% names(df))) {
    stop(sprintf("hood_140_col '%s' not in df", hood_140_col),
         call. = FALSE)
  }
  if (is.null(crosswalk)) crosswalk <- morie_to_hood_crosswalk()
  if (is.null(count_cols)) {
    count_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    count_cols <- setdiff(count_cols, hood_140_col)
  }
  if (length(count_cols) == 0L) {
    stop("no numeric count columns found in df", call. = FALSE)
  }
  key <- .morie_to_normalise_hood_code(df[[hood_140_col]])
  df_n <- df
  df_n$.cw_key <- key
  cw <- crosswalk[, c("hood_140", "name_140", "hood_158", "name_158",
                       "pct_140_in_158")]
  out <- merge(df_n, cw, by.x = ".cw_key", by.y = "hood_140",
               all.x = TRUE, sort = FALSE)
  w <- out$pct_140_in_158 / 100
  for (cn in count_cols) {
    out[[cn]] <- out[[cn]] * w
  }
  keep <- c("hood_158", "name_158", "hood_140", count_cols,
            "pct_140_in_158")
  names(out)[names(out) == ".cw_key"] <- "hood_140"
  out[, keep, drop = FALSE]
}

#' Aggregate per-158 counts to per-140 counts (EXACT for pure cake-cuts)
#'
#' Cake-cutting in the REVERSE direction. Given a `data.frame` of per-
#' current-hood counts (one row per `hood_158`, one or more numeric
#' count columns), sums across the 158-children of each 140 weighted
#' by `pct_158_in_140 / 100`. For 1:1 hoods the count passes through.
#' For splits the children's counts are summed exactly (each child has
#' `pct_158_in_140 == 100` for clean cake-cuts).
#'
#' Unlike [morie_tps_disaggregate_140_to_158()], this requires NO
#' uniform-density assumption when the source is a clean cake-cut --
#' the partition is exhaustive and disjoint by construction. The only
#' lossy case is the `split+merge` edge (one Willowdale East sliver
#' in the bundled OT data); the function handles it via the
#' `pct_158_in_140` weights regardless.
#'
#' @param df A `data.frame` keyed on a 158-hood column.
#' @param hood_158_col Name of the 158-hood column. Default
#'   `"HOOD_158"`.
#' @param count_cols Character vector of numeric count columns. By
#'   default every numeric column except `hood_158_col`.
#' @param crosswalk Optional pre-loaded crosswalk.
#' @return A `data.frame` with one row per 140-hood, columns
#'   `hood_140`, `name_140`, and the summed `count_cols`.
#' @examples
#' df <- data.frame(HOOD_158 = c("167", "168", "001"),
#'                  incidents = c(40, 60, 42))
#' morie_tps_aggregate_158_to_140(df)
#' @export
morie_tps_aggregate_158_to_140 <- function(df,
                                             hood_158_col = "HOOD_158",
                                             count_cols = NULL,
                                             crosswalk = NULL) {
  if (!(hood_158_col %in% names(df))) {
    stop(sprintf("hood_158_col '%s' not in df", hood_158_col),
         call. = FALSE)
  }
  if (is.null(crosswalk)) crosswalk <- morie_to_hood_crosswalk()
  if (is.null(count_cols)) {
    count_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    count_cols <- setdiff(count_cols, hood_158_col)
  }
  if (length(count_cols) == 0L) {
    stop("no numeric count columns found in df", call. = FALSE)
  }
  key <- .morie_to_normalise_hood_code(df[[hood_158_col]])
  df_n <- df
  df_n$.cw_key <- key
  cw <- crosswalk[, c("hood_140", "name_140", "hood_158",
                       "pct_158_in_140")]
  joined <- merge(df_n, cw, by.x = ".cw_key", by.y = "hood_158",
                   all.x = TRUE, sort = FALSE)
  w <- joined$pct_158_in_140 / 100
  for (cn in count_cols) {
    joined[[cn]] <- joined[[cn]] * w
  }
  agg <- stats::aggregate(
    joined[, count_cols, drop = FALSE],
    by = list(hood_140 = joined$hood_140,
              name_140 = joined$name_140),
    FUN = sum, na.rm = TRUE)
  agg <- agg[order(agg$hood_140), ]
  rownames(agg) <- NULL
  agg
}
