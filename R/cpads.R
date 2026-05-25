# SPDX-License-Identifier: AGPL-3.0-or-later
#
# cpads.R -- CPADS (Canadian Postsecondary Alcohol and Drug Use Survey)
# data-contract helpers for local-private analysis.
#
# Ported from src/morie/cpads.py.  CPADS PUMF row-level microdata is
# local-private: users must supply their own copy on disk; morie will
# NOT redistribute or fetch it.  These helpers describe the canonical
# analysis-column contract, validate frames against it, and canonicalize
# raw PUMF column names into MORIE's analysis schema.

#' Canonical CPADS analysis variables required by morie workflows.
#'
#' @keywords internal
#' @noRd
.MORIE_CPADS_REQUIRED_VARIABLES <- c(
  "weight",
  "alcohol_past12m",
  "heavy_drinking_30d",
  "ebac_tot",
  "ebac_legal",
  "cannabis_any_use",
  "age_group",
  "gender",
  "province_region",
  "mental_health",
  "physical_health"
)

#' Mapping from canonical CPADS analysis names to raw PUMF column names.
#'
#' @keywords internal
#' @noRd
.MORIE_CPADS_RAW_COLUMN_MAP <- list(
  weight = "wtpumf",
  alcohol_past12m = "alc05",
  heavy_drinking_30d_total = "alc12_30d_prev_total",
  heavy_drinking_30d_fallback = "alc12_30d_prev",
  cannabis_any_use = "can05",
  age_group = "age_groups",
  gender = "dvdemq01",
  province_region = "region",
  mental_health = "hwbq02",
  physical_health = "hwbq01",
  ebac_tot = "ebac_tot",
  ebac_legal = "ebac_legal"
)

#' Return the CPADS analysis-frame contract.
#'
#' Describes the morie CPADS contract: the canonical analysis
#' variables expected in a wrangled frame, the raw -> canonical
#' column map, and the conventional on-disk cache path used when a
#' user has wrangled the PUMF themselves.
#'
#' CPADS is open data (Open Government Licence -- Canada). The
#' Public Use Microdata File is available at open.canada.ca, dataset
#' `736fa9b2-62e4-4e31-aea4-51869605b363` (resource
#' `d2639429-c304-45a6-90b3-770562f4d46d`,
#' file `cpads-2021-2022-pumf2.csv`). Aggregate dashboards at
#' \url{https://health-infobase.canada.ca/substance-use/reports/cpads/}.
#' morie ships a 30-row synthetic at
#' `inst/extdata/cpads_pumf_synthetic.csv` for offline CRAN-safe
#' tests; `morie_datasets_cpads(offline = FALSE)` fetches the live
#' PUMF. Earlier morie versions wrongly claimed CPADS was
#' "FOI/agreement-only"; that was incorrect and has been retracted
#' as of 3MMM.
#'
#' @return A named list with fields `source_kind`,
#'   `expected_wrangled_path`, `required_variables`,
#'   `raw_column_map`, and `note`.
#' @export
#' @examples
#' contract <- morie_cpads_contract()
#' contract$required_variables
morie_cpads_contract <- function() {
  list(
    source_kind = "open_data_pumf",
    expected_wrangled_path = "data/cache/cpads_pumf_wrangled.rds",
    required_variables = as.character(.MORIE_CPADS_REQUIRED_VARIABLES),
    raw_column_map = .MORIE_CPADS_RAW_COLUMN_MAP,
    note = paste(
      "CPADS PUMF is open data (OGL-Canada). morie ships a synthetic",
      "fixture for offline tests; pass offline = FALSE to fetch the",
      "live CKAN PUMF from open.canada.ca resource",
      "d2639429-c304-45a6-90b3-770562f4d46d."
    )
  )
}

#' Identify missing canonical CPADS variables in a column set.
#'
#' @param columns Character vector of column names (e.g. `colnames(df)`).
#' @return Character vector of missing canonical CPADS variables (empty
#'   if every required variable is present).
#' @export
#' @examples
#' morie_cpads_missing_variables(c("weight", "alcohol_past12m"))
morie_cpads_missing_variables <- function(columns) {
  columns <- as.character(columns)
  setdiff(.MORIE_CPADS_REQUIRED_VARIABLES, columns)
}

#' Validate a data frame against the canonical CPADS analysis contract.
#'
#' @param frame A `data.frame` (or `tibble`).
#' @param strict Logical; if `TRUE` (default), raise an error when any
#'   required variable is missing.  If `FALSE`, return the missing
#'   names without raising.
#' @return Character vector of missing canonical variable names
#'   (invisibly when strict and complete).
#' @export
#' @examples
#' \dontrun{
#' morie_cpads_validate_frame(df, strict = TRUE)
#' }
morie_cpads_validate_frame <- function(frame, strict = TRUE) {
  if (!is.data.frame(frame)) {
    stop("morie_cpads_validate_frame: `frame` must be a data.frame.")
  }
  missing <- morie_cpads_missing_variables(colnames(frame))
  if (isTRUE(strict) && length(missing) > 0L) {
    stop(
      "CPADS frame is missing required variables: ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(missing)
}

#' Detect whether a data frame looks like raw CPADS PUMF data.
#'
#' Accepts either `wtpumf` (PUMF release) or `wtdf` (full dataset) as
#' the weight column; otherwise all documented raw PUMF columns must be
#' present.
#'
#' @param frame A `data.frame`.
#' @return Logical scalar; `TRUE` if the frame contains the raw CPADS
#'   PUMF schema, `FALSE` otherwise.
#' @export
morie_cpads_has_raw_columns <- function(frame) {
  if (!is.data.frame(frame)) {
    return(FALSE)
  }
  raw_cols <- unname(unlist(.MORIE_CPADS_RAW_COLUMN_MAP))
  cn <- colnames(frame)
  raw_cols_alt <- c(setdiff(raw_cols, "wtpumf"), "wtdf")
  all(raw_cols %in% cn) || all(raw_cols_alt %in% cn)
}

#' Coerce numeric, mapping non-numeric strings to `NA_real_` (silent).
#' @keywords internal
#' @noRd
.morie_cpads_to_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Recode a vector using a named map; values absent from the map become `NA`.
#' Codes 98 / 99 (CPADS "don't know" / "refused") collapse to `NA`.
#' @keywords internal
#' @noRd
.morie_cpads_recode_yn <- function(x) {
  # CPADS yes/no convention: 1 = yes, 2 = no, 98 / 99 = missing.
  out <- rep(NA_real_, length(x))
  out[x == 1] <- 1
  out[x == 2] <- 0
  out
}

#' Replace CPADS-style missing codes (98, 99) with `NA`.
#' @keywords internal
#' @noRd
.morie_cpads_strip_dknr <- function(x) {
  x[x == 98 | x == 99] <- NA
  x
}

#' Canonicalize raw CPADS PUMF columns into morie's analysis schema.
#'
#' First-pass canonicalization layer based on the public CPADS PUMF
#' field names.  If `frame` already carries the canonical columns it is
#' returned unchanged (after validation).  Otherwise raw PUMF columns
#' are remapped using `.MORIE_CPADS_RAW_COLUMN_MAP` and missing/DKNR
#' codes (98, 99) are converted to `NA`.
#'
#' @param frame A `data.frame` carrying raw CPADS PUMF columns (or the
#'   already-canonical analysis columns).
#' @return A `data.frame` with the canonical CPADS analysis columns.
#' @export
morie_cpads_canonicalize_frame <- function(frame) {
  if (!is.data.frame(frame)) {
    stop("morie_cpads_canonicalize_frame: `frame` must be a data.frame.")
  }
  if (!morie_cpads_has_raw_columns(frame)) {
    morie_cpads_validate_frame(frame, strict = TRUE)
    return(frame)
  }
  out <- frame
  weight_col <- if ("wtpumf" %in% colnames(frame)) "wtpumf" else "wtdf"
  out[["weight"]] <- .morie_cpads_to_numeric(frame[[weight_col]])
  out[["alcohol_past12m"]] <- .morie_cpads_recode_yn(frame[["alc05"]])
  # heavy_drinking_30d: prefer `_total` column, fall back to `_prev`.
  total <- frame[["alc12_30d_prev_total"]]
  fallback <- frame[["alc12_30d_prev"]]
  hd <- rep(NA_real_, length(total))
  hd[total == 1] <- 1
  hd[total == 0] <- 0
  na_mask <- is.na(hd)
  hd[na_mask & fallback == 1] <- 1
  hd[na_mask & fallback == 0] <- 0
  out[["heavy_drinking_30d"]] <- hd
  out[["cannabis_any_use"]] <- .morie_cpads_recode_yn(frame[["can05"]])
  out[["age_group"]] <- .morie_cpads_strip_dknr(frame[["age_groups"]])
  out[["gender"]] <- .morie_cpads_strip_dknr(frame[["dvdemq01"]])
  out[["province_region"]] <- .morie_cpads_strip_dknr(frame[["region"]])
  out[["mental_health"]] <- .morie_cpads_strip_dknr(frame[["hwbq02"]])
  out[["physical_health"]] <- .morie_cpads_strip_dknr(frame[["hwbq01"]])
  out[["ebac_tot"]] <- .morie_cpads_to_numeric(frame[["ebac_tot"]])
  out[["ebac_legal"]] <- .morie_cpads_to_numeric(frame[["ebac_legal"]])
  morie_cpads_validate_frame(out, strict = TRUE)
  out
}

#' Infer the on-disk file format for a CPADS file path.
#'
#' Recognises `.csv`, `.xlsx` / `.xls`, and `.rds`.  Raises for any
#' other extension.
#'
#' @param path A character scalar file path.
#' @return One of `"csv"`, `"excel"`, or `"rds"`.
#' @export
#' @examples
#' morie_cpads_infer_file_format("data/cache/cpads_pumf_wrangled.rds")
morie_cpads_infer_file_format <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    stop("morie_cpads_infer_file_format: `path` must be a character scalar.")
  }
  suffix <- tolower(tools::file_ext(path))
  if (identical(suffix, "csv")) {
    return("csv")
  }
  if (suffix %in% c("xlsx", "xls")) {
    return("excel")
  }
  if (identical(suffix, "rds")) {
    return("rds")
  }
  stop("Unsupported CPADS file format for path: ", path)
}
