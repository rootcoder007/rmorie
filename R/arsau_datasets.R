# SPDX-License-Identifier: AGPL-3.0-or-later
#' ARSAU CKAN sidecar helpers + tidy registry view (R-side companion
#' to \code{morie.arsau_datasets}).
#'
#' The main R-side loaders + registry list-of-lists already live in
#' \code{R/arsau.R} (\code{morie_arsau_load_main_records()},
#' \code{morie_arsau_load_individual_records()},
#' \code{morie_arsau_load_probe_cycle_records()},
#' \code{morie_arsau_load_weapon_records()},
#' \code{morie_arsau_load_aggregate_summary()},
#' \code{morie_arsau_load_detailed_dataset()}, plus
#' \code{ARSAU_REGISTRY()}, \code{ARSAU_YEARS()}, \code{ARSAU_KINDS()},
#' \code{morie_arsau_read_sidecar()}, \code{morie_arsau_available_*()},
#' and \code{morie_arsau_describe()}).  This file does NOT duplicate
#' those.  It adds the remaining surface from the Python source:
#'
#' \itemize{
#'   \item \code{\link{morie_arsau_registry_df}}: the registry as a
#'         tidy \code{data.frame} (one row per published file), the
#'         canonical R equivalent of \code{ARSAU_REGISTRY} in Python.
#'   \item \code{\link{morie_arsau_sidecar_schema}}: simplified
#'         \code{[name, type, notes]} extraction from a CKAN sidecar
#'         (mirrors \code{morie.arsau_datasets.sidecar_schema}).
#'   \item \code{\link{morie_arsau_sidecar_to_frame}}: convert the
#'         array-of-arrays \code{records} body of a CKAN sidecar to a
#'         \code{data.frame} keyed by \code{fields[].id} (mirrors
#'         \code{morie.arsau_datasets.sidecar_to_frame}).
#'   \item \code{\link{morie_arsau_read_xlsx_dictionary}}: read an
#'         Ontario-Catalogue XLSX data-dictionary sidecar
#'         (requires \pkg{readxl}; gated with \code{requireNamespace}).
#'   \item \code{\link{morie_arsau_read_markdown_dictionary}}: read an
#'         Ontario-Catalogue Markdown data-dictionary sidecar with no
#'         extra dependencies.
#'   \item \code{\link{morie_arsau_ckan_url}} +
#'         \code{\link{morie_arsau_fetch_sidecar}}: build the upstream
#'         CKAN \code{datastore_search} URL and (optionally) fetch
#'         the sidecar JSON via \pkg{httr2}.
#' }
#'
#' @references
#' Ontario Ministry of the Solicitor General. \emph{Data on Police Use
#' of Force in Ontario}, 2020-2022 / 2023 / 2024 releases.  Published
#' on the Ontario Data Catalogue:
#' \url{https://data.ontario.ca/dataset/police-use-of-force-race-based-data}.
#' CKAN \code{datastore_search} endpoint:
#' \code{datastore_search} (\url{https://data.ontario.ca/}).
#' Each annual release ships per-resource technical notes; the 2023
#' weapon_records release is explicitly flagged as containing
#' non-compliant data and the open-data file is renamed accordingly.
#'
#' @name arsau_datasets
NULL


# ---------------------------------------------------------------------------
# Registry as a tidy data.frame
# ---------------------------------------------------------------------------

#' ARSAU registry rendered as a tidy \code{data.frame}.
#'
#' Returns one row per \code{(year_or_range, kind)} entry in the
#' package's internal registry, with the same columns as the Python
#' \code{ARSAU_REGISTRY} mapping but in row-major \code{data.frame}
#' form.  The underlying list-of-lists is still available via
#' \code{\link{ARSAU_REGISTRY}}.
#'
#' @param language "en" or "fr"; selects the description column.
#' @return A \code{data.frame} with columns \code{year_or_range},
#'   \code{kind}, \code{csv_filename}, \code{sidecar_filename},
#'   \code{expected_rows}, \code{expected_cols}, \code{is_valid},
#'   \code{description}.
#' @references Ontario Ministry of the Solicitor General, ARSAU
#'   per-resource technical release notes (2020-2022 / 2023 / 2024).
#' @export
morie_arsau_registry_df <- function(language = "en") {
  reg <- ARSAU_REGISTRY()
  if (length(reg) == 0L) {
    return(data.frame(
      year_or_range    = character(0),
      kind             = character(0),
      csv_filename     = character(0),
      sidecar_filename = character(0),
      expected_rows    = integer(0),
      expected_cols    = integer(0),
      is_valid         = logical(0),
      description      = character(0),
      stringsAsFactors = FALSE
    ))
  }
  use_fr <- tolower(substr(language, 1L, 2L)) == "fr"
  rows <- lapply(reg, function(e) {
    desc <- if (use_fr) e$description_fr else e$description_en
    data.frame(
      year_or_range    = e$year_or_range,
      kind             = e$kind,
      csv_filename     = e$csv_filename,
      sidecar_filename = if (is.null(e$sidecar_filename)) NA_character_
                         else e$sidecar_filename,
      expected_rows    = as.integer(e$expected_rows),
      expected_cols    = as.integer(e$expected_cols),
      is_valid         = isTRUE(e$is_valid),
      description      = as.character(desc),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


# ---------------------------------------------------------------------------
# CKAN sidecar JSON helpers
# ---------------------------------------------------------------------------

#' Extract a simplified \code{[name, type, notes]} schema from a
#' parsed CKAN sidecar.
#'
#' Accepts the result of \code{\link{morie_arsau_read_sidecar}} (a
#' list with \code{fields} and \code{records} entries) and returns a
#' tidy \code{data.frame} of column metadata.  Entries that lack an
#' \code{id} are dropped.
#'
#' @param sidecar A list as returned by
#'   \code{morie_arsau_read_sidecar()}.
#' @return A \code{data.frame} with columns \code{name},
#'   \code{type}, \code{notes}.
#' @references CKAN \emph{datastore_search} response schema, as
#'   served by \code{datastore_search} (\url{https://data.ontario.ca/}).
#' @export
morie_arsau_sidecar_schema <- function(sidecar) {
  if (!is.list(sidecar)) {
    stop("morie_arsau_sidecar_schema: 'sidecar' must be a list.",
         call. = FALSE)
  }
  fields <- sidecar$fields
  if (is.null(fields) || length(fields) == 0L) {
    return(data.frame(name = character(0), type = character(0),
                      notes = character(0), stringsAsFactors = FALSE))
  }
  rows <- list()
  for (f in fields) {
    if (!is.list(f)) next
    nm <- trimws(as.character(f$id %||% ""))
    if (!nzchar(nm)) next
    ftype <- f$type
    info  <- if (is.list(f$info)) f$info else list()
    notes <- info$notes
    rows[[length(rows) + 1L]] <- data.frame(
      name  = nm,
      type  = if (is.null(ftype)) NA_character_ else as.character(ftype),
      notes = if (is.null(notes)) NA_character_ else as.character(notes),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame(name = character(0), type = character(0),
                      notes = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Convert a CKAN sidecar's \code{records} array-of-arrays into a
#' \code{data.frame}.
#'
#' The \code{fields[].id} array supplies the column names; records
#' are array-of-array, so the column order in the JSON matches the
#' column order in the resulting \code{data.frame}.
#'
#' @param sidecar A list as returned by
#'   \code{morie_arsau_read_sidecar()}.
#' @return A \code{data.frame} (zero rows if \code{records} is empty).
#' @references CKAN \emph{datastore_search} response schema.
#' @export
morie_arsau_sidecar_to_frame <- function(sidecar) {
  if (!is.list(sidecar)) {
    stop("morie_arsau_sidecar_to_frame: 'sidecar' must be a list.",
         call. = FALSE)
  }
  fields  <- sidecar$fields  %||% list()
  records <- sidecar$records %||% list()
  col_names <- vapply(
    fields,
    function(f) if (is.list(f) && !is.null(f$id)) as.character(f$id) else "",
    character(1)
  )
  col_names <- col_names[nzchar(col_names)]

  if (length(records) == 0L) {
    if (length(col_names) == 0L) return(data.frame())
    empty_cols <- replicate(length(col_names), character(0),
                            simplify = FALSE)
    names(empty_cols) <- col_names
    return(as.data.frame(empty_cols, stringsAsFactors = FALSE))
  }

  # Each record is either a list-keyed-by-name (older CKAN) or an
  # array-of-values aligned with fields[] (newer CKAN).  Detect by
  # the first record.
  first <- records[[1L]]
  if (is.list(first) && !is.null(names(first)) && all(nzchar(names(first)))) {
    # Named-record shape.
    rows <- lapply(records, function(r) {
      vals <- if (length(col_names) > 0L) {
        lapply(col_names, function(k) {
          v <- r[[k]]
          if (is.null(v)) NA else v
        })
      } else {
        lapply(r, function(v) if (is.null(v)) NA else v)
      }
      names(vals) <- if (length(col_names) > 0L) col_names else names(r)
      as.data.frame(vals, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
  } else {
    # Array-of-values shape.
    rows <- lapply(records, function(r) {
      vals <- lapply(seq_along(r), function(i) {
        v <- r[[i]]
        if (is.null(v)) NA else v
      })
      if (length(col_names) >= length(vals)) {
        names(vals) <- col_names[seq_along(vals)]
      } else {
        names(vals) <- sprintf("V%d", seq_along(vals))
      }
      as.data.frame(vals, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
  }
  rownames(out) <- NULL
  out
}


# ---------------------------------------------------------------------------
# XLSX / Markdown data-dictionary readers
# ---------------------------------------------------------------------------

#' Read an Ontario-Catalogue XLSX data-dictionary sidecar.
#'
#' Some ARSAU releases ship a companion \code{*.xlsx} file alongside
#' the CSV that holds the column-level data dictionary (variable name
#' + dtype + notes).  This helper reads the first sheet via
#' \pkg{readxl} and normalises the column names to
#' \code{name / type / notes}.  Requires the optional \pkg{readxl}
#' dependency.
#'
#' @param path Path to the XLSX file.
#' @param sheet Sheet identifier (name or 1-based integer).  Default
#'   \code{1L}.
#' @return A \code{data.frame} with columns \code{name}, \code{type},
#'   \code{notes}.  Other columns from the XLSX are preserved with
#'   their upstream names.
#' @references Ontario Ministry of the Solicitor General data
#'   dictionaries accompanying the ARSAU CSV releases.
#' @export
morie_arsau_read_xlsx_dictionary <- function(path, sheet = 1L) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop(
      "morie_arsau_read_xlsx_dictionary requires the optional ",
      "'readxl' package; install it with install.packages('readxl').",
      call. = FALSE
    )
  }
  if (!file.exists(path)) {
    stop(sprintf("XLSX data-dictionary not found at %s", path),
         call. = FALSE)
  }
  df <- as.data.frame(
    readxl::read_excel(path, sheet = sheet),
    stringsAsFactors = FALSE
  )
  # Normalise column names: lower-case + strip non-alnum.
  norm <- tolower(gsub("[^a-z0-9]", "", tolower(names(df))))
  # Map the most common upstream spellings to canonical fields.
  rename_map <- c(
    field = "name", fieldname = "name", variable = "name",
    variablename = "name", columnname = "name", column = "name",
    name = "name",
    datatype = "type", type = "type", dtype = "type",
    description = "notes", notes = "notes", definition = "notes",
    comment = "notes"
  )
  for (i in seq_along(norm)) {
    if (norm[i] %in% names(rename_map)) {
      names(df)[i] <- rename_map[[norm[i]]]
    }
  }
  for (req in c("name", "type", "notes")) {
    if (!(req %in% names(df))) df[[req]] <- NA_character_
  }
  # Put the canonical triple first.
  ordered <- c("name", "type", "notes",
               setdiff(names(df), c("name", "type", "notes")))
  df <- df[, ordered, drop = FALSE]
  rownames(df) <- NULL
  df
}


#' Read an Ontario-Catalogue Markdown data-dictionary sidecar.
#'
#' Parses a simple pipe-table Markdown sidecar of the form
#' \preformatted{
#' | name | type | notes |
#' |------|------|-------|
#' | foo  | int  | ...   |
#' }
#' as published by some ARSAU releases.  No external dependencies are
#' required: the parser is pure base R.
#'
#' @param path Path to the Markdown file.
#' @return A \code{data.frame} with one row per table row.  Returns
#'   an empty \code{data.frame} if the file has no parseable pipe
#'   table.
#' @references Ontario Ministry of the Solicitor General data
#'   dictionaries accompanying the ARSAU CSV releases.
#' @export
morie_arsau_read_markdown_dictionary <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Markdown data-dictionary not found at %s", path),
         call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # Keep only pipe-table lines (start and end with a pipe, possibly
  # with surrounding whitespace).
  pipe_re <- "^\\s*\\|.*\\|\\s*$"
  pipe_lines <- lines[grepl(pipe_re, lines)]
  # Drop the divider rows (cells made of dashes + colons).
  divider_re <- "^\\s*\\|?\\s*:?-+:?\\s*(\\|\\s*:?-+:?\\s*)+\\|?\\s*$"
  pipe_lines <- pipe_lines[!grepl(divider_re, pipe_lines)]
  if (length(pipe_lines) < 2L) {
    return(data.frame())
  }
  split_row <- function(s) {
    s <- gsub("^\\s*\\|", "", s)
    s <- gsub("\\|\\s*$", "", s)
    trimws(strsplit(s, "|", fixed = TRUE)[[1L]])
  }
  header <- split_row(pipe_lines[1L])
  body   <- pipe_lines[-1L]
  rows <- lapply(body, function(s) {
    vals <- split_row(s)
    length(vals) <- length(header)
    setNames(as.list(vals), header)
  })
  out <- do.call(rbind, lapply(rows, as.data.frame,
                               stringsAsFactors = FALSE))
  if (is.null(out)) return(data.frame())
  rownames(out) <- NULL
  out
}


# ---------------------------------------------------------------------------
# CKAN URL builder + optional sidecar fetcher
# ---------------------------------------------------------------------------

#' Build the upstream CKAN \code{datastore_search} URL for a registry
#' entry.
#'
#' Returns \code{NA_character_} for entries that do not publish a
#' sidecar (e.g. the 2023 weapon_records release).
#'
#' @param kind One of \code{ARSAU_KINDS()}.
#' @param year One of \code{ARSAU_YEARS()}.
#' @param limit Integer; CKAN \code{limit} parameter.  Default 5000.
#' @return Character scalar URL, or \code{NA_character_}.
#' @references Ontario Data Catalogue CKAN API:
#'   \code{datastore_search} (\url{https://data.ontario.ca/}).
#' @export
morie_arsau_ckan_url <- function(kind, year, limit = 5000L) {
  reg <- ARSAU_REGISTRY()
  key <- paste(as.character(year), kind, sep = "|")
  if (!(key %in% names(reg))) {
    stop(sprintf("ARSAU has no %s entry for %s.",
                 sQuote(kind), sQuote(year)), call. = FALSE)
  }
  entry <- reg[[key]]
  if (is.null(entry$sidecar_filename)) {
    return(NA_character_)
  }
  # sidecar_filename is "<resource_id>.json"; strip the extension.
  resource_id <- sub("\\.json$", "", entry$sidecar_filename)
  sprintf(
    "https://data.ontario.ca/api/3/action/datastore_search?resource_id=%s&limit=%d",
    resource_id, as.integer(limit)
  )
}


#' Fetch the CKAN sidecar JSON for a registry entry.
#'
#' Optional helper.  Requires \pkg{httr2} (and \pkg{jsonlite} via the
#' existing \code{\link{morie_arsau_read_sidecar}} contract).
#'
#' @param kind One of \code{ARSAU_KINDS()}.
#' @param year One of \code{ARSAU_YEARS()}.
#' @param limit Integer; CKAN \code{limit} parameter.  Default 5000.
#' @param timeout_sec Request timeout in seconds.  Default 30.
#' @return A list with \code{fields} and \code{records}, ready for
#'   \code{\link{morie_arsau_sidecar_schema}} /
#'   \code{\link{morie_arsau_sidecar_to_frame}}.
#' @references Ontario Data Catalogue CKAN API.
#' @export
morie_arsau_fetch_sidecar <- function(kind, year, limit = 5000L,
                                      timeout_sec = 30L) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "morie_arsau_fetch_sidecar requires the optional 'httr2' ",
      "package; install it with install.packages('httr2').",
      call. = FALSE
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "morie_arsau_fetch_sidecar requires the 'jsonlite' package.",
      call. = FALSE
    )
  }
  url <- morie_arsau_ckan_url(kind = kind, year = year, limit = limit)
  if (is.na(url)) {
    stop(sprintf(
      "ARSAU %s %s does not publish a CKAN sidecar.",
      sQuote(kind), sQuote(year)
    ), call. = FALSE)
  }
  # 3XX: route through the shared libcurl backend (with httr2
  # fallback). User-Agent is now whatever the C++ helper's default
  # is (`morie-R/0.9.5.5 (...) libcurl`) -- close enough to the
  # original arsau-specific string for upstream-portal analytics
  # purposes. timeout_sec preserved verbatim.
  body <- .morie_dataset_http_text(url, timeout_s = as.integer(timeout_sec))
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  # CKAN wraps the useful content under $result.
  if (!is.null(payload$result) && is.list(payload$result)) {
    return(list(
      fields  = payload$result$fields  %||% list(),
      records = payload$result$records %||% list()
    ))
  }
  list(
    fields  = payload$fields  %||% list(),
    records = payload$records %||% list()
  )
}


# ---------------------------------------------------------------------------
# Stub: full network bulk-download driver
# ---------------------------------------------------------------------------

#' Bulk-download every ARSAU CSV + sidecar from the upstream Catalogue.
#'
#' This is the R-side equivalent of running the maintainer's
#' \code{scripts/refresh_arsau.py} mirror — a non-trivial pipeline that
#' walks the CKAN package, follows per-resource redirects, handles
#' rate-limits, verifies SHA digests against the published values, and
#' lands the files under \code{MORIE_ARSAU_DIR}.  Porting it requires
#' an end-to-end retry + checksum manager that does not yet have a
#' tested R analogue; per the morie maintenance policy, network bulk
#' fetches must be reproducible across CRAN test environments before
#' the wrapper is exposed.  Stubbed for now.
#'
#' @param target_dir Destination directory.
#' @param ... Reserved.
#' @return Stops with \code{NotYetPorted}.
#' @export
morie_arsau_download <- function(target_dir, ...) {
  stop(
    "NotYetPorted: morie_arsau_download() is not yet implemented in R. ",
    "Use the Python pipeline (scripts/refresh_arsau.py) or download ",
    "the CSVs manually from ",
    "https://data.ontario.ca/dataset/police-use-of-force-race-based-data ",
    "and point MORIE_ARSAU_DIR at the result.",
    call. = FALSE
  )
}
