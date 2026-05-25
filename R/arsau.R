# SPDX-License-Identifier: AGPL-3.0-or-later
#' ARSAU dataset loaders + registry (R-side mirror of morie.arsau_datasets)
#'
#' ARSAU = the Ontario Ministry of the Solicitor General's provincial
#' release of Police Use-of-Force incident records (formally
#' "Race-Based and Identity-Based Data on Police Use of Force in
#' Ontario").  Published on the Ontario Data Catalogue at
#' \url{https://data.ontario.ca/dataset/police-use-of-force-race-based-data}.
#'
#' This file ships the R-side equivalents of the Python
#' \code{morie.arsau_datasets} module:
#'
#' \itemize{
#'   \item \code{ARSAU_REGISTRY()}: returns the registered (year x kind)
#'         entries as a list of lists.
#'   \item \code{morie_arsau_load_main_records()},
#'         \code{morie_arsau_load_individual_records()},
#'         \code{morie_arsau_load_probe_cycle_records()},
#'         \code{morie_arsau_load_weapon_records()},
#'         \code{morie_arsau_load_aggregate_summary()},
#'         \code{morie_arsau_load_detailed_dataset()}: per-record-type
#'         loaders, returning a named list with \code{data},
#'         \code{schema}, \code{sidecar}, \code{year}, \code{kind},
#'         \code{language}, \code{is_valid}, \code{n_rows},
#'         \code{n_cols}, \code{interpretation}.
#'   \item \code{morie_arsau_available_years()},
#'         \code{morie_arsau_available_datasets()},
#'         \code{morie_arsau_describe()}: discovery callables.
#' }
#'
#' Path portability
#' ----------------
#'
#' No path on the maintainer's workstation is hard-coded.  All file
#' resolution goes through \code{.morie_resolve_arsau_dir} (defined
#' below), which honours, in order:
#'
#' \enumerate{
#'   \item an explicit \code{data_dir =} argument
#'   \item the \code{MORIE_ARSAU_DIR} environment variable
#'   \item \code{MORIE_DATA_DIR/arsau}
#'   \item \code{morie_cache_dir("arsau")} (only if already populated by
#'         a previous \code{morie_arsau_download()} call -- never
#'         auto-created at read-time, per CRAN policy)
#'   \item \code{system.file("extdata", "arsau", package = "morie")} --
#'         the bundled tiny fixture for unit tests + tutorials
#'   \item stop with a remediation paragraph
#' }
#'
#' 2023 weapon-records invalidity gate
#' -----------------------------------
#'
#' The 2023 release ships \code{uof_weapon_records_invaliddata.csv},
#' flagged by the ministry as non-compliant.
#' \code{morie_arsau_load_weapon_records(2023)} signals an error unless
#' the caller passes \code{allow_invalid = TRUE}; when allowed, the
#' returned object's \code{is_valid} field is \code{FALSE} and its
#' \code{warnings} list opens with an explicit caveat paragraph.
#'
#' @name arsau
NULL


# ---------------------------------------------------------------------------
# Internal: path resolver
# ---------------------------------------------------------------------------

.morie_env <- function(name) {
  v <- Sys.getenv(name, unset = NA_character_)
  if (is.na(v) || !nzchar(trimws(v))) {
    return(NULL)
  }
  trimws(v)
}

#' Resolve the ARSAU data directory.
#'
#' Walks the documented cascade.  Returns a normalised absolute path.
#' Stops with an informative error if nothing exists and
#' \code{require_exists = TRUE}.
#'
#' @param data_dir Optional explicit path.
#' @param require_exists If \code{TRUE} (default), error on no match.
#' @return Character scalar path.
#' @keywords internal
.morie_resolve_arsau_dir <- function(data_dir = NULL, require_exists = TRUE) {
  candidates <- list()

  if (!is.null(data_dir) && nzchar(data_dir)) {
    candidates[["data_dir argument"]] <- normalizePath(
      data_dir, mustWork = FALSE
    )
  }
  e <- .morie_env("MORIE_ARSAU_DIR")
  if (!is.null(e)) {
    candidates[["MORIE_ARSAU_DIR env var"]] <- normalizePath(e, mustWork = FALSE)
  }
  e2 <- .morie_env("MORIE_DATA_DIR")
  if (!is.null(e2)) {
    candidates[["MORIE_DATA_DIR + /arsau"]] <- normalizePath(
      file.path(e2, "arsau"), mustWork = FALSE
    )
    candidates[["MORIE_DATA_DIR + /ARSAU"]] <- normalizePath(
      file.path(e2, "ARSAU"), mustWork = FALSE
    )
  }

  # Opt-in persistent cache via existing morie_cache_dir contract
  # (R/database.R).  Use it only if the directory already exists.
  if (exists("morie_cache_dir", mode = "function")) {
    cache_candidate <- tryCatch(morie_cache_dir("arsau"), error = function(e) NULL)
    if (!is.null(cache_candidate)) {
      candidates[["morie_cache_dir('arsau') (opt-in)"]] <- cache_candidate
    }
  }

  # Bundled tiny fixture in the installed package.
  fixture <- system.file("extdata", "arsau", package = "morie")
  if (nzchar(fixture)) {
    candidates[["bundled fixture (inst/extdata/arsau)"]] <- fixture
  }

  if (!require_exists) {
    if (length(candidates) == 0L) {
      return(NA_character_)
    }
    return(candidates[[1]])
  }

  for (lbl in names(candidates)) {
    p <- candidates[[lbl]]
    if (dir.exists(p)) {
      return(p)
    }
  }

  tried <- paste0(
    "  - ", names(candidates), ": ", unlist(candidates), collapse = "\n"
  )
  stop(
    "morie: could not find ARSAU data directory.\n",
    "Tried (in order):\n", tried, "\n\n",
    "To fix, do one of:\n",
    "  - pass data_dir = '/path/to/ARSAU' to the loader\n",
    "  - Sys.setenv(MORIE_ARSAU_DIR = '/path/to/ARSAU')\n",
    "  - Sys.setenv(MORIE_DATA_DIR = '/path/to/morie-data') (must contain arsau/)\n",
    "  - call morie_arsau_download(target_dir = ...) (when available)\n",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

.arsau_make_entry <- function(year_or_range, kind, csv_filename, sidecar_filename,
                                expected_rows, expected_cols, is_valid,
                                description_en, description_fr) {
  list(
    year_or_range = year_or_range,
    kind = kind,
    csv_filename = csv_filename,
    sidecar_filename = sidecar_filename,
    expected_rows = as.integer(expected_rows),
    expected_cols = as.integer(expected_cols),
    is_valid = isTRUE(is_valid),
    description_en = description_en,
    description_fr = description_fr
  )
}

.ARSAU_REGISTRY_LIST <- list(
  `2020-2022|aggregate_summary` = .arsau_make_entry(
    "2020-2022", "aggregate_summary",
    "useofforce_agrregatesummarybyyear_2020-2022.csv",
    "7560d405-444c-4340-95c4-f73849015501.json",
    151L, 6L, TRUE,
    "Aggregate annual summary of Ontario police use-of-force reports for 2020-2022; one row per service-year.",
    "Sommaire annuel agrege des rapports d'usage de la force policiere en Ontario pour 2020-2022."
  ),
  `2020-2022|detailed_dataset` = .arsau_make_entry(
    "2020-2022", "detailed_dataset",
    "useofforce_detaileddataset_2020-2022.csv",
    "2150ac23-4e55-474a-b61f-81baf6850851.json",
    23092L, 167L, TRUE,
    "Detailed incident-level Ontario use-of-force dataset for 2020-2022 (single combined file); 167-column wide layout.",
    "Jeu de donnees detaille au niveau de l'incident pour le recours a la force, 2020-2022."
  ),
  `2023|individual_records` = .arsau_make_entry(
    "2023", "individual_records", "uof_individual_records.csv",
    "133c73fa-9d8e-435e-8c6d-7d1e14d1e88d.json",
    12805L, 112L, TRUE,
    "2023 Ontario use-of-force individual-records dataset, one row per civilian involved.",
    "Jeu de donnees 2023 sur les enregistrements individuels, une ligne par civil implique."
  ),
  `2023|main_records` = .arsau_make_entry(
    "2023", "main_records", "uof_main_records.csv",
    "94f303a2-963e-4fd1-958d-6681b310cb6d.json",
    10935L, 64L, TRUE,
    "2023 Ontario use-of-force main records: one row per incident.",
    "Enregistrements principaux 2023 du recours a la force: une ligne par incident."
  ),
  `2023|probe_cycle_records` = .arsau_make_entry(
    "2023", "probe_cycle_records", "uof_probe_cycle_records.csv",
    "339b9e63-9521-44a6-8719-c2cb9aa39a8a.json",
    1136L, 3L, TRUE,
    "2023 Ontario CEW probe-cycle telemetry, one row per cycle.",
    "Enregistrements 2023 du cycle de sonde CEW, une ligne par cycle."
  ),
  `2023|weapon_records` = .arsau_make_entry(
    "2023", "weapon_records", "uof_weapon_records_invaliddata.csv",
    NULL,
    8711L, 4L, FALSE,
    "2023 Ontario use-of-force weapon records \u2014 INVALID per the ministry's technical report. Do not use for comparative analysis.",
    "Enregistrements 2023 sur les armes \u2014 DONNEES INVALIDES selon le rapport technique du ministere."
  ),
  `2024|individual_records` = .arsau_make_entry(
    "2024", "individual_records", "uof_individual_records.csv",
    "690d4c5e-095e-49a0-bbab-b7fc680f3c6b.json",
    12921L, 112L, TRUE,
    "2024 Ontario use-of-force individual-records dataset, one row per civilian.",
    "Jeu de donnees 2024 sur les enregistrements individuels."
  ),
  `2024|main_records` = .arsau_make_entry(
    "2024", "main_records", "uof_main_records.csv",
    "ea9dc29c-b4f1-4426-b1f2-974ce995aca1.json",
    10849L, 65L, TRUE,
    "2024 Ontario use-of-force main records: one row per incident (10849 logical records parsed by read.csv).",
    "Enregistrements principaux 2024 du recours a la force: une ligne par incident (10849 enregistrements logiques)."
  ),
  `2024|probe_cycle_records` = .arsau_make_entry(
    "2024", "probe_cycle_records", "uof_probe_cycle_records.csv",
    "76875b6a-4352-4722-a3f6-997cc220dc4f.json",
    972L, 3L, TRUE,
    "2024 Ontario CEW probe-cycle records, one row per cycle.",
    "Enregistrements 2024 du cycle de sonde CEW."
  ),
  `2024|weapon_records` = .arsau_make_entry(
    "2024", "weapon_records", "uof_weapon_records.csv",
    "2c1ab494-d636-4c17-9699-3819112982a5.json",
    9282L, 5L, TRUE,
    "2024 Ontario use-of-force weapon records, one row per weapon used. Resumes the valid annual series after the 2023 invalid-data interruption.",
    "Enregistrements 2024 sur les armes utilisees lors du recours a la force."
  )
)


#' Return the ARSAU registry as a list of entries.
#'
#' Each entry is itself a named list with \code{year_or_range},
#' \code{kind}, \code{csv_filename}, \code{sidecar_filename}, expected
#' row / column counts, \code{is_valid}, and bilingual descriptions.
#'
#' @return Named list-of-lists.
#' @export
ARSAU_REGISTRY <- function() {
  .ARSAU_REGISTRY_LIST
}

#' Known ARSAU year/range keys.
#' @export
ARSAU_YEARS <- function() {
  sort(unique(vapply(.ARSAU_REGISTRY_LIST, function(e) e$year_or_range, character(1))))
}

#' Known ARSAU dataset kinds.
#' @export
ARSAU_KINDS <- function() {
  sort(unique(vapply(.ARSAU_REGISTRY_LIST, function(e) e$kind, character(1))))
}


# ---------------------------------------------------------------------------
# Internal: sidecar reader
# ---------------------------------------------------------------------------

#' Read a CKAN datastore_search JSON sidecar.
#'
#' Handles both bare \code{{fields, records}} and the
#' \code{{result: {fields, records}}} wrapper shape.
#'
#' @param path Path to the JSON file.
#' @return Named list with \code{fields} and \code{records}.
#' @export
morie_arsau_read_sidecar <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("morie_arsau_read_sidecar requires the 'jsonlite' package.")
  }
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.null(payload$fields) || !is.null(payload$records)) {
    return(list(
      fields = payload$fields %||% list(),
      records = payload$records %||% list()
    ))
  }
  if (!is.null(payload$result) && is.list(payload$result)) {
    return(list(
      fields = payload$result$fields %||% list(),
      records = payload$result$records %||% list()
    ))
  }
  list(fields = list(), records = list())
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# ---------------------------------------------------------------------------
# Internal: shared loader
# ---------------------------------------------------------------------------

.arsau_lookup <- function(year_or_range, kind) {
  key <- paste(as.character(year_or_range), kind, sep = "|")
  if (!(key %in% names(.ARSAU_REGISTRY_LIST))) {
    return(NULL)
  }
  .ARSAU_REGISTRY_LIST[[key]]
}

.arsau_coerce_year_key <- function(year, range_ok = FALSE) {
  s <- trimws(as.character(year))
  yrs <- ARSAU_YEARS()
  if (s %in% yrs) {
    return(s)
  }
  if (range_ok) {
    s2 <- gsub("to", "-", gsub("_", "-", s, fixed = TRUE), fixed = TRUE)
    if (s2 %in% yrs) return(s2)
  }
  stop(sprintf("Unknown ARSAU year %s. Valid keys: %s",
               sQuote(year), paste(yrs, collapse = ", ")),
       call. = FALSE)
}


.arsau_load_one <- function(entry, data_dir = NULL, language = "en", allow_invalid = FALSE) {
  if (!entry$is_valid && !allow_invalid) {
    stop(sprintf(
      "ARSAU '%s' '%s' is flagged invalid by the publishing ministry ('%s'). Pass allow_invalid = TRUE to inspect for QA only.",
      entry$year_or_range, entry$kind, entry$csv_filename
    ), call. = FALSE)
  }

  root <- .morie_resolve_arsau_dir(data_dir = data_dir)
  csv_path <- file.path(root, entry$year_or_range, entry$csv_filename)
  if (!file.exists(csv_path)) {
    stop(sprintf(
      "ARSAU CSV not found at %s. Expected %s under %s.\nTo fix: set MORIE_ARSAU_DIR or pass data_dir.",
      csv_path, entry$csv_filename, file.path(root, entry$year_or_range)
    ), call. = FALSE)
  }

  # check.names = FALSE preserves the upstream-typo "IndivInjuries_PhysicalInjuries " with trailing space.
  df <- utils::read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE)

  sidecar <- NULL
  if (!is.null(entry$sidecar_filename)) {
    sc_path <- file.path(root, entry$year_or_range, entry$sidecar_filename)
    if (file.exists(sc_path)) {
      sidecar <- tryCatch(morie_arsau_read_sidecar(sc_path),
                          error = function(e) NULL)
    }
  }

  desc <- if (tolower(substr(language, 1, 2)) == "fr") {
    entry$description_fr
  } else {
    entry$description_en
  }

  warnings <- character(0)
  if (!entry$is_valid) {
    warnings <- c(warnings, "Ministry-flagged invalid data \u2014 do not use for comparative or quantitative analysis.")
  }
  if (nrow(df) != entry$expected_rows) {
    warnings <- c(warnings, sprintf(
      "CSV has %d rows; registry expected %d.  Upstream may have refreshed.",
      nrow(df), entry$expected_rows
    ))
  }
  if (ncol(df) != entry$expected_cols) {
    warnings <- c(warnings, sprintf(
      "CSV has %d columns; registry expected %d.", ncol(df), entry$expected_cols
    ))
  }

  interp <- if (tolower(substr(language, 1, 2)) == "fr") {
    sprintf("Donnees ARSAU chargees: %s pour %s. %d lignes \u00d7 %d colonnes. %s %s",
            entry$kind, entry$year_or_range, nrow(df), ncol(df),
            if (entry$is_valid) "Validite: OK." else "INVALIDE.", desc)
  } else {
    sprintf("ARSAU data loaded: %s for %s. %d rows \u00d7 %d columns. %s %s",
            entry$kind, entry$year_or_range, nrow(df), ncol(df),
            if (entry$is_valid) "Valid for analysis." else "INVALID \u2014 see warnings.",
            desc)
  }

  out <- list(
    title = sprintf("ARSAU %s %s", entry$year_or_range, entry$kind),
    call = sprintf("morie_arsau_load_%s(year=%s, language=%s)",
                   entry$kind, sQuote(entry$year_or_range), sQuote(language)),
    summary_lines = list(
      `Year/range` = entry$year_or_range,
      Kind = entry$kind,
      Rows = nrow(df),
      Columns = ncol(df),
      Valid = if (entry$is_valid) "yes" else "no - invalid data",
      Sidecar = if (!is.null(sidecar)) "yes" else "no"
    ),
    warnings = warnings,
    interpretation = interp,
    data = df,
    sidecar = sidecar,
    year = entry$year_or_range,
    kind = entry$kind,
    language = language,
    is_valid = entry$is_valid,
    n_rows = nrow(df),
    n_cols = ncol(df),
    csv_path = csv_path
  )
  class(out) <- c("morie_arsau_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# Public loaders
# ---------------------------------------------------------------------------

#' Load ARSAU main_records CSV for the given year.
#' @param year 2023 or 2024.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @export
morie_arsau_load_main_records <- function(year, language = "en", data_dir = NULL) {
  key <- .arsau_coerce_year_key(year)
  entry <- .arsau_lookup(key, "main_records")
  if (is.null(entry)) {
    stop(sprintf("ARSAU main_records not published for %s.", sQuote(key)),
         call. = FALSE)
  }
  .arsau_load_one(entry, data_dir = data_dir, language = language)
}

#' Load ARSAU individual_records CSV.
#' @inheritParams morie_arsau_load_main_records
#' @export
morie_arsau_load_individual_records <- function(year, language = "en", data_dir = NULL) {
  key <- .arsau_coerce_year_key(year)
  entry <- .arsau_lookup(key, "individual_records")
  if (is.null(entry)) {
    stop(sprintf("ARSAU individual_records not published for %s.", sQuote(key)),
         call. = FALSE)
  }
  .arsau_load_one(entry, data_dir = data_dir, language = language)
}

#' Load ARSAU probe_cycle_records CSV (CEW telemetry).
#' @inheritParams morie_arsau_load_main_records
#' @export
morie_arsau_load_probe_cycle_records <- function(year, language = "en", data_dir = NULL) {
  key <- .arsau_coerce_year_key(year)
  entry <- .arsau_lookup(key, "probe_cycle_records")
  if (is.null(entry)) {
    stop(sprintf("ARSAU probe_cycle_records not published for %s.", sQuote(key)),
         call. = FALSE)
  }
  .arsau_load_one(entry, data_dir = data_dir, language = language)
}

#' Load ARSAU weapon_records CSV.
#'
#' 2023 requires \code{allow_invalid = TRUE} (ministry-flagged invalid).
#' @inheritParams morie_arsau_load_main_records
#' @param allow_invalid Logical; required \code{TRUE} for 2023.
#' @export
morie_arsau_load_weapon_records <- function(year, allow_invalid = FALSE,
                                              language = "en", data_dir = NULL) {
  key <- .arsau_coerce_year_key(year)
  entry <- .arsau_lookup(key, "weapon_records")
  if (is.null(entry)) {
    stop(sprintf("ARSAU weapon_records not published for %s.", sQuote(key)),
         call. = FALSE)
  }
  .arsau_load_one(entry, data_dir = data_dir, language = language,
                   allow_invalid = allow_invalid)
}

#' Load ARSAU aggregate-summary-by-year CSV (2020-2022 only).
#' @param year_range "2020-2022".
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @export
morie_arsau_load_aggregate_summary <- function(year_range = "2020-2022",
                                                 language = "en", data_dir = NULL) {
  key <- .arsau_coerce_year_key(year_range, range_ok = TRUE)
  entry <- .arsau_lookup(key, "aggregate_summary")
  if (is.null(entry)) {
    stop(sprintf("ARSAU aggregate_summary not published for %s.", sQuote(key)),
         call. = FALSE)
  }
  .arsau_load_one(entry, data_dir = data_dir, language = language)
}

#' Load ARSAU detailed-incident-level CSV (2020-2022 only).
#' @inheritParams morie_arsau_load_aggregate_summary
#' @export
morie_arsau_load_detailed_dataset <- function(year_range = "2020-2022",
                                                language = "en", data_dir = NULL) {
  key <- .arsau_coerce_year_key(year_range, range_ok = TRUE)
  entry <- .arsau_lookup(key, "detailed_dataset")
  if (is.null(entry)) {
    stop(sprintf("ARSAU detailed_dataset not published for %s.", sQuote(key)),
         call. = FALSE)
  }
  .arsau_load_one(entry, data_dir = data_dir, language = language)
}


# ---------------------------------------------------------------------------
# Discovery callables
# ---------------------------------------------------------------------------

#' List ARSAU year / year-range buckets.
#'
#' @param data_dir Optional explicit ARSAU root.
#' @param language "en" or "fr".
#' @export
morie_arsau_available_years <- function(data_dir = NULL, language = "en") {
  years <- ARSAU_YEARS()
  root <- tryCatch(
    .morie_resolve_arsau_dir(data_dir = data_dir, require_exists = FALSE),
    error = function(e) NA_character_
  )

  present <- character(0)
  missing <- character(0)
  if (!is.na(root) && dir.exists(root)) {
    for (y in years) {
      if (dir.exists(file.path(root, y))) {
        present <- c(present, y)
      } else {
        missing <- c(missing, y)
      }
    }
  }

  interp <- if (tolower(substr(language, 1, 2)) == "fr") {
    sprintf("ARSAU connait %d annee(s)/plage(s): %s. %d presente(s) sur disque, %d absente(s).",
            length(years), paste(years, collapse = ", "), length(present), length(missing))
  } else {
    sprintf("ARSAU knows %d year/range bucket(s): %s. %d present on disk, %d missing.",
            length(years), paste(years, collapse = ", "), length(present), length(missing))
  }

  out <- list(
    title = "ARSAU available years",
    call = "morie_arsau_available_years()",
    summary_lines = list(
      `Years known` = paste(years, collapse = ", "),
      `Data root` = if (!is.na(root)) root else "(unset)",
      `Present on disk` = if (length(present) > 0L) paste(present, collapse = ", ") else "(none)",
      `Missing` = if (length(missing) > 0L) paste(missing, collapse = ", ") else "(none)"
    ),
    warnings = character(0),
    interpretation = interp,
    n = length(years),
    years = years,
    present = present,
    missing = missing,
    data_root = if (!is.na(root)) root else NULL
  )
  class(out) <- c("morie_arsau_result", "morie_rich_result", "list")
  out
}

#' List ARSAU dataset kinds, optionally restricted to one year.
#'
#' @param year Optional year; \code{NULL} lists everything.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @export
morie_arsau_available_datasets <- function(year = NULL, language = "en", data_dir = NULL) {
  if (is.null(year)) {
    entries <- .ARSAU_REGISTRY_LIST
  } else {
    key <- .arsau_coerce_year_key(year, range_ok = TRUE)
    entries <- Filter(function(e) e$year_or_range == key, .ARSAU_REGISTRY_LIST)
    if (length(entries) == 0L) {
      stop(sprintf("No ARSAU datasets registered for year %s.", sQuote(year)),
           call. = FALSE)
    }
  }

  rows <- lapply(entries, function(e) {
    desc <- if (tolower(substr(language, 1, 2)) == "fr") e$description_fr else e$description_en
    list(
      year_or_range = e$year_or_range,
      kind = e$kind,
      csv = e$csv_filename,
      valid = if (e$is_valid) "yes" else "INVALID",
      rows = e$expected_rows,
      cols = e$expected_cols,
      description = substr(desc, 1L, 80L)
    )
  })

  interp <- if (tolower(substr(language, 1, 2)) == "fr") {
    sprintf("%d entree(s) ARSAU %s sont enregistrees.", length(entries),
            if (is.null(year)) "(toutes annees)" else sprintf("pour %s", sQuote(year)))
  } else {
    sprintf("%d ARSAU entry/entries %s registered.", length(entries),
            if (is.null(year)) "(all years)" else sprintf("for %s", sQuote(year)))
  }

  out <- list(
    title = sprintf("ARSAU available datasets%s",
                    if (!is.null(year)) sprintf(" (%s)", year) else ""),
    call = sprintf("morie_arsau_available_datasets(year=%s)",
                   if (is.null(year)) "NULL" else sQuote(year)),
    summary_lines = list(
      Entries = length(entries),
      `Year filter` = if (is.null(year)) "(none)" else as.character(year)
    ),
    warnings = character(0),
    interpretation = interp,
    n = length(entries),
    entries = rows
  )
  class(out) <- c("morie_arsau_result", "morie_rich_result", "list")
  out
}

#' Describe a single ARSAU dataset entry.
#'
#' @param kind One of \code{ARSAU_KINDS()}.
#' @param year One of \code{ARSAU_YEARS()}.
#' @param language "en" or "fr".
#' @param data_dir Optional explicit ARSAU root.
#' @param n_preview_rows Number of rows from the CSV head to include.
#' @export
morie_arsau_describe <- function(kind, year, language = "en", data_dir = NULL,
                                   n_preview_rows = 3L) {
  key <- .arsau_coerce_year_key(year, range_ok = TRUE)
  entry <- .arsau_lookup(key, kind)
  if (is.null(entry)) {
    stop(sprintf("ARSAU has no %s entry for %s.", sQuote(kind), sQuote(key)),
         call. = FALSE)
  }

  root <- tryCatch(
    .morie_resolve_arsau_dir(data_dir = data_dir, require_exists = FALSE),
    error = function(e) NA_character_
  )
  csv_present <- FALSE
  preview <- NULL
  sidecar <- NULL
  if (!is.na(root)) {
    csv_path <- file.path(root, entry$year_or_range, entry$csv_filename)
    if (file.exists(csv_path)) {
      csv_present <- TRUE
      preview <- tryCatch(
        utils::read.csv(csv_path, nrows = n_preview_rows, check.names = FALSE,
                         stringsAsFactors = FALSE),
        error = function(e) NULL
      )
    }
    if (!is.null(entry$sidecar_filename)) {
      sc_path <- file.path(root, entry$year_or_range, entry$sidecar_filename)
      if (file.exists(sc_path)) {
        sidecar <- tryCatch(morie_arsau_read_sidecar(sc_path),
                            error = function(e) NULL)
      }
    }
  }

  desc <- if (tolower(substr(language, 1, 2)) == "fr") entry$description_fr else entry$description_en
  warnings <- character(0)
  if (!entry$is_valid) warnings <- c(warnings, "Ministry-flagged invalid data.")
  if (!csv_present) warnings <- c(warnings, sprintf("CSV not present under %s.", root))

  interp <- if (tolower(substr(language, 1, 2)) == "fr") {
    sprintf("ARSAU %s pour %s: %d lignes \u00d7 %d colonnes. %s %s",
            entry$kind, entry$year_or_range, entry$expected_rows, entry$expected_cols,
            if (entry$is_valid) "Validite OK." else "DONNEES INVALIDES.", desc)
  } else {
    sprintf("ARSAU %s for %s: %d rows \u00d7 %d columns. %s %s",
            entry$kind, entry$year_or_range, entry$expected_rows, entry$expected_cols,
            if (entry$is_valid) "Valid for analysis." else "INVALID.", desc)
  }

  out <- list(
    title = sprintf("ARSAU %s %s", entry$year_or_range, entry$kind),
    call = sprintf("morie_arsau_describe(kind=%s, year=%s)",
                   sQuote(kind), sQuote(year)),
    summary_lines = list(
      `Year/range` = entry$year_or_range,
      Kind = entry$kind,
      `CSV filename` = entry$csv_filename,
      `Sidecar resource_id` = entry$sidecar_filename %||% "(none)",
      `Expected rows` = entry$expected_rows,
      `Expected cols` = entry$expected_cols,
      Valid = if (entry$is_valid) "yes" else "no",
      `CSV on disk` = if (csv_present) "yes" else "no"
    ),
    warnings = warnings,
    interpretation = interp,
    entry = entry,
    csv_present = csv_present,
    preview = preview,
    sidecar = sidecar
  )
  class(out) <- c("morie_arsau_result", "morie_rich_result", "list")
  out
}

#' @export
print.morie_arsau_result <- function(x, ...) {
  cat(x$title, "\n", strrep("=", nchar(x$title)), "\n", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\n\n", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\n", label_w, nms[i], format(v)))
    }
    cat("\n")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\n")
    cat("\n")
  }
  if (nzchar(x$interpretation)) cat(x$interpretation, "\n")
  invisible(x)
}
