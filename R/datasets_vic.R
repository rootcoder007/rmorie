# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Victorian crime data (Crime Statistics Agency, Victoria, Australia).
#
# The agency publishes one workbook per topic each quarter, every one a
# multi-sheet .xlsx with a Contents sheet, a Footnotes sheet, and the
# data in "Table 01", "Table 02", ... The files are NOT bundled: they
# total ~65 MB per release and are refreshed quarterly, so the catalog
# below carries the download URLs and the loader caches to disk, the
# same shape as the other morie dataset families.
#
# Licence: Creative Commons Attribution 4.0 International (CC BY 4.0).
# Attribution: Crime Statistics Agency, Victoria.
# Source: https://www.crimestatistics.vic.gov.au/crime-statistics/
#         latest-victorian-crime-data/download-data

#' Catalog of the Victorian Crime Statistics Agency data tables
#'
#' @return A \code{data.frame} with one row per published workbook and
#'   the columns \code{key}, \code{file} and \code{url}.
#' @references Crime Statistics Agency (Victoria),
#'   \url{https://www.crimestatistics.vic.gov.au/}. Released under
#'   CC BY 4.0.
#' @examples
#' cat_df <- morie_datasets_vic_catalog()
#' nrow(cat_df)
#' head(cat_df$key)
#' @export
morie_datasets_vic_catalog <- function() {
  entries <- list(
    list(
      key = "bail_justice_bail",
      file = "Bail_Justice_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Bail_Ju",
        "stice_Data_Tables_Bail_Visualisation_Year_Ending_March_2",
        "026_0.xlsx"
      )
    ),
    list(
      key = "children_s_court_bail",
      file = "Children's_Court_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Childre",
        "n%27s_Court_Data_Tables_Bail_Visualisation_Year_Ending_M",
        "arch_2026_0.xlsx"
      )
    ),
    list(
      key = "corrections_victoria_bail",
      file = "Corrections_Victoria_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Correct",
        "ions_Victoria_Data_Tables_Bail_Visualisation_Year_Ending",
        "_March_2026_0.xlsx"
      )
    ),
    list(
      key = "county_court_bail",
      file = "County_Court_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/County_",
        "Court_Data_Tables_Bail_Visualisation_Year_Ending_March_2",
        "026_0.xlsx"
      )
    ),
    list(
      key = "alleged_offender_incidents",
      file = "Data_Tables_Alleged_Offender_Incidents_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_Alleged_Offender_Incidents_Visualisation_Year_Endin",
        "g_March_2026_0.xlsx"
      )
    ),
    list(
      key = "criminal_incidents",
      file = "Data_Tables_Criminal_Incidents_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_Criminal_Incidents_Visualisation_Year_Ending_March_",
        "2026_0.xlsx"
      )
    ),
    list(
      key = "family_incidents",
      file = "Data_Tables_Family_Incidents_Visualisation_Year_Ending_March_2026.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_Family_Incidents_Visualisation_Year_Ending_March_20",
        "26.xlsx"
      )
    ),
    list(
      key = "lga_alleged_offenders",
      file = "Data_Tables_LGA_Alleged_Offenders_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_LGA_Alleged_Offenders_Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "lga_criminal_incidents",
      file = "Data_Tables_LGA_Criminal_Incidents_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_LGA_Criminal_Incidents_Year_Ending_March_2026_0.xls",
        "x"
      )
    ),
    list(
      key = "lga_family_incidents",
      file = "Data_Tables_LGA_Family_Incidents_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_LGA_Family_Incidents_Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "lga_recorded_offences",
      file = "Data_Tables_LGA_Recorded_Offences_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_LGA_Recorded_Offences_Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "lga_victim_reports",
      file = "Data_Tables_LGA_Victim_Reports_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_LGA_Victim_Reports_Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "property_items",
      file = "Data_Tables_Property_Items_Visualisation_Year_Ending_March_2026.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_Property_Items_Visualisation_Year_Ending_March_2026",
        ".xlsx"
      )
    ),
    list(
      key = "recorded_offences",
      file = "Data_Tables_Recorded_Offences_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_Recorded_Offences_Visualisation_Year_Ending_March_2",
        "026_0.xlsx"
      )
    ),
    list(
      key = "victim_reports",
      file = "Data_Tables_Victim_Reports_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Data_Ta",
        "bles_Victim_Reports_Visualisation_Year_Ending_March_2026",
        "_0.xlsx"
      )
    ),
    list(
      key = "indigenous_alleged_offender_incidents",
      file = "Indigenous_Data_Tables_Alleged_Offender_Incidents_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_Alleged_Offender_Incidents_Visualisation",
        "_Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_family_incidents",
      file = "Indigenous_Data_Tables_Family_Incidents_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_Family_Incidents_Visualisation_Year_Endi",
        "ng_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_lga_alleged_offenders",
      file = "Indigenous_Data_Tables_LGA_Alleged_Offenders_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_LGA_Alleged_Offenders_Visualisation_Year",
        "_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_lga_family_incidents_afms",
      file = "Indigenous_Data_Tables_LGA_Family_Incidents_AFMs_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_LGA_Family_Incidents_AFMs_Visualisation_",
        "Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_lga_family_incidents_oths",
      file = "Indigenous_Data_Tables_LGA_Family_Incidents_OTHs_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_LGA_Family_Incidents_OTHs_Visualisation_",
        "Year_Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_lga_family_incidents",
      file = "Indigenous_Data_Tables_LGA_Family_Incidents_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_LGA_Family_Incidents_Visualisation_Year_",
        "Ending_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_lga_victim_reports",
      file = "Indigenous_Data_Tables_LGA_Victim_Reports_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_LGA_Victim_Reports_Visualisation_Year_En",
        "ding_March_2026_0.xlsx"
      )
    ),
    list(
      key = "indigenous_victim_reports",
      file = "Indigenous_Data_Tables_Victim_Reports_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Indigen",
        "ous_Data_Tables_Victim_Reports_Visualisation_Year_Ending",
        "_March_2026_0.xlsx"
      )
    ),
    list(
      key = "magistrates_court_bail",
      file = "Magistrates'_Court_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Magistr",
        "ates%27_Court_Data_Tables_Bail_Visualisation_Year_Ending",
        "_March_2026_0.xlsx"
      )
    ),
    list(
      key = "supreme_court_bail",
      file = "Supreme_Court_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Supreme",
        "_Court_Data_Tables_Bail_Visualisation_Year_Ending_March_",
        "2026_0.xlsx"
      )
    ),
    list(
      key = "victoria_police_bail",
      file = "Victoria_Police_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Victori",
        "a_Police_Data_Tables_Bail_Visualisation_Year_Ending_Marc",
        "h_2026_0.xlsx"
      )
    ),
    list(
      key = "youth_justice_bail",
      file = "Youth_Justice_Data_Tables_Bail_Visualisation_Year_Ending_March_2026_0.xlsx",
      url = paste0(
        "https://files.crimestatistics.vic.gov.au/2026-06/Youth_J",
        "ustice_Data_Tables_Bail_Visualisation_Year_Ending_March_",
        "2026_0.xlsx"
      )
    ),
    NULL
  )
  entries <- Filter(Negate(is.null), entries)
  data.frame(
    key = vapply(entries, function(e) e$key, character(1)),
    file = vapply(entries, function(e) e$file, character(1)),
    url = vapply(entries, function(e) e$url, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Local cache directory for the Victorian workbooks
#'
#' @param cache_dir Optional directory. Defaults to the
#'   \code{MORIE_VIC_CACHE} environment variable, then to a
#'   \code{vic} folder under \code{tools::R_user_dir("rmorie", "cache")}.
#' @return The cache directory path, created if absent.
#' @examples
#' d <- morie_datasets_vic_cache_dir(tempfile())
#' dir.exists(d)
#' @export
morie_datasets_vic_cache_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- Sys.getenv("MORIE_VIC_CACHE", "")
  }
  if (!nzchar(cache_dir)) {
    cache_dir <- file.path(tools::R_user_dir("rmorie", "cache"), "vic")
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

#' Read one Victorian crime data table
#'
#' Loads a sheet from one of the Crime Statistics Agency workbooks,
#' downloading and caching the file on first use.
#'
#' @param key A catalog key; see \code{morie_datasets_vic_catalog()}.
#' @param table Sheet to read: an integer table number (1 gives
#'   "Table 01") or a sheet name.
#' @param cache_dir Optional cache directory.
#' @param offline When \code{TRUE} (the default) the function never
#'   downloads: it reads a cached copy and otherwise returns a 0-row
#'   frame, so examples and tests never touch the network.
#' @return A \code{data.frame} of the requested table, or a 0-row frame
#'   when the workbook is not cached and \code{offline} is \code{TRUE}.
#' @references Crime Statistics Agency (Victoria), CC BY 4.0.
#' @examples
#' # offline by default: returns a 0-row frame unless already cached
#' df <- morie_datasets_vic_table("criminal_incidents", table = 1)
#' ncol(df)
#' @export
morie_datasets_vic_table <- function(key, table = 1, cache_dir = NULL,
                                     offline = TRUE) {
  if (!is.character(key) || length(key) != 1L) {
    stop("`key` must be a single catalog key", call. = FALSE)
  }
  cat_df <- morie_datasets_vic_catalog()
  hit <- cat_df[cat_df$key == key, , drop = FALSE]
  if (nrow(hit) == 0L) {
    stop(sprintf("unknown key '%s'; see morie_datasets_vic_catalog()", key),
         call. = FALSE)
  }
  dest <- file.path(morie_datasets_vic_cache_dir(cache_dir), hit$file[1])
  if (!file.exists(dest)) {
    if (isTRUE(offline)) {
      return(data.frame())
    }
    utils::download.file(hit$url[1], dest, mode = "wb", quiet = TRUE)
  }
  sheet <- if (is.numeric(table)) sprintf("Table %02d", as.integer(table))
           else as.character(table)
  .morie_vic_read_sheet(dest, sheet)
}

#' Sheet names in a cached Victorian workbook
#'
#' @inheritParams morie_datasets_vic_table
#' @return A character vector of sheet names, or an empty vector when
#'   the workbook is not cached.
#' @examples
#' morie_datasets_vic_sheets("criminal_incidents")
#' @export
morie_datasets_vic_sheets <- function(key, cache_dir = NULL) {
  cat_df <- morie_datasets_vic_catalog()
  hit <- cat_df[cat_df$key == key, , drop = FALSE]
  if (nrow(hit) == 0L) {
    stop(sprintf("unknown key '%s'; see morie_datasets_vic_catalog()", key),
         call. = FALSE)
  }
  dest <- file.path(morie_datasets_vic_cache_dir(cache_dir), hit$file[1])
  if (!file.exists(dest)) {
    return(character(0))
  }
  .morie_vic_sheet_names(dest)
}


# --- native .xlsx reading -------------------------------------------------
#
# The workbooks are read with base unz() plus morie_xml_sax(), so no
# optional Excel package is needed. Sheets are resolved through the
# OOXML relationship map, NOT by sorting part names: a workbook with ten
# or more sheets sorts "sheet10.xml" before "sheet2.xml", and several of
# these releases have ten.

#' @keywords internal
#' @noRd
.morie_vic_local <- function(tag) {
  # The workbook parts are namespace-prefixed ("ns0:sheet"), so every
  # tag comparison has to run on the LOCAL name.
  sub("^[^:]+:", "", tag)
}

#' @keywords internal
#' @noRd
.morie_vic_zip_text <- function(path, member) {
  con <- unz(path, member, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, "raw", n = 64e6)
  if (!length(raw)) return("")
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}

#' @keywords internal
#' @noRd
.morie_vic_zip_has <- function(path, member) {
  member %in% utils::unzip(path, list = TRUE)$Name
}

#' @keywords internal
#' @noRd
.morie_vic_sheet_map <- function(path) {
  rels <- list()
  if (.morie_vic_zip_has(path, "xl/_rels/workbook.xml.rels")) {
    txt <- .morie_vic_zip_text(path, "xl/_rels/workbook.xml.rels")
    morie_xml_sax(txt, on_start = function(tag, attrs) {
      if (identical(.morie_vic_local(tag), "Relationship") &&
          !is.null(attrs[["Id"]])) {
        tgt <- attrs[["Target"]]
        if (is.null(tgt)) return(invisible(NULL))
        tgt <- sub("^/", "", tgt)
        if (!grepl("^xl/", tgt)) tgt <- paste0("xl/", tgt)
        rels[[attrs[["Id"]]]] <<- sub("xl/\\./", "xl/", tgt)
      }
      invisible(NULL)
    })
  }
  names_ <- character(0)
  parts <- character(0)
  txt <- .morie_vic_zip_text(path, "xl/workbook.xml")
  morie_xml_sax(txt, on_start = function(tag, attrs) {
    if (identical(.morie_vic_local(tag), "sheet")) {
      rid <- attrs[["r:id"]]
      names_ <<- c(names_, attrs[["name"]])
      parts <<- c(parts, if (!is.null(rid) && !is.null(rels[[rid]]))
                          rels[[rid]] else NA_character_)
    }
    invisible(NULL)
  })
  if (anyNA(parts)) {
    # No usable rels: order the parts NUMERICALLY, never lexically.
    all_parts <- grep("^xl/worksheets/sheet[0-9]+\\.xml$",
                      utils::unzip(path, list = TRUE)$Name, value = TRUE)
    n <- as.integer(gsub("\\D", "", basename(all_parts)))
    all_parts <- all_parts[order(n)]
    parts <- all_parts[seq_along(names_)]
  }
  list(names = names_, parts = parts)
}

#' @keywords internal
#' @noRd
.morie_vic_sheet_names <- function(path) {
  .morie_vic_sheet_map(path)$names
}

#' @keywords internal
#' @noRd
.morie_vic_shared_strings <- function(path) {
  if (!.morie_vic_zip_has(path, "xl/sharedStrings.xml")) return(character(0))
  txt <- .morie_vic_zip_text(path, "xl/sharedStrings.xml")
  out <- character(0)
  cur <- NULL
  depth_si <- 0L
  morie_xml_sax(
    txt,
    on_start = function(tag, attrs) {
      if (identical(.morie_vic_local(tag), "si")) {
        depth_si <<- 1L
        cur <<- ""
      }
      invisible(NULL)
    },
    on_text = function(txt2) {
      if (depth_si == 1L) cur <<- paste0(cur, txt2)
      invisible(NULL)
    },
    on_end = function(tag) {
      if (identical(.morie_vic_local(tag), "si")) {
        out <<- c(out, if (is.null(cur)) "" else cur)
        depth_si <<- 0L
        cur <<- NULL
      }
      invisible(NULL)
    }
  )
  out
}

#' @keywords internal
#' @noRd
.morie_vic_col_index <- function(ref) {
  letters_ <- toupper(gsub("[^A-Za-z]", "", ref))
  chars <- strsplit(letters_, "")[[1]]
  idx <- 0L
  for (ch in chars) idx <- idx * 26L + (utf8ToInt(ch) - 64L)
  idx
}

#' @keywords internal
#' @noRd
.morie_vic_read_sheet <- function(path, sheet, header = 1L) {
  smap <- .morie_vic_sheet_map(path)
  i <- match(sheet, smap$names)
  if (is.na(i)) {
    stop(sprintf("no sheet named '%s'; the workbook has: %s",
                 sheet, paste(smap$names, collapse = ", ")), call. = FALSE)
  }
  shared <- .morie_vic_shared_strings(path)
  txt <- .morie_vic_zip_text(path, smap$parts[i])

  cells <- new.env(parent = emptyenv())
  maxc <- 0L
  maxr <- 0L
  cur_ref <- NULL
  cur_t <- NULL
  in_v <- FALSE
  buf <- NULL
  morie_xml_sax(
    txt,
    on_start = function(tag, attrs) {
      if (identical(.morie_vic_local(tag), "c")) {
        cur_ref <<- attrs[["r"]]
        cur_t <<- attrs[["t"]]
        buf <<- NULL
      } else if (.morie_vic_local(tag) %in% c("v", "t")) {
        in_v <<- TRUE
        if (is.null(buf)) buf <<- ""
      }
      invisible(NULL)
    },
    on_text = function(txt2) {
      if (in_v) buf <<- paste0(buf, txt2)
      invisible(NULL)
    },
    on_end = function(tag) {
      if (.morie_vic_local(tag) %in% c("v", "t")) {
        in_v <<- FALSE
      } else if (identical(.morie_vic_local(tag), "c") &&
                 !is.null(cur_ref) &&
                 !is.null(buf)) {
        r <- as.integer(gsub("\\D", "", cur_ref))
        cc <- .morie_vic_col_index(cur_ref)
        val <- buf
        if (identical(cur_t, "s")) {
          k <- suppressWarnings(as.integer(val))
          val <- if (!is.na(k) && k + 1L <= length(shared)) shared[k + 1L]
                 else NA_character_
        } else if (identical(cur_t, "b")) {
          val <- if (identical(val, "1")) "TRUE" else "FALSE"
        }
        assign(paste0(r, "_", cc), val, envir = cells)
        maxr <<- max(maxr, r)
        maxc <<- max(maxc, cc)
        cur_ref <<- NULL
        buf <<- NULL
      }
      invisible(NULL)
    }
  )
  if (maxr == 0L || maxc == 0L) return(data.frame())

  get_cell <- function(r, cc) {
    k <- paste0(r, "_", cc)
    if (exists(k, envir = cells, inherits = FALSE)) get(k, envir = cells)
    else NA_character_
  }
  hdr <- vapply(seq_len(maxc), function(cc) get_cell(header, cc),
                character(1))
  hdr[is.na(hdr) | !nzchar(hdr)] <- paste0("V", which(
    is.na(hdr) | !nzchar(hdr)))
  body_rows <- seq_len(maxr)[-seq_len(header)]
  cols <- lapply(seq_len(maxc), function(cc) {
    v <- vapply(body_rows, function(r) get_cell(r, cc), character(1))
    num <- suppressWarnings(as.numeric(v))
    if (all(is.na(num) == is.na(v))) num else v
  })
  names(cols) <- make.unique(hdr)
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}


# --- analyses -------------------------------------------------------------

#' Offence-division trend from the Victorian criminal incidents table
#'
#' Aggregates recorded incidents by year and offence division and
#' reports the change over the observed window.
#'
#' @param data A frame shaped like
#'   \code{morie_datasets_vic_table("criminal_incidents", 1)}, with
#'   \code{Year}, \code{Offence Division} and \code{Incidents Recorded}.
#' @return A \code{data.frame} with one row per division:
#'   \code{division}, \code{first_year}, \code{last_year},
#'   \code{first_count}, \code{last_count}, \code{abs_change} and
#'   \code{pct_change}.
#' @references Crime Statistics Agency (Victoria), CC BY 4.0.
#' @examples
#' d <- data.frame(
#'   Year = c(2024, 2025, 2024, 2025),
#'   `Offence Division` = c("A", "A", "B", "B"),
#'   `Incidents Recorded` = c(100, 150, 80, 60),
#'   check.names = FALSE
#' )
#' morie_vic_offence_trend(d)
#' @export
morie_vic_offence_trend <- function(data) {
  need <- c("Year", "Offence Division", "Incidents Recorded")
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    stop(sprintf("missing column(s): %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  }
  if (nrow(data) == 0L) {
    return(data.frame(division = character(0), first_year = numeric(0),
                      last_year = numeric(0), first_count = numeric(0),
                      last_count = numeric(0), abs_change = numeric(0),
                      pct_change = numeric(0), stringsAsFactors = FALSE))
  }
  yr <- as.numeric(data[["Year"]])
  div <- as.character(data[["Offence Division"]])
  n <- as.numeric(data[["Incidents Recorded"]])
  agg <- stats::aggregate(list(count = n), by = list(division = div, year = yr),
                          FUN = function(v) sum(v, na.rm = TRUE))
  out <- lapply(sort(unique(agg$division)), function(d) {
    sub <- agg[agg$division == d, , drop = FALSE]
    sub <- sub[order(sub$year), , drop = FALSE]
    f <- sub$count[1]
    l <- sub$count[nrow(sub)]
    data.frame(
      division = d,
      first_year = sub$year[1], last_year = sub$year[nrow(sub)],
      first_count = f, last_count = l,
      abs_change = l - f,
      pct_change = if (f > 0) 100 * (l - f) / f else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Rank Victorian local government areas by offence rate
#'
#' @param data A frame shaped like
#'   \code{morie_datasets_vic_table("lga_criminal_incidents", 1)}.
#' @param year Optional year to restrict to; defaults to the latest.
#' @param top Number of areas to return (default 10).
#' @return A \code{data.frame} ordered by rate, with \code{lga},
#'   \code{year}, \code{incidents}, \code{rate} and \code{rank}.
#' @references Crime Statistics Agency (Victoria), CC BY 4.0.
#' @examples
#' d <- data.frame(
#'   Year = c(2025, 2025, 2025),
#'   `Local Government Area` = c("Alpha", "Beta", "Gamma"),
#'   `Incidents Recorded` = c(500, 300, 900),
#'   `Rate per 100,000 population` = c(1200, 800, 2500),
#'   check.names = FALSE
#' )
#' morie_vic_lga_rates(d, top = 2)
#' @export
morie_vic_lga_rates <- function(data, year = NULL, top = 10L) {
  need <- c("Year", "Local Government Area", "Incidents Recorded",
            "Rate per 100,000 population")
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    stop(sprintf("missing column(s): %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  }
  if (nrow(data) == 0L) {
    return(data.frame(lga = character(0), year = numeric(0),
                      incidents = numeric(0), rate = numeric(0),
                      rank = integer(0), stringsAsFactors = FALSE))
  }
  yr <- as.numeric(data[["Year"]])
  keep <- if (is.null(year)) yr == max(yr, na.rm = TRUE) else yr == year
  sub <- data[keep, , drop = FALSE]
  out <- data.frame(
    lga = as.character(sub[["Local Government Area"]]),
    year = as.numeric(sub[["Year"]]),
    incidents = as.numeric(sub[["Incidents Recorded"]]),
    rate = as.numeric(sub[["Rate per 100,000 population"]]),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$rate), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  utils::head(out, as.integer(top))
}

#' Indigenous victimisation rate ratio from the Victorian tables
#'
#' Compares victim-report counts for Aboriginal and/or Torres Strait
#' Islander people against the non-Indigenous count, per offence
#' division. This reports a COUNT ratio, not a population-adjusted rate
#' ratio: the published table carries counts only, so a value above 1
#' does not by itself establish over-representation without the
#' denominator populations. The table's "Total People" rows are
#' excluded, and one year is used at a time.
#'
#' @param data A frame shaped like
#'   \code{morie_datasets_vic_table("indigenous_victim_reports", 1)},
#'   carrying \code{Indigenous Status} and \code{Victim Reports}.
#' @param by Column to group by (default \code{"Offence Division"}).
#' @param year Optional year; defaults to the latest present. The table
#'   spans several years, so aggregating all of them silently mixes
#'   reporting periods.
#' @return A \code{data.frame} with \code{group}, \code{indigenous},
#'   \code{non_indigenous} and \code{count_ratio}.
#' @references Crime Statistics Agency (Victoria), CC BY 4.0.
#' @examples
#' d <- data.frame(
#'   `Offence Division` = c("A", "A", "B", "B"),
#'   `Indigenous Status` = c("Aboriginal and/or Torres Strait Islander",
#'                           "Non-Indigenous",
#'                           "Aboriginal and/or Torres Strait Islander",
#'                           "Non-Indigenous"),
#'   `Victim Reports` = c(8, 179, 20, 100),
#'   check.names = FALSE
#' )
#' morie_vic_indigenous_ratio(d)
#' @export
morie_vic_indigenous_ratio <- function(data, by = "Offence Division",
                                      year = NULL) {
  need <- c(by, "Indigenous Status", "Victim Reports")
  miss <- setdiff(need, names(data))
  if (length(miss)) {
    stop(sprintf("missing column(s): %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  }
  if (nrow(data) == 0L) {
    return(data.frame(group = character(0), indigenous = numeric(0),
                      non_indigenous = numeric(0), count_ratio = numeric(0),
                      stringsAsFactors = FALSE))
  }
  # The published table carries a "Total People" row alongside the two
  # status categories. Anything matching "Total" is dropped: folding it
  # into the non-Indigenous count double-counts every victim and roughly
  # doubles the denominator.
  if ("Year" %in% names(data)) {
    yr <- as.numeric(data[["Year"]])
    target <- if (is.null(year)) max(yr, na.rm = TRUE) else year
    data <- data[!is.na(yr) & yr == target, , drop = FALSE]
    if (nrow(data) == 0L) {
      return(data.frame(group = character(0), indigenous = numeric(0),
                        non_indigenous = numeric(0),
                        count_ratio = numeric(0),
                        stringsAsFactors = FALSE))
    }
  }
  grp <- as.character(data[[by]])
  status <- as.character(data[["Indigenous Status"]])
  n <- as.numeric(data[["Victim Reports"]])
  keep <- !grepl("Total", status, fixed = TRUE)
  grp <- grp[keep]
  status <- status[keep]
  n <- n[keep]
  is_ind <- grepl("Aboriginal", status, fixed = TRUE)
  is_non <- grepl("Non-Indigenous", status, fixed = TRUE)
  keys <- sort(unique(grp))
  out <- lapply(keys, function(k) {
    a <- sum(n[grp == k & is_ind], na.rm = TRUE)
    b <- sum(n[grp == k & is_non], na.rm = TRUE)
    data.frame(group = k, indigenous = a, non_indigenous = b,
               count_ratio = if (b > 0) a / b else NA_real_,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
