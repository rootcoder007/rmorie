# SPDX-License-Identifier: AGPL-3.0-or-later
#' Internal helper: Morie Cihi Pick Data Sheet
#' @noRd
.morie_cihi_pick_data_sheet <- function(path, ...) {
  morie_ensure_extras("readxl")
  sheets <- readxl::excel_sheets(path)
  best_df <- NULL
  best_cells <- -1L
  best_name <- sheets[1L]
  for (nm in sheets) {
    df <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = nm, ...)),
                    error = function(e) NULL)
    if (is.null(df)) next
    cells <- as.integer(nrow(df)) * as.integer(ncol(df))
    if (!is.na(cells) && cells > best_cells) {
      best_name <- nm
      best_df <- df
      best_cells <- cells
    }
  }
  if (is.null(best_df)) stop("No readable sheets found in CIHI workbook: ", path, call. = FALSE)
  attr(best_df, "morie_cihi_sheet") <- best_name
  best_df
}

#' Catalogue of CIHI open data-table workbooks
#'
#' Returns the CIHI data-table catalogue -- every public data-table
#' \code{.xlsx} workbook on CIHI's \dQuote{Access data and reports > Data
#' tables} page (\url{https://www.cihi.ca/en/access-data-and-reports/data-tables}),
#' with its direct \code{url} and an Internet Archive \code{wayback_url}
#' fallback. The full ~218-row catalogue is bundled in \pkg{rmoriedata}
#' (\code{rmoriedata::load_cihi_data_tables()}); if that package is not
#' installed this returns a small built-in subset so the function still
#' works. Pass any \code{url} to \code{\link{morie_ingest_cihi_xlsx}}.
#'
#' @return A base R \code{data.frame} with columns \code{title}, \code{url},
#'   and (when \pkg{rmoriedata} is present) \code{wayback_url}.
#' @examples
#' cat <- morie_datasets_cihi_data_tables()
#' nrow(cat)
#' cat$title[1]
#' @export
morie_datasets_cihi_data_tables <- function() {
  if (requireNamespace("rmoriedata", quietly = TRUE)) {
    return(rmoriedata::load_cihi_data_tables())
  }
  base <- "https://www.cihi.ca/sites/default/files/document/"
  data.frame(
    title = c(
      "Injury and Trauma Emergency Department and Hospitalization Statistics, 2024-2025",
      "Wait Times for Priority Procedures in Canada, 2008 to 2025",
      "Health Workforce in Canada, 2024 - Quick Stats",
      "Hospital Beds, 2024-2025",
      "Inpatient Hospitalization, Surgery and Newborn Statistics, 2024-2025"
    ),
    url = paste0(base, c(
      "injury-trauma-emergency-dept-hospitalizations-2024-2025-data-tables-en.xlsx",
      "wait-times-priority-procedures-in-canada-2008-2025-data-tables-en.xlsx",
      "health-workforce-quickstats-2024-data-tables-en.xlsx",
      "hospital-beds-2024-2025-data-tables-en.xlsx",
      "dad-hmdb-childbirth-2024-2025-data-tables-en.xlsx"
    )),
    wayback_url = "",
    stringsAsFactors = FALSE
  )
}

#' Download a CIHI indicator .xlsx data table
#' @param url Direct URL of the CIHI .xlsx data table. See
#'   \code{\link{morie_datasets_cihi_data_tables}} for the published catalogue.
#' @param sheet Worksheet name or 1-based index. NULL = largest sheet.
#' @param timeout HTTP timeout in seconds (default 120).
#' @param user_agent User-Agent string.
#' @param wayback_url Optional Internet Archive snapshot URL used as a
#'   fallback if the live \code{url} download fails (CIHI rotates files).
#'   \code{NULL} (default) auto-resolves one via
#'   \code{rmoriebricklayer::wayback_snapshot_url()} when that package is
#'   installed; \code{""} disables the fallback.
#' @param ... forwarded to readxl::read_excel.
#' @return base R data.frame.
#' @examplesIf requireNamespace("httr2", quietly = TRUE) && requireNamespace("readxl", quietly = TRUE)
#' \dontrun{
#' # Any table from the catalogue, e.g. the injury/trauma ED table:
#' u <- morie_datasets_cihi_data_tables()$url[1]
#' df <- morie_ingest_cihi_xlsx(u)
#' }
#' @examples
#' \dontshow{if (requireNamespace("httr2", quietly = TRUE) && requireNamespace("readxl", quietly = TRUE)) withAutoprint(\{ # examplesIf}
#' \dontrun{
#' # Any table from the catalogue, e.g. the injury/trauma ED table:
#' u <- morie_datasets_cihi_data_tables()$url[1]
#' df <- morie_ingest_cihi_xlsx(u)
#' }
#' \dontshow{\}) # examplesIf}
#' @export
morie_ingest_cihi_xlsx <- function(url, sheet = NULL, timeout = 120,
                                   user_agent = "morie/r (+https://github.com/rootcoder007/rmorie)",
                                   wayback_url = NULL, ...) {
  if (!is.character(url) || length(url) != 1L || !nzchar(url))
    stop("`url` must be a single non-empty string.", call. = FALSE)
  morie_ensure_extras(c("httr2", "readxl"))
  tmp <- tempfile(fileext = ".xlsx", tmpdir = tempdir())
  on.exit(if (file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)
  # 3YY: libcurl-backed binary fetch with httr2 fallback. On failure,
  # retry the Internet Archive snapshot (CIHI rotates its file paths).
  fetched <- tryCatch({
    bytes <- .morie_dataset_http_bytes(url, timeout_s = as.integer(timeout))
    writeBin(bytes, tmp)
    TRUE
  }, error = function(e) FALSE)
  if (!fetched) {
    wb <- wayback_url
    if (is.null(wb) && requireNamespace("rmoriebricklayer", quietly = TRUE)) {
      wb <- tryCatch(rmoriebricklayer::wayback_snapshot_url(url),
                     error = function(e) "")
    }
    if (!is.null(wb) && nzchar(wb)) {
      tryCatch({
        bytes <- .morie_dataset_http_bytes(wb, timeout_s = as.integer(timeout))
        writeBin(bytes, tmp)
        fetched <- TRUE
      }, error = function(e) NULL)
    }
    if (!fetched)
      stop("morie_ingest_cihi_xlsx: download failed for ", url,
           " (and no working Wayback fallback).", call. = FALSE)
  }
  tryCatch({
    if (is.null(sheet)) .morie_cihi_pick_data_sheet(tmp, ...)
    else as.data.frame(readxl::read_excel(tmp, sheet = sheet, ...))
  }, error = function(e) {
    stop("morie_ingest_cihi_xlsx: parse failed for ", url, "\n  ",
         conditionMessage(e), call. = FALSE)
  })
}
