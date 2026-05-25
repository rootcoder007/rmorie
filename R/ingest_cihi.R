# SPDX-License-Identifier: AGPL-3.0-or-later
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

#' Download a CIHI indicator .xlsx data table
#' @param url Direct URL of the CIHI .xlsx data table.
#' @param sheet Worksheet name or 1-based index. NULL = largest sheet.
#' @param timeout HTTP timeout in seconds (default 120).
#' @param user_agent User-Agent string.
#' @param ... forwarded to readxl::read_excel.
#' @return base R data.frame.
#' @export
morie_ingest_cihi_xlsx <- function(url, sheet = NULL, timeout = 120,
                                   user_agent = "morie/r (+https://hadesllm.com)", ...) {
  if (!is.character(url) || length(url) != 1L || !nzchar(url))
    stop("`url` must be a single non-empty string.", call. = FALSE)
  morie_ensure_extras(c("httr2", "readxl"))
  tmp <- tempfile(fileext = ".xlsx", tmpdir = tempdir())
  on.exit(if (file.exists(tmp)) unlink(tmp, force = TRUE), add = TRUE)
  # 3YY: libcurl-backed binary fetch with httr2 fallback.
  tryCatch({
    bytes <- .morie_dataset_http_bytes(url,
                                         timeout_s = as.integer(timeout))
    writeBin(bytes, tmp)
  }, error = function(e) {
    stop("morie_ingest_cihi_xlsx: download failed for ", url, "\n  ",
         conditionMessage(e), call. = FALSE)
  })
  tryCatch({
    if (is.null(sheet)) .morie_cihi_pick_data_sheet(tmp, ...)
    else as.data.frame(readxl::read_excel(tmp, sheet = sheet, ...))
  }, error = function(e) {
    stop("morie_ingest_cihi_xlsx: parse failed for ", url, "\n  ",
         conditionMessage(e), call. = FALSE)
  })
}
