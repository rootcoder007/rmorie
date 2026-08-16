# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Statistics Canada (StatCan) direct-download ingest.
#
# StatCan distributes Public Use Microdata Files (PUMFs) and other
# products from www150.statcan.gc.ca/n1/pub/... as .zip archives
# containing one or more CSV files.
#
# `morie_ingest_statcan_csv()` mirrors the Python `fetch_statcan_csv`:
# streams a `_CSV.zip` to a tempfile, extracts the chosen CSV member,
# and returns the contents as a base R data.frame. PUMF zips can be
# hundreds of megabytes, so the archive is streamed to disk rather
# than buffered in memory, and read with `readr::read_csv()` (which
# is fast on the large CSVs StatCan ships) when available, falling
# back to `utils::read.csv` for tiny products.
#
# `morie_ingest_statcan_cansim()` wraps the CRAN `cansim` package for
# the StatCan NDM / cansim tabular API and honours the
# STATCAN_API_KEY env var when present.

# Internal: read a CSV member out of a zip into a base data.frame.
#' Internal helper: Morie Statcan Csv From Zip
#' @noRd
.morie_statcan_csv_from_zip <- function(zip_path,
                                        member = NULL,
                                        ...) {
  names <- utils::unzip(zip_path, list = TRUE)$Name
  if (length(names) == 0L) {
    stop("StatCan archive is empty: ", zip_path, call. = FALSE)
  }
  csvs <- names[grepl("\\.csv$", names, ignore.case = TRUE)]
  if (length(csvs) == 0L) {
    stop("No .csv file inside the StatCan archive: ", zip_path,
      call. = FALSE
    )
  }
  chosen <- if (!is.null(member) && nzchar(member)) member else csvs[1L]
  if (!chosen %in% names) {
    stop(
      "member '", chosen, "' not in the archive; CSVs present: ",
      paste(csvs, collapse = ", "),
      call. = FALSE
    )
  }

  exdir <- file.path(tempdir(), "morie-statcan")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  extracted <- utils::unzip(zip_path, files = chosen, exdir = exdir)
  on.exit(unlink(extracted, force = TRUE), add = TRUE)

  if (requireNamespace("readr", quietly = TRUE)) {
    df <- readr::read_csv(extracted,
      show_col_types = FALSE,
      progress = FALSE, ...
    )
    return(as.data.frame(df))
  }
  utils::read.csv(extracted, stringsAsFactors = FALSE, ...)
}

#' Download a StatCan PUMF / CSV product
#'
#' Downloads a Statistics Canada \code{_CSV.zip} product from
#' \code{www150.statcan.gc.ca}, extracts a CSV member, and returns
#' the contents as a base R \code{data.frame}. The archive is
#' streamed to a session-scoped tempfile (PUMF zips can be hundreds
#' of megabytes), and the tempfile is removed when the function
#' returns. Nothing is written under \code{~/.cache} unless the
#' caller explicitly opts in via \code{\link{morie_cache_dir}}.
#'
#' Note that a StatCan \emph{catalogue} page (e.g.
#' \code{/n1/en/catalogue/82M0013X}) is only an HTML index --- the
#' actual data is linked from the \emph{product} page
#' (\code{/n1/pub/82m0013x/82m0013x2024001-eng.htm}), which points at
#' the real \code{..._CSV.zip}.
#'
#' @param url Direct URL of the StatCan \code{.zip} product, e.g.
#'   \code{https://www150.statcan.gc.ca/n1/pub/82m0013x/2024001/2022_CSV.zip}.
#' @param member Name of the CSV inside the archive; defaults to the
#'   first \code{.csv} entry.
#' @param timeout HTTP timeout in seconds (default 600).
#' @param user_agent User-Agent string sent with the request.
#' @param ... Further arguments forwarded to
#'   \code{\link[readr]{read_csv}} (or
#'   \code{\link[utils]{read.csv}} if \pkg{readr} is unavailable).
#' @return A base R \code{data.frame}.
#' @examplesIf requireNamespace("httr2", quietly = TRUE)
#' \donttest{
#' # Requires network access.
#' url <- paste0(
#'   "https://www150.statcan.gc.ca/n1/pub/82m0013x/",
#'   "2024001/2022_CSV.zip"
#' )
#' df <- morie_ingest_statcan_csv(url)
#' head(df)
#' }
#' @seealso \code{\link{morie_ingest_statcan_cansim}},
#'   \code{\link{morie_cache_dir}}
#' @export
morie_ingest_statcan_csv <- function(url,
                                     member = NULL,
                                     timeout = 600,
                                     user_agent = "morie/r (+https://github.com/rootcoder007/rmorie)",
                                     ...) {
  if (!is.character(url) || length(url) != 1L || !nzchar(url)) {
    stop("`url` must be a single non-empty string.", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package 'httr2' is required for morie_ingest_statcan_csv(). ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }

  tmp <- tempfile(fileext = ".zip", tmpdir = tempdir())
  on.exit(
    if (file.exists(tmp)) unlink(tmp, force = TRUE),
    add = TRUE
  )

  tryCatch(
    {
      # 3YY: libcurl-backed binary fetch with httr2 fallback.
      bytes <- .morie_dataset_http_bytes(url,
                                           timeout_s = as.integer(timeout))
      writeBin(bytes, tmp)
    },
    error = function(e) {
      stop(
        "morie_ingest_statcan_csv: download failed for ", url, "\
  ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  tryCatch(
    .morie_statcan_csv_from_zip(tmp, member = member, ...),
    error = function(e) {
      stop(
        "morie_ingest_statcan_csv: extract/parse failed for ", url, "\
  ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Fetch a Statistics Canada NDM / cansim table
#'
#' Convenience wrapper around the CRAN \pkg{cansim} package, which
#' talks to the Statistics Canada NDM ("cansim") tabular data API.
#' Use this for canonical CANSIM tables (e.g. \code{"35-10-0177-01"})
#' rather than for PUMF \code{_CSV.zip} downloads --- those go
#' through \code{\link{morie_ingest_statcan_csv}}.
#'
#' No credentials are required: the StatCan Web Data Service is
#' public. Setting the optional \code{STATCAN_API_KEY} environment
#' variable only raises your WDS rate limits; without it the fetch
#' still works (it is forwarded to \pkg{cansim} via the
#' \code{CANSIM_API_KEY} name the package reads internally).
#'
#' @param table_id A StatCan / NDM table identifier, e.g.
#'   \code{"35-10-0177"} or \code{"35-10-0177-01"}.
#' @param language One of \code{"eng"} or \code{"fra"}.
#' @param refresh If \code{TRUE}, force \pkg{cansim} to re-download
#'   rather than using its on-disk cache.
#' @param ... Further arguments forwarded to
#'   \code{\link[cansim]{get_cansim}}.
#' @return A base R \code{data.frame}.
#' @examplesIf requireNamespace("httr2", quietly = TRUE)
#' \donttest{
#' # Requires the 'cansim' package and network access.
#' df <- morie_ingest_statcan_cansim("35-10-0177")
#' head(df)
#' }
#' @seealso \code{\link{morie_ingest_statcan_csv}}
#' @export
morie_ingest_statcan_cansim <- function(table_id,
                                        language = c("eng", "fra"),
                                        refresh = FALSE,
                                        ...) {
  language <- match.arg(language)
  if (!is.character(table_id) || length(table_id) != 1L ||
    !nzchar(table_id)) {
    stop("`table_id` must be a single non-empty string.", call. = FALSE)
  }
  if (!requireNamespace("cansim", quietly = TRUE)) {
    stop(
      "Package 'cansim' is required for morie_ingest_statcan_cansim(). ",
      "install.packages('cansim')",
      call. = FALSE
    )
  }

  # cansim has no set_cansim_api_key() helper in current CRAN
  # releases (the function does not exist).  Authenticated calls
  # are configured via the CANSIM_API_KEY env var, which cansim
  # reads internally; STATCAN_API_KEY is morie's alias.
  api_key <- Sys.getenv("STATCAN_API_KEY", "")
  if (nzchar(api_key) && !nzchar(Sys.getenv("CANSIM_API_KEY", ""))) {
    # Bridge the key to the name cansim reads, but restore the user's
    # environment on exit (CRAN: do not persistently modify the user's env).
    Sys.setenv(CANSIM_API_KEY = api_key)
    on.exit(Sys.unsetenv("CANSIM_API_KEY"), add = TRUE)
  }

  tryCatch(
    {
      as.data.frame(.morie_statcan_wds_table(table_id,
                                             language = language))
    },
    error = function(e) {
      stop(
        "morie_ingest_statcan_cansim: fetch failed for table '",
        table_id, "'\
  ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Fetch StatCan series by vector ID (keyless WDS, latest-N periods)
#'
#' Retrieves specific data series by their StatCan vector identifiers
#' through the public Web Data Service
#' \code{getDataFromVectorsAndLatestNPeriods} endpoint. This is the
#' small, targeted alternative to
#' \code{\link{morie_ingest_statcan_cansim}}, which downloads a whole
#' table (millions of rows). No credentials are required; the WDS is
#' public. The POST runs through rmorie's native libcurl backend --- no
#' extra package dependency.
#'
#' @param vectors Character or numeric StatCan vector IDs. A leading
#'   \code{"v"} is accepted and stripped (e.g. \code{"v41690973"} or
#'   \code{41690973}).
#' @param periods Number of most-recent reference periods to return
#'   per vector.
#' @param timeout Per-request timeout, seconds.
#' @return A base R \code{data.frame}, one row per (vector, period):
#'   \code{vector}, \code{ref_date}, \code{value}, \code{decimals},
#'   \code{scalar_factor}, \code{symbol_code}, \code{release_time}.
#' @examplesIf requireNamespace("httr2", quietly = TRUE)
#' \donttest{
#' # Two CPI series, last 3 periods each -- no API key needed.
#' morie_ingest_statcan_vectors(c("v41690973", "v41691045"), periods = 3)
#' }
#' @seealso \code{\link{morie_ingest_statcan_cansim}}
#' @export
morie_ingest_statcan_vectors <- function(vectors, periods = 12L,
                                         timeout = 60L) {
  if (length(vectors) == 0L) {
    stop("`vectors` must be a non-empty vector of StatCan vector IDs.",
         call. = FALSE)
  }
  ids <- suppressWarnings(
    as.integer(gsub("[^0-9]", "", as.character(vectors)))
  )
  if (anyNA(ids)) {
    stop("`vectors` must be StatCan vector IDs (numbers, optionally ",
         "'v'-prefixed).", call. = FALSE)
  }
  periods <- as.integer(periods)
  if (is.na(periods) || periods < 1L) {
    stop("`periods` must be a positive integer.", call. = FALSE)
  }
  body <- .s03json_toJSON(
    data.frame(vectorId = ids, latestN = periods),
    auto_unbox = TRUE
  )
  resp <- .morie_http_post_with_status(
    "https://www150.statcan.gc.ca/t1/wds/rest/getDataFromVectorsAndLatestNPeriods",
    body,
    content_type = "application/json",
    timeout_s = as.integer(timeout),
    user_agent = "morie/r (+https://github.com/rootcoder007/rmorie)"
  )
  if (!identical(as.integer(resp$status_code), 200L)) {
    stop("StatCan WDS vector request failed (HTTP ",
         resp$status_code, ").", call. = FALSE)
  }
  parsed <- .s03json_fromJSON(resp$body, simplifyVector = FALSE)
  rows <- lapply(parsed, function(el) {
    if (!identical(el$status, "SUCCESS")) return(NULL)
    ob <- el$object
    dp <- ob$vectorDataPoint
    if (length(dp) == 0L) return(NULL)
    do.call(rbind, lapply(dp, function(p) data.frame(
      vector        = ob$vectorId,
      ref_date      = p$refPer %||% NA_character_,
      value         = p$value %||% NA_real_,
      decimals      = p$decimals %||% NA_integer_,
      scalar_factor = p$scalarFactorCode %||% NA_integer_,
      symbol_code   = p$symbolCode %||% NA_integer_,
      release_time  = p$releaseTime %||% NA_character_,
      stringsAsFactors = FALSE
    )))
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0L) {
    stop("StatCan WDS returned no data for the requested vector(s).",
         call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

# Native StatCan Web Data Service client -- fetches the full-table
# CSV for a table id ("NN-MM-XXXX" or "NN-MM-XXXX-NN" cansim style, or
# a bare 8-digit PID) via the getFullTableDownloadCSV endpoint. Base R
# only (download.file + unzip + read.csv); replaces cansim::get_cansim.
# Column set is StatCan's raw CSV schema (REF_DATE, GEO, VALUE, ...),
# which is the subset of get_cansim() output the callers use.
#' Native StatCan Web Data Service client -- fetches the full-table
#'
#' CSV for a table id ("NN-MM-XXXX" or "NN-MM-XXXX-NN" cansim style, or
#' a bare 8-digit PID) via the getFullTableDownloadCSV endpoint. Base R
#' only (download.file + unzip + read.csv); replaces cansim::get_cansim.
#' Column set is StatCan\'s raw CSV schema (REF_DATE, GEO, VALUE, ...),
#' which is the subset of get_cansim() output the callers use.
#'
#' @param table_id Character; passed to \code{gsub}.
#' @param language Character; passed to \code{tolower}. Defaults to \code{"en"}.
#' @return The value of \code{utils::read.csv}.
#' @export
.morie_statcan_wds_table <- function(table_id, language = "en") {
  pid <- gsub("[^0-9]", "", table_id)
  if (nchar(pid) >= 10L) pid <- substr(pid, 1, 8L)
  if (nchar(pid) == 6L) pid <- paste0(pid, "01")
  if (nchar(pid) != 8L) {
    stop("cannot derive an 8-digit StatCan product id from '",
         table_id, "'", call. = FALSE)
  }
  lang <- if (startsWith(tolower(language), "fr")) "fr" else "en"
  meta_url <- paste0(
    "https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/",
    pid, "/", lang
  )
  meta_raw <- tryCatch(
    suppressWarnings(readLines(meta_url, warn = FALSE)),
    error = function(e) stop("WDS metadata request failed: ",
                             conditionMessage(e), call. = FALSE)
  )
  meta <- paste(meta_raw, collapse = "")
  zip_url <- regmatches(meta, regexpr("https://[^\"]+\\.zip", meta))
  if (length(zip_url) != 1L || !nzchar(zip_url)) {
    stop("WDS did not return a download URL for pid ", pid, call. = FALSE)
  }
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)
  utils::download.file(zip_url, tmp_zip, mode = "wb", quiet = TRUE)
  exdir <- tempfile("statcan_wds_")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  files <- utils::unzip(tmp_zip, exdir = exdir)
  csvs <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  # The data file is <pid>.csv; the _MetaData.csv companion is skipped.
  data_csv <- csvs[!grepl("MetaData", csvs, ignore.case = TRUE)]
  if (length(data_csv) == 0L) {
    stop("WDS archive for pid ", pid, " contained no data CSV.",
         call. = FALSE)
  }
  utils::read.csv(data_csv[1L], check.names = FALSE,
                  stringsAsFactors = FALSE)
}
