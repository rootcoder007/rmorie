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
#' @examples
#' \dontrun{
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
                                     user_agent = "morie/r (+https://hadesllm.com)",
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
#' If the \code{STATCAN_API_KEY} environment variable is set, it is
#' passed to \code{cansim::set_cansim_api_key()} so authenticated
#' rate limits apply.
#'
#' @param table_id A StatCan / NDM table identifier, e.g.
#'   \code{"35-10-0177"} or \code{"35-10-0177-01"}.
#' @param language One of \code{"eng"} or \code{"fra"}.
#' @param refresh If \code{TRUE}, force \pkg{cansim} to re-download
#'   rather than using its on-disk cache.
#' @param ... Further arguments forwarded to
#'   \code{\link[cansim]{get_cansim}}.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
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
    Sys.setenv(CANSIM_API_KEY = api_key)
  }

  tryCatch(
    {
      df <- cansim::get_cansim(table_id,
        language = language,
        refresh = isTRUE(refresh), ...
      )
      as.data.frame(df)
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
