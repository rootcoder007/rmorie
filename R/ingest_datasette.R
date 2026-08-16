# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie/ingest_datasette.R -- generic Datasette JSON-API connector.
#
# Datasette (https://datasette.io/) exposes any SQLite database as a
# JSON API: /<db>.json lists tables, /<db>/<table>.json returns rows,
# and /<db>.json?sql=... runs read-only SQL. The instance base URL
# is configurable via MORIE_DATASETTE_URL, so these functions work
# against any Datasette instance the caller has access to.

#' .morie_datasette_base
#'
#' A step of the ingest_datasette implementation. Called by \code{morie_datasette_databases}, \code{morie_datasette_read}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param base_url Defaults to \code{NULL}.
#' @return The value of \code{sub}.
#' @export
.morie_datasette_base <- function(base_url = NULL) {
  url <- base_url
  if (is.null(url) || !nzchar(url)) {
    url <- Sys.getenv("MORIE_DATASETTE_URL", "")
  }
  if (!nzchar(url)) {
    stop(
      "No Datasette instance configured. Pass base_url= or set the ",
      "MORIE_DATASETTE_URL environment variable to the instance root ",
      "(e.g. 'https://example.org/data').",
      call. = FALSE
    )
  }
  sub("/+$", "", url)
}

#' .morie_datasette_get_json
#'
#' A step of the ingest_datasette implementation. Called by \code{morie_datasette_databases}, \code{morie_datasette_read}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param url See Usage.
#' @param timeout Defaults to \code{60}.
#' @return The value of \code{.s03json_fromJSON}.
#' @export
.morie_datasette_get_json <- function(url, timeout = 60) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for the Datasette connector. ",
         "install.packages('jsonlite')", call. = FALSE)
  }
  con <- url(url, open = "rb")
  on.exit(close(con), add = TRUE)
  .s03json_fromJSON(rawToChar(readBin(con, "raw", n = 64L * 1024L^2)),
                     simplifyVector = TRUE)
}

#' List the databases served by a Datasette instance
#'
#' Queries \code{/-/databases.json} on the instance and returns one row
#' per database.
#'
#' @param base_url Instance root URL; defaults to the
#'   \code{MORIE_DATASETTE_URL} environment variable.
#' @param timeout HTTP timeout in seconds.
#' @return A \code{data.frame} with one row per database (name, path,
#'   size, table count as provided by the instance).
#' @examplesIf nzchar(Sys.getenv("MORIE_DATASETTE_URL"))
#' dbs <- morie_datasette_databases()
#' head(dbs$name)
#' @export
morie_datasette_databases <- function(base_url = NULL, timeout = 60) {
  base <- .morie_datasette_base(base_url)
  out <- .morie_datasette_get_json(paste0(base, "/-/databases.json"),
                                   timeout = timeout)
  # Newer Datasette wraps the list as {ok, databases: [...]}; older
  # versions return the bare array.
  if (is.list(out) && !is.null(out$databases)) out <- out$databases
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Read rows from a Datasette table (or run read-only SQL)
#'
#' Fetches \code{/<db>/<table>.json} (or \code{/<db>.json?sql=...})
#' from a Datasette instance and returns the rows as a base R
#' \code{data.frame}. Datasette's SQL surface is read-only by design,
#' so this cannot mutate the remote database.
#'
#' @param db Database name as served by the instance.
#' @param table Table name; ignored when \code{sql} is given.
#' @param sql Optional read-only SQL string executed against \code{db}.
#' @param limit Row cap forwarded as Datasette's \code{_size} (table
#'   mode) or appended as \code{LIMIT} advice in SQL mode (default 1000).
#' @param base_url Instance root URL; defaults to the
#'   \code{MORIE_DATASETTE_URL} environment variable.
#' @param timeout HTTP timeout in seconds.
#' @return A base R \code{data.frame}.
#' @examplesIf nzchar(Sys.getenv("MORIE_DATASETTE_URL"))
#' dbs <- morie_datasette_databases()
#' # Peek at the first table of the first database:
#' tabs <- morie_datasette_read(dbs$name[1],
#'   sql = "SELECT name FROM sqlite_master WHERE type='table' LIMIT 5")
#' tabs
#' @export
morie_datasette_read <- function(db, table = NULL, sql = NULL,
                                 limit = 1000L, base_url = NULL,
                                 timeout = 60) {
  if (!is.character(db) || length(db) != 1L || !nzchar(db)) {
    stop("`db` must be a single non-empty string.", call. = FALSE)
  }
  base <- .morie_datasette_base(base_url)
  lim <- as.integer(limit)
  if (is.na(lim) || lim < 1L) {
    stop("`limit` must be a positive integer.", call. = FALSE)
  }
  if (!is.null(sql)) {
    u <- paste0(base, "/", utils::URLencode(db, reserved = TRUE),
                ".json?sql=", utils::URLencode(sql, reserved = TRUE),
                "&_shape=array")
  } else {
    if (is.null(table) || !nzchar(table)) {
      stop("Pass `table=` (or `sql=`).", call. = FALSE)
    }
    u <- paste0(base, "/", utils::URLencode(db, reserved = TRUE), "/",
                utils::URLencode(table, reserved = TRUE),
                ".json?_shape=array&_size=", lim)
  }
  out <- .morie_datasette_get_json(u, timeout = timeout)
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  if (!is.null(sql) && nrow(df) > lim) df <- utils::head(df, lim)
  df
}
