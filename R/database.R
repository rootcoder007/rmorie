# database.R -- DBI-backed generic-SQL data layer for MORIE
#
# Built-in database: inst/extdata/morie.db ships with the package and is
# always SQLite (read-only; portable across R and Python).
#
# User cache: any DBI-compatible backend. The default is SQLite at
# morie.db under the per-user cache directory. Users who want a server
# backend (PostgreSQL, MariaDB) or a columnar one (DuckDB) pass a
# pre-opened DBI connection via the `con` argument on every cache
# function. The same code path then talks to that backend through DBI.
#
# Examples:
#   # default SQLite (current behaviour)
#   morie_cache_store(df, "tbl")
#
#   # DuckDB
#   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "morie.duckdb")
#   morie_cache_store(df, "tbl", con = con)
#
#   # PostgreSQL
#   con <- DBI::dbConnect(RPostgres::Postgres(),
#     host = "localhost", dbname = "morie", user = "...")
#   morie_load_dataset("ocp21", con = con)

# Internal: resolve a DBI connection. Accepts a pre-opened connection
# (used as-is, caller owns disconnection) OR a SQLite path string (we
# open + own + close). The default path is the per-user cache.
#
# Returns: list(con = DBIConnection, close = logical).
.morie_db_handle <- function(con = NULL, db_path = NULL) {
  if (!is.null(con)) {
    if (!inherits(con, "DBIConnection")) {
      stop("`con` must be a DBIConnection (see `?DBI::dbConnect`).",
        call. = FALSE
      )
    }
    return(list(con = con, close = FALSE))
  }
  list(con = morie_db_connect(db_path), close = TRUE)
}

#' morie cache contract
#'
#' morie functions that persist artifacts to disk (e.g.
#' \code{morie_fetch_siu(cache_html = TRUE)}) default to a
#' \emph{session-scoped} subdirectory of \code{\link[base]{tempdir}()},
#' which R automatically removes when the session ends. This is the
#' most conservative CRAN-Policy-compliant default: nothing morie
#' writes ever survives the R session unless the user explicitly
#' opts in.
#'
#' Users who want \emph{persistent} caching across sessions opt in by
#' passing the result of \code{morie_cache_dir(subdir)} as the
#' \code{cache_dir} argument, e.g.:
#'
#' \preformatted{
#'   morie_fetch_siu(
#'     cache_dir = morie_cache_dir("siu"),
#'     cache_html = TRUE
#'   )
#' }
#'
#' The persistent location is \code{tools::R_user_dir("morie", "cache")}
#' (R \eqn{\ge}{>=} 4.0), which on Linux defaults to
#' \code{~/.cache/R/morie/}, on macOS to
#' \code{~/Library/Caches/org.R-project.R/R/morie/}, and on Windows to
#' \code{\%LOCALAPPDATA\%/R/cache/R/morie/}. Users can override this
#' location by setting the \code{MORIE_CACHE_DIR} environment variable
#' before calling \code{morie_cache_dir()}.
#'
#' \strong{Active management.} CRAN Policy requires persistent caches
#' to be actively managed. Use \code{\link{morie_cache_clear}()} to
#' empty the persistent cache (or a subdirectory of it). Cached SIU
#' HTML is ~80-100 MB at full sweep, so clearing it occasionally is
#' usually unnecessary, but it is supported.
#'
#' @param subdir Optional subdirectory under the morie cache root
#'   (e.g. \code{"siu"}, \code{"tps"}). If \code{NULL}, the cache
#'   root itself is returned.
#' @return A file path string. The directory is \emph{not} created;
#'   callers create it lazily only when they actually persist to disk.
#' @examples
#' # Persistent cache root (does not write anything to disk):
#' morie_cache_dir()
#' # Per-subsystem persistent path:
#' morie_cache_dir("siu")
#' @seealso \code{\link{morie_cache_clear}}
#' @export
morie_cache_dir <- function(subdir = NULL) {
  override <- Sys.getenv("MORIE_CACHE_DIR", "")
  base <- if (nzchar(override)) {
    path.expand(override)
  } else {
    tools::R_user_dir("morie", which = "cache")
  }
  if (is.null(subdir)) base else file.path(base, subdir)
}

#' Clear morie's persistent cache directory
#'
#' Removes files cached by morie under
#' \code{tools::R_user_dir("morie", "cache")} (or
#' \code{MORIE_CACHE_DIR} if set). morie's default behaviour writes
#' caches to a session-scoped \code{\link[base]{tempdir}()}
#' subdirectory, so this function only matters if you have explicitly
#' opted in to persistent caching by passing
#' \code{cache_dir = morie_cache_dir(...)} to any of the morie
#' fetchers.
#'
#' @param subdir Optional subdirectory under the morie cache root to
#'   target (e.g. \code{"siu"}, \code{"tps"}). If \code{NULL}, removes
#'   the entire morie persistent-cache root.
#' @param confirm If \code{TRUE} (default in interactive sessions),
#'   prompts the user before deleting. Set \code{FALSE} in scripts /
#'   batch use to skip the prompt.
#' @return Invisibly, the number of files removed.
#' @examples
#' \donttest{
#' # Non-interactive: skip the confirmation prompt.
#' morie_cache_clear("siu", confirm = FALSE)
#' }
#' @seealso \code{\link{morie_cache_dir}}
#' @export
morie_cache_clear <- function(subdir = NULL, confirm = interactive()) {
  path <- morie_cache_dir(subdir)
  if (!dir.exists(path)) {
    return(invisible(0L))
  }
  if (isTRUE(confirm)) {
    ans <- readline(sprintf("Delete %s ? [y/N] ", path))
    if (!tolower(trimws(ans)) %in% c("y", "yes")) {
      message("Aborted.")
      return(invisible(0L))
    }
  }
  n_files <- length(list.files(path, recursive = TRUE, full.names = TRUE))
  unlink(path, recursive = TRUE, force = TRUE)
  invisible(n_files)
}

#' Get path to the built-in MORIE datasets database
#'
#' Returns the path to \code{morie.db} that ships with the package
#' (\code{inst/extdata/morie.db}). This database contains all CPADS,
#' CCS, CSADS, CSUS, HealthInfobase, and CIHI datasets pre-loaded as
#' SQLite tables.
#'
#' @return File path string.
#' @examples
#' morie_builtin_db()
#' @export
morie_builtin_db <- function() {
  db <- system.file("extdata", "morie.db", package = "morie")
  if (nzchar(db)) {
    return(db)
  }
  # Source-checkout / dev fallback: the per-user cache copy.
  file.path(morie_cache_dir(), "morie.db")
}

#' Connect to the MORIE cache database
#'
#' Opens (or creates) the per-user cache database. The default backend
#' is **DuckDB** — zero-config like SQLite, but vectorised + columnar,
#' so it handles the multi-GB-scale open-data PUMFs (TPS, CPADS bulk)
#' that morie ingests without breaking down on analytical queries. For
#' back-compat, an existing SQLite cache at `morie.db` is reused; if
#' duckdb is unavailable, falls back to SQLite.
#'
#' For non-default backends (PostgreSQL, MariaDB, MS SQL Server, ...),
#' construct your own DBI connection and pass it as `con` to the
#' `morie_cache_*` and `morie_load_dataset` functions:
#'
#' \preformatted{
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'   host = "...", dbname = "morie", user = "...", password = "...")
#' morie_load_dataset("ocp21", con = con)
#' }
#'
#' @param db_path Optional path to a DuckDB (\code{*.duckdb}) or SQLite
#'   (\code{*.db}) file. Defaults to the \code{MORIE_CACHE_DB} env var,
#'   else \code{morie.duckdb} / \code{morie.db} in the per-user cache
#'   directory.
#' @return A DBI connection object.
#' @examples
#' \donttest{
#' # DuckDB (default when 'duckdb' is installed); pass a '.db' path for SQLite.
#' if (requireNamespace("duckdb", quietly = TRUE) &&
#'   requireNamespace("DBI", quietly = TRUE)) {
#'   tmp <- tempfile(fileext = ".duckdb")
#'   con <- morie_db_connect(db_path = tmp)
#'   DBI::dbListTables(con)
#'   DBI::dbDisconnect(con)
#'   file.remove(tmp)
#' }
#' }
#' @export
morie_db_connect <- function(db_path = NULL) {
  morie_ensure_extras("DBI")
  # CRAN Policy: by default never write under user HOME. When the
  # caller doesn't supply a path and the MORIE_CACHE_DB env var is
  # unset, default to a session-scoped subdirectory of tempdir(). R
  # cleans this up when the session ends. Users opt in to persistent
  # caching by passing `db_path = morie_cache_dir("morie.duckdb")`
  # explicitly (or by setting the MORIE_CACHE_DB env var).
  cache_dir <- file.path(tempdir(), "morie")
  duckdb_default <- file.path(cache_dir, "morie.duckdb")
  sqlite_default <- file.path(cache_dir, "morie.db")

  if (is.null(db_path)) {
    db_path <- Sys.getenv("MORIE_CACHE_DB", "")
    if (!nzchar(db_path)) {
      # Resolution: prefer an existing morie.duckdb; else reuse an
      # existing morie.db (back-compat with the SQLite era); else
      # create morie.duckdb if duckdb is available, otherwise morie.db.
      if (file.exists(duckdb_default)) {
        db_path <- duckdb_default
      } else if (file.exists(sqlite_default)) {
        db_path <- sqlite_default
      } else if (requireNamespace("duckdb", quietly = TRUE)) {
        db_path <- duckdb_default
      } else {
        db_path <- sqlite_default
      }
    }
  }
  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

  # Dispatch on extension: .duckdb -> DuckDB; anything else -> SQLite.
  is_duckdb <- grepl("\\.duckdb$", db_path, ignore.case = TRUE)
  if (is_duckdb) {
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("DuckDB path requested but the 'duckdb' package isn't installed.\n",
        "  install.packages('duckdb')  -- or pass db_path ending in '.db' ",
        "for SQLite.",
        call. = FALSE
      )
    }
    return(DBI::dbConnect(duckdb::duckdb(), dbdir = db_path))
  }
  # SQLite fallback path.
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("SQLite path requested but the 'RSQLite' package isn't installed.\n",
      "  install.packages('RSQLite')  -- or install 'duckdb' and pass a ",
      "'.duckdb' path.",
      call. = FALSE
    )
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode=WAL")
  con
}

#' Store a data frame in the MORIE cache
#'
#' Writes (or replaces) a table in the shared SQLite cache.
#'
#' @param data A data.frame to cache.
#' @param table_name Name of the destination table.
#' @param db_path Optional path to a SQLite file (default backend).
#' @param con Optional pre-opened DBI connection. When supplied, the
#'   table is written through `con` and `db_path` is ignored. Use this
#'   for non-SQLite backends (PostgreSQL, DuckDB, MariaDB).
#' @return Number of rows written (invisible).
#' @examples
#' \donttest{
#' db <- tempfile(fileext = ".db")
#' morie_cache_store(
#'   data = data.frame(x = rnorm(50), y = rnorm(50)),
#'   table_name = "demo",
#'   db_path = db
#' )
#' file.remove(db)
#' }
#' @export
morie_cache_store <- function(data, table_name, db_path = NULL, con = NULL) {
  h <- .morie_db_handle(con, db_path)
  if (h$close) on.exit(DBI::dbDisconnect(h$con), add = TRUE)
  DBI::dbWriteTable(h$con, table_name, data, overwrite = TRUE)
  # Auto-create the cardinality-driven indexes for known dataset
  # tables (SIU, OTIS a01/b01..d07, ARSAU uof_*, TPS crime-table
  # family). Unknown table_names are silently no-op. See
  # R/db_indexes.R for the per-dataset registry + cardinality rationale.
  morie_db_create_indexes(h$con, table_name)
  invisible(nrow(data))
}

#' Load a table from the MORIE cache
#'
#' @param table_name Name of the table.
#' @param db_path Optional path to a SQLite file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame, or \code{NULL} if the table does not exist.
#' @examples
#' \donttest{
#' db <- tempfile(fileext = ".db")
#' morie_cache_store(
#'   data = data.frame(x = 1:5),
#'   table_name = "demo",
#'   db_path = db
#' )
#' morie_cache_load(table_name = "demo", db_path = db)
#' file.remove(db)
#' }
#' @export
morie_cache_load <- function(table_name, db_path = NULL, con = NULL) {
  h <- .morie_db_handle(con, db_path)
  if (h$close) on.exit(DBI::dbDisconnect(h$con), add = TRUE)
  if (!DBI::dbExistsTable(h$con, table_name)) {
    return(NULL)
  }
  DBI::dbReadTable(h$con, table_name)
}

#' List all tables in the MORIE cache
#'
#' @param db_path Optional path to a SQLite file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame with columns \code{table} and \code{rows}.
#' @examples
#' \donttest{
#' db <- tempfile(fileext = ".db")
#' morie_cache_store(data.frame(x = 1:3), "demo", db_path = db)
#' morie_cache_list(db_path = db)
#' file.remove(db)
#' }
#' @export
morie_cache_list <- function(db_path = NULL, con = NULL) {
  h <- .morie_db_handle(con, db_path)
  if (h$close) on.exit(DBI::dbDisconnect(h$con), add = TRUE)
  tables <- DBI::dbListTables(h$con)
  if (length(tables) == 0L) {
    return(data.frame(table = character(), rows = integer()))
  }
  # Quote identifiers per the backend's own conventions so this works on
  # SQLite ([tbl]), PostgreSQL ("tbl"), MariaDB (`tbl`), DuckDB ("tbl"), ...
  # COUNT(*) returns integer on SQLite/PostgreSQL but double on DuckDB; cast
  # so the vapply FUN.VALUE matches across backends.
  counts <- vapply(tables, function(t) {
    q <- DBI::dbQuoteIdentifier(h$con, t)
    as.integer(DBI::dbGetQuery(h$con, sprintf("SELECT COUNT(*) AS n FROM %s", q))$n)
  }, integer(1))
  data.frame(table = tables, rows = counts, stringsAsFactors = FALSE)
}

#' Cache local RDS/CSV data into the SQLite database
#'
#' Reads a local file and writes it to the cache so that CI and Docker
#' environments (which may lack the original files) can still run tests.
#'
#' @param path Path to a CSV or RDS file.
#' @param table_name Name for the cached table.
#' @param db_path Optional path to a SQLite file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return Number of rows cached (invisible).
#' @examples
#' tdir <- tempfile("morie-cache-")
#' dir.create(tdir)
#' f <- file.path(tdir, "demo.csv")
#' write.csv(data.frame(x = 1:3, y = 4:6), f, row.names = FALSE)
#' morie_cache_file(f, "demo", db_path = file.path(tdir, "cache.db"))
#' @export
morie_cache_file <- function(path, table_name, db_path = NULL, con = NULL) {
  ext <- tolower(tools::file_ext(path))
  data <- if (ext == "rds") {
    readRDS(path)
  } else if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported format: ", ext, call. = FALSE)
  }
  morie_cache_store(data, table_name, db_path = db_path, con = con)
}

#' Load CPADS data: local files -> cache -> CKAN API
#'
#' Resolution order:
#' \enumerate{
#'   \item Local RDS/CSV files in standard project locations
#'   \item SQLite cache (\code{data/cache/morie.db})
#'   \item CKAN API fetch (requires internet)
#' }
#'
#' @param db_path Optional path to a SQLite/DuckDB file (default backend).
#' @param use_ckan Logical; if TRUE and data not found locally or in cache,
#'   attempt to fetch from the CKAN API.
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame with canonical CPADS columns.
#' @examples
#' \dontrun{
#' # Needs the CPADS PUMF (local file, cache, or a live CKAN fetch).
#' cpads <- morie_load_cpads(use_ckan = TRUE)
#' if (!is.null(cpads)) head(cpads)
#' }
#' @export
morie_load_cpads <- function(db_path = NULL, use_ckan = TRUE, con = NULL) {
  # 1. Local files.
  local_paths <- c(
    "data/datasets/oc/CPADS/2021-2022/cpads-2021-2022-pumf2.csv",
    "data/cache/cpads_pumf_wrangled.rds"
  )
  for (p in local_paths) {
    if (file.exists(p)) {
      message("Loading CPADS from local: ", p)
      ext <- tolower(tools::file_ext(p))
      data <- if (ext == "rds") readRDS(p) else utils::read.csv(p, stringsAsFactors = FALSE)
      tryCatch(
        morie_cache_store(data, "cpads_canonical", db_path = db_path, con = con),
        error = function(e) NULL
      )
      return(data)
    }
  }

  # 2. DBI cache (DuckDB by default; SQLite if older cache exists).
  cached <- morie_cache_load("cpads_canonical", db_path = db_path, con = con)
  if (!is.null(cached)) {
    message("Loading CPADS from cache (", nrow(cached), " rows)")
    return(cached)
  }

  # 3. CKAN API.
  if (use_ckan) {
    message("Fetching CPADS from CKAN API...")
    data <- morie_fetch_ckan("cpads", db_path = db_path, con = con)
    return(data)
  }

  stop("CPADS data not found locally, in cache, or via CKAN.", call. = FALSE)
}

#' Fetch data from the CKAN API and cache it
#'
#' @param dataset_key One of \code{"cpads"}, \code{"csads"}, \code{"csus"}.
#' @param limit Maximum records to fetch. The CKAN datastore caps a
#'   single request at 32000 rows, so larger resources are paged through
#'   with `offset`; the default reads the entire resource.
#' @param db_path Optional override for the database path.
#' @param resource_id Optional CKAN datastore resource id. When supplied
#'   (e.g. from \code{morie_dataset_catalog()$ckan_resource_id}) it is used
#'   directly, so any catalogued dataset can be fetched without a built-in
#'   database; \code{dataset_key} then only labels the cache table.
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame.
#' @examples
#' \dontrun{
#' # Requires network access. Fetches the first 5000 rows of the
#' # Canadian Postsecondary Alcohol and Drug Use Survey from the
#' # Government of Canada CKAN datastore:
#' cpads <- morie_fetch_ckan(dataset_key = "cpads", limit = 5000L)
#' nrow(cpads)
#' }
#' @export
morie_fetch_ckan <- function(dataset_key = "cpads", limit = Inf,
                             db_path = NULL, resource_id = NULL,
                             con = NULL) {
  ckan_base <- "https://open.canada.ca/data/en/api/3/action/datastore_search"

  resource_ids <- list(
    cpads = "d2639429-c304-45a6-90b3-770562f4d46d",
    csads = NULL,
    csus  = NULL
  )

  metadata_urls <- list(
    cpads = "https://open.canada.ca/data/api/action/package_show?id=736fa9b2-62e4-4e31-aea4-51869605b363",
    csads = "https://open.canada.ca/data/api/action/package_show?id=1f15ca45-8bfd-4f9c-9ec6-2c0c440e69c2",
    csus  = "https://open.canada.ca/data/api/action/package_show?id=65e2d45e-efc6-4c29-9a9b-db59bc96aa0e"
  )

  # A catalog-supplied resource id is used directly; otherwise fall back
  # to the survey-keyed lookup, then to package-metadata resolution.
  rid <- if (!is.null(resource_id) && nzchar(resource_id)) {
    resource_id
  } else {
    resource_ids[[dataset_key]]
  }
  if (is.null(rid) || !nzchar(rid)) {
    # Resolve from package metadata.
    meta_url <- metadata_urls[[dataset_key]]
    if (is.null(meta_url)) {
      stop("Unknown dataset / no CKAN resource id: ", dataset_key, call. = FALSE)
    }
    meta_raw <- readLines(url(meta_url), warn = FALSE)
    meta <- jsonlite::fromJSON(paste(meta_raw, collapse = ""))
    resources <- meta$result$resources
    csv_idx <- which(toupper(resources$format) == "CSV")
    rid <- if (length(csv_idx) > 0) resources$id[csv_idx[1]] else resources$id[1]
  }

  # CKAN datastore_search caps a single request at 32000 rows, so page
  # through with `offset` until the whole resource (or `limit`) is read.
  cap <- as.integer(min(limit, .Machine$integer.max))
  page <- min(cap, 32000L)
  message("Fetching from CKAN datastore: resource_id=", rid)
  pages <- list()
  fetched <- 0L
  total <- NA_real_
  repeat {
    api_url <- sprintf(
      "%s?resource_id=%s&limit=%d&offset=%d",
      ckan_base, rid, page, fetched
    )
    raw <- readLines(url(api_url), warn = FALSE)
    payload <- jsonlite::fromJSON(paste(raw, collapse = ""))
    recs <- payload$result$records
    if (is.null(recs) || NROW(recs) == 0L) break
    pages[[length(pages) + 1L]] <- recs
    fetched <- fetched + NROW(recs)
    if (is.na(total)) {
      total <- if (!is.null(payload$result$total)) {
        as.numeric(payload$result$total)
      } else {
        fetched
      }
    }
    if (fetched >= total || fetched >= cap) break
  }
  records <- if (length(pages) == 0L) {
    NULL
  } else if (length(pages) == 1L) {
    pages[[1L]]
  } else {
    do.call(rbind, pages)
  }

  if (is.null(records) || NROW(records) == 0L) {
    stop("CKAN returned 0 records for ", dataset_key, call. = FALSE)
  }

  # Drop CKAN internal column.
  records[["_id"]] <- NULL

  # Cache.
  table_name <- paste0(dataset_key, "_raw")
  tryCatch(
    morie_cache_store(records, table_name, db_path = db_path, con = con),
    error = function(e) {
      message("Warning: could not cache: ", conditionMessage(e))
    }
  )

  records
}


# ---------------------------------------------------------------------------
# Unified load interface
# ---------------------------------------------------------------------------

.fuzzy_match_key <- function(key) {
  catalog <- morie_dataset_catalog()
  key_lower <- tolower(gsub("-", "_", key))
  # Exact match on new short keys.
  idx <- which(catalog$key == key_lower)
  if (length(idx) == 1L) {
    return(catalog$key[idx])
  }
  # Backward-compat: resolve old long keys to new short keys.
  if (key_lower %in% names(.OLD_TO_SHORT)) {
    short <- .OLD_TO_SHORT[[key_lower]]
    idx <- which(catalog$key == short)
    if (length(idx) == 1L) {
      return(catalog$key[idx])
    }
  }
  # Substring match on keys.
  idx <- which(grepl(key_lower, catalog$key, fixed = TRUE))
  if (length(idx) >= 1L) {
    return(catalog$key[idx[1L]])
  }
  # Substring match on dataset names.
  idx <- which(grepl(key_lower, tolower(catalog$name), fixed = TRUE))
  if (length(idx) >= 1L) {
    return(catalog$key[idx[1L]])
  }
  NULL
}

#' Load a dataset by catalog key
#'
#' Resolution tiers, tried in order: built-in DB -> user cache -> local
#' file -> CKAN datastore -> direct download URL -> ArcGIS layer ->
#' error. Supports fuzzy matching: \code{morie_load_dataset("cpads_2021")}
#' resolves to \code{ocp21}.
#'
#' @param key Dataset catalog key (or fuzzy match).
#' @param db_path Optional path to a SQLite/DuckDB file (default backend).
#' @param refresh If \code{TRUE}, bypass the built-in database and the
#'   user cache (and, for remotely-backed datasets, the local file) and
#'   re-fetch from the remote source, overwriting the cached copy. Use
#'   this to pick up time-to-time updates to a dataset.
#' @param con Optional pre-opened DBI connection for the user cache
#'   (overrides `db_path`). The built-in DB read is always SQLite-based
#'   and is unaffected by `con`.
#' @return A data.frame.
#' @examples
#' \dontrun{
#' df <- morie_load_dataset("ocp21") # CPADS 2021-2022 (default DuckDB cache)
#' df <- morie_load_dataset("ocp21", refresh = TRUE) # force re-fetch
#'
#' # PostgreSQL cache (run a server first):
#' # con <- DBI::dbConnect(RPostgres::Postgres(),
#' #   host = "localhost", dbname = "morie", user = "...")
#' # df <- morie_load_dataset("ocp21", con = con)
#' }
#' @seealso \code{\link{morie_fetch}}, \code{\link{morie_ckan_search}}
#' @export
morie_load_dataset <- function(key, db_path = NULL, refresh = FALSE,
                               con = NULL) {
  matched <- .fuzzy_match_key(key)
  if (is.null(matched)) {
    stop("Unknown dataset key: '", key, "'. See morie_dataset_catalog().", call. = FALSE)
  }
  catalog <- morie_dataset_catalog()
  entry <- catalog[catalog$key == matched, ]
  has <- function(col) col %in% names(entry) && nzchar(entry[[col]])
  has_remote <- has("ckan_resource_id") || has("download_url") ||
    has("arcgis_url")

  if (!refresh) {
    # 1. Built-in database (ships with package).
    builtin_path <- tryCatch(morie_builtin_db(), error = function(e) NULL)
    if (!is.null(builtin_path) && requireNamespace("DBI", quietly = TRUE) &&
      requireNamespace("RSQLite", quietly = TRUE)) {
      bcon <- DBI::dbConnect(RSQLite::SQLite(), dbname = builtin_path)
      on.exit(DBI::dbDisconnect(bcon), add = TRUE)
      if (DBI::dbExistsTable(bcon, entry$table_name)) {
        data <- DBI::dbReadTable(bcon, entry$table_name)
        message(
          "Loaded ", matched, " from built-in DB (", nrow(data),
          " rows)"
        )
        return(data)
      }
    }

    # 2. User cache (DuckDB by default; SQLite if older cache exists).
    cached <- morie_cache_load(entry$table_name, db_path = db_path, con = con)
    if (!is.null(cached)) {
      message("Loaded ", matched, " from cache (", nrow(cached), " rows)")
      return(cached)
    }
  }

  # 3. Local file. Skipped on refresh when a remote source exists, so a
  #    refresh re-pulls from the authoritative remote rather than a stale
  #    on-disk copy; for local-only datasets the file remains the source.
  if (file.exists(entry$local_path) && !(refresh && has_remote)) {
    message("Ingesting ", matched, " from local: ", entry$local_path)
    ext <- tolower(tools::file_ext(entry$local_path))
    data <- if (ext == "csv") {
      utils::read.csv(entry$local_path, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl required")
      as.data.frame(readxl::read_excel(entry$local_path))
    } else if (ext == "rds") {
      readRDS(entry$local_path)
    } else {
      stop("Unsupported format: ", ext, call. = FALSE)
    }
    morie_cache_store(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 4. CKAN datastore -- resolved directly from the catalog resource id,
  #    matching the Python load_dataset() design (no built-in DB needed).
  if (has("ckan_resource_id")) {
    message("Fetching ", matched, " from the CKAN datastore ...")
    data <- morie_fetch_ckan(
      dataset_key = matched,
      resource_id = entry$ckan_resource_id,
      db_path = db_path,
      con = con
    )
    morie_cache_store(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 5. Direct download URL -- open-data files not exposed through the CKAN
  #    datastore (direct CSV/XLSX, or a file bundled inside a .zip archive).
  if (has("download_url")) {
    message("Downloading ", matched, " from ", entry$download_url, " ...")
    zm <- if ("zip_member" %in% names(entry)) entry$zip_member else ""
    is_zip <- grepl("\\.zip$", entry$download_url, ignore.case = TRUE)
    data <- morie_fetch(entry$download_url,
      format = if (is_zip) "zip" else "auto",
      zip_member = zm
    )
    morie_cache_store(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 6. ArcGIS FeatureServer / MapServer layer (e.g. TPS crime open data).
  if (has("arcgis_url")) {
    message("Querying ", matched, " from the ArcGIS layer ...")
    data <- morie_fetch_arcgis(entry$arcgis_url)
    morie_cache_store(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  stop("Dataset '", matched, "' not found locally, in cache, via CKAN, ",
    "via a direct download URL, or via an ArcGIS layer.\n",
    "Run: Rscript data-raw/ingest_datasets.R --only ", matched,
    call. = FALSE
  )
}

#' List all datasets with cache status
#'
#' @param db_path Optional path to a SQLite/DuckDB file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame with columns: key, name, source, survey, year, type,
#'   cached (logical), rows (integer or NA).
#' @examples
#' morie_list_datasets()
#' @export
morie_list_datasets <- function(db_path = NULL, con = NULL) {
  catalog <- morie_dataset_catalog()
  cached_tables <- tryCatch(
    {
      cl <- morie_cache_list(db_path = db_path, con = con)
      stats::setNames(cl$rows, cl$table)
    },
    error = function(e) stats::setNames(integer(0), character(0))
  )

  catalog$cached <- catalog$table_name %in% names(cached_tables)
  catalog$rows <- as.integer(cached_tables[catalog$table_name])
  catalog[, c("key", "name", "source", "survey", "year", "type", "cached", "rows")]
}

#' Get metadata for a single dataset
#'
#' @param key Dataset catalog key (or fuzzy match).
#' @return A named list with dataset metadata.
#' @examples
#' # Use a real catalog key (run `morie_dataset_catalog()$key` to list them):
#' info <- morie_dataset_info("ocp21")
#' info$source
#' info$year
#' # Fuzzy match works for partial / forgiving keys:
#' morie_dataset_info("cpads")$key
#' @export
morie_dataset_info <- function(key) {
  matched <- .fuzzy_match_key(key)
  if (is.null(matched)) stop("Unknown dataset key: '", key, "'", call. = FALSE)
  catalog <- morie_dataset_catalog()
  entry <- catalog[catalog$key == matched, ]
  as.list(entry)
}

#' Get path to an MORIE userguide
#'
#' Lists or retrieves bundled userguide PDF files. These are the official
#' PUMF codebooks and user guides from Health Canada / Statistics Canada.
#'
#' @param name Filename (e.g., \code{"20212022-cpads-pumf-user-guide.pdf"}).
#'   If \code{NULL}, lists all available userguides.
#' @return File path string, or character vector of filenames.
#' @examples
#' morie_userguide()
#' @export
morie_userguide <- function(name = NULL) {
  if (is.null(name)) {
    dir(system.file("extdata", "userguides", package = "morie"))
  } else {
    system.file("extdata", "userguides", name, package = "morie", mustWork = TRUE)
  }
}


#' Download bootstrap weight files from CKAN API
#'
#' Downloads large bootstrap weight CSVs that are too big to ship with the
#' package. Data is cached in the user cache database for future use.
#'
#' @param survey One of \code{"csads_2021"}, \code{"csads_2023"},
#'   \code{"csus_2019"}, \code{"csus_2023"}, or \code{"all"} (default).
#' @param limit Max records per CKAN request (default 32000).
#' @param db_path Optional path to a SQLite/DuckDB file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return Invisibly, the number of CSV files successfully downloaded.
#' @examples
#' \donttest{
#' # See the package vignettes for usage examples:
#' #   vignette(package = "morie")
#' }
#' @export
morie_download_bootstrap <- function(survey = "all", limit = 32000L,
                                     db_path = NULL, con = NULL) {
  # Current short catalog keys (see morie_dataset_catalog()); the older
  # oc_<survey>_<year>_bootstrap long keys are no longer in the catalog.
  bootstrap_keys <- list(
    csads_2021 = "ocs22bt",
    csads_2023 = "ocs24bt",
    csus_2019  = "cu20bt",
    csus_2023  = "cu23bt"
  )
  if (survey == "all") {
    targets <- unlist(bootstrap_keys, use.names = FALSE)
  } else {
    targets <- bootstrap_keys[[survey]]
    if (is.null(targets)) stop("Unknown survey: ", survey, call. = FALSE)
  }

  catalog <- morie_dataset_catalog()
  for (key in targets) {
    entry <- catalog[catalog$key == key, ]
    if (nrow(entry) == 0L) {
      message("Unknown key: ", key)
      next
    }

    # Try local file first.
    if (file.exists(entry$local_path)) {
      message("Ingesting ", key, " from local file: ", entry$local_path)
      morie_cache_file(entry$local_path, entry$table_name, db_path = db_path, con = con)
      message("  OK: cached ", entry$table_name)
      next
    }

    # Try CKAN.
    if (nzchar(entry$ckan_resource_id)) {
      message("Downloading ", key, " from CKAN (limit=", limit, ")...")
      tryCatch(
        {
          data <- morie_fetch_ckan(entry$survey,
            limit = limit,
            db_path = db_path, con = con
          )
          morie_cache_store(data, entry$table_name, db_path = db_path, con = con)
          message("  OK: ", nrow(data), " rows cached as ", entry$table_name)
        },
        error = function(e) {
          message("  ERROR: ", conditionMessage(e))
        }
      )
    } else {
      message("  ", key, ": no CKAN resource ID. Download CSV manually to ", entry$local_path)
    }
  }
  invisible(NULL)
}
