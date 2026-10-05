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

# Internal: SQL backend when DBI and a driver are installed, else the file
# backend with a message. The cache never fails for want of an optional
# package; DBI stays the opt-in path for SQL and server backends.
.morie_dbi_available <- function() {
  requireNamespace("DBI", quietly = TRUE) &&
    (requireNamespace("RSQLite", quietly = TRUE) || requireNamespace("duckdb", quietly = TRUE))
}

.morie_cache_note <- new.env(parent = emptyenv())

.morie_sql_or_fallback <- function(db_path = NULL) {
  if (.morie_dbi_available()) {
    con <- if (is.null(db_path)) morie_db_connect() else morie_db_connect(db_path)
    return(list(type = "dbi", con = con, close = TRUE))
  }
  # the file backend is a working default cache: say so only when a SQL file was asked for by name,
  # since that request is not honoured as asked
  dir <- .morie_cache_fs_dir()
  if (!is.null(db_path)) {
    # the cache the caller named stays theirs: its files sit beside db_path, so two
    # databases never share (or read) each other's tables
    dir <- paste0(tools::file_path_sans_ext(db_path), "_files")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    .morie_cache_note$shown <- TRUE
    message("cache: DBI with RSQLite or duckdb is not installed; using the file backend in ", dir,
            " (morie_install_extras(c('DBI', 'RSQLite')) enables SQL caches)")
  }
  if (requireNamespace("nanoparquet", quietly = TRUE)) {
    return(list(type = "parquet", dir = dir, close = FALSE))
  }
  list(type = "rds", dir = dir, close = FALSE)
}

# Internal: resolve a DBI connection. Accepts a pre-opened connection
# (used as-is, caller owns disconnection) OR a SQLite path string (we
# open + own + close). The default path is the per-user cache.
#
# Returns: list(con = DBIConnection, close = logical).
#' Internal helper: Morie Db Handle
#' @noRd
.morie_db_handle <- function(con = NULL, db_path = NULL) {
  if (!is.null(con)) {
    if (!inherits(con, "DBIConnection")) {
      stop("`con` must be a DBIConnection (see `?DBI::dbConnect`).",
        call. = FALSE
      )
    }
    return(list(type = "dbi", con = con, close = FALSE))
  }
  # Explicit db_path -> the user wants a SQL (SQLite/DuckDB) file backend.
  if (!is.null(db_path) && nzchar(db_path)) {
    return(.morie_sql_or_fallback(db_path))
  }
  # Explicit backend choice via env: rds | parquet | duckdb | sqlite.
  be <- Sys.getenv("MORIE_CACHE_BACKEND", "")
  if (be == "rds") {
    return(list(type = "rds", dir = .morie_cache_fs_dir(), close = FALSE))
  }
  if (be == "parquet") {
    return(list(type = "parquet", dir = .morie_cache_fs_dir(), close = FALSE))
  }
  if (be %in% c("duckdb", "sqlite")) {
    return(.morie_sql_or_fallback())
  }
  # Back-compat: honour MORIE_CACHE_DB or an existing cache DB file -> SQL.
  cache_dir <- file.path(tempdir(), "morie")
  if (nzchar(Sys.getenv("MORIE_CACHE_DB", "")) ||
    file.exists(file.path(cache_dir, "morie.duckdb")) ||
    file.exists(file.path(cache_dir, "morie.db"))) {
    return(.morie_sql_or_fallback())
  }
  # Default: zero/light-compile file backend. Parquet (cross-language) when
  # nanoparquet is available; else base-R RDS. DuckDB/SQLite stay opt-in (see
  # morie_db_connect); PostgreSQL etc. via `con=`.
  if (requireNamespace("nanoparquet", quietly = TRUE)) {
    return(list(type = "parquet", dir = .morie_cache_fs_dir(), close = FALSE))
  }
  list(type = "rds", dir = .morie_cache_fs_dir(), close = FALSE)
}

# ---- Filesystem cache backends (no compiled DB dependency) -------------------
# One file per table under a session-scoped cache dir. Parquet (via nanoparquet,
# cross-language: Python/DuckDB/Arrow/Rust can read it) is the default when
# available; RDS (base R) is the zero-dependency fallback. This is what lets
# morie_cache_* work on a fresh install with no DuckDB/RSQLite. For SQL /
# out-of-core queries, install duckdb (see morie_db_connect); for the multi-user
# server tier, pass a PostgreSQL `con=`.
#' Internal helper: Morie Cache Fs Dir
#' @noRd
.morie_cache_fs_dir <- function() {
  # the per-user cache (MORIE_CACHE_DIR overrides): under tempdir() every session re-downloaded
  # the 39 MB PUMF that `pull` had just said was cached
  d <- morie_cache_dir("fscache")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}
#' Internal helper: Morie Cache Fs Path
#' @noRd
.morie_cache_fs_path <- function(dir, table_name, ext) {
  if (!is.character(table_name) || length(table_name) != 1L ||
    grepl("[/\\\\]|\\.\\.", table_name)) {
    stop("Invalid table_name for the file cache: ", table_name, call. = FALSE)
  }
  file.path(dir, paste0(table_name, ".", ext))
}
# Crash-safe write: serialise to a temp file, then atomic rename over target,
# so a crash mid-write cannot corrupt an existing cached table.
#' Internal helper: Morie Atomic Write
#' @noRd
.morie_atomic_write <- function(path, writer) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  res <- tryCatch(suppressWarnings(writer(tmp)), error = function(e) e)
  # an unwritable directory: say which, instead of the writer's "cannot open the connection"
  if (inherits(res, "error") || !file.exists(tmp)) {
    if (file.access(dirname(path), 2L) != 0L) {
      stop(sprintf("cannot write %s (the directory is not writable)", dirname(path)), call. = FALSE)
    }
    if (inherits(res, "error")) stop(res)
    stop(sprintf("cannot write %s", path), call. = FALSE)
  }
  if (!suppressWarnings(file.rename(tmp, path)) && !file.copy(tmp, path, overwrite = TRUE)) {
    stop(sprintf("cannot write %s", path), call. = FALSE)
  }
  invisible(path)
}

# The cache a loader fills on the way is optional: a read-only or full home directory (HPC,
# containers) must not stop the data from reaching the caller or `pull --out`. Say so once and
# return the data; morie_cache_store() itself still fails when called on purpose.
#' Internal helper: best-effort cache store
#' @noRd
.morie_cache_store_soft <- function(data, table_name, db_path = NULL, con = NULL) {
  tryCatch(
    withCallingHandlers(
      morie_cache_store(data, table_name, db_path = db_path, con = con),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) {
      dir <- if (is.null(db_path) && is.null(con)) morie_cache_dir("fscache") else (db_path %||% "the database")
      message(sprintf("cache skipped for %s (%s is not writable); the data are returned uncached",
                      table_name, dir))
      invisible(0L)
    }
  )
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
#' @seealso \code{\link{morie_cache_clear}}
#' @examples
#' \dontshow{if (morie_has("sql")) withAutoprint(\{ # examplesIf}
#' # Persistent cache root (does not write anything to disk):
#' morie_cache_dir()
#' # Per-subsystem persistent path:
#' morie_cache_dir("siu")
#' \dontshow{\}) # examplesIf}
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
#' @seealso \code{\link{morie_cache_dir}}
#' @examples
#' \donttest{
#' if (morie_has("sql")) withAutoprint({
#' # Non-interactive: skip the confirmation prompt.
#' morie_cache_clear("siu", confirm = FALSE)
#' })
#' }
#' @export
morie_cache_clear <- function(subdir = NULL, confirm = interactive()) {
  if (!is.null(subdir) && (!is.character(subdir) || length(subdir) != 1L || grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)|^([A-Za-z]:)?[/\\\\]|^~", subdir))) {
    stop("`subdir` must be a folder inside the cache (no \"..\", no absolute path)", call. = FALSE)
  }
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
  .morie_unlink_owned(path)
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
#' \dontshow{if (morie_has("sql")) withAutoprint(\{ # examplesIf}
#' morie_builtin_db()
#' \dontshow{\}) # examplesIf}
#' @export
morie_builtin_db <- function() {
  db <- .rmorie_extdata("morie.db")
  if (nzchar(db)) {
    return(db)
  }
  # Source-checkout / dev fallback: the per-user cache copy.
  file.path(morie_cache_dir(), "morie.db")
}

#' Connect to the MORIE cache database
#'
#' Opens (or creates) a per-user **SQL** cache database. This is the
#' opt-in SQL backend and requires DuckDB or RSQLite to be installed.
#'
#' Note: the DEFAULT cache used by `morie_cache_store()` / `_load()` /
#' `_list()` needs **no SQL backend at all** -- it uses a zero/light
#' dependency file store: Parquet via \pkg{nanoparquet} (cross-language)
#' when available, else base-R `.rds`. Install `duckdb` (or `RSQLite`),
#' or set `MORIE_CACHE_BACKEND=duckdb`/`sqlite`, or pass `db_path=`, to
#' use SQL instead -- DuckDB is preferred (vectorised + columnar, handles
#' multi-GB PUMFs and out-of-core analytical queries); an existing
#' `morie.db` / `morie.duckdb` cache is reused for back-compat. For the
#' multi-user server tier, pass your own PostgreSQL `con=`.
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
#' if (morie_has("sql")) withAutoprint({
#' # DuckDB (default when 'duckdb' is installed); pass a '.db' path for SQLite.
#' if (requireNamespace("duckdb", quietly = TRUE) &&
#'   requireNamespace("DBI", quietly = TRUE)) {
#'   tmp <- tempfile(fileext = ".duckdb")
#'   con <- morie_db_connect(db_path = tmp)
#'   DBI::dbListTables(con)
#'   DBI::dbDisconnect(con)
#'   file.remove(tmp)
#' }
#' })
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
    # duckdb prints an 8-line note about its extension directory on first connect; a listing verb is not the place
    return(suppressMessages(DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)))
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
#' set.seed(1)
#' \donttest{
#' if (morie_has("sql")) withAutoprint({
#' db <- tempfile(fileext = ".db")
#' morie_cache_store(
#'   data = data.frame(x = rnorm(50), y = rnorm(50)),
#'   table_name = "demo",
#'   db_path = db
#' )
#' file.remove(db)
#' })
#' }
#' @export
morie_cache_store <- function(data, table_name, db_path = NULL, con = NULL) {
  h <- .morie_db_handle(con, db_path)
  if (h$type == "parquet") {
    p <- .morie_cache_fs_path(h$dir, table_name, "parquet")
    .morie_atomic_write(p, function(f) {
      nanoparquet::write_parquet(as.data.frame(data), f)
    })
    return(invisible(nrow(data)))
  }
  if (h$type == "rds") {
    p <- .morie_cache_fs_path(h$dir, table_name, "rds")
    .morie_atomic_write(p, function(f) saveRDS(as.data.frame(data), f))
    .morie_cache_rows_note(p, nrow(data))
    return(invisible(nrow(data)))
  }
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
#' if (morie_has("sql")) withAutoprint({
#' db <- tempfile(fileext = ".db")
#' morie_cache_store(
#'   data = data.frame(x = 1:5),
#'   table_name = "demo",
#'   db_path = db
#' )
#' morie_cache_load(table_name = "demo", db_path = db)
#' file.remove(db)
#' })
#' }
#' @export
morie_cache_load <- function(table_name, db_path = NULL, con = NULL) {
  h <- .morie_db_handle(con, db_path)
  if (h$type == "parquet") {
    p <- .morie_cache_fs_path(h$dir, table_name, "parquet")
    if (!file.exists(p)) {
      return(NULL)
    }
    return(as.data.frame(nanoparquet::read_parquet(p)))
  }
  if (h$type == "rds") {
    p <- .morie_cache_fs_path(h$dir, table_name, "rds")
    if (!file.exists(p)) {
      return(NULL)
    }
    return(readRDS(p))
  }
  if (h$close) on.exit(DBI::dbDisconnect(h$con), add = TRUE)
  if (!DBI::dbExistsTable(h$con, table_name)) {
    return(NULL)
  }
  DBI::dbReadTable(h$con, table_name)
}

# Row count of a cached RDS table without reading it: a "<file>.n" note written when the
# table is stored, trusted while it is not older than the table. Reading every table to
# count it took minutes once the catalogue was cached.
.morie_cache_rows_note <- function(f, n) {
  try(writeLines(as.character(as.integer(n)), paste0(f, ".n")), silent = TRUE)
  invisible(n)
}

.morie_cache_rows <- function(f) {
  side <- paste0(f, ".n")
  if (file.exists(side) && isTRUE(file.mtime(side) >= file.mtime(f))) {
    n <- suppressWarnings(as.integer(readLines(side, n = 1L, warn = FALSE)))
    if (length(n) == 1L && !is.na(n)) return(n)
  }
  .morie_cache_rows_note(f, as.integer(nrow(readRDS(f))))
}

#' List all tables in the MORIE cache
#'
#' @param db_path Optional path to a SQLite file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame with columns \code{table} and \code{rows}.
#' @examples
#' \donttest{
#' if (morie_has("sql")) withAutoprint({
#' db <- tempfile(fileext = ".db")
#' morie_cache_store(data.frame(x = 1:3), "demo", db_path = db)
#' morie_cache_list(db_path = db)
#' file.remove(db)
#' })
#' }
#' @export
morie_cache_list <- function(db_path = NULL, con = NULL) {
  h <- .morie_db_handle(con, db_path)
  if (h$type %in% c("parquet", "rds")) {
    ext <- if (h$type == "parquet") "parquet" else "rds"
    files <- list.files(h$dir,
      pattern = paste0("\\.", ext, "$"),
      full.names = TRUE
    )
    if (length(files) == 0L) {
      return(data.frame(
        table = character(), rows = integer(),
        stringsAsFactors = FALSE
      ))
    }
    rows <- vapply(files, function(f) {
      if (ext == "parquet") {
        n <- tryCatch(as.integer(nanoparquet::read_parquet_info(f)$num_rows),
          error = function(e) NA_integer_
        )
        if (is.na(n)) as.integer(nrow(nanoparquet::read_parquet(f))) else n
      } else {
        .morie_cache_rows(f)
      }
    }, integer(1))
    return(data.frame(
      table = sub(paste0("\\.", ext, "$"), "", basename(files)),
      rows = unname(rows), stringsAsFactors = FALSE
    ))
  }
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
#' \dontshow{if (morie_has("sql")) withAutoprint(\{ # examplesIf}
#' # The SQLite backend needs the optional 'RSQLite' package.
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   tdir <- tempfile("morie-cache-")
#'   dir.create(tdir)
#'   f <- file.path(tdir, "demo.csv")
#'   write.csv(data.frame(x = 1:3, y = 4:6), f, row.names = FALSE)
#'   morie_cache_file(f, "demo", db_path = file.path(tdir, "cache.db"))
#' }
#' \dontshow{\}) # examplesIf}
#' @export
morie_cache_file <- function(path, table_name, db_path = NULL, con = NULL) {
  ext <- tolower(tools::file_ext(path))
  data <- if (ext == "rds") {
    .morie_safe_readRDS(path, "importing an .rds cache file")
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
#' \dontshow{if (morie_has("httr2", "jsonlite")) withAutoprint(\{ # examplesIf}
#' # Local-first and offline: use_ckan = FALSE consults the bundled copy
#' # and the local cache only, and errors when neither is present.
#' cpads <- try(morie_load_cpads(use_ckan = FALSE), silent = TRUE)
#' if (!inherits(cpads, "try-error")) head(cpads)
#' \dontrun{
#' # The live CKAN fetch pages through the datastore; it ran for over ten
#' # minutes in the docs build, so it is shown rather than executed.
#' cpads <- morie_load_cpads(use_ckan = TRUE)
#' if (!is.null(cpads)) head(cpads)
#' }
#' \dontshow{\}) # examplesIf}
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
#' @param portal Optional CKAN portal base URL (e.g. \code{"https://data.ontario.ca"}); the
#'   default is open.canada.ca. The catalog gives it for resources on another portal.
#' @return A data.frame.
#' @examples
#' \donttest{
#' # Requires network access. Fetches the first 200 rows of the
#' # Canadian Postsecondary Alcohol and Drug Use Survey from the
#' # Government of Canada CKAN datastore:
#' cpads <- morie_fetch_ckan(dataset_key = "cpads", limit = 200L)
#' nrow(cpads)
#' }
#' @export
morie_fetch_ckan <- function(dataset_key = "cpads", limit = Inf,
                             db_path = NULL, resource_id = NULL,
                             con = NULL, portal = NULL) {
  # the catalog names the portal a resource lives on (the OTIS tables are on data.ontario.ca;
  # asked on open.canada.ca they 404ed)
  ckan_base <- if (!is.null(portal) && nzchar(portal)) {
    paste0(sub("/+$", "", portal), "/api/3/action/datastore_search")
  } else {
    getOption("morie.ckan_base", "https://open.canada.ca/data/en/api/3/action/datastore_search")
  }

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
    meta <- .morie_from_json(paste(meta_raw, collapse = ""))
    resources <- meta$result$resources
    csv_idx <- which(toupper(resources$format) == "CSV")
    rid <- if (length(csv_idx) > 0) resources$id[csv_idx[1]] else resources$id[1]
  }

  # CKAN datastore_search caps a single request at 32000 rows, so page
  # through with `offset` until the whole resource (or `limit`) is read.
  cap <- as.integer(min(limit, .Machine$integer.max))
  page <- min(cap, 32000L)
  message("Fetching ", dataset_key, " from CKAN (resource ", rid, ")")
  # a 40 MB PUMF page from open.canada.ca takes longer than R's 60 s default
  # 120 s per page: a slow datastore falls through to the resource file (39 MB
  # in seconds) instead of waiting ten minutes a page, as the Python arm does
  old_timeout <- options(timeout = 120)
  on.exit(options(old_timeout), add = TRUE)
  # The resource file first: one download with a live progress bar (the 39 MB
  # CPADS zip in seconds) instead of paging 32,000-record JSON pages through
  # R's parser (minutes a page). The datastore API is the route for a row
  # limit, and the fallback when the resource has no file URL.
  records <- NULL
  if (!is.finite(limit)) {
    records <- tryCatch(.morie_ckan_resource_file(rid, dataset_key, ckan_base),
      error = function(e) {
        message("Resource file route failed (", conditionMessage(e), "); paging the datastore instead")
        NULL
      })
    if (!is.null(records) && NROW(records) == 0L) records <- NULL
  }
  if (is.null(records)) {
    pages <- list()
    fetched <- 0L
    total <- NA_real_
    fallback <- FALSE
    show <- !.morie_dl_quiet()
    tty <- isatty(stderr())
    t0 <- proc.time()[["elapsed"]]
    repeat {
      api_url <- sprintf(
        "%s?resource_id=%s&limit=%d&offset=%d",
        ckan_base, rid, page, fetched
      )
      # status-aware: a server that answered (a datastore query it refuses, HTTP 4xx/5xx) is not an
      # unreachable one, and the message says which
      resp <- tryCatch(.morie_http_get_with_status(api_url, timeout_s = 120L), error = function(e) e)
      why <- if (inherits(resp, "error")) {
        sprintf("CKAN datastore unreachable (%s)", conditionMessage(resp))
      } else if (!identical(as.integer(resp$status_code), 200L)) {
        sprintf("the CKAN datastore answered HTTP %d to the query (limit=%d, offset=%d)",
                as.integer(resp$status_code), page, fetched)
      }
      if (!is.null(why)) {
        if (fetched > 0L) stop(why, " after ", fetched, " rows", call. = FALSE)
        # the resource file is the route then, live or from its Wayback Machine snapshot (the
        # Python arm does the same)
        message(why, "; reading the resource file instead")
        fallback <- TRUE
        break
      }
      raw <- resp$body
      payload <- .morie_from_json(paste(raw, collapse = ""))
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
      if (show) {
        line <- .morie_dl_line(paste0(dataset_key, " (CKAN datastore)"), fetched, total, t0, 0L, unit = "rows")
        if (tty) cat("\r", line, sep = "", file = stderr()) else cat("  ", line, "\n", sep = "", file = stderr())
      }
      if (fetched >= total || fetched >= cap) break
    }
    if (show && tty && fetched > 0L) cat("\n", file = stderr())
    records <- if (length(pages) == 0L) {
      NULL
    } else if (length(pages) == 1L) {
      pages[[1L]]
    } else {
      do.call(rbind, pages)
    }

    if (fallback || is.null(records) || NROW(records) == 0L) {
      records <- .morie_ckan_resource_file(rid, dataset_key, ckan_base)
    }
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

#' Internal helper: Fuzzy Match Key
#' @noRd
.fuzzy_match_key <- function(key) {
  catalog <- morie_dataset_catalog()
  # Exact match on the key as written (the NAPS keys carry hyphens), then with - read as _.
  idx <- which(catalog$key == tolower(key))
  if (length(idx) == 1L) {
    return(catalog$key[idx])
  }
  key_lower <- tolower(gsub("-", "_", key))
  idx <- which(catalog$key == key_lower)
  if (length(idx) == 1L) {
    return(catalog$key[idx])
  }
  key_hyphen <- tolower(gsub("_", "-", key))  # naps_co_on_2023 -> naps-co-on-2023
  idx <- which(catalog$key == key_hyphen)
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
#' @seealso \code{\link{morie_fetch}}, \code{\link{morie_ckan_search}}
#' @examples
#' \donttest{
#' if (morie_has("sql")) withAutoprint({
#' # CPADS 2021-2022: downloaded once from open.canada.ca (39 MB), then a local
#' # read; try() so a missing optional backend does not fail the check
#' df <- try(morie_load_dataset("ocp21"))
#' # re-fetch from the portal to pick up an upstream revision (network):
#' # df <- morie_load_dataset("ocp21", refresh = TRUE)
#'
#' # PostgreSQL cache (run a server first):
#' # con <- DBI::dbConnect(RPostgres::Postgres(),
#' #   host = "localhost", dbname = "morie", user = "...")
#' # df <- morie_load_dataset("ocp21", con = con)
#' })
#' }
#' @export
morie_load_dataset <- function(key, db_path = NULL, refresh = FALSE,
                               con = NULL) {
  # a workbook streamed to CSV reports progress under the dataset key, not a temp file name
  op <- options(morie.xlsx.label = key)
  on.exit(options(op), add = TRUE)
  df <- .morie_load_dataset_raw(key, db_path = db_path, refresh = refresh, con = con)
  # the published SIU corpus holds page text cut at the wrong place in this column: the few
  # real categories (morie.data._post_load does the same), missing kept missing
  if (identical(tolower(key), "siu") && is.data.frame(df) && "sex_gender_affected" %in% names(df)) {
    v <- df$sex_gender_affected
    miss <- is.na(v) | !nzchar(trimws(as.character(v)))
    df$sex_gender_affected <- ifelse(miss, NA_character_, .siu_an_sex(v))
  }
  df
}

.morie_load_dataset_raw <- function(key, db_path = NULL, refresh = FALSE,
                                    con = NULL) {
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop("key must be a single dataset key (rmorie list-datasets / morie_list_datasets())", call. = FALSE)
  }
  # the bootstrap-weight files are 600 MB: R's 60 s default timeout truncated
  # them mid-download (2026-10-01); every route below inherits this
  old_timeout <- options(timeout = max(getOption("timeout", 60), 3600))
  on.exit(options(old_timeout), add = TRUE)
  matched <- .fuzzy_match_key(key)
  if (is.null(matched)) {
    # A curated table at data.rmorie.com (db/table), opened by the MORIE key.
    if (.morie_data_is_key(key)) {
      return(morie_load_hosted_dataset(key, db_path = db_path, refresh = refresh))
    }
    # Unified OPEN-data front door: if `key` is a included data slug (open data
    # shipped in rmoriedata), load it from there so newcomers have one reliable
    # entry point.
    if (requireNamespace("rmoriedata", quietly = TRUE)) {
      slugs <- tryCatch(rmoriedata::morie_data_catalog()$slug,
        error = function(e) character()
      )
      if (key %in% slugs) {
        return(as.data.frame(rmoriedata::morie_data_load(key)))
      }
    }
    stop(
      "Unknown dataset '", key, "'. Open-data access points:\n",
      "  - morie_dataset_catalog()           remote/CKAN dataset KEYS (e.g. 'ocp21')\n",
      "  - rmoriedata::morie_data_catalog()  bundled data SLUGS (e.g. 'chicago_iucr_codes')\n",
      "  - morie_datasets_*()                dedicated fetchers ",
      "(e.g. morie_datasets_chicago_iucr_codes())\n",
      "  - morie_hosted_datasets()           curated db/table keys at data.rmorie.com (after rmorie login, GitHub or --email)",
      call. = FALSE
    )
  }
  catalog <- morie_dataset_catalog()
  entry <- catalog[catalog$key == matched, ]
  has <- function(col) col %in% names(entry) && nzchar(entry[[col]])
  has_remote <- has("ckan_resource_id") || has("download_url") ||
    has("arcgis_url") || has("fetcher") || has("rmoriedata")

  if (!refresh) {
    # 1. Built-in database (ships with package).
    builtin_path <- tryCatch(morie_builtin_db(), error = function(e) NULL)
    if (!is.null(builtin_path) && !file.exists(builtin_path)) {
      builtin_path <- NULL # dev fallback path may not exist; skip tier 1
    }
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
  local_file <- .morie_own_file_path(entry$local_path)
  if (!is.null(local_file) && !(refresh && has_remote)) {
    message("Ingesting ", matched, " from local: ", local_file)
    ext <- tolower(tools::file_ext(local_file))
    data <- if (ext == "csv") {
      utils::read.csv(local_file, stringsAsFactors = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      .morie_xlsx_data_sheet(local_file)
    } else if (ext == "rds") {
      readRDS(local_file)
    } else {
      stop("Unsupported format: ", ext, call. = FALSE)
    }
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 3b. Research files that are not tables (R environments) kept at
  #     data.rmorie.com: fetched into the data directory and opened here.
  if (has("hosted_file")) {
    dest <- file.path(.morie_data_root(), entry$local_path)
    if (!file.exists(dest)) {
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      .morie_data_get(paste0("/files/", entry$hosted_file), dest)
    }
    ext <- tolower(tools::file_ext(dest))
    if (ext == "rds") return(readRDS(dest))
    e <- new.env(parent = emptyenv())
    # objects saved from the producing session (a Quarto render hook, mlr3 learners) name packages
    # this session may not have; R rebinds them to .GlobalEnv and says so once per object
    withCallingHandlers(load(dest, envir = e), warning = function(w) {
      if (grepl("is not available and has been replaced", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
    })
    attr(e, "morie_path") <- dest
    message("Loaded ", matched, " as an environment with ", length(ls(e)), " objects (", dest, ")")
    return(e)
  }

  # 3c. The data.rmorie.com copy of a table whose portal file is absent or
  #     fails to download (Health Infobase tables, ...).
  hosted_copy <- function(why) {
    if (is.null(.morie_llm_hosted_key())) {
      stop(matched, ": ", why, "; the data.rmorie.com copy (", entry$hosted_key,
           ") opens with your MORIE key: run `rmorie login` (GitHub) or `rmorie login --email you@example.com` once.", call. = FALSE)
    }
    data <- morie_load_hosted_dataset(entry$hosted_key, db_path = db_path, refresh = refresh)
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    data
  }

  # 4. CKAN datastore -- resolved directly from the catalog resource id,
  #    matching the Python load_dataset() design (no built-in DB needed).
  if (has("ckan_resource_id")) {
    message("Fetching ", matched, " from CKAN ...")
    data <- morie_fetch_ckan(
      dataset_key = matched,
      resource_id = entry$ckan_resource_id,
      db_path = db_path,
      con = con,
      portal = if (has("ckan_portal")) entry$ckan_portal else NULL
    )
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 4a. A table shipped by rmoriedata on CRAN (the reviewed SIU corpus and its manifest).
  if (has("rmoriedata")) {
    if (!requireNamespace("rmoriedata", quietly = TRUE)) {
      stop(matched, " ships in the rmoriedata package: install.packages(\"rmoriedata\")", call. = FALSE)
    }
    data <- as.data.frame(rmoriedata::morie_data_load(entry$rmoriedata))
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 4b. A fetcher in this package (the NAPS hourly files), with its catalogued arguments.
  if (has("fetcher")) {
    message("Fetching ", matched, " via ", entry$fetcher, "() ...")
    fetcher <- get(entry$fetcher, envir = asNamespace(utils::packageName()))
    data <- do.call(fetcher, .morie_parse_fetcher_args(if (has("fetcher_args")) entry$fetcher_args else ""))
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  # 5. Direct download URL -- open-data files not exposed through the CKAN
  #    datastore (direct CSV/XLSX, or a file included inside a .zip archive).
  if (has("download_url")) {
    # .xlsx is read by the package's own reader; only the binary .xls format (BIFF) needs readxl,
    # and that is said before the download, not after it
    if (grepl("\\.xls($|\\?)", entry$download_url, ignore.case = TRUE) && !requireNamespace("readxl", quietly = TRUE)) {
      stop(matched, " is a binary .xls workbook: install.packages(\"readxl\") to read it", call. = FALSE)
    }
    message("Downloading ", matched, " from ", entry$download_url, " ...")
    zm <- if ("zip_member" %in% names(entry)) entry$zip_member else ""
    is_zip <- grepl("\\.zip$", entry$download_url, ignore.case = TRUE)
    data <- tryCatch(
      morie_fetch(entry$download_url, format = if (is_zip) "zip" else "auto", zip_member = zm),
      error = function(e) e
    )
    if (inherits(data, "error")) {
      if (has("hosted_key")) {
        message("Portal download failed (", conditionMessage(data), "); using the data.rmorie.com copy")
        return(hosted_copy(paste0("the portal download failed (", conditionMessage(data), ")")))
      }
      stop(data)
    }
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }
  if (has("hosted_key")) {
    return(hosted_copy("no portal file is catalogued"))
  }

  # 6. ArcGIS FeatureServer / MapServer layer (e.g. TPS crime open data).
  if (has("arcgis_url")) {
    message("Querying ", matched, " from the ArcGIS layer ...")
    data <- morie_fetch_arcgis(entry$arcgis_url, label = matched)
    .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
    return(data)
  }

  where <- .morie_own_file_target(entry$local_path)
  if (identical(matched, "mapq")) {
    # participant-level MAPQ data are not distributed: a synthetic toy panel stands in
    message(matched, ": your file is not at ", where, "; returning the synthetic toy panel (n = 400, ",
            "planted structure) so the analyses run. Its numbers demonstrate the pipeline, they are not findings.")
    return(.morie_mapq_synth_panel())
  }
  stop(matched, " is one of your own research files, not found at ", where,
       ": place it there (set MORIE_DATA_DIR to use another data directory)", call. = FALSE)
}

# Where an own-file dataset belongs, when the file is not there yet; NULL otherwise
# (also NULL for every key that downloads).
.morie_own_file_missing <- function(key) {
  cat_df <- morie_dataset_catalog()
  e <- cat_df[cat_df$key == key, , drop = FALSE]
  if (!nrow(e) || !startsWith(.cli_dataset_route(e[1L, ]), "own file")) return(NULL)
  lp <- e$local_path[1L]
  if (!is.null(.morie_own_file_path(lp))) return(NULL)
  .morie_own_file_target(lp)
}

.morie_own_file_target <- function(local_path) {
  if (grepl("^(/|[A-Za-z]:[/\\\\]|~)", local_path)) path.expand(local_path) else
    file.path(.morie_data_root(), sub("^data/", "", local_path))
}

# Your own research file: the path as catalogued (relative to the working directory),
# then under the data directory (MORIE_DATA_DIR, or the per-user data directory).
.morie_own_file_path <- function(local_path) {
  if (!is.character(local_path) || !nzchar(local_path)) return(NULL)
  for (p in c(local_path, file.path(.morie_data_root(), sub("^data/", "", local_path)))) {
    if (file.exists(p)) return(p)
  }
  NULL
}

#' List all datasets with cache status
#'
#' @param db_path Optional path to a SQLite/DuckDB file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @return A data.frame with columns: key, name, source, survey, year, type,
#'   cached (logical), rows (integer or NA).
#' @examples
#' \dontshow{if (morie_has("sql")) withAutoprint(\{ # examplesIf}
#' morie_list_datasets()
#' \dontshow{\}) # examplesIf}
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
  out <- catalog[, c("key", "name", "source", "survey", "year", "type", "cached", "rows")]
  # the curated tables at data.rmorie.com: fetched when a key is stored (cached a day), else the last copy
  m <- if (!is.null(.morie_llm_hosted_key())) {
    tryCatch(morie_hosted_manifest(), error = function(e) .morie_data_cached_manifest())
  } else {
    .morie_data_cached_manifest()
  }
  hub <- .morie_data_entries(m)
  if (nrow(hub)) {
    hub_rows <- data.frame(key = hub$key, name = hub$name, source = "data.rmorie.com", survey = hub$source,
                           year = "", type = "hosted", cached = hub$table_name %in% names(cached_tables),
                           rows = ifelse(hub$table_name %in% names(cached_tables),
                                         as.integer(cached_tables[hub$table_name]), hub$rows),
                           stringsAsFactors = FALSE)
    out <- rbind(out, hub_rows)
  }
  rownames(out) <- NULL
  out
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
#' Lists or retrieves included userguide PDF files. These are the official
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
    dir(.rmorie_extdata("userguides"))
  } else {
    .rmorie_extdata("userguides", name, mustWork = TRUE)
  }
}


#' Download bootstrap weight files from CKAN API
#'
#' Downloads large bootstrap weight CSVs that are too big to ship with the
#' package. Data is cached in the user cache database for future use.
#'
#' @param survey A bootstrap key of \code{\link{morie_dataset_catalog}}
#'   (\code{"ocs22bt"}, \code{"ocs24bt"}, \code{"cu20bt"},
#'   \code{"cu23bt"}), its survey-year name (\code{"csads_2021"},
#'   \code{"csads_2023"}, \code{"csus_2019"}, \code{"csus_2023"}), or
#'   \code{"all"} (default).
#' @param limit Max records per CKAN request (default 32000).
#' @param db_path Optional path to a SQLite/DuckDB file (default backend).
#' @param con Optional pre-opened DBI connection (overrides `db_path`).
#' @param refresh Download again even when the table is already cached (default
#'   \code{FALSE}: a cached table is reported and kept).
#' @return Invisibly, the number of surveys available (downloaded or already cached).
#' @examples
#' # the bootstrap tables and their keys
#' cat_ <- morie_dataset_catalog()
#' cat_[cat_$type == "bootstrap", c("key", "name")]
#' \donttest{
#' # the CSADS 2021 bootstrap weights are 376 MB: fetched once, then read from the cache
#' if (interactive()) morie_download_bootstrap(survey = "csads_2021")
#' }
#' @export
morie_download_bootstrap <- function(survey = "all", limit = 32000L,
                                     db_path = NULL, con = NULL, refresh = FALSE) {
  targets <- .morie_bootstrap_targets(survey)
  catalog <- morie_dataset_catalog()
  n_ok <- 0L
  failed <- character()
  for (key in targets) {
    entry <- catalog[catalog$key == key, ]
    if (nrow(entry) == 0L) {
      failed <- c(failed, sprintf("%s: not in the dataset catalogue", key))
      next
    }

    # Try local file first.
    if (file.exists(entry$local_path)) {
      message("Ingesting ", key, " from local file: ", entry$local_path)
      morie_cache_file(entry$local_path, entry$table_name, db_path = db_path, con = con)
      message("  OK: cached ", entry$table_name)
      n_ok <- n_ok + 1L
      next
    }

    # already cached (as `pull` would find it): not fetched again unless refresh = TRUE
    if (!isTRUE(refresh)) {
      cached <- tryCatch(morie_cache_load(entry$table_name, db_path = db_path, con = con),
                         error = function(e) NULL)
      if (is.data.frame(cached) && nrow(cached)) {
        message(sprintf("  %s: %s rows already cached as %s (refresh = TRUE to download again)",
                        key, format(nrow(cached), big.mark = ","), entry$table_name))
        n_ok <- n_ok + 1L
        next
      }
    }

    # The datastore when the catalogue names a CKAN resource (honours `limit`), else the
    # catalogue's own download route, the one `rmorie pull` takes.
    message("Downloading ", key, " (", entry$name, ") ...")
    r <- tryCatch(
      if (nzchar(entry$ckan_resource_id)) {
        data <- morie_fetch_ckan(key, limit = limit, db_path = db_path, con = con,
                                 resource_id = entry$ckan_resource_id)
        .morie_cache_store_soft(data, entry$table_name, db_path = db_path, con = con)
        # morie_fetch_ckan keeps its own "<key>_raw" copy: one cached copy of a 230 MB table
        if (!identical(paste0(key, "_raw"), entry$table_name)) .morie_cache_drop(paste0(key, "_raw"), db_path, con)
        data
      } else {
        morie_load_dataset(key, db_path = db_path, con = con)
      },
      error = function(e) e
    )
    if (inherits(r, "error")) {
      failed <- c(failed, sprintf("%s: %s", key, conditionMessage(r)))
      message("  ERROR: ", conditionMessage(r))
    } else {
      n_ok <- n_ok + 1L
      message("  OK: ", nrow(r), " rows cached as ", entry$table_name)
    }
  }
  if (n_ok == 0L && length(failed)) {
    stop("no bootstrap file could be downloaded:\n  ", paste(failed, collapse = "\n  "), call. = FALSE)
  }
  invisible(n_ok)
}

#' Internal helper: the catalogue keys a download-bootstrap request names
#'
#' A bootstrap key itself, its survey-year name, or "all"; shared by
#' morie_download_bootstrap() and `rmorie download-bootstrap`.
#' @noRd
.morie_bootstrap_targets <- function(survey) {
  aliases <- c(csads_2021 = "ocs22bt", csads_2023 = "ocs24bt", csus_2019 = "cu20bt", csus_2023 = "cu23bt")
  if (!is.character(survey) || length(survey) != 1L || is.na(survey)) {
    stop("`survey` must be one string: a bootstrap key, its survey-year name, or \"all\"", call. = FALSE)
  }
  if (identical(survey, "all")) return(unname(aliases))
  if (survey %in% aliases) return(survey)
  if (survey %in% names(aliases)) return(unname(aliases[[survey]]))
  stop(sprintf("Unknown survey '%s'. Keys: %s (or %s, or all)", survey,
               paste(aliases, collapse = ", "), paste(names(aliases), collapse = ", ")), call. = FALSE)
}

#' Internal helper: the download URL of a CKAN resource (resource_show)
#' @noRd
.morie_ckan_resource_url <- function(rid, ckan_base) {
  .morie_ckan_resource_meta(rid, ckan_base)$url
}

#' Internal helper: a CKAN resource's file URL and size (bytes), from resource_show
#' @noRd
.morie_ckan_resource_meta <- function(rid, ckan_base) {
  base <- sub("/datastore_search$", "", ckan_base)
  err <- NULL
  meta <- tryCatch(
    .morie_from_json(paste(suppressWarnings(readLines(url(paste0(base, "/resource_show?id=", rid)), warn = FALSE)), collapse = "")),
    error = function(e) {
      err <<- conditionMessage(e)  # the portal did not answer: not the same as a resource with no file
      NULL
    })
  u <- meta$result$url
  size <- suppressWarnings(as.numeric(meta$result$size %||% NA))
  list(url = if (is.character(u) && length(u) == 1L && nzchar(u)) u else NULL,
       size = if (length(size) == 1L && is.finite(size) && size > 0) size else NULL,
       error = err, host = sub("^https?://([^/]+).*$", "\\1", base))
}

#' Internal helper: a CKAN resource read as a file, live or from the Wayback Machine
#'
#' The datastore API is not archived; the resource file (a CSV or a zip of
#' CSVs) is, so when the API is down this is the route. `morie_download()`
#' tries the live URL first and the closest Internet Archive snapshot second.
#' @noRd
.morie_ckan_resource_file <- function(rid, dataset_key, ckan_base) {
  meta <- .morie_ckan_resource_meta(rid, ckan_base)
  src <- meta$url
  if (is.null(src) && !is.null(meta$error)) {
    stop(sprintf("%s could not be reached (%s); nothing cached for %s", meta$host, meta$error, dataset_key),
         call. = FALSE)
  }
  if (is.null(src)) {
    stop("CKAN returned 0 records for ", dataset_key, " and resource ", rid, " has no file URL", call. = FALSE)
  }
  dest <- file.path(tempdir(), paste0("ckan-", rid, "-", basename(src)))
  if (!file.exists(dest)) {
    message("Downloading ", src, " (live, then the Wayback Machine snapshot if the portal is down)")
    morie_download(src, dest, attempt_wayback = TRUE, label = dataset_key, size = meta$size)
  }
  path <- dest
  if (grepl("\\.zip$", dest, ignore.case = TRUE)) {
    files <- utils::unzip(dest, exdir = tempfile("ckan-zip"))
    csvs <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
    if (!length(csvs)) stop(basename(dest), " for ", dataset_key, " holds no CSV", call. = FALSE)
    path <- csvs[[1L]]
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Internal: the data directory ($MORIE_DATA_DIR, else the per-user data dir)
#' @noRd
.morie_data_root <- function() {
  env <- Sys.getenv("MORIE_DATA_DIR", "")
  if (nzchar(env)) return(path.expand(env))
  tools::R_user_dir("morie", which = "data")
}

# Remove one table from the default file cache (a no-op for a database cache or a missing file).
.morie_cache_drop <- function(table_name, db_path = NULL, con = NULL) {
  h <- tryCatch(.morie_db_handle(con, db_path), error = function(e) NULL)
  if (is.null(h) || !h$type %in% c("rds", "parquet")) return(invisible(FALSE))
  p <- .morie_cache_fs_path(h$dir, table_name, h$type)
  invisible(file.exists(p) && unlink(p) == 0L)
}
