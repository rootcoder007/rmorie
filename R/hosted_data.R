# Curated datasets at data.rmorie.com, opened by the MORIE key.
#
# The project materialises public BigQuery datasets and serves their tables
# as CSV from the edge (R2 behind a Worker), reachable whenever the internet
# is, with the key morie_llm_login() stores. /manifest.json lists every table
# with rows, columns, size, SHA-256 and the BigQuery source; /<db>/<table>.csv.gz
# is the table. Downloads are cached in the dataset store. Mirrors
# morie.datahub (Python).

.morie_data_url <- function() {
  sub("/+$", "", Sys.getenv("MORIE_DATA_URL", "https://data.rmorie.com"))
}

.morie_data_manifest_path <- function() {
  file.path(tools::R_user_dir("morie", which = "cache"), "data_rmorie_manifest.json")
}

#' Internal helper: GET a path from data.rmorie.com with the stored key
#' @noRd
.morie_data_get <- function(path, dest, timeout = 600, size = NULL) {
  key <- .morie_llm_hosted_key()
  if (is.null(key)) {
    stop(paste0("data.rmorie.com needs your MORIE key: run `rmorie login` (GitHub) or `rmorie login --email you@example.com` once (R: morie_llm_login(), or morie_llm_login(email = \"you@example.com\"))", .morie_httr2_note(), "."), call. = FALSE)
  }
  label <- sub("^/", "", path)
  if (is.null(size) && grepl("\\.csv\\.gz$", path)) {
    # the manifest knows the compressed size: a percent bar instead of a spinner
    label <- sub("\\.csv\\.gz$", "", label)
    m <- tryCatch(morie_hosted_manifest(), error = function(e) NULL)
    for (d in m$datasets %||% list()) {
      if (identical(d$key, label)) {
        size <- d$bytes_gz
        break
      }
    }
  }
  rc <- tryCatch(
    .morie_dl(paste0(.morie_data_url(), path), dest, label = label, size = size, timeout = timeout,
              headers = c(Authorization = paste("Bearer", key),
                          "User-Agent" = "rmorie/1 (+https://rmorie.com)")),
    error = function(e) e, warning = function(w) w)
  if (inherits(rc, "condition")) {
    msg <- conditionMessage(rc)
    if (grepl("401|403|Unauthorized|Forbidden", msg)) {
      stop("data.rmorie.com rejected the stored key; run `rmorie login` again.", call. = FALSE)
    }
    stop("data.rmorie.com: ", msg, call. = FALSE)
  }
  invisible(dest)
}

#' Curated datasets at data.rmorie.com
#'
#' The MORIE project keeps 160 databases materialised from Google BigQuery public datasets, plus the Health Infobase tables and the OTIS research files,
#' public datasets (Chicago crime, EPA air quality, US census, FEC, FDA,
#' NOAA, NHTSA, Hacker News, Ethereum, World Bank, ...) and serves their
#' tables from the edge. They open with the key \code{\link{morie_llm_login}}
#' stores. \code{morie_hosted_manifest()} returns the gateway's manifest
#' (every table with rows, columns, size, SHA-256 and its BigQuery source),
#' cached for a day; \code{morie_hosted_datasets()} the same as a data frame;
#' \code{morie_load_hosted_dataset("db/table")} one table, cached in the
#' dataset store so later calls are local. \code{\link{morie_load_dataset}}
#' and \code{rmorie pull} accept the same \code{db/table} keys.
#'
#' @param refresh Fetch again even when a day-old copy is cached.
#' @param key A \code{db/table} key from the manifest.
#' @param db_path Dataset-store path, as in \code{\link{morie_load_dataset}}.
#' @return \code{morie_hosted_manifest()}: a list; \code{morie_hosted_datasets()}:
#'   a data frame with \code{key}, \code{name}, \code{rows}, \code{source};
#'   \code{morie_load_hosted_dataset()}: the table as a data frame.
#' @examples
#' \donttest{
#' # with a stored MORIE key (rmorie login); without one this stops with that advice
#' try({
#'   head(morie_hosted_datasets())
#'   df <- morie_load_hosted_dataset("fec_cm_2020/fec_cm_2020")
#' })
#' }
#' @export
morie_hosted_manifest <- function(refresh = FALSE) {
  p <- .morie_data_manifest_path()
  fresh <- file.exists(p) && as.numeric(difftime(Sys.time(), file.mtime(p), units = "secs")) < 86400
  if (!refresh && fresh) {
    return(.morie_from_json(paste(readLines(p, warn = FALSE), collapse = "\n"), simplifyVector = FALSE))
  }
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  .morie_data_get("/manifest.json", p, timeout = 60)
  .morie_from_json(paste(readLines(p, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)
}

#' Internal helper: the manifest if it was fetched before (no network, no key)
#' @noRd
.morie_data_cached_manifest <- function() {
  p <- .morie_data_manifest_path()
  if (!file.exists(p)) return(NULL)
  tryCatch(.morie_from_json(paste(readLines(p, warn = FALSE), collapse = "\n"), simplifyVector = FALSE),
           error = function(e) NULL)
}

#' @rdname morie_hosted_manifest
#' @export
morie_hosted_datasets <- function(refresh = FALSE) {
  m <- morie_hosted_manifest(refresh = refresh)
  .morie_data_entries(m)
}

.morie_data_entries <- function(m) {
  ds <- if (is.null(m)) list() else m$datasets
  if (!length(ds)) {
    return(data.frame(key = character(), name = character(), rows = integer(), source = character(),
                      table_name = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    key = vapply(ds, function(d) d$key, ""),
    name = vapply(ds, function(d) (d$meta$description %||% d$source %||% d$key)[[1L]], ""),
    rows = vapply(ds, function(d) as.integer(d$rows %||% NA), 1L),
    source = vapply(ds, function(d) (d$source %||% "")[[1L]], ""),
    table_name = vapply(ds, function(d) .morie_data_table_name(d$key), ""),
    stringsAsFactors = FALSE)
}

.morie_data_table_name <- function(key) paste0("hub_", gsub("/", "__", key, fixed = TRUE))

.morie_data_is_key <- function(key) {
  is.character(key) && length(key) == 1L && grepl("/", key, fixed = TRUE) &&
    !startsWith(key, "/") && !startsWith(key, ".") && !endsWith(key, "/")
}

#' @rdname morie_hosted_manifest
#' @export
morie_load_hosted_dataset <- function(key, db_path = NULL, refresh = FALSE) {
  if (!.morie_data_is_key(key)) {
    stop("'", key, "' is not a data.rmorie.com key (expected db/table; see morie_hosted_datasets())", call. = FALSE)
  }
  table <- .morie_data_table_name(key)
  if (!refresh) {
    cached <- tryCatch(morie_cache_load(table, db_path = db_path), error = function(e) NULL)
    if (!is.null(cached)) return(cached)
  }
  parts <- strsplit(key, "/", fixed = TRUE)[[1L]]
  tmp <- tempfile(fileext = ".csv.gz")
  on.exit(unlink(tmp), add = TRUE)
  .morie_data_get(sprintf("/%s/%s.csv.gz", parts[[1L]], paste(parts[-1L], collapse = "/")), tmp)
  df <- utils::read.csv(gzfile(tmp), stringsAsFactors = FALSE, skipNul = TRUE)  # some sources carry NUL bytes
  .morie_cache_store_soft(df, table, db_path = db_path)
  df
}
