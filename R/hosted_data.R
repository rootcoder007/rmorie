# Curated datasets at data.rmorie.com, opened by the MORIE key.
#
# The project materialises public BigQuery datasets and serves their tables
# as CSV from the edge (R2 behind a Worker), reachable whenever the internet
# is, with the data key morie_llm_login() stores. /manifest.json lists every table
# with rows, columns, size, SHA-256 and the BigQuery source; /<db>/<table>.csv.gz
# is the table. Downloads are cached in the dataset store. Mirrors
# morie.datahub (Python).

.morie_data_url <- function() {
  env <- sub("/+$", "", trimws(Sys.getenv("MORIE_DATA_URL", unset = "")))
  if (nzchar(env)) return(env)
  # the signed services document (through rmoriebricklayer) names the data service;
  # a bricklayer too old to read it leaves the default
  svc <- NULL
  if (requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    ns <- asNamespace("rmoriebricklayer")
    if (exists("bricklayer_services", envir = ns, inherits = FALSE)) {
      svc <- tryCatch(get("bricklayer_services", envir = ns)(offline = TRUE)$data, error = function(e) NULL)
    }
  }
  if (is.null(svc)) return("https://data.rmorie.com")
  if (!identical(svc$mode, "key") || !nzchar(svc$base_url %||% "")) {
    stop("the curated-data service is not available right now (see ",
         svc$request_access %||% "https://rmorie.com/access", ")", call. = FALSE)
  }
  svc$base_url
}

# The tables whose source licence asks more than attribution (ICPSR studies, the NCVS 38604
# extract, the a2aj case law): data.rmorie.com opens them once the terms are accepted.
.morie_data_restricted <- function(key) grepl("^(icpsr_|ncvs_38604|a2aj_)", sub("^(db|files)/", "", key))

# A downloaded table is used only when it matches the manifest's SHA-256: the manifest is fetched
# again once before a mismatch is final (a table rebuilt upstream has a new digest), and a table
# the manifest lists without a digest is used as it is. A failing file is deleted.
.morie_data_check_sha <- function(key, path) {
  digest <- function(refresh) {
    m <- morie_hosted_manifest(refresh = refresh)
    for (d in m$datasets %||% list()) {
      if (identical(d$key, key)) {
        s <- d$sha256
        return(if (is.character(s) && length(s) == 1L && nzchar(s)) tolower(s) else NA_character_)
      }
    }
    NULL
  }
  unlisted <- function() {
    unlink(path)
    stop(key, " is not listed in the manifest of data.rmorie.com", call. = FALSE)
  }
  want <- digest(FALSE)
  if (is.null(want)) unlisted()
  if (is.na(want)) return(invisible(TRUE))
  have <- tolower(rmoriebricklayer::sha256_file(path))
  if (!identical(have, want)) {
    want <- digest(TRUE)
    if (is.null(want)) unlisted()
    if (is.na(want)) return(invisible(TRUE))
  }
  if (!identical(have, want)) {
    unlink(path)
    stop(sprintf(paste("%s does not match the manifest's SHA-256 (%s... vs %s...): the download was",
                       "deleted; run again, and if it still fails it was altered in transit"),
                 key, substr(have, 1L, 12L), substr(want, 1L, 12L)), call. = FALSE)
  }
  invisible(TRUE)
}

.morie_data_manifest_path <- function() {
  file.path(tools::R_user_dir("morie", which = "cache"), "data_rmorie_manifest.json")
}

#' Internal helper: GET a path from data.rmorie.com with the stored key
#' @noRd
.morie_data_get <- function(path, dest, timeout = 600, size = NULL) {
  key <- .morie_llm_hosted_key()
  if (is.null(key)) {
    stop(paste0("the curated tables need your MORIE data key: ", .morie_llm_access_hint(), "."), call. = FALSE)
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
    if (grepl("403|Forbidden", msg) && .morie_data_restricted(label)) {
      stop(label, " is a restricted table (its source asks more than a citation): read and accept the terms at ",
           "https://data.rmorie.com/terms, signed in with your key, then ask again.", call. = FALSE)
    }
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
#' tables from the edge. They open with the data key \code{\link{morie_llm_login}}
#' stores; a downloaded table is used only when it matches the SHA-256 the
#' manifest lists for it. The ICPSR, NCVS 38604 and a2aj tables also need their
#' terms accepted at \url{https://data.rmorie.com/terms}. \code{morie_hosted_manifest()} returns the gateway's manifest
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
  .morie_data_check_sha(key, tmp)
  df <- utils::read.csv(gzfile(tmp), stringsAsFactors = FALSE, skipNul = TRUE)  # some sources carry NUL bytes
  .morie_cache_store_soft(df, table, db_path = db_path)
  df
}
