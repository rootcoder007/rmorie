# SPDX-License-Identifier: AGPL-3.0-or-later
#
# CKAN open-data portal client (R port).
#
# CKAN (https://ckan.org) is the open-source data-portal stack used
# by Canada (open.canada.ca), the United States (data.gov), the UK
# (data.gov.uk), the European Union (data.europa.eu), and many
# provincial / municipal portals.  Every CKAN endpoint exposes the
# same Action API at `/api/3/action/<verb>`, so a single thin client
# covers every portal a sociolegal researcher is likely to touch.
#
# Mirrors the Python `morie.ingest.ckan` module:
#
#   * `morie_ingest_ckan_package_search()` - free-text / fq search
#   * `morie_ingest_ckan_package_show()`   - single package metadata
#   * `morie_ingest_ckan_resource_show()`  - single resource metadata
#   * `morie_ingest_ckan_read_resource()`  - download a resource as df
#   * `morie_ingest_ckan_fetch_package_csvs()` - all CSV/TSV resources
#   * `morie_ingest_ckan_search_packages()`    - search -> flat df
#
# HTTP: routes via .morie_dataset_http_text + .morie_dataset_http_bytes (3YY -> libcurl C++ backend with httr2 fallback). JSON: jsonlite::fromJSON (which delegates to
# jsonlite).  CSV/TSV: prefer `readr` when installed; fall back to
# `utils::read.csv` / `read.delim`.  XLSX needs `readxl` (Suggests).
# Parquet needs `arrow` (Suggests).  Each optional dep errors cleanly
# with an install hint when missing.

.MORIE_CKAN_DEFAULT_UA <- "morie/r (+https://hadesllm.com)"
.MORIE_CKAN_DEFAULT_TIMEOUT <- 30

# NOTE: canonical .morie_ckan_portal lives in data_access.R; that one
# resolves short names ("open.canada.ca" -> "https://open.canada.ca/data/en")
# and errors on unknown short names. Don't redefine it here — the
# alphabetical load order would clobber the resolver and tests would fail.

# Internal: perform a CKAN Action-API call and unwrap `result`.
# 3YY: collapsed .morie_ckan_build_req + _call into a single
# function that routes through .morie_dataset_http_text (libcurl
# with httr2 fallback) + jsonlite::fromJSON(simplifyVector=FALSE).
.morie_ckan_call <- function(portal,
                             action,
                             params = NULL,
                             api_key = NULL,
                             user_agent = .MORIE_CKAN_DEFAULT_UA,
                             timeout = .MORIE_CKAN_DEFAULT_TIMEOUT) {
  url <- sprintf(
    "%s/api/3/action/%s", .morie_ckan_portal(portal), action
  )
  headers <- if (!is.null(api_key) && nzchar(api_key)) {
    paste0("Authorization: ", api_key)
  } else {
    character()
  }
  body <- tryCatch(
    .morie_dataset_http_text(url,
                              query = params,
                              headers = headers,
                              timeout_s = as.integer(timeout)),
    error = function(e) {
      stop("morie CKAN ", action, " request failed: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  payload <- tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(e) {
      stop("morie CKAN ", action, ": response was not JSON: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!isTRUE(payload$success)) {
    stop("morie CKAN ", action, " failed: ",
      paste(utils::capture.output(str(payload$error)), collapse = " "),
      call. = FALSE
    )
  }
  payload$result
}

# Internal: sniff a resource format from URL extension when unset.
.morie_ckan_sniff_format <- function(url, as_format = NULL) {
  if (!is.null(as_format) && nzchar(as_format)) {
    return(tolower(as_format))
  }
  ext <- tools::file_ext(sub("[?#].*$", "", url))
  if (!nzchar(ext)) {
    return("csv")
  }
  tolower(ext)
}

# Internal: read a downloaded resource path into a data.frame by format.
.morie_ckan_read_path <- function(path, fmt) {
  fmt <- tolower(fmt)
  if (fmt %in% c("csv")) {
    if (requireNamespace("readr", quietly = TRUE)) {
      df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
      return(as.data.frame(df))
    }
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }
  if (fmt %in% c("tsv", "tab")) {
    if (requireNamespace("readr", quietly = TRUE)) {
      df <- readr::read_tsv(path, show_col_types = FALSE, progress = FALSE)
      return(as.data.frame(df))
    }
    return(utils::read.delim(path, sep = "\t", stringsAsFactors = FALSE))
  }
  if (fmt %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading CKAN Excel resources requires the 'readxl' package. ",
        "install.packages('readxl')",
        call. = FALSE
      )
    }
    return(as.data.frame(readxl::read_excel(path)))
  }
  if (fmt %in% c("json")) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Reading CKAN JSON resources requires the 'jsonlite' package. ",
        "install.packages('jsonlite')",
        call. = FALSE
      )
    }
    return(as.data.frame(jsonlite::fromJSON(path, flatten = TRUE)))
  }
  if (fmt %in% c("parquet")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Reading CKAN Parquet resources requires the 'arrow' package. ",
        "install.packages('arrow')",
        call. = FALSE
      )
    }
    return(as.data.frame(arrow::read_parquet(path)))
  }
  # Unknown extension: most open-data resources are CSV with bad MIME
  # types, so try CSV as a last resort (matches Python behaviour).
  if (requireNamespace("readr", quietly = TRUE)) {
    df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
    return(as.data.frame(df))
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Search a CKAN portal for packages (raw)
#'
#' Calls the CKAN \code{package_search} Action verb against a portal
#' base URL and returns the raw result list (\code{count},
#' \code{results}, etc.).  For a flattened metadata data.frame, use
#' \code{\link{morie_ingest_ckan_search_packages}}.
#'
#' @param portal Base URL of the CKAN portal, e.g.
#'   \code{"https://open.canada.ca/data"}.  Trailing slash optional.
#' @param query Free-text \code{q=} query, or \code{NULL}.
#' @param fq CKAN \code{fq=} filter query (Solr-style), or \code{NULL}.
#' @param rows Maximum rows to return (default 100).
#' @param start Pagination offset (default 0).
#' @param api_key Optional CKAN API key (rare for open portals).
#' @param user_agent User-Agent header sent with the request.
#' @param timeout HTTP timeout in seconds.
#' @return A named list as returned by the CKAN Action API.
#' @examples
#' \dontrun{
#' res <- morie_ingest_ckan_package_search(
#'   "https://open.canada.ca/data",
#'   query = "corrections"
#' )
#' length(res$results)
#' }
#' @export
morie_ingest_ckan_package_search <- function(portal,
                                             query = NULL,
                                             fq = NULL,
                                             rows = 100L,
                                             start = 0L,
                                             api_key = NULL,
                                             user_agent =
                                               .MORIE_CKAN_DEFAULT_UA,
                                             timeout =
                                               .MORIE_CKAN_DEFAULT_TIMEOUT) {
  params <- list(rows = as.integer(rows), start = as.integer(start))
  if (!is.null(query) && nzchar(query)) params$q <- query
  if (!is.null(fq) && nzchar(fq)) params$fq <- fq
  .morie_ckan_call(
    portal = portal, action = "package_search", params = params,
    api_key = api_key, user_agent = user_agent, timeout = timeout
  )
}

#' Fetch one CKAN package's metadata
#'
#' Calls the CKAN \code{package_show} verb.  The returned list
#' contains \code{title}, \code{notes}, \code{resources}, etc. -
#' resources are the individual downloadable files in the package.
#'
#' @param portal Base URL of the CKAN portal.
#' @param package_id Package id or slug, e.g.
#'   \code{"canadian-postsecondary-alcohol-and-drug-use-survey"}.
#' @param api_key Optional CKAN API key.
#' @param user_agent User-Agent header sent with the request.
#' @param timeout HTTP timeout in seconds.
#' @return The package metadata list.
#' @export
morie_ingest_ckan_package_show <- function(portal,
                                           package_id,
                                           api_key = NULL,
                                           user_agent =
                                             .MORIE_CKAN_DEFAULT_UA,
                                           timeout =
                                             .MORIE_CKAN_DEFAULT_TIMEOUT) {
  if (!is.character(package_id) || length(package_id) != 1L ||
    !nzchar(package_id)) {
    stop("`package_id` must be a single non-empty string.", call. = FALSE)
  }
  .morie_ckan_call(
    portal = portal, action = "package_show",
    params = list(id = package_id),
    api_key = api_key, user_agent = user_agent, timeout = timeout
  )
}

#' Fetch one CKAN resource's metadata
#'
#' Calls the CKAN \code{resource_show} verb to resolve a resource id
#' into its download URL plus metadata.
#'
#' @param portal Base URL of the CKAN portal.
#' @param resource_id CKAN resource id (UUID).
#' @param api_key Optional CKAN API key.
#' @param user_agent User-Agent header sent with the request.
#' @param timeout HTTP timeout in seconds.
#' @return The resource metadata list.
#' @export
morie_ingest_ckan_resource_show <- function(portal,
                                            resource_id,
                                            api_key = NULL,
                                            user_agent =
                                              .MORIE_CKAN_DEFAULT_UA,
                                            timeout =
                                              .MORIE_CKAN_DEFAULT_TIMEOUT) {
  if (!is.character(resource_id) || length(resource_id) != 1L ||
    !nzchar(resource_id)) {
    stop("`resource_id` must be a single non-empty string.", call. = FALSE)
  }
  .morie_ckan_call(
    portal = portal, action = "resource_show",
    params = list(id = resource_id),
    api_key = api_key, user_agent = user_agent, timeout = timeout
  )
}

#' Download a CKAN resource as a data.frame
#'
#' \code{url_or_id} may be a direct download URL (as it appears in
#' \code{resource$url}) or a CKAN resource id, in which case the URL
#' is resolved via \code{\link{morie_ingest_ckan_resource_show}}.
#'
#' Format detection: if \code{as_format} is given it wins.  Otherwise
#' the extension is sniffed off the URL; unknown extensions fall back
#' to CSV (matching the Python helper).
#'
#' Excel / JSON / Parquet readers require optional dependencies
#' (\pkg{readxl} / \pkg{jsonlite} / \pkg{arrow}) and error with an
#' install hint if missing.
#'
#' @param portal Base URL of the CKAN portal (only used when
#'   \code{url_or_id} is a bare resource id).
#' @param url_or_id A direct URL or a CKAN resource id.
#' @param as_format Optional format override
#'   (\code{"csv"}, \code{"tsv"}, \code{"xlsx"}, \code{"json"},
#'   \code{"parquet"}).
#' @param api_key Optional CKAN API key.
#' @param user_agent User-Agent header sent with the request.
#' @param timeout HTTP timeout in seconds.
#' @return A base R \code{data.frame}.
#' @export
morie_ingest_ckan_read_resource <- function(portal,
                                            url_or_id,
                                            as_format = NULL,
                                            api_key = NULL,
                                            user_agent =
                                              .MORIE_CKAN_DEFAULT_UA,
                                            timeout =
                                              .MORIE_CKAN_DEFAULT_TIMEOUT) {
  if (!is.character(url_or_id) || length(url_or_id) != 1L ||
    !nzchar(url_or_id)) {
    stop("`url_or_id` must be a single non-empty string.", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for morie_ingest_ckan_read_resource(). ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }
  if (grepl("^https?://", url_or_id)) {
    url <- url_or_id
    fmt <- .morie_ckan_sniff_format(url, as_format)
  } else {
    meta <- morie_ingest_ckan_resource_show(
      portal = portal, resource_id = url_or_id,
      api_key = api_key, user_agent = user_agent, timeout = timeout
    )
    url <- meta$url
    fmt <- tolower(
      if (!is.null(as_format) && nzchar(as_format)) {
        as_format
      } else if (!is.null(meta$format) && nzchar(meta$format)) {
        meta$format
      } else {
        "csv"
      }
    )
  }

  ext <- if (nzchar(fmt)) paste0(".", fmt) else ".bin"
  tmp <- tempfile(fileext = ext, tmpdir = tempdir())
  on.exit(
    if (file.exists(tmp)) unlink(tmp, force = TRUE),
    add = TRUE
  )

  # 3YY: route through .morie_dataset_http_bytes (libcurl-backed
  # raw-byte fetcher with httr2 fallback) + writeBin to tmp.
  headers <- if (!is.null(api_key) && nzchar(api_key)) {
    paste0("Authorization: ", api_key)
  } else {
    character()
  }
  tryCatch({
    bytes <- .morie_dataset_http_bytes(url,
                                         headers = headers,
                                         timeout_s = as.integer(timeout))
    writeBin(bytes, tmp)
  }, error = function(e) {
    stop("morie_ingest_ckan_read_resource: download failed for ", url,
      "\
  ", conditionMessage(e),
      call. = FALSE
    )
  })
  tryCatch(
    .morie_ckan_read_path(tmp, fmt),
    error = function(e) {
      stop("morie_ingest_ckan_read_resource: parse failed for ", url,
        " (fmt=", fmt, ")\
  ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Fetch every CSV / TSV resource in a CKAN package
#'
#' Mirrors the Python \code{fetch_package_csvs} helper: pulls the
#' package metadata, walks its \code{resources} list, and downloads
#' each CSV / TSV into a named list of data.frames keyed by resource
#' \code{name} (falling back to \code{url}, then \code{id}).  Non-CSV /
#' TSV resources are skipped; individual download failures are
#' captured as a single-row error data.frame keyed
#' \code{_failed_<name>} so the overall fetch still returns the
#' successful ones.
#'
#' @param portal Base URL of the CKAN portal.
#' @param package_id Package id or slug.
#' @param api_key Optional CKAN API key.
#' @param user_agent User-Agent header sent with the request.
#' @param timeout HTTP timeout in seconds.
#' @return A named list of data.frames.
#' @export
morie_ingest_ckan_fetch_package_csvs <- function(
    portal,
    package_id,
    api_key = NULL,
    user_agent = .MORIE_CKAN_DEFAULT_UA,
    timeout = .MORIE_CKAN_DEFAULT_TIMEOUT) {
  pkg <- morie_ingest_ckan_package_show(
    portal = portal, package_id = package_id,
    api_key = api_key, user_agent = user_agent, timeout = timeout
  )
  resources <- pkg$resources
  if (is.null(resources) || length(resources) == 0L) {
    return(list())
  }

  out <- list()
  for (r in resources) {
    fmt <- tolower(if (is.null(r$format)) "" else r$format)
    if (!fmt %in% c("csv", "tsv")) {
      next
    }
    key <- r$name
    if (is.null(key) || !nzchar(key)) key <- r$url
    if (is.null(key) || !nzchar(key)) key <- r$id
    if (is.null(key) || !nzchar(key)) key <- fmt
    df <- tryCatch(
      morie_ingest_ckan_read_resource(
        portal = portal, url_or_id = r$url, as_format = fmt,
        api_key = api_key, user_agent = user_agent, timeout = timeout
      ),
      error = function(e) {
        data.frame(error = conditionMessage(e), stringsAsFactors = FALSE)
      }
    )
    if (identical(names(df), "error") && nrow(df) == 1L) {
      out[[paste0("_failed_", key)]] <- df
    } else {
      out[[key]] <- df
    }
  }
  out
}

#' Search a CKAN portal and return a flat metadata data.frame
#'
#' Convenience wrapper over
#' \code{\link{morie_ingest_ckan_package_search}} that flattens the
#' most-useful columns into a single data.frame: \code{id},
#' \code{name}, \code{title}, \code{organization}, \code{license_id},
#' \code{metadata_modified}, \code{num_resources}, \code{url} (the
#' canonical \code{<portal>/dataset/<name>} URL).
#'
#' @param portal Base URL of the CKAN portal.
#' @param query Free-text query string.
#' @param rows Maximum rows to return (default 50).
#' @param api_key Optional CKAN API key.
#' @param user_agent User-Agent header sent with the request.
#' @param timeout HTTP timeout in seconds.
#' @return A base R \code{data.frame}.
#' @export
morie_ingest_ckan_search_packages <- function(portal,
                                              query,
                                              rows = 50L,
                                              api_key = NULL,
                                              user_agent =
                                                .MORIE_CKAN_DEFAULT_UA,
                                              timeout =
                                                .MORIE_CKAN_DEFAULT_TIMEOUT) {
  resp <- morie_ingest_ckan_package_search(
    portal = portal, query = query, rows = rows,
    api_key = api_key, user_agent = user_agent, timeout = timeout
  )
  results <- resp$results
  if (is.null(results) || length(results) == 0L) {
    return(data.frame(
      id = character(), name = character(), title = character(),
      organization = character(), license_id = character(),
      metadata_modified = character(), num_resources = integer(),
      url = character(), stringsAsFactors = FALSE
    ))
  }
  base <- paste0(.morie_ckan_portal(portal), "/dataset/")
  pick <- function(x, k) {
    v <- x[[k]]
    if (is.null(v)) NA else v
  }
  do.call(rbind, lapply(results, function(p) {
    org <- p$organization
    org_title <- if (is.list(org)) {
      v <- org$title
      if (is.null(v)) NA_character_ else v
    } else {
      NA_character_
    }
    data.frame(
      id                = as.character(pick(p, "id")),
      name              = as.character(pick(p, "name")),
      title             = as.character(pick(p, "title")),
      organization      = as.character(org_title),
      license_id        = as.character(pick(p, "license_id")),
      metadata_modified = as.character(pick(p, "metadata_modified")),
      num_resources     = suppressWarnings(
        as.integer(pick(p, "num_resources"))
      ),
      url = paste0(
        base,
        if (is.null(p$name)) "" else as.character(p$name)
      ),
      stringsAsFactors = FALSE
    )
  }))
}
