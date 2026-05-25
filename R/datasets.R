# SPDX-License-Identifier: AGPL-3.0-or-later
#
# datasets.R -- one-call dataset loaders for common Canadian / US
# sociolegal open data.
#
# Ported from src/morie/datasets.py.  Each function is a thin, stable
# wrapper that resolves to either a live HTTP fetch (httr2) or a
# bundled-in-package synthetic frame.  Synthetic frames are CSVs under
# `inst/extdata/` of the morie R package; absence of a synthetic CSV is
# surfaced as a clean `FileNotFoundError`-style condition.

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Resolve a bundled synthetic CSV path from the morie R package.
#' @keywords internal
#' @noRd
.morie_dataset_pkg_csv <- function(name) {
  path <- system.file("extdata", paste0(name, ".csv"), package = "morie")
  if (!nzchar(path)) {
    return(NA_character_)
  }
  path
}

#' Read a bundled synthetic frame, warning the user it's a toy dataset.
#' @keywords internal
#' @noRd
.morie_dataset_read_synthetic <- function(name, kind, columns = NULL) {
  path <- .morie_dataset_pkg_csv(name)
  if (is.na(path)) {
    if (!is.null(columns)) {
      empty <- as.data.frame(
        stats::setNames(replicate(length(columns), character(0), simplify = FALSE), columns),
        stringsAsFactors = FALSE
      )
      return(empty)
    }
    stop(sprintf(
      "morie_datasets_%s(offline=TRUE): no bundled synthetic frame at inst/extdata/%s.csv.",
      kind, name
    ))
  }
  warning(sprintf(
    paste0(
      "morie_datasets_%s(offline=TRUE): using the bundled synthetic %s frame. ",
      "This is a toy dataset with the documented schema but random data; ",
      "do not interpret outputs as findings about the real population."
    ),
    kind, kind
  ), call. = FALSE)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Build a SQL-ish `WHERE` clause for an "OCC_YEAR = ?" or "1=1" filter.
#' @keywords internal
#' @noRd
.morie_dataset_year_where <- function(year) {
  if (is.null(year)) {
    return("1=1")
  }
  sprintf("OCC_YEAR = %d", as.integer(year))
}

#' Assemble a fully-formed URL with percent-encoded query parameters.
#' Mirrors httr2::req_url_query() but doesn't require httr2 at the
#' call site -- needed so the libcurl-backed C++ HTTP path can take
#' a single, already-built URL.
#' @keywords internal
#' @noRd
.morie_dataset_build_url <- function(url, query = NULL) {
  if (is.null(query) || length(query) == 0L) return(url)
  pairs <- vapply(seq_along(query), function(i) {
    nm <- names(query)[[i]]
    val <- query[[i]]
    if (is.null(val) || length(val) == 0L) return(NA_character_)
    paste0(utils::URLencode(as.character(nm), reserved = TRUE),
           "=",
           utils::URLencode(as.character(val), reserved = TRUE))
  }, character(1L))
  pairs <- pairs[!is.na(pairs)]
  if (length(pairs) == 0L) return(url)
  sep <- if (grepl("\\?", url)) "&" else "?"
  paste0(url, sep, paste(pairs, collapse = "&"))
}

#' Detect whether the C++ libcurl backend (morie_http.cpp, 3VV) is
#' available. The Rcpp export only exists when the package was built
#' with libcurl visible to curl-config; on minimal CRAN macOS images
#' the C++ symbols may be absent. Fallback: httr2.
#' @keywords internal
#' @noRd
.morie_dataset_http_backend_cpp <- function() {
  exists(".morie_http_get",
         where = asNamespace("morie"),
         mode = "function")
}

#' GET that returns the response body as a UTF-8 character string.
#' 3VV: routes through morie's C++ libcurl backend (.morie_http_get,
#' src/morie_http.cpp) when available; falls back to httr2 if not.
#' @keywords internal
#' @noRd
.morie_dataset_http_text <- function(url, query = NULL,
                                       headers = character(),
                                       timeout_s = 60L) {
  full_url <- .morie_dataset_build_url(url, query)
  if (.morie_dataset_http_backend_cpp()) {
    return(.morie_http_get(full_url, timeout_s = as.integer(timeout_s),
                            headers = as.character(headers)))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("morie datasets HTTP fetch needs either the libcurl-backed ",
         "C++ backend (built into morie.so via src/morie_http.cpp) ",
         "or the 'httr2' R package.")
  }
  req <- httr2::request(full_url)
  if (length(headers) > 0L) {
    # Convert "Key: Value" strings into a named list for req_headers.
    kv <- strsplit(headers, ":\\s*")
    kv <- Filter(function(x) length(x) == 2L, kv)
    if (length(kv) > 0L) {
      named <- stats::setNames(vapply(kv, `[[`, character(1L), 2L),
                                vapply(kv, `[[`, character(1L), 1L))
      req <- do.call(httr2::req_headers, c(list(req), as.list(named)))
    }
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_string(resp)
}

#' Status-aware GET. 3ZZ: returns a list(body, status_code) so the
#' caller can format custom error messages for 4xx (e.g. NamUs's
#' 401/403 API-key handling). Routes through libcurl + httr2 fallback.
#' @keywords internal
#' @noRd
.morie_dataset_http_text_with_status <- function(url, query = NULL,
                                                    headers = character(),
                                                    timeout_s = 60L) {
  full_url <- .morie_dataset_build_url(url, query)
  if (exists(".morie_http_get_with_status",
              where = asNamespace("morie"),
              mode = "function")) {
    return(.morie_http_get_with_status(
      full_url, timeout_s = as.integer(timeout_s),
      headers = as.character(headers)))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("morie datasets HTTP fetch needs either the libcurl-backed ",
         "C++ backend or the 'httr2' R package.")
  }
  req <- httr2::request(full_url)
  if (length(headers) > 0L) {
    kv <- strsplit(headers, ":\\s*")
    kv <- Filter(function(x) length(x) == 2L, kv)
    if (length(kv) > 0L) {
      named <- stats::setNames(vapply(kv, `[[`, character(1L), 2L),
                                vapply(kv, `[[`, character(1L), 1L))
      req <- do.call(httr2::req_headers, c(list(req), as.list(named)))
    }
  }
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  list(body = httr2::resp_body_string(resp),
       status_code = as.integer(httr2::resp_status(resp)))
}

#' Status-aware POST + JSON body. 3ZZ.
#' @keywords internal
#' @noRd
.morie_dataset_http_post_json_with_status <- function(url, body,
                                                         query = NULL,
                                                         headers = character(),
                                                         timeout_s = 60L,
                                                         auto_unbox = TRUE) {
  full_url <- .morie_dataset_build_url(url, query)
  body_str <- jsonlite::toJSON(body, auto_unbox = auto_unbox,
                                 null = "null")
  if (exists(".morie_http_post_with_status",
              where = asNamespace("morie"),
              mode = "function")) {
    return(.morie_http_post_with_status(
      full_url, body = as.character(body_str),
      content_type = "application/json",
      timeout_s = as.integer(timeout_s),
      headers = as.character(headers)))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("morie datasets HTTP POST needs either the libcurl-backed ",
         "C++ backend or the 'httr2' R package.")
  }
  req <- httr2::request(full_url)
  req <- httr2::req_body_json(req, body, auto_unbox = auto_unbox)
  if (length(headers) > 0L) {
    kv <- strsplit(headers, ":\\s*")
    kv <- Filter(function(x) length(x) == 2L, kv)
    if (length(kv) > 0L) {
      named <- stats::setNames(vapply(kv, `[[`, character(1L), 2L),
                                vapply(kv, `[[`, character(1L), 1L))
      req <- do.call(httr2::req_headers, c(list(req), as.list(named)))
    }
  }
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  list(body = httr2::resp_body_string(resp),
       status_code = as.integer(httr2::resp_status(resp)))
}

#' Synchronous POST + parse JSON. 3YY: routes through morie's C++
#' libcurl .morie_http_post when available; falls back to httr2's
#' req_body_json + req_perform + resp_body_json otherwise. Body
#' (R list) is serialised via jsonlite::toJSON.
#' @keywords internal
#' @noRd
.morie_dataset_http_post_json <- function(url, body, query = NULL,
                                            headers = character(),
                                            timeout_s = 60L,
                                            auto_unbox = TRUE) {
  full_url <- .morie_dataset_build_url(url, query)
  body_str <- jsonlite::toJSON(body, auto_unbox = auto_unbox,
                                 null = "null")
  if (exists(".morie_http_post",
              where = asNamespace("morie"),
              mode = "function")) {
    resp <- .morie_http_post(full_url,
                              body = as.character(body_str),
                              content_type = "application/json",
                              timeout_s = as.integer(timeout_s),
                              headers = as.character(headers))
    if (!nzchar(resp)) {
      stop(sprintf("morie HTTP POST failed (libcurl returned empty body): %s",
                   full_url), call. = FALSE)
    }
    return(jsonlite::fromJSON(resp, simplifyVector = FALSE))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("morie datasets POST needs either the libcurl-backed C++ ",
         "backend or the 'httr2' R package.")
  }
  req <- httr2::request(full_url)
  req <- httr2::req_body_json(req, body, auto_unbox = auto_unbox)
  if (length(headers) > 0L) {
    kv <- strsplit(headers, ":\\s*")
    kv <- Filter(function(x) length(x) == 2L, kv)
    if (length(kv) > 0L) {
      named <- stats::setNames(vapply(kv, `[[`, character(1L), 2L),
                                vapply(kv, `[[`, character(1L), 1L))
      req <- do.call(httr2::req_headers, c(list(req), as.list(named)))
    }
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Binary-safe GET. 3XX: routes through morie's C++ libcurl
#' .morie_http_get_bytes when available; falls back to httr2's
#' resp_body_raw otherwise. Returns a raw vector (no NUL truncation).
#' Used for shapefile zips, FGDB zips, PDFs, KMZ, and any other
#' binary payload the text-returning helpers would corrupt.
#' @keywords internal
#' @noRd
.morie_dataset_http_bytes <- function(url, query = NULL,
                                        headers = character(),
                                        timeout_s = 60L) {
  full_url <- .morie_dataset_build_url(url, query)
  if (exists(".morie_http_get_bytes",
              where = asNamespace("morie"),
              mode = "function")) {
    return(.morie_http_get_bytes(full_url,
                                  timeout_s = as.integer(timeout_s),
                                  headers = as.character(headers)))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("morie datasets binary fetch needs either the libcurl-backed ",
         "C++ backend (built into morie.so via src/morie_http.cpp) ",
         "or the 'httr2' R package.")
  }
  req <- httr2::request(full_url)
  if (length(headers) > 0L) {
    kv <- strsplit(headers, ":\\s*")
    kv <- Filter(function(x) length(x) == 2L, kv)
    if (length(kv) > 0L) {
      named <- stats::setNames(vapply(kv, `[[`, character(1L), 2L),
                                vapply(kv, `[[`, character(1L), 1L))
      req <- do.call(httr2::req_headers, c(list(req), as.list(named)))
    }
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_raw(resp)
}

#' GET + parse JSON. 3VV: when the C++ backend is available the
#' body is fetched via libcurl then parsed via jsonlite (avoids the
#' httr2 dep entirely); falls back to httr2 + httr2::resp_body_json
#' otherwise.
#' @keywords internal
#' @noRd
.morie_dataset_http_json <- function(url, query = NULL,
                                       headers = character(),
                                       timeout_s = 60L) {
  full_url <- .morie_dataset_build_url(url, query)
  if (.morie_dataset_http_backend_cpp()) {
    body <- .morie_http_get(full_url,
                             timeout_s = as.integer(timeout_s),
                             headers = as.character(headers))
    if (!nzchar(body)) {
      stop(sprintf("morie HTTP fetch failed (libcurl returned empty body): %s",
                   full_url), call. = FALSE)
    }
    return(jsonlite::fromJSON(body, simplifyVector = TRUE))
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("morie datasets HTTP fetch needs either the libcurl-backed ",
         "C++ backend (built into morie.so via src/morie_http.cpp) ",
         "or the 'httr2' R package.")
  }
  req <- httr2::request(full_url)
  if (length(headers) > 0L) {
    kv <- strsplit(headers, ":\\s*")
    kv <- Filter(function(x) length(x) == 2L, kv)
    if (length(kv) > 0L) {
      named <- stats::setNames(vapply(kv, `[[`, character(1L), 2L),
                                vapply(kv, `[[`, character(1L), 1L))
      req <- do.call(httr2::req_headers, c(list(req), as.list(named)))
    }
  }
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Convert a list-of-records / data.frame response into a clean data.frame.
#' @keywords internal
#' @noRd
.morie_dataset_records_to_df <- function(records) {
  if (is.data.frame(records)) {
    return(records)
  }
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  do.call(rbind, lapply(records, function(r) {
    as.data.frame(lapply(r, function(v) if (is.null(v)) NA else v),
                  stringsAsFactors = FALSE)
  }))
}

# ---------------------------------------------------------------------------
# TPS -- Toronto Police Service ArcGIS
# ---------------------------------------------------------------------------

#' Default TPS ArcGIS layer registry (verified 2026-05).
#' @keywords internal
#' @noRd
.MORIE_TPS_LAYER_REGISTRY <- list(
  `major-crime` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Major_Crime_Indicators_Open_Data/FeatureServer/0"
  ),
  `shooting-firearms` = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Shooting_and_Firearm_Discharges_Open_Data/FeatureServer/0"
  ),
  homicide = paste0(
    "https://services.arcgis.com/S9th0jAJ7bqgIRjw/arcgis/rest/services/",
    "Homicides_Open_Data_ASR_RC_TBL_002/FeatureServer/0"
  )
)

#' Fetch a TPS ArcGIS FeatureServer layer as a data frame.
#' @keywords internal
#' @noRd
.morie_dataset_tps_fetch <- function(layer_url, where = "1=1",
                                     max_features = NULL,
                                     return_geometry = FALSE) {
  query <- list(
    where = where,
    outFields = "*",
    f = "json",
    returnGeometry = if (isTRUE(return_geometry)) "true" else "false"
  )
  if (!is.null(max_features)) {
    query$resultRecordCount <- as.integer(max_features)
  }
  body <- .morie_dataset_http_json(paste0(layer_url, "/query"), query = query)
  features <- body$features
  if (is.null(features) || length(features) == 0L) {
    return(data.frame())
  }
  attrs <- lapply(features, function(f) f$attributes)
  df <- .morie_dataset_records_to_df(attrs)
  if (isTRUE(return_geometry)) {
    geoms <- lapply(features, function(f) f$geometry)
    df$geom_x <- vapply(geoms, function(g) if (is.null(g$x)) NA_real_ else g$x,
                       numeric(1))
    df$geom_y <- vapply(geoms, function(g) if (is.null(g$y)) NA_real_ else g$y,
                       numeric(1))
  }
  df
}

#' TPS Major Crime Indicators feed.
#'
#' @param year Integer or `NULL`.  If set, filter to `OCC_YEAR == year`
#'   server-side.
#' @param max_features Integer or `NULL`; cap on returned rows.
#' @param include_geometry Logical; include `geom_x` / `geom_y`.
#' @param offline Logical; return the bundled synthetic frame instead
#'   of hitting the live ArcGIS endpoint.
#' @return A `data.frame` with the documented TPS schema.
#' @export
morie_datasets_tps_major_crime <- function(year = NULL,
                                           max_features = NULL,
                                           include_geometry = FALSE,
                                           offline = FALSE) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic("tps_major_crime", "tps_major_crime")
    if (!is.null(year) && "OCC_YEAR" %in% colnames(df)) {
      df <- df[df$OCC_YEAR == year, , drop = FALSE]
      rownames(df) <- NULL
    }
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  .morie_dataset_tps_fetch(
    .MORIE_TPS_LAYER_REGISTRY[["major-crime"]],
    where = .morie_dataset_year_where(year),
    max_features = max_features,
    return_geometry = include_geometry
  )
}

#' TPS Shootings and Firearm Discharges feed.
#'
#' @inheritParams morie_datasets_tps_major_crime
#' @return A `data.frame`.
#' @export
morie_datasets_tps_shootings <- function(year = NULL, max_features = NULL) {
  .morie_dataset_tps_fetch(
    .MORIE_TPS_LAYER_REGISTRY[["shooting-firearms"]],
    where = .morie_dataset_year_where(year),
    max_features = max_features
  )
}

#' TPS Homicides feed.
#'
#' @inheritParams morie_datasets_tps_major_crime
#' @return A `data.frame`.
#' @export
morie_datasets_tps_homicide <- function(year = NULL, max_features = NULL) {
  .morie_dataset_tps_fetch(
    .MORIE_TPS_LAYER_REGISTRY[["homicide"]],
    where = .morie_dataset_year_where(year),
    max_features = max_features
  )
}

#' List the TPS open-data layers bundled with morie.
#'
#' @return A `data.frame` with columns `name` and `url`.
#' @export
morie_datasets_tps_layers <- function() {
  data.frame(
    name = names(.MORIE_TPS_LAYER_REGISTRY),
    url = unlist(unname(.MORIE_TPS_LAYER_REGISTRY)),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# CPADS / OTIS
# ---------------------------------------------------------------------------

#' Load the Canadian Postsecondary Alcohol and Drug-use Survey (CPADS)
#'
#' Resolves a CPADS analysis frame from one of three sources:
#'
#' \enumerate{
#'   \item A pre-wrangled local RDS at `morie_cpads_contract()$expected_wrangled_path`
#'         (only useful if you've already produced one), OR
#'   \item the bundled 30-row real PUMF sample at
#'         `inst/extdata/cpads_pumf_synthetic.csv` (when `offline = TRUE`,
#'         the default; carries all 394 raw PUMF columns plus the 11
#'         morie canonical analysis aliases), OR
#'   \item the live PUMF via the CKAN
#'         \href{https://docs.ckan.org/en/latest/maintaining/datastore.html#ckanext.datastore.logic.action.datastore_search}{datastore_search}
#'         API (default, supports \code{limit}/\code{q}) or the full
#'         PUMF CSV (with \code{mode = "csv"}):
#'         \url{https://open.canada.ca/data/dataset/736fa9b2-62e4-4e31-aea4-51869605b363/resource/d2639429-c304-45a6-90b3-770562f4d46d/download/cpads-2021-2022-pumf2.csv}
#'         (when `offline = FALSE`).
#' }
#'
#' CPADS is open data published by Health Canada / Statistics Canada
#' (Open Government Licence -- Canada). Aggregate dashboards at
#' \url{https://health-infobase.canada.ca/substance-use/reports/cpads/};
#' PUMF user guide:
#' \url{https://open.canada.ca/data/dataset/736fa9b2-62e4-4e31-aea4-51869605b363/resource/a078e4c3-a910-4349-b00e-6ea0d31d391d/download/20212022-cpads-pumf-user-guide.pdf}.
#' Sister surveys (CSADS, CSUS, CTADS) are at
#' \url{https://health-infobase.canada.ca/substance-use/}.
#'
#' @param offline Logical. `TRUE` (default) prefers the bundled fixture
#'   for fast/CRAN-safe runs; `FALSE` fetches the live PUMF.
#' @param mode Character; one of `"datastore_search"` (default;
#'   queryable CKAN datastore endpoint, supports `limit`/`q`) or
#'   `"csv"` (whole PUMF CSV download). Ignored when `offline = TRUE`.
#' @param limit Integer or `NULL`; row cap, forwarded to
#'   `datastore_search` when `mode = "datastore_search"`. `NULL`
#'   returns the datastore default (100 rows).
#' @param q Character or `NULL`; free-text query forwarded to
#'   `datastore_search` (e.g. `q = "title:jones"`). Ignored for
#'   `mode = "csv"`.
#' @return A `data.frame` carrying every CPADS PUMF column plus
#'   morie's canonical analysis aliases (`weight`,
#'   `alcohol_past12m`, `heavy_drinking_30d`, `ebac_tot`,
#'   `ebac_legal`, `cannabis_any_use`, `age_group`, `gender`,
#'   `province_region`, `mental_health`, `physical_health`).
#' @seealso [morie_cpads_contract()] for the canonical schema +
#'   column map; [morie_datasets_load_by_key()] for catalog-wide
#'   dispatch.
#' @export
morie_datasets_cpads <- function(offline = TRUE,
                                 mode = c("datastore_search", "csv"),
                                 limit = NULL,
                                 q = NULL) {
  mode <- match.arg(mode)
  contract <- morie_cpads_contract()
  if (file.exists(contract$expected_wrangled_path)) {
    frame <- readRDS(contract$expected_wrangled_path)
    return(morie_cpads_canonicalize_frame(frame))
  }
  if (isTRUE(offline)) {
    return(.morie_dataset_read_synthetic("cpads_pumf_synthetic", "cpads"))
  }
  raw <- if (identical(mode, "datastore_search")) {
    query <- list(
      resource_id = "d2639429-c304-45a6-90b3-770562f4d46d"
    )
    if (!is.null(limit)) query$limit <- as.integer(limit)
    if (!is.null(q)) query$q <- as.character(q)
    body <- .morie_dataset_http_json(
      "https://open.canada.ca/data/api/3/action/datastore_search",
      query = query
    )
    records <- body$result$records %||% body$records %||% body
    .morie_dataset_records_to_df(records)
  } else {
    morie_fetch(paste0(
      "https://open.canada.ca/data/dataset/",
      "736fa9b2-62e4-4e31-aea4-51869605b363/resource/",
      "d2639429-c304-45a6-90b3-770562f4d46d/download/",
      "cpads-2021-2022-pumf2.csv"
    ), format = "csv")
  }
  if (morie_cpads_has_raw_columns(raw)) {
    morie_cpads_canonicalize_frame(raw)
  } else {
    raw
  }
}

#' Load the OTIS A01 Restrictive-Confinement Detailed Dataset
#'
#' Thin compatibility shim that delegates to
#' [morie_datasets_otis_a01_restrictive_confinement()]. The OTIS A01
#' dataset is published openly at
#' \url{https://data.ontario.ca/dataset/data-on-inmates-in-ontario}
#' (Ontario Solicitor General; Open Government Licence -- Ontario,
#' CKAN resource id \code{5a0c5804-a055-4031-9743-73f556e43bb4}).
#'
#' Earlier morie versions wrongly claimed this data was FOI-only;
#' that was incorrect and has been retracted as of 3MMM.
#'
#' @param offline Logical. `TRUE` (default) reads the bundled
#'   `otis_a01_restrictive_confinement_sample.csv` fixture. `FALSE`
#'   fetches the live CKAN dataset.
#' @param ... Forwarded to
#'   [morie_datasets_otis_a01_restrictive_confinement()].
#' @return A `data.frame`.
#' @seealso [morie_datasets_otis_a01_restrictive_confinement()],
#'   [morie_datasets_load_by_key()].
#' @export
morie_datasets_otis_a01 <- function(offline = TRUE, ...) {
  morie_datasets_otis_a01_restrictive_confinement(offline = offline, ...)
}

# ---------------------------------------------------------------------------
# SIU -- Special Investigations Unit director's reports
# ---------------------------------------------------------------------------

#' SIU director's-reports index (legacy PDF anchors).
#'
#' The SIU re-launched their site in 2025 with a JS-rendered case list;
#' this returns the legacy-pattern anchor frame which may be empty.
#'
#' @return A `data.frame` with columns `case_number`, `url`, `posted_date`.
#' @export
morie_datasets_siu_director_reports <- function() {
  if (!requireNamespace("rvest", quietly = TRUE) ||
      !requireNamespace("xml2", quietly = TRUE)) {
    warning("morie_datasets_siu_director_reports: 'rvest' + 'xml2' required for live scrape.",
            call. = FALSE)
    return(data.frame(case_number = character(),
                      url = character(),
                      posted_date = as.Date(character()),
                      stringsAsFactors = FALSE))
  }
  page <- xml2::read_html("https://www.siu.on.ca/en/directors_reports.php")
  anchors <- rvest::html_elements(page, "a[href$='.pdf']")
  urls <- rvest::html_attr(anchors, "href")
  if (length(urls) == 0L) {
    return(data.frame(case_number = character(),
                      url = character(),
                      stringsAsFactors = FALSE))
  }
  case_re <- "([0-9]{2}-[A-Z]{2,4}-[0-9]{3,4})"
  m <- regmatches(urls, regexpr(case_re, urls))
  data.frame(
    case_number = ifelse(nchar(m) > 0L, m, NA_character_),
    url = urls,
    stringsAsFactors = FALSE
  )
}

#' Download an SIU director's-report PDF and return its plain text.
#'
#' @param url Character; direct PDF URL.  Required unless `offline = TRUE`.
#' @param offline Logical; if `TRUE`, return the bundled synthetic
#'   `24-OFD-001` report text instead of hitting the SIU site.
#' @return Character scalar (the plain text).
#' @export
morie_datasets_siu_report_text <- function(url = NULL, offline = FALSE) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "siu_24-OFD-001_synthetic.txt",
                        package = "morie")
    if (!nzchar(path)) {
      stop("morie_datasets_siu_report_text(offline=TRUE): bundled synthetic missing.")
    }
    return(paste(readLines(path, warn = FALSE), collapse = "\
"))
  }
  if (is.null(url)) {
    stop("morie_datasets_siu_report_text: provide url=... or offline=TRUE")
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("morie_datasets_siu_report_text: 'pdftools' is required to extract PDF text.")
  }
  tmp <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp), add = TRUE)
  # 3XX: prefer the libcurl backend (.morie_dataset_http_bytes) +
  # fall back to httr2 if the C++ symbol is unavailable. Both paths
  # write raw bytes to a tempfile that pdftools then parses.
  writeBin(.morie_dataset_http_bytes(url), tmp)
  paste(pdftools::pdf_text(tmp), collapse = "\
")
}

#' Extract structured fields from an SIU director's-report text or URL.
#'
#' @param text_or_url Character scalar; either the report text (re-used)
#'   or a PDF URL (fetched and parsed first).
#' @return A named list with fields `report_id`, `incident_date`,
#'   `conclusion`, `sections`.
#' @export
morie_datasets_siu_report_fields <- function(text_or_url) {
  text <- text_or_url
  if (grepl("^https?://", text_or_url)) {
    text <- morie_datasets_siu_report_text(text_or_url)
  }
  report_id_m <- regmatches(
    text, regexpr("[0-9]{2}-[A-Z]{2,4}-[0-9]{3,4}", text)
  )
  date_m <- regmatches(
    text,
    regexpr(
      "(January|February|March|April|May|June|July|August|September|October|November|December)\\s+[0-9]{1,2},\\s*[0-9]{4}",
      text
    )
  )
  conclusion_m <- regmatches(
    text,
    regexpr("(?si)Director'?s\\s+Decision.*?(?=(Issued|Dated|$))", text, perl = TRUE)
  )
  list(
    report_id = if (length(report_id_m)) report_id_m else NA_character_,
    incident_date = if (length(date_m)) date_m else NA_character_,
    conclusion = if (length(conclusion_m)) conclusion_m else NA_character_,
    sections = strsplit(text, "\
\\s*\
")[[1]]
  )
}

# ---------------------------------------------------------------------------
# Socrata -- Chicago + NYC OpenData
# ---------------------------------------------------------------------------

#' Documented Socrata column schema for Chicago Crimes (ijzp-q8t2).
#' @keywords internal
#' @noRd
.MORIE_CHICAGO_CRIME_COLUMNS <- c(
  "id", "case_number", "date", "block", "iucr", "primary_type",
  "description", "location_description", "arrest", "domestic",
  "beat", "district", "ward", "community_area", "fbi_code",
  "x_coordinate", "y_coordinate", "year", "updated_on",
  "latitude", "longitude", "location"
)

#' Generic Socrata JSON fetch (handles `$where`, `$limit`, `$$app_token`).
#'
#' Two modes:
#'   * `paginate = FALSE` (default): single SODA2 request. Honours
#'     `$where`, `$limit = max_features` and `$$app_token`. Without an
#'     explicit `$limit` the server enforces its default cap (1,000
#'     rows on the open NYC + Chicago portals).
#'   * `paginate = TRUE`: walk `$offset` in `page_size` chunks until
#'     the server returns a short page (exhausted) or `max_features`
#'     is reached. `max_pages` is a safety net against runaway pulls.
#'     Without an app_token most portals cap `$limit` at 1,000 per
#'     request, so the natural `page_size` is 1000; with a token the
#'     hard server-side cap is 50,000.
#'
#' @keywords internal
#' @noRd
.morie_dataset_socrata_fetch <- function(url, where = NULL,
                                         max_features = NULL,
                                         app_token = NULL,
                                         paginate = FALSE,
                                         page_size = 1000L,
                                         max_pages = 200L,
                                         select = NULL) {
  if (!isTRUE(paginate)) {
    query <- list()
    if (!is.null(where)) query$`$where` <- where
    if (!is.null(select)) query$`$select` <- select
    if (!is.null(max_features)) query$`$limit` <- as.integer(max_features)
    if (!is.null(app_token)) query$`$$app_token` <- app_token
    records <- .morie_dataset_http_json(url, query = query)
    return(.morie_dataset_records_to_df(records))
  }
  page_size <- as.integer(page_size)
  if (is.na(page_size) || page_size <= 0L) {
    stop(".morie_dataset_socrata_fetch: paginate=TRUE requires page_size >= 1.",
         call. = FALSE)
  }
  max_pages <- as.integer(max_pages)
  if (is.na(max_pages) || max_pages <= 0L) max_pages <- 200L
  cap <- if (is.null(max_features)) NA_integer_ else as.integer(max_features)
  total <- 0L
  offset <- 0L
  chunks <- vector("list", 0L)
  for (pg in seq_len(max_pages)) {
    this_limit <- if (is.na(cap)) page_size else min(page_size, cap - total)
    if (this_limit <= 0L) break
    query <- list(`$limit` = this_limit, `$offset` = offset)
    if (!is.null(where)) query$`$where` <- where
    if (!is.null(select)) query$`$select` <- select
    if (!is.null(app_token)) query$`$$app_token` <- app_token
    records <- .morie_dataset_http_json(url, query = query)
    chunk <- .morie_dataset_records_to_df(records)
    n <- if (is.data.frame(chunk)) nrow(chunk) else 0L
    if (n == 0L) break
    chunks[[length(chunks) + 1L]] <- chunk
    total <- total + n
    offset <- offset + n
    if (n < this_limit) break
  }
  if (length(chunks) == 0L) return(data.frame())
  do.call(rbind, chunks)
}

#' Generic Socrata SODA3 SoQL-query fetch
#'
#' SODA2 (`/resource/<id>.<ext>`) exposes datasets via key=value query
#' parameters (`$where`, `$limit`, `$offset`). SODA3
#' (`/api/v3/views/<id>/query.<ext>`) takes a single full SoQL string
#' via `?query=SELECT ... WHERE ... ORDER BY ... LIMIT ... OFFSET ...`.
#' This helper is the SODA3 sibling of [.morie_dataset_socrata_fetch()].
#'
#' Use SODA3 when:
#'   * The dataset is a *filtered view* or *map view* (e.g.
#'     `ahwe-kpsy` "Crimes - Map" derived from `ijzp-q8t2`). SODA2
#'     against these returns `[{}]` -- empty rows -- because column
#'     resolution doesn't fire on derived views.
#'   * You want to send a full SoQL `SELECT ... WHERE ...` with
#'     aggregations / joins / arbitrary expressions that SODA2's
#'     URL-param grammar can't express.
#'
#' Pagination here uses `LIMIT n OFFSET m` clauses baked into the
#' SoQL string (SODA3 does NOT accept `$offset` as a separate query
#' parameter). If the caller's `soql` already contains its own
#' `LIMIT` / `OFFSET`, the loader leaves it alone and treats the
#' result as a single page; pagination mode strips any caller-supplied
#' `LIMIT`/`OFFSET` before walking.
#'
#' @param view_id Socrata view id (UUID `xxxx-xxxx` or publisher
#'   alias e.g. `crimes`).
#' @param soql Raw SoQL string. Default `"SELECT *"`.
#' @param app_token Optional Socrata app token (sent as `X-App-Token`
#'   header, not as `$$app_token` query param -- SODA3 prefers the
#'   header form).
#' @param paginate Logical; if `TRUE`, walk `LIMIT page_size OFFSET m`
#'   pages until exhausted.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net on paginated walks.
#' @param max_features Optional total cap across all paged requests.
#' @param base_url Portal base URL.  Default
#'   `"https://data.cityofchicago.org"`.
#' @note SODA3 (`/api/v3/views/<id>/query.{json,csv}`) requires
#'   authentication per Socrata's API documentation
#'   (\url{https://support.socrata.com/hc/en-us/articles/34730618169623-SODA3-API}).
#'   Pass an `app_token` (Socrata application token) for higher
#'   per-host rate limits and to satisfy authenticated endpoints.
#' @keywords internal
#' @noRd
.morie_dataset_soda3_query <- function(view_id, soql = "SELECT *",
                                        app_token = NULL,
                                        paginate = FALSE,
                                        page_size = 1000L,
                                        max_pages = 200L,
                                        max_features = NULL,
                                        base_url = "https://data.cityofchicago.org") {
  # 3WW: routes through .morie_dataset_http_json (libcurl backend
  # from 3VV with httr2 fallback) instead of calling httr2 directly.
  # Single HTTP code path for SODA2 + SODA3 + raw text fetches.
  url <- sprintf("%s/api/v3/views/%s/query.json", base_url, view_id)
  headers <- if (is.null(app_token)) {
    character()
  } else {
    paste0("X-App-Token: ", app_token)
  }
  send_one <- function(soql_string) {
    records <- .morie_dataset_http_json(
      url,
      query = list(query = soql_string),
      headers = headers)
    .morie_dataset_records_to_df(records)
  }
  if (!isTRUE(paginate)) {
    return(send_one(soql))
  }
  # Paginated mode: strip caller-supplied LIMIT / OFFSET so we can
  # control the walk ourselves. Use case-insensitive trailing match;
  # we don't try to be clever about embedded LIMIT inside subqueries.
  base_soql <- sub("\\s+limit\\s+\\d+\\s*(offset\\s+\\d+\\s*)?$",
                    "", soql, ignore.case = TRUE)
  page_size <- as.integer(page_size)
  max_pages <- as.integer(max_pages)
  cap <- if (is.null(max_features)) NA_integer_ else as.integer(max_features)
  total <- 0L
  offset <- 0L
  chunks <- vector("list", 0L)
  for (pg in seq_len(max_pages)) {
    this_limit <- if (is.na(cap)) page_size else min(page_size, cap - total)
    if (this_limit <= 0L) break
    paged_soql <- sprintf("%s LIMIT %d OFFSET %d",
                           base_soql, this_limit, offset)
    chunk <- send_one(paged_soql)
    n <- if (is.data.frame(chunk)) nrow(chunk) else 0L
    if (n == 0L) break
    chunks[[length(chunks) + 1L]] <- chunk
    total <- total + n
    offset <- offset + n
    if (n < this_limit) break
  }
  if (length(chunks) == 0L) return(data.frame())
  do.call(rbind, chunks)
}

#' Generic Socrata OData v4 fetch
#'
#' Socrata exposes every dataset (base + filtered + map) under a
#' third API mode in addition to SODA2 + SODA3 -- **OData v4** at
#' `/api/odata/v4/<view_id>`. OData is the protocol Tableau, Power
#' BI, Excel, and other BI tools speak natively; reaching it from
#' R is mostly useful when you want morie to ingest the same way
#' those tools do, or to use OData-specific features like
#' server-driven `@odata.nextLink` pagination.
#'
#' Coverage parity vs the other two API modes:
#'   * **Base datasets** (e.g. `ijzp-q8t2`): all three modes work.
#'   * **Derived / map / filtered views** (e.g. `ahwe-kpsy`): OData
#'     returns `value: [{}]` (empty objects) -- same failure mode as
#'     SODA2. Use SODA3 ([.morie_dataset_soda3_query()]) for these.
#'
#' **Known Socrata limitation -- `$filter`**. As of 2026-05 Socrata's
#' OData parser frequently rejects equality filters with
#' `"The types 'Edm.Boolean' and 'Edm.String' (or 'Edm.Decimal') are
#' not compatible."` -- the filter expression tree is mis-typed
#' inside Socrata's engine. `$top`, `$skip`, `$select`, and
#' `$orderby` all work reliably. If you need filtering, prefer SODA3
#' (`morie_datasets_chicago_crime_soql`) or SODA2's `$where`
#' (`morie_datasets_chicago_crime`).
#'
#' Pagination:
#'   * `paginate = TRUE` follows the server-provided
#'     `@odata.nextLink` until it disappears (Socrata returns a
#'     nextLink whenever there are more rows; absence == exhausted).
#'   * `max_features` caps the total row count across pages.
#'   * `max_pages` is the safety net.
#'
#' Each response also carries `@odata.id` / `@odata.context` /
#' `@odata.metadata` metadata fields per row; these are stripped
#' from the returned data.frame so columns match the dataset schema.
#'
#' @param view_id Socrata view id (UUID `xxxx-xxxx` or publisher alias).
#' @param filter Optional OData `$filter` string (caller-supplied
#'   verbatim). See limitation above.
#' @param select Optional OData `$select` (comma-separated column
#'   list); defaults to `NULL` = all columns.
#' @param orderby Optional OData `$orderby`.
#' @param top Optional integer `$top` (per-request row count when
#'   `paginate = FALSE`; per-page row count when `paginate = TRUE`).
#'   `NULL` accepts server default.
#' @param skip Optional integer `$skip` (start offset, mostly useful
#'   when `paginate = FALSE`).
#' @param app_token Optional Socrata app token (sent as `X-App-Token`).
#' @param paginate Logical; if `TRUE`, follow `@odata.nextLink`.
#' @param max_pages Safety net on paginated walks.
#' @param max_features Optional total row cap.
#' @param base_url Portal base URL. Default Chicago.
#' @keywords internal
#' @noRd
.morie_dataset_odata_fetch <- function(view_id, filter = NULL,
                                        select = NULL,
                                        orderby = NULL,
                                        top = NULL, skip = NULL,
                                        app_token = NULL,
                                        paginate = FALSE,
                                        max_pages = 200L,
                                        max_features = NULL,
                                        base_url = "https://data.cityofchicago.org") {
  # 3WW: routes through .morie_dataset_http_json (libcurl backend
  # with httr2 fallback) instead of calling httr2 directly. Single
  # HTTP code path across all four Socrata API modes (SODA2 SODA3
  # OData + raw text) + the ArcGIS-Hub catalog discovery layer.
  strip_odata_meta <- function(df) {
    if (!is.data.frame(df) || ncol(df) == 0L) return(df)
    drop <- grepl("^@odata\\.", names(df))
    if (any(drop)) df[, !drop, drop = FALSE] else df
  }
  headers <- if (is.null(app_token)) {
    character()
  } else {
    paste0("X-App-Token: ", app_token)
  }
  send <- function(url, query = NULL) {
    .morie_dataset_http_json(url, query = query, headers = headers)
  }
  build_query <- function() {
    q <- list()
    if (!is.null(filter)) q$`$filter` <- filter
    if (!is.null(select)) q$`$select` <- select
    if (!is.null(orderby)) q$`$orderby` <- orderby
    if (!is.null(top)) q$`$top` <- as.integer(top)
    if (!is.null(skip)) q$`$skip` <- as.integer(skip)
    q
  }
  url0 <- sprintf("%s/api/odata/v4/%s", base_url, view_id)
  if (!isTRUE(paginate)) {
    body <- send(url0, build_query())
    df <- .morie_dataset_records_to_df(body$value)
    return(strip_odata_meta(df))
  }
  # Paginated mode: follow @odata.nextLink until absent / empty /
  # max_features / max_pages. The nextLink is a fully-formed URL
  # so we send it with no extra query parameters.
  cap <- if (is.null(max_features)) NA_integer_ else as.integer(max_features)
  max_pages <- as.integer(max_pages)
  chunks <- vector("list", 0L)
  total <- 0L
  body <- send(url0, build_query())
  for (pg in seq_len(max_pages)) {
    chunk <- strip_odata_meta(.morie_dataset_records_to_df(body$value))
    n <- if (is.data.frame(chunk)) nrow(chunk) else 0L
    if (n == 0L) break
    # Truncate the last chunk if it would push us past cap.
    if (!is.na(cap) && total + n > cap) {
      chunk <- chunk[seq_len(cap - total), , drop = FALSE]
      n <- nrow(chunk)
    }
    chunks[[length(chunks) + 1L]] <- chunk
    total <- total + n
    if (!is.na(cap) && total >= cap) break
    next_link <- body[["@odata.nextLink"]]
    if (is.null(next_link) || !nzchar(next_link)) break
    body <- send(next_link, query = NULL)
  }
  if (length(chunks) == 0L) return(data.frame())
  do.call(rbind, chunks)
}

#' City of Chicago "Crimes -- 2001 to Present" feed (`ijzp-q8t2`)
#'
#' Wraps the City of Chicago "Crimes -- 2001 to Present" open dataset
#' (Socrata resource id `ijzp-q8t2`; portal landing
#' \url{https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2/about_data}).
#' 22-column schema, one row per reported crime incident (except
#' murders, where one row per victim). Data are extracted from the
#' Chicago PD CLEAR system, refreshed daily with a seven-day lag,
#' and addresses are block-only redacted.
#'
#' **Scale warning.** As of 2026-05 the live feed carries ~8,557,071
#' rows (8.56M; last refreshed 2026-05-23) -- too large for
#' spreadsheet programs and slow even for programmatic pulls without
#' filtering. Always prefer narrowing the query first (`year = ...`
#' server-side filter) or paginating with `paginate = TRUE` + a
#' large `page_size` (and ideally an app_token). A full unfiltered
#' pull at the default `page_size = 1000` would issue ~8,560
#' requests; with `page_size = 50000` + an app_token it drops to
#' ~172.
#'
#' Socrata accepts both the numeric id (`/resource/ijzp-q8t2.json`)
#' and the publisher's `crimes` alias (`/resource/crimes.json`).
#' SODA3 endpoints are also available
#' (`/api/v3/views/crimes/query.json`), as are CSV variants
#' (`/resource/crimes.csv`, `/api/v3/views/crimes/query.csv`).
#' morie defaults to SODA2 JSON via the UUID for stability.
#'
#' **Cross-referenced datasets (Chicago Open Data).** The 22-col
#' schema carries geographic and crime-classification foreign keys
#' that other Chicago datasets resolve:
#'
#' \describe{
#'   \item{beat}{morie wraps via
#'     [morie_datasets_chicago_police_beats()] (`n9it-hstw`).}
#'   \item{district}{morie wraps via
#'     [morie_datasets_chicago_police_districts()] (`24zt-jpfn`).}
#'   \item{ward}{morie wraps via
#'     [morie_datasets_chicago_wards()] (`sp34-6z76`, 3UU).}
#'   \item{community_area}{morie wraps via
#'     [morie_datasets_chicago_community_areas()] (`cauq-8yn6`, 3UU).}
#'   \item{iucr / fbi_code}{morie wraps via
#'     [morie_datasets_chicago_iucr_codes()] (`c7ck-438e`, 3UU).}
#' }
#'
#' @param year Integer or `NULL`; server-side year filter.
#' @param max_features Integer or `NULL`; cap on returned rows. When
#'   `paginate = TRUE` this is the total cap across all walked pages.
#' @param offline Logical; if `TRUE`, return the bundled synthetic frame.
#' @param mode One of `"soda2"` (default) or `"soda3"`. Selects the
#'   API path for live mode:
#'   * `"soda2"` -> `/resource/<id>.json?$where=...` via
#'     [.morie_dataset_socrata_fetch()] (URL-param SoQL grammar).
#'   * `"soda3"` -> `/api/v3/views/<id>/query.json?query=SELECT ...`
#'     via [.morie_dataset_soda3_query()] (full SoQL passthrough).
#'   Both modes return the same 22-column schema; SODA3 is required
#'   when a derived/map view is involved (none here, but available
#'   for parity with [morie_datasets_chicago_crime_map()]) and for
#'   the canonical "single SoQL string" experience.
#' @param paginate Logical; if `TRUE` and `offline = FALSE`, walk
#'   pagination in `page_size` chunks. SODA2 uses `$offset`; SODA3
#'   uses `LIMIT page_size OFFSET m` baked into the SoQL.
#' @param page_size Integer; per-page row count when `paginate = TRUE`.
#'   Default 1,000 (the unauthenticated SODA2 ceiling).
#' @param max_pages Integer; safety net on `paginate = TRUE` walks
#'   (default 200 -> up to 200,000 rows without an app_token).
#' @param app_token Optional Socrata app token (SODA3 only -- sent
#'   as the `X-App-Token` header; ignored under `mode = "soda2"`).
#' @return A `data.frame` with the documented Socrata schema.
#' @export
morie_datasets_chicago_crime <- function(year = NULL,
                                         max_features = NULL,
                                         offline = TRUE,
                                         mode = c("soda2", "soda3"),
                                         paginate = FALSE,
                                         page_size = 1000L,
                                         max_pages = 200L,
                                         app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic(
      "chicago_crime_synthetic", "chicago_crime",
      columns = .MORIE_CHICAGO_CRIME_COLUMNS
    )
    if (!is.null(year) && "year" %in% colnames(df) && nrow(df) > 0L) {
      df <- df[df$year == year, , drop = FALSE]
      rownames(df) <- NULL
    }
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (mode == "soda2") {
    where <- if (is.null(year)) NULL else sprintf("year=%d", as.integer(year))
    return(.morie_dataset_socrata_fetch(
      "https://data.cityofchicago.org/resource/ijzp-q8t2.json",
      where = where,
      max_features = max_features,
      paginate = paginate,
      page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3": route via SoQL passthrough.
  soql <- if (is.null(year)) {
    "SELECT *"
  } else {
    sprintf("SELECT * WHERE year=%d", as.integer(year))
  }
  .morie_dataset_soda3_query(
    "ijzp-q8t2", soql = soql,
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features)
}

#' NYC OpenData SQF resource map (verified 2026-05).
#' @keywords internal
#' @noRd
.MORIE_NYC_SQF_RESOURCES <- list(
  `2024` = "https://data.cityofnewyork.us/resource/7v9w-k82r.json",
  `2023` = "https://data.cityofnewyork.us/resource/rbed-zzin.json",
  `2022` = "https://data.cityofnewyork.us/resource/e4yi-bvqr.json"
)

#' NYPD Stop, Question and Frisk (SQF) microdata via NYC OpenData.
#'
#' @param year Integer or `NULL`; release year (one of 2022, 2023, 2024).
#'   `NULL` defaults to the most-recent registered year.
#' @param max_features Integer or `NULL`; cap on returned rows. When
#'   `paginate = TRUE` this is the total cap across all walked pages.
#' @param offline Logical; if `TRUE`, return the bundled synthetic frame.
#' @param paginate Logical; if `TRUE` and `offline = FALSE`, walk
#'   SODA2 `$offset` in `page_size` chunks. Default `FALSE`.
#' @param page_size Integer; per-page row count when paginating
#'   (default 1,000, the unauthenticated SODA2 ceiling).
#' @param max_pages Integer; safety net on paginated walks (default 200).
#' @return A `data.frame`.  Schema is NOT normalised across years.
#' @export
morie_datasets_nyc_stop_and_frisk <- function(year = NULL,
                                              max_features = NULL,
                                              offline = TRUE,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic("nyc_sqf_synthetic", "nyc_stop_and_frisk")
    if (!is.null(max_features) && nrow(df) > 0L) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  known_years <- as.integer(names(.MORIE_NYC_SQF_RESOURCES))
  chosen_year <- if (is.null(year)) max(known_years) else as.integer(year)
  if (!(chosen_year %in% known_years)) {
    stop(sprintf(
      paste0(
        "morie_datasets_nyc_stop_and_frisk: no built-in NYC OpenData ",
        "resource for year=%d.  Known years: %s."
      ),
      chosen_year, paste(sort(known_years), collapse = ", ")
    ))
  }
  .morie_dataset_socrata_fetch(
    .MORIE_NYC_SQF_RESOURCES[[as.character(chosen_year)]],
    max_features = max_features,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages
  )
}

# ---------------------------------------------------------------------------
# BigQuery -- thin wrapper (optional dep: bigrquery)
# ---------------------------------------------------------------------------

#' Pull a BigQuery table (or filtered slice) as a `data.frame`.
#'
#' Requires the `bigrquery` package and Application Default Credentials.
#'
#' @param project Source project (e.g. `"bigquery-public-data"`).
#' @param dataset Source dataset (e.g. `"chicago_crime"`).
#' @param table Source table (e.g. `"crime"`).
#' @param where Raw SQL WHERE clause (without leading `WHERE`).
#' @param limit Optional integer `LIMIT`.
#' @param select Projection list; defaults to `"*"`.
#' @param billing_project GCP project to bill; `NULL` uses ADC-discovered.
#' @return A `data.frame`.
#' @export
morie_datasets_bigquery <- function(project, dataset, table,
                                    where = NULL, limit = NULL,
                                    select = "*", billing_project = NULL) {
  if (!requireNamespace("bigrquery", quietly = TRUE)) {
    stop("morie_datasets_bigquery: install 'bigrquery' to use this loader.")
  }
  sql <- sprintf("SELECT %s FROM `%s.%s.%s`", select, project, dataset, table)
  if (!is.null(where)) sql <- paste(sql, "WHERE", where)
  if (!is.null(limit)) sql <- paste(sql, "LIMIT", as.integer(limit))
  bill <- if (is.null(billing_project)) project else billing_project
  bigrquery::bq_table_download(bigrquery::bq_project_query(bill, sql))
}

# ---------------------------------------------------------------------------
# CKAN -- generic open-data portal helpers
# ---------------------------------------------------------------------------

#' Search a CKAN open-data portal by free-text query.
#'
#' Examples: `"https://open.canada.ca/data"`, `"https://data.ontario.ca"`,
#' `"https://data.gov.uk"`, `"https://data.europa.eu"`.
#'
#' @param portal Character; base URL of the CKAN portal.
#' @param query Character; free-text search.
#' @param rows Integer; max packages to return (default 50).
#' @return A `data.frame` of package metadata.
#' @export
morie_datasets_ckan_search <- function(portal, query, rows = 50L) {
  url <- paste0(sub("/$", "", portal), "/api/3/action/package_search")
  body <- .morie_dataset_http_json(url, query = list(q = query, rows = as.integer(rows)))
  results <- body$result$results
  if (is.null(results) || length(results) == 0L) {
    return(data.frame())
  }
  if (is.data.frame(results)) {
    return(results)
  }
  .morie_dataset_records_to_df(results)
}

#' Pull every CSV resource of a CKAN package as a list of data frames.
#'
#' @param portal Character; CKAN portal base URL.
#' @param package_id Character; CKAN package id or slug.
#' @return Named list mapping `resource_name -> data.frame`.
#' @export
morie_datasets_ckan_package <- function(portal, package_id) {
  url <- paste0(sub("/$", "", portal), "/api/3/action/package_show")
  body <- .morie_dataset_http_json(url, query = list(id = package_id))
  resources <- body$result$resources
  out <- list()
  for (i in seq_along(resources)) {
    res <- resources[[i]]
    fmt <- tolower(as.character(res$format %||% ""))
    if (identical(fmt, "csv") && nzchar(res$url %||% "")) {
      name <- as.character(res$name %||% paste0("resource_", i))
      out[[name]] <- tryCatch(
        utils::read.csv(res$url, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
    }
  }
  out
}

# Null-coalescing helper (avoids dep on rlang).
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# US forensics endpoints -- NIBRS, NamUs, NIST RDS
# ---------------------------------------------------------------------------

#' FBI NIBRS offence-event records via the Crime Data Explorer API.
#'
#' Requires an API key (`api_key=` or `FBI_CDE_API_KEY` env var).
#'
#' @param year Integer; reporting year (required unless `offline = TRUE`).
#' @param max_features Integer or `NULL`; cap on returned rows.
#' @param state Character; two-letter US state code, or `NULL` for national.
#' @param offense Character; NIBRS offence slug, or `NULL` for all.
#' @param api_key Character; FBI CDE API key (or `NULL` -> env var).
#' @param offline Logical; if `TRUE`, return a bundled synthetic frame.
#' @return A `data.frame`.
#' @export
morie_datasets_nibrs <- function(year = NULL, max_features = NULL,
                                 state = NULL, offense = NULL,
                                 api_key = NULL, offline = FALSE) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic(
      "nibrs_synthetic", "nibrs",
      columns = c("ori", "state_abbr", "offense_code", "offense_name",
                  "data_year", "incident_count")
    )
    if (!is.null(max_features) && nrow(df) > 0L) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(year)) {
    stop("morie_datasets_nibrs: year=... is required unless offline=TRUE")
  }
  api_key <- api_key %||% Sys.getenv("FBI_CDE_API_KEY", unset = NA)
  if (is.na(api_key) || !nzchar(api_key)) {
    stop("morie_datasets_nibrs: provide api_key=... or set FBI_CDE_API_KEY env var.")
  }
  base <- "https://api.usa.gov/crime/fbi/cde/nibrs"
  path <- paste(c(base, state, offense, year), collapse = "/")
  body <- .morie_dataset_http_json(path, query = list(API_KEY = api_key))
  df <- .morie_dataset_records_to_df(body$results %||% body)
  if (!is.null(max_features) && nrow(df) > 0L) {
    df <- utils::head(df, as.integer(max_features))
  }
  df
}

#' NamUs missing-persons case metadata.
#'
#' @param state Character; two-letter US state code or `NULL` (national).
#' @param max_features Integer or `NULL`; cap on returned rows.
#' @param offline Logical; if `TRUE`, return a bundled synthetic frame.
#' @return A `data.frame`.
#' @export
morie_datasets_namus_missing_persons <- function(state = NULL,
                                                 max_features = NULL,
                                                 offline = FALSE) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic(
      "namus_missing_persons_synthetic", "namus_missing_persons",
      columns = c("caseNumber", "firstName", "lastName", "state",
                  "city", "county", "ageFrom", "ageTo", "sex",
                  "race", "dateOfLastContact")
    )
    if (!is.null(state) && "state" %in% colnames(df)) {
      df <- df[toupper(as.character(df$state)) == toupper(state), , drop = FALSE]
      rownames(df) <- NULL
    }
    if (!is.null(max_features) && nrow(df) > 0L) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  url <- "https://www.namus.gov/api/CaseSets/NamUs/MissingPersons/Search"
  body <- .morie_dataset_http_json(
    url, query = c(list(take = max_features %||% 200L),
                   if (!is.null(state)) list(state = state) else NULL)
  )
  .morie_dataset_records_to_df(body$results %||% body)
}

#' NIST Reference Datasets (RDS) catalog metadata.
#'
#' @param dataset_id Character or `NULL`; specific NIST RDS id.
#' @param query Character or `NULL`; free-text search.
#' @param max_features Integer or `NULL`; cap on returned rows.
#' @param offline Logical; if `TRUE`, return a bundled synthetic frame.
#' @return A `data.frame` with the NIST RDS catalog schema.
#' @export
morie_datasets_nist_rds <- function(dataset_id = NULL, query = NULL,
                                    max_features = NULL, offline = FALSE) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic("nist_rds_synthetic", "nist_rds")
    if (!is.null(dataset_id) && "dataset_id" %in% colnames(df)) {
      df <- df[as.character(df$dataset_id) == dataset_id, , drop = FALSE]
      rownames(df) <- NULL
    }
    if (!is.null(max_features) && nrow(df) > 0L) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  url <- "https://data.nist.gov/rmm/records"
  q <- list()
  if (!is.null(dataset_id)) q$`@id` <- dataset_id
  if (!is.null(query)) q$searchphrase <- query
  if (!is.null(max_features)) q$size <- as.integer(max_features)
  body <- .morie_dataset_http_json(url, query = q)
  .morie_dataset_records_to_df(body$ResultData %||% body)
}

# ---------------------------------------------------------------------------
# Chicago neighbourhoods (Office of Tourism boundary)
# ---------------------------------------------------------------------------

#' Chicago Neighborhoods boundary (Office of Tourism)
#'
#' Wraps the City of Chicago "Boundaries - Neighborhoods" open dataset
#' (Socrata resource id `y6yq-dbs2`; portal landing
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Neighborhoods/bbvz-uum9}).
#' 98 neighbourhoods, originally derived from Neighborhoods_2012b
#' (updated 2025-02-20). The City notes these boundaries are
#' approximate and the names are not official.
#'
#' Offline mode reads a bundled 98-row attribute-only fixture
#' (`pri_neigh`, `sec_neigh`, `shape_area`, `shape_len`) -- the
#' `the_geom` MultiPolygon column is stripped to keep the bundled
#' size sane (full GeoJSON is ~800 KB). Live mode hits the SODA2
#' endpoint via `.morie_dataset_socrata_fetch()` (mockable).
#'
#' To get the polygons, pass `geometry = TRUE` in live mode, which
#' includes the SODA2 `the_geom` column.
#'
#' @param offline If `TRUE` (default), read the bundled attribute-only
#'   fixture from `inst/extdata/chicago_neighborhoods.csv`.
#' @param geometry If `TRUE` and `offline = FALSE`, include the
#'   `the_geom` MultiPolygon column in the live-mode result.
#' @param max_features Optional cap on returned rows.
#' @param resource_id Optional Socrata resource id override.
#' @return A `data.frame` with 4 attribute columns (offline mode) or
#'   5 cols including `the_geom` (live mode with `geometry = TRUE`).
#' @references City of Chicago Data Portal, "Boundaries -
#'   Neighborhoods"; based on Neighborhoods_2012b.
#' @examples
#' df <- morie_datasets_chicago_neighborhoods(offline = TRUE)
#' head(df[, c("pri_neigh", "sec_neigh")])
#' @export
morie_datasets_chicago_neighborhoods <- function(offline = TRUE,
                                                  geometry = FALSE,
                                                  max_features = NULL,
                                                  resource_id = NULL,
                                                  mode = c("soda2", "soda3"),
                                                  paginate = FALSE,
                                                  page_size = 1000L,
                                                  max_pages = 200L,
                                                  app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata", "chicago_neighborhoods.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago neighborhoods fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "y6yq-dbs2"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofchicago.org/resource/%s.json",
                   resource_id)
    # When the caller doesn't want geometry, ask the server for the
    # attribute subset via $select; this saves the bandwidth + parsing
    # of the (large) MultiPolygon column.
    if (!isTRUE(geometry)) {
      url <- paste0(url,
                    "?$select=pri_neigh,sec_neigh,shape_area,shape_len")
    }
    return(.morie_dataset_socrata_fetch(
      url, max_features = max_features,
      paginate = paginate,
      page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3"
  select_clause <- if (isTRUE(geometry)) {
    "*"
  } else {
    "pri_neigh, sec_neigh, shape_area, shape_len"
  }
  .morie_dataset_soda3_query(
    resource_id,
    soql = sprintf("SELECT %s", select_clause),
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features)
}

# ---------------------------------------------------------------------------
# Chicago Crimes -- OData v4 wrapper (3RR)
# ---------------------------------------------------------------------------

#' City of Chicago Crimes feed via OData v4 (`ijzp-q8t2`)
#'
#' Third Socrata API mode: OData v4 at
#' `/api/odata/v4/<view_id>`, the same protocol Tableau / Power BI /
#' Excel speak natively. Use this when you want morie to consume the
#' Crimes feed the same way those tools do, or when you want
#' server-driven `@odata.nextLink` pagination instead of the
#' client-driven `$offset` walk that SODA2/SODA3 use.
#'
#' When to reach for which API mode:
#'
#' \tabular{lll}{
#'   \strong{Mode}      \tab \strong{morie wrapper}                       \tab \strong{best for} \cr
#'   SODA2              \tab [morie_datasets_chicago_crime()]            \tab base-feed pulls + `$where` filtering \cr
#'   SODA3 (SoQL)       \tab [morie_datasets_chicago_crime_soql()]       \tab arbitrary `SELECT ... WHERE` \cr
#'   SODA3 (map view)   \tab [morie_datasets_chicago_crime_map()]        \tab derived/filtered views (ahwe-kpsy) \cr
#'   OData v4           \tab `morie_datasets_chicago_crime_odata()`      \tab third-party tool ingestion \cr
#' }
#'
#' **Known Socrata limitation.** `$filter` is unreliable on Socrata's
#' OData implementation -- the parser frequently rejects equality
#' filters with `"The types 'Edm.Boolean' and 'Edm.String' (or
#' 'Edm.Decimal') are not compatible."`. `$top` / `$skip` /
#' `$select` / `$orderby` all work; for filtering, use SODA3.
#'
#' @param filter Optional OData `$filter` string (caller-supplied
#'   verbatim; see limitation above).
#' @param select Optional comma-separated column list.
#' @param orderby Optional OData `$orderby`.
#' @param top Optional per-request row count (= `$top`).
#' @param skip Optional start offset (= `$skip`).
#' @param max_features Optional total row cap across pages.
#' @param offline Logical; default `TRUE` reads the bundled 22-col
#'   `chicago_crime_synthetic.csv` fixture.
#' @param resource_id Optional view id override (default
#'   `"ijzp-q8t2"`; pass `"crimes"` for the publisher alias).
#' @param paginate Logical; if `TRUE`, follow `@odata.nextLink`.
#' @param max_pages Safety net on paginated walks.
#' @param app_token Optional Socrata app token (sent as `X-App-Token`).
#' @return A `data.frame`.
#' @references Socrata OData docs:
#'   \url{https://support.socrata.com/hc/en-us/articles/115005364207-Access-Data-Insights-Data-using-OData}
#' @examples
#' df <- morie_datasets_chicago_crime_odata(offline = TRUE)
#' nrow(df)
#' @export
morie_datasets_chicago_crime_odata <- function(filter = NULL,
                                                 select = NULL,
                                                 orderby = NULL,
                                                 top = NULL,
                                                 skip = NULL,
                                                 max_features = NULL,
                                                 offline = TRUE,
                                                 resource_id = NULL,
                                                 paginate = FALSE,
                                                 max_pages = 200L,
                                                 app_token = NULL) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic(
      "chicago_crime_synthetic", "chicago_crime_odata",
      columns = .MORIE_CHICAGO_CRIME_COLUMNS)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "ijzp-q8t2"
  .morie_dataset_odata_fetch(resource_id,
                              filter = filter,
                              select = select,
                              orderby = orderby,
                              top = top,
                              skip = skip,
                              app_token = app_token,
                              paginate = paginate,
                              max_pages = max_pages,
                              max_features = max_features)
}

# ---------------------------------------------------------------------------
# Chicago Crimes -- "Map" filtered view (SODA3-only) + arbitrary-SoQL escape
# ---------------------------------------------------------------------------

#' City of Chicago "Crimes -- 2001 to Present -- Map" view (`ahwe-kpsy`)
#'
#' Wraps the Socrata MAP VIEW derived from the main Crimes feed
#' (parent_fxf = `ijzp-q8t2`). Verified live as
#' `type: map, parent_fxf: [ijzp-q8t2]` via the Socrata catalog API;
#' landing page at
#' \url{https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present-Map/ahwe-kpsy}.
#'
#' **SODA3-only**. The SODA2 endpoint `/resource/ahwe-kpsy.json` does
#' technically return HTTP 200 but ships rows as empty objects
#' (`[{}]`) -- column resolution doesn't fire on map/filtered views.
#' This loader uses the SODA3 endpoint
#' `/api/v3/views/ahwe-kpsy/query.json?query=SELECT ... WHERE ...`
#' via [.morie_dataset_soda3_query()].
#'
#' The live ahwe-kpsy view returns a **39-column** schema:
#'   * 22 base ijzp-q8t2 columns (id, case_number, date, ..., location)
#'   * 4 reverse-geocoded extras (`location_address`, `location_city`,
#'     `location_state`, `location_zip`)
#'   * 4 Socrata-internal metadata cols (`:id`, `:version`,
#'     `:created_at`, `:updated_at`)
#'   * 9 `:@computed_region_*` spatial-overlay columns mapping each
#'     row to other Chicago boundary layers (wards, community areas,
#'     etc.) via Socrata's automatic point-in-polygon computation
#'
#' Offline mode reads a bundled 5-row 39-col fixture
#' (`inst/extdata/chicago_crime_map_ahwe_kpsy_sample.csv`).
#'
#' @param date_from Lower bound on `date` (inclusive). Accepts a
#'   `Date`, `POSIXct`, or ISO-8601 string. `NULL` defaults to one
#'   year before `date_to` (matches the upstream Map view's rolling
#'   1-year window).
#' @param date_to Upper bound on `date` (exclusive). `NULL` defaults
#'   to today.
#' @param where Optional additional SoQL `WHERE` fragment ANDed onto
#'   the date window. e.g. `"primary_type='HOMICIDE'"`.
#' @param max_features Optional total row cap.
#' @param offline Logical; if `TRUE` (default), read the bundled
#'   39-col fixture.
#' @param resource_id Optional view id override (default
#'   `"ahwe-kpsy"`).
#' @param paginate Logical; opt-in pagination via baked-in
#'   `LIMIT n OFFSET m`.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net on paginated walks.
#' @param app_token Optional Socrata app token (sent as
#'   `X-App-Token`).
#' @return A `data.frame` with the 39-col schema.
#' @references City of Chicago Data Portal, "Crimes - 2001 to
#'   Present - Map" (`ahwe-kpsy`), derived from `ijzp-q8t2`.
#' @examples
#' df <- morie_datasets_chicago_crime_map(offline = TRUE)
#' df$primary_type
#' @export
morie_datasets_chicago_crime_map <- function(date_from = NULL,
                                              date_to = NULL,
                                              where = NULL,
                                              max_features = NULL,
                                              offline = TRUE,
                                              resource_id = NULL,
                                              paginate = FALSE,
                                              page_size = 1000L,
                                              max_pages = 200L,
                                              app_token = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata",
                        "chicago_crime_map_ahwe_kpsy_sample.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago Crime Map fixture missing", call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  # Live: build a date-windowed SoQL.
  to_iso <- function(x) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "POSIXt") || inherits(x, "Date")) {
      return(format(x, "%Y-%m-%dT00:00:00.000"))
    }
    as.character(x)
  }
  d_to <- to_iso(if (is.null(date_to)) Sys.Date() else date_to)
  d_from <- to_iso(if (is.null(date_from)) Sys.Date() - 365L else date_from)
  clauses <- c(sprintf("`date` >= '%s'", d_from),
               sprintf("`date` < '%s'", d_to))
  if (!is.null(where) && nzchar(where)) clauses <- c(clauses, where)
  soql <- sprintf("SELECT * WHERE %s",
                   paste(clauses, collapse = " AND "))
  if (is.null(resource_id)) resource_id <- "ahwe-kpsy"
  .morie_dataset_soda3_query(resource_id, soql = soql,
                              app_token = app_token,
                              paginate = paginate,
                              page_size = page_size,
                              max_pages = max_pages,
                              max_features = max_features)
}

#' City of Chicago Crimes feed -- arbitrary-SoQL escape hatch
#'
#' Sibling to [morie_datasets_chicago_crime()] but hits the
#' **SODA3** `/api/v3/views/crimes/query.json` endpoint instead of
#' SODA2's `/resource/ijzp-q8t2.json`. The 8.56M-row scale of the
#' base feed makes SODA2's URL-param `$where` clumsy for non-trivial
#' filters; SODA3 lets you send the full SoQL `SELECT ... WHERE ...
#' ORDER BY ...` string in one go.
#'
#' @param where Optional SoQL `WHERE` fragment (without leading
#'   `WHERE`). e.g. `"primary_type='HOMICIDE' AND year=2024"`.
#' @param select Projection list (default `"*"`).
#' @param order Optional SoQL `ORDER BY` fragment.
#' @param max_features Optional total row cap.
#' @param offline Logical; if `TRUE` (default), read the 22-col
#'   `chicago_crime_synthetic.csv` fixture (same one
#'   [morie_datasets_chicago_crime()] uses).
#' @param resource_id Optional view id override (default
#'   `"ijzp-q8t2"`; pass `"crimes"` for the publisher alias path).
#' @param paginate Logical; opt-in pagination.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net.
#' @param app_token Optional Socrata app token (sent as header).
#' @return A `data.frame`.
#' @examples
#' df <- morie_datasets_chicago_crime_soql(offline = TRUE)
#' nrow(df)
#' @export
morie_datasets_chicago_crime_soql <- function(where = NULL,
                                                select = "*",
                                                order = NULL,
                                                max_features = NULL,
                                                offline = TRUE,
                                                resource_id = NULL,
                                                paginate = FALSE,
                                                page_size = 1000L,
                                                max_pages = 200L,
                                                app_token = NULL) {
  if (isTRUE(offline)) {
    df <- .morie_dataset_read_synthetic(
      "chicago_crime_synthetic", "chicago_crime_soql",
      columns = .MORIE_CHICAGO_CRIME_COLUMNS)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  parts <- sprintf("SELECT %s", select)
  if (!is.null(where) && nzchar(where)) {
    parts <- paste(parts, sprintf("WHERE %s", where))
  }
  if (!is.null(order) && nzchar(order)) {
    parts <- paste(parts, sprintf("ORDER BY %s", order))
  }
  if (is.null(resource_id)) resource_id <- "ijzp-q8t2"
  .morie_dataset_soda3_query(resource_id, soql = parts,
                              app_token = app_token,
                              paginate = paginate,
                              page_size = page_size,
                              max_pages = max_pages,
                              max_features = max_features)
}

# ---------------------------------------------------------------------------
# Chicago Police Beats + Districts (Socrata boundary layers)
# ---------------------------------------------------------------------------

#' Chicago Police Beats (current) boundaries (`n9it-hstw`)
#'
#' Wraps the City of Chicago "Boundaries - Police Beats (current)"
#' open dataset (Socrata resource id `n9it-hstw`; portal landing
#' \url{https://data.cityofchicago.org/Public-Safety/Boundaries-Police-Beats-current-/aerh-rz74}).
#' Returns 277 Chicago Police beats with their parent sector + district
#' codes (verified live 2026-05). Attribute schema:
#'
#' \describe{
#'   \item{beat_num}{4-digit beat id (district + 2-digit beat).}
#'   \item{beat}{Within-sector beat sequence number (string).}
#'   \item{sector}{Within-district sector number (string).}
#'   \item{district}{Parent district number (string).}
#' }
#'
#' Offline mode reads a bundled attribute-only fixture
#' (`inst/extdata/chicago_police_beats.csv`) -- the `the_geom`
#' MultiPolygon column is stripped to keep bundle size sane.
#' Live mode hits the SODA2 JSON endpoint via
#' `.morie_dataset_socrata_fetch()` (mockable); pass `geometry = TRUE`
#' to include `the_geom`. Threads through the 3OO pagination args.
#'
#' @param offline If `TRUE` (default), read the bundled fixture.
#' @param geometry If `TRUE` and `offline = FALSE`, include
#'   `the_geom` (MultiPolygon).
#' @param max_features Optional row cap.
#' @param resource_id Optional Socrata resource id override (UUID
#'   default; pass `"police-beats"` style alias if known).
#' @param paginate Logical; 3OO opt-in pagination.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net on paginated walks.
#' @return A `data.frame` with 4 attribute cols (offline) or 5
#'   including `the_geom` (live, `geometry = TRUE`).
#' @references City of Chicago Data Portal, "Boundaries - Police
#'   Beats (current)" (`n9it-hstw`).
#' @examples
#' df <- morie_datasets_chicago_police_beats(offline = TRUE)
#' head(df)
#' @export
morie_datasets_chicago_police_beats <- function(offline = TRUE,
                                                  geometry = FALSE,
                                                  max_features = NULL,
                                                  resource_id = NULL,
                                                  mode = c("soda2", "soda3"),
                                                  paginate = FALSE,
                                                  page_size = 1000L,
                                                  max_pages = 200L,
                                                  app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata", "chicago_police_beats.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago Police Beats fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(beat_num = "character",
                                          beat = "character",
                                          sector = "character",
                                          district = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "n9it-hstw"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofchicago.org/resource/%s.json",
                   resource_id)
    if (!isTRUE(geometry)) {
      url <- paste0(url,
                    "?$select=beat_num,beat,sector,district")
    }
    return(.morie_dataset_socrata_fetch(
      url, max_features = max_features,
      paginate = paginate, page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3"
  select_clause <- if (isTRUE(geometry)) {
    "*"
  } else {
    "beat_num, beat, sector, district"
  }
  .morie_dataset_soda3_query(
    resource_id,
    soql = sprintf("SELECT %s", select_clause),
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features)
}

#' Chicago Police Districts (current) boundaries (`24zt-jpfn`)
#'
#' Wraps the City of Chicago "Boundaries - Police Districts (current)"
#' open dataset (Socrata resource id `24zt-jpfn`; portal landing
#' \url{https://data.cityofchicago.org/Public-Safety/Boundaries-Police-Districts-current-/fthy-xz3r}).
#' Returns 22 active districts (1-12, 14-20, 22, 24, 25) plus the
#' special "31" headquarters polygon. Attribute schema:
#'
#' \describe{
#'   \item{dist_num}{District number (string, "1"-"31").}
#'   \item{dist_label}{Display label (e.g. `"1ST"`, `"22ND"`).}
#' }
#'
#' Offline mode reads a bundled attribute-only fixture
#' (`inst/extdata/chicago_police_districts.csv`). Live mode hits
#' SODA2 JSON; pass `geometry = TRUE` for `the_geom`.
#'
#' Socrata exposes this dataset in all four format permutations
#' (SODA2 + SODA3, JSON + GeoJSON + CSV):
#'   * SODA2 JSON: `/resource/24zt-jpfn.json`
#'   * SODA2 GeoJSON: `/resource/24zt-jpfn.geojson`
#'   * SODA2 CSV: `/resource/24zt-jpfn.csv`
#'   * SODA3 JSON: `/api/v3/views/24zt-jpfn/query.json`
#'   * SODA3 GeoJSON: `/api/v3/views/24zt-jpfn/query.geojson`
#'
#' morie defaults to SODA2 JSON; pass an explicit URL via
#' `resource_id` to exercise the others (e.g. for direct sf reads
#' you'd typically hit the GeoJSON variant via `sf::st_read()`
#' yourself rather than going through this loader).
#'
#' @param offline If `TRUE` (default), read the bundled fixture.
#' @param geometry If `TRUE` and `offline = FALSE`, include
#'   `the_geom` (MultiPolygon).
#' @param max_features Optional row cap.
#' @param resource_id Optional Socrata resource id override.
#' @param paginate Logical; 3OO opt-in pagination.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net on paginated walks.
#' @return A `data.frame` with 2 attribute cols (offline) or 3
#'   including `the_geom` (live, `geometry = TRUE`).
#' @references City of Chicago Data Portal, "Boundaries - Police
#'   Districts (current)" (`24zt-jpfn`).
#' @examples
#' df <- morie_datasets_chicago_police_districts(offline = TRUE)
#' head(df)
#' @export
morie_datasets_chicago_police_districts <- function(offline = TRUE,
                                                      geometry = FALSE,
                                                      max_features = NULL,
                                                      resource_id = NULL,
                                                      mode = c("soda2", "soda3"),
                                                      paginate = FALSE,
                                                      page_size = 1000L,
                                                      max_pages = 200L,
                                                      app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata", "chicago_police_districts.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago Police Districts fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(dist_num = "character",
                                          dist_label = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "24zt-jpfn"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofchicago.org/resource/%s.json",
                   resource_id)
    if (!isTRUE(geometry)) {
      url <- paste0(url, "?$select=dist_num,dist_label")
    }
    return(.morie_dataset_socrata_fetch(
      url, max_features = max_features,
      paginate = paginate, page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3"
  select_clause <- if (isTRUE(geometry)) "*" else "dist_num, dist_label"
  .morie_dataset_soda3_query(
    resource_id,
    soql = sprintf("SELECT %s", select_clause),
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features)
}

# ---------------------------------------------------------------------------
# Chicago Crime "resolved" analyzer -- 3VV+ join across all 5 cross-refs
# ---------------------------------------------------------------------------

#' One-call Chicago crime + boundary + dictionary join
#'
#' Phase 3VV+. Pulls a slice of [morie_datasets_chicago_crime()]
#' and left-joins each of its five canonical foreign keys against
#' the matching resolver dataset shipped in morie:
#'
#' \tabular{lll}{
#'   \strong{crime field}     \tab \strong{resolver}                       \tab \strong{join key}        \cr
#'   `beat`                   \tab [morie_datasets_chicago_police_beats()] \tab `beat == beat_num`       \cr
#'   `district`               \tab [morie_datasets_chicago_police_districts()] \tab `district == dist_num` \cr
#'   `ward`                   \tab [morie_datasets_chicago_wards()]        \tab `ward == ward`           \cr
#'   `community_area`         \tab [morie_datasets_chicago_community_areas()] \tab `community_area == area_numbe` \cr
#'   `iucr`                   \tab [morie_datasets_chicago_iucr_codes()]   \tab `iucr == iucr`           \cr
#' }
#'
#' The resolvers are loaded in offline mode (they're all bundled +
#' small), so this analyzer only touches the network for the crime
#' pull itself. Resolver columns are prefixed with the source name
#' (`ward_*`, `community_*`, `beat_*`, `district_*`, `iucr_*`) to
#' avoid collisions with the crime schema.
#'
#' Both `mode = "soda2"` and `mode = "soda3"` are honoured for the
#' crime fetch, matching the dual-API design from 3VV+.
#'
#' @inheritParams morie_datasets_chicago_crime
#' @param resolvers Character subset of the 5 resolver names to
#'   join. Default joins all 5. Pass a shorter vector to skip
#'   specific joins (e.g. `"iucr"` only).
#' @return A wide `data.frame`: crime columns first, then the
#'   joined resolver columns with their canonical prefixes.
#' @examples
#' df <- morie_datasets_chicago_crime_resolved(
#'   offline = TRUE,
#'   max_features = 5L,
#'   resolvers = c("ward", "iucr"))
#' names(df)
#' @export
morie_datasets_chicago_crime_resolved <- function(
    year = NULL,
    max_features = NULL,
    offline = TRUE,
    mode = c("soda2", "soda3"),
    paginate = FALSE,
    page_size = 1000L,
    max_pages = 200L,
    app_token = NULL,
    resolvers = c("ward", "community_area", "beat",
                  "district", "iucr")) {
  mode <- match.arg(mode)
  resolvers <- match.arg(resolvers,
                          choices = c("ward", "community_area",
                                       "beat", "district", "iucr"),
                          several.ok = TRUE)
  crime <- morie_datasets_chicago_crime(
    year = year,
    max_features = max_features,
    offline = offline,
    mode = mode,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    app_token = app_token)
  out <- crime

  # Helper: prefix a resolver's non-join columns to avoid collisions.
  prefix_cols <- function(df, drop, prefix) {
    keep <- setdiff(names(df), drop)
    names(df)[match(keep, names(df))] <- paste0(prefix, "_", keep)
    df
  }

  # ward (crime$ward -- character "1".."50" -- vs wards$ward)
  if ("ward" %in% resolvers && "ward" %in% names(out)) {
    w <- morie_datasets_chicago_wards(offline = TRUE)
    w <- prefix_cols(w, drop = "ward", prefix = "ward")
    out$ward <- as.character(out$ward)
    out <- merge(out, w, by = "ward",
                  all.x = TRUE, sort = FALSE)
  }

  # community_area (crime$community_area vs areas$area_numbe)
  if ("community_area" %in% resolvers &&
      "community_area" %in% names(out)) {
    ca <- morie_datasets_chicago_community_areas(offline = TRUE)
    names(ca)[names(ca) == "area_numbe"] <- "community_area"
    ca <- prefix_cols(ca, drop = "community_area",
                       prefix = "community")
    out$community_area <- as.character(out$community_area)
    out <- merge(out, ca, by = "community_area",
                  all.x = TRUE, sort = FALSE)
  }

  # beat (crime$beat matches beats$beat_num -- the 4-digit form).
  # The beats fixture also carries a `beat` column (within-sector
  # 1-digit form) which would collide with the join key after the
  # rename below; drop it first.
  if ("beat" %in% resolvers && "beat" %in% names(out)) {
    b <- morie_datasets_chicago_police_beats(offline = TRUE)
    b$beat <- NULL  # the within-sector beat int; not the join key
    names(b)[names(b) == "beat_num"] <- "beat"
    b <- prefix_cols(b, drop = "beat", prefix = "beat")
    out$beat <- as.character(out$beat)
    out <- merge(out, b, by = "beat",
                  all.x = TRUE, sort = FALSE)
  }

  # district (crime$district vs districts$dist_num)
  if ("district" %in% resolvers && "district" %in% names(out)) {
    d <- morie_datasets_chicago_police_districts(offline = TRUE)
    names(d)[names(d) == "dist_num"] <- "district"
    d <- prefix_cols(d, drop = "district", prefix = "district")
    out$district <- as.character(out$district)
    out <- merge(out, d, by = "district",
                  all.x = TRUE, sort = FALSE)
  }

  # iucr (crime$iucr vs codes$iucr). The upstream Chicago Crime feed
  # carries iucr as 4-char codes with leading zeros ("0820"); the
  # IUCR dictionary stores numeric codes without leading zeros
  # ("820"). Normalise both sides to 4-char zero-padded so the join
  # actually resolves -- alphanumeric codes ("031A") are already
  # 4-char and pass through unchanged.
  if ("iucr" %in% resolvers && "iucr" %in% names(out)) {
    i <- morie_datasets_chicago_iucr_codes(offline = TRUE)
    pad4 <- function(x) {
      x <- as.character(x)
      ifelse(nchar(x) < 4L,
              paste0(strrep("0", pmax(0L, 4L - nchar(x))), x),
              x)
    }
    i$iucr <- pad4(i$iucr)
    i <- prefix_cols(i, drop = "iucr", prefix = "iucr")
    out$iucr <- pad4(out$iucr)
    out <- merge(out, i, by = "iucr",
                  all.x = TRUE, sort = FALSE)
  }

  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# Chicago Wards + Community Areas + IUCR codes (3UU)
# Cross-references flagged by chicago_crime: every ijzp-q8t2 row carries
#   * `ward`           -> wards          sp34-6z76 (50 wards, SODA3-only)
#   * `community_area` -> community areas cauq-8yn6 (77 areas, SODA3-only)
#   * `iucr`/`fbi_code` -> IUCR codes    c7ck-438e (410 codes, SODA2)
# ---------------------------------------------------------------------------

#' Chicago City Council Ward boundaries (`sp34-6z76`)
#'
#' Wraps the City of Chicago "Boundaries - Wards (2023-)" open dataset
#' (Socrata resource id `sp34-6z76`; portal landing
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2023-/sp34-6z76}).
#' 50 wards in the current City Council district map. Resolves the
#' `ward` foreign key carried by every
#' [morie_datasets_chicago_crime()] row.
#'
#' **SODA3-only.** The SODA2 endpoint `/resource/sp34-6z76.json`
#' returns empty objects -- this is a filtered/derived view on
#' Socrata. Live mode uses SODA3
#' (`/api/v3/views/sp34-6z76/query.json`) via
#' [.morie_dataset_soda3_query()].
#'
#' Offline mode reads a bundled 50-row attribute-only fixture
#' (`inst/extdata/chicago_wards.csv`: `ward` / `shape_leng` /
#' `shape_area`). Live mode with `geometry = TRUE` also includes the
#' `the_geom` MultiPolygon column.
#'
#' @param offline If `TRUE` (default), read the bundled fixture.
#' @param geometry If `TRUE` and `offline = FALSE`, include the
#'   `the_geom` MultiPolygon.
#' @param max_features Optional row cap.
#' @param resource_id Optional view id override (default
#'   `"sp34-6z76"`).
#' @param paginate Logical; 3OO/3QQ opt-in pagination via
#'   `LIMIT n OFFSET m`.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net.
#' @param app_token Optional Socrata app token (sent as
#'   `X-App-Token`).
#' @return A `data.frame` with 3 attribute cols (offline) or 4
#'   including `the_geom` (live, `geometry = TRUE`).
#' @references City of Chicago Data Portal, "Boundaries - Wards
#'   (2023-)" (`sp34-6z76`).
#' @examples
#' df <- morie_datasets_chicago_wards(offline = TRUE)
#' head(df)
#' @export
morie_datasets_chicago_wards <- function(offline = TRUE,
                                          geometry = FALSE,
                                          max_features = NULL,
                                          resource_id = NULL,
                                          paginate = FALSE,
                                          page_size = 1000L,
                                          max_pages = 200L,
                                          app_token = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "chicago_wards.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago wards fixture missing", call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(ward = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "sp34-6z76"
  select_clause <- if (isTRUE(geometry)) {
    "*"
  } else {
    "ward, shape_leng, shape_area"
  }
  soql <- sprintf("SELECT %s ORDER BY ward", select_clause)
  .morie_dataset_soda3_query(resource_id, soql = soql,
                              app_token = app_token,
                              paginate = paginate,
                              page_size = page_size,
                              max_pages = max_pages,
                              max_features = max_features)
}

#' Chicago Community Area boundaries (`cauq-8yn6`)
#'
#' Wraps the City of Chicago "Boundaries - Community Areas (current)"
#' open dataset (Socrata resource id `cauq-8yn6`; portal landing
#' \url{https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6}).
#' The 77 canonical Chicago community areas
#' (Rogers Park, West Ridge, Uptown, Lincoln Square, ..., Edgewater).
#' Resolves the `community_area` foreign key carried by every
#' [morie_datasets_chicago_crime()] row.
#'
#' **SODA3-only** (same filtered/derived-view caveat as Wards).
#'
#' Offline mode reads a bundled 77-row attribute-only fixture
#' (`inst/extdata/chicago_community_areas.csv`: 5 cols --
#' `area_numbe`, `community`, `area_num_1`, `shape_area`,
#' `shape_len`). The `community` column carries the official
#' canonical name in ALL CAPS.
#'
#' @inheritParams morie_datasets_chicago_wards
#' @return A `data.frame` with 5 attribute cols (offline) or 6
#'   including `the_geom` (live, `geometry = TRUE`).
#' @references City of Chicago Data Portal, "Boundaries - Community
#'   Areas (current)" (`cauq-8yn6`).
#' @examples
#' df <- morie_datasets_chicago_community_areas(offline = TRUE)
#' head(df[, c("area_numbe", "community")])
#' @export
morie_datasets_chicago_community_areas <- function(offline = TRUE,
                                                     geometry = FALSE,
                                                     max_features = NULL,
                                                     resource_id = NULL,
                                                     paginate = FALSE,
                                                     page_size = 1000L,
                                                     max_pages = 200L,
                                                     app_token = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata", "chicago_community_areas.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago community areas fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(area_numbe = "character",
                                          area_num_1 = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "cauq-8yn6"
  select_clause <- if (isTRUE(geometry)) {
    "*"
  } else {
    paste("area_numbe, community, area_num_1,",
           "shape_area, shape_len")
  }
  soql <- sprintf("SELECT %s ORDER BY area_numbe", select_clause)
  .morie_dataset_soda3_query(resource_id, soql = soql,
                              app_token = app_token,
                              paginate = paginate,
                              page_size = page_size,
                              max_pages = max_pages,
                              max_features = max_features)
}

#' Chicago Police Department -- Illinois Uniform Crime Reporting
#' (IUCR) code dictionary (`c7ck-438e`)
#'
#' Wraps the City of Chicago "Chicago Police Department - Illinois
#' Uniform Crime Reporting (IUCR) Codes" reference table (Socrata
#' resource id `c7ck-438e`; portal landing
#' \url{https://data.cityofchicago.org/Public-Safety/Chicago-Police-Department-Illinois-Uniform-Crime-R/c7ck-438e}).
#' 410 IUCR codes mapping the `iucr` foreign key carried by every
#' [morie_datasets_chicago_crime()] row to a human-readable
#' description. Five columns:
#'
#' \describe{
#'   \item{iucr}{4-character IUCR code (e.g. `"110"` for homicide).}
#'   \item{primary_description}{Top-level category (e.g.
#'     `"HOMICIDE"`).}
#'   \item{secondary_description}{Subcategory (e.g. `"FIRST DEGREE
#'     MURDER"`).}
#'   \item{index_code}{`"I"` (FBI index crime) or other.}
#'   \item{active}{`TRUE` if the code is currently active.}
#' }
#'
#' Available via SODA2 (single-shot or paginated) -- this is a
#' base dataset, not a filtered view.
#'
#' Offline mode reads a bundled 410-row complete fixture
#' (`inst/extdata/chicago_iucr_codes.csv`).
#'
#' @param offline If `TRUE` (default), read the bundled full
#'   410-row fixture.
#' @param max_features Optional row cap.
#' @param resource_id Optional view id override.
#' @param paginate Logical; 3OO opt-in pagination.
#' @param page_size Per-page row count when paginating.
#' @param max_pages Safety net.
#' @return A `data.frame`.
#' @references City of Chicago Data Portal, "Chicago Police
#'   Department - Illinois Uniform Crime Reporting (IUCR) Codes"
#'   (`c7ck-438e`).
#' @examples
#' df <- morie_datasets_chicago_iucr_codes(offline = TRUE)
#' subset(df, primary_description == "HOMICIDE")
#' @export
morie_datasets_chicago_iucr_codes <- function(offline = TRUE,
                                                max_features = NULL,
                                                resource_id = NULL,
                                                mode = c("soda2", "soda3"),
                                                paginate = FALSE,
                                                page_size = 1000L,
                                                max_pages = 200L,
                                                app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata", "chicago_iucr_codes.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago IUCR codes fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE,
                           colClasses = c(iucr = "character",
                                          index_code = "character"))
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "c7ck-438e"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofchicago.org/resource/%s.json",
                   resource_id)
    return(.morie_dataset_socrata_fetch(
      url, max_features = max_features,
      paginate = paginate, page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3"
  .morie_dataset_soda3_query(
    resource_id, soql = "SELECT *",
    app_token = app_token,
    paginate = paginate,
    page_size = page_size,
    max_pages = max_pages,
    max_features = max_features)
}

# ---------------------------------------------------------------------------
# Chicago Open Data Arrests (Socrata, dpt3-jri9)
# ---------------------------------------------------------------------------

#' City of Chicago Open Data -- Arrests feed (`dpt3-jri9`)
#'
#' Wraps the City of Chicago "Arrests" open dataset (Socrata resource
#' id `dpt3-jri9`; portal landing
#' \url{https://data.cityofchicago.org/Public-Safety/Arrests/dpt3-jri9/about_data}).
#' 24 columns covering up to four charges per arrest plus the
#' pipe-concatenated rollup quartet (`charges_statute` /
#' `charges_description` / `charges_type` / `charges_class`).
#'
#' Socrata accepts two resource specifiers interchangeably -- the
#' numeric/UUID id (`/resource/dpt3-jri9.json`) and the human-readable
#' alias the publisher assigned (`/resource/arrests.json`). morie
#' defaults to the UUID for stability; pass `resource_id = "arrests"`
#' if you want to exercise the alias path.
#'
#' Offline mode reads a bundled 5-row synthetic fixture
#' (`inst/extdata/chicago_arrests_dpt3_jri9_sample.csv`) carrying the
#' real upstream snake_case schema. Live mode hits the SODA2 endpoint
#' via `.morie_dataset_socrata_fetch()` and honours the 3OO opt-in
#' pagination (`paginate = TRUE`).
#'
#' @param year Integer or `NULL`; server-side year filter (uses
#'   `date_extract_y(arrest_date) = <year>` SoQL).
#' @param max_features Integer or `NULL`; cap on returned rows. When
#'   `paginate = TRUE` this is the total cap across walked pages.
#' @param offline Logical; if `TRUE` (default, safer post-3EE), read
#'   the bundled synthetic frame.
#' @param resource_id Optional Socrata resource id override. Accepts
#'   the UUID (`dpt3-jri9`, default) or the publisher's alias
#'   (`arrests`).
#' @param paginate Logical; if `TRUE` and `offline = FALSE`, walk
#'   SODA2 `$offset` in `page_size` chunks. Default `FALSE`.
#' @param page_size Per-page row count when paginating (default 1,000,
#'   the unauthenticated SODA2 ceiling).
#' @param max_pages Safety net on paginated walks (default 200).
#' @return A `data.frame` with the documented 24-col Socrata schema.
#' @references City of Chicago Data Portal, "Arrests" (`dpt3-jri9`).
#' @examples
#' df <- morie_datasets_chicago_arrests(offline = TRUE)
#' df$arrest_date
#' @export
morie_datasets_chicago_arrests <- function(year = NULL,
                                            max_features = NULL,
                                            offline = TRUE,
                                            resource_id = NULL,
                                            mode = c("soda2", "soda3"),
                                            paginate = FALSE,
                                            page_size = 1000L,
                                            max_pages = 200L,
                                            app_token = NULL) {
  mode <- match.arg(mode)
  if (isTRUE(offline)) {
    path <- system.file("extdata",
                        "chicago_arrests_dpt3_jri9_sample.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled Chicago Arrests fixture missing", call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(year) && "arrest_date" %in% colnames(df) &&
        nrow(df) > 0L) {
      yr <- substr(df$arrest_date, 1L, 4L)
      df <- df[yr == as.character(as.integer(year)), , drop = FALSE]
      rownames(df) <- NULL
    }
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(resource_id)) resource_id <- "dpt3-jri9"
  if (mode == "soda2") {
    url <- sprintf("https://data.cityofchicago.org/resource/%s.json",
                   resource_id)
    where <- NULL
    if (!is.null(year)) {
      where <- sprintf("date_extract_y(arrest_date) = %d",
                       as.integer(year))
    }
    return(.morie_dataset_socrata_fetch(
      url, where = where,
      max_features = max_features,
      paginate = paginate,
      page_size = page_size,
      max_pages = max_pages))
  }
  # mode == "soda3"
  soql <- if (is.null(year)) {
    "SELECT *"
  } else {
    sprintf("SELECT * WHERE date_extract_y(arrest_date) = %d",
            as.integer(year))
  }
  .morie_dataset_soda3_query(resource_id, soql = soql,
                              app_token = app_token,
                              paginate = paginate,
                              page_size = page_size,
                              max_pages = max_pages,
                              max_features = max_features)
}

# ---------------------------------------------------------------------------
# Chicago PD -- public historical arrests CSV (chicagopolice.org)
# ---------------------------------------------------------------------------

#' Chicago Police Department -- Public Arrest Data (2014-2017)
#'
#' Wraps the static historical arrests CSV published by the Chicago
#' Police Department at
#' \url{https://www.chicagopolice.org/statistics-data/public-arrest-data/}
#' covering adult and juvenile arrests from 01 JAN 2014 through 31
#' DEC 2017, with all personally identifying information removed.
#' Ten upper-case-coded columns matching the CPD data dictionary:
#'
#' \describe{
#'   \item{ARR_DISTRICT}{Chicago PD district (geographic boundary).}
#'   \item{ARR_BEAT}{Chicago PD beat (geographic boundary).}
#'   \item{ARR_YEAR}{Calendar year of the arrest.}
#'   \item{ARR_MONTH}{Calendar month of the arrest.}
#'   \item{RACE_CODE_CD}{Perceived race code.}
#'   \item{FBI_CODE}{IUCR/FBI crime category code.}
#'   \item{STATUTE}{ILCS / MCC statute charged.}
#'   \item{STAT_DESCR}{Plain-text statute title.}
#'   \item{CHARGE_CLASS_CD}{ILCS/MCC charge class code.}
#'   \item{CHARGE_TYPE_CD}{"M" = misdemeanour, "F" = felony.}
#' }
#'
#' Unlike the SODA2 feeds, CPD publishes this as a single direct CSV
#' download with no documented API; the file URL is not stable across
#' CPD's quarterly republications. morie therefore ships an offline-
#' first loader; pass a `url` for live mode (visit the landing page
#' to find the current direct-CSV URL).
#'
#' @param url Optional direct-CSV URL. If `NULL` and `offline = FALSE`,
#'   the loader errors with a "lookup pending" message pointing at
#'   the chicagopolice.org landing page.
#' @param offline Logical; if `TRUE` (default), read the bundled
#'   synthetic 5-row fixture
#'   (`inst/extdata/cpd_public_release_arrests_sample.csv`).
#' @param max_features Integer or `NULL`; cap on returned rows.
#' @return A `data.frame` with the 10-col CPD schema.
#' @references Chicago Police Department, "Public Arrest Data";
#'   landing page at chicagopolice.org/statistics-data/public-
#'   arrest-data/.
#' @examples
#' df <- morie_datasets_cpd_public_arrests(offline = TRUE)
#' df$STAT_DESCR
#' @export
morie_datasets_cpd_public_arrests <- function(url = NULL,
                                                offline = TRUE,
                                                max_features = NULL) {
  if (isTRUE(offline)) {
    path <- system.file("extdata",
                        "cpd_public_release_arrests_sample.csv",
                        package = "morie")
    if (!nzchar(path)) {
      stop("bundled CPD public-arrests fixture missing",
           call. = FALSE)
    }
    df <- utils::read.csv(path, stringsAsFactors = FALSE,
                           check.names = FALSE)
    if (!is.null(max_features)) {
      df <- utils::head(df, as.integer(max_features))
    }
    return(df)
  }
  if (is.null(url) || !nzchar(url)) {
    stop(paste0(
      "morie_datasets_cpd_public_arrests: live mode requires `url`; ",
      "the chicagopolice.org Public Arrest Data file's direct-CSV ",
      "URL (lookup pending) is not stable across quarterly ",
      "republications. Visit ",
      "https://www.chicagopolice.org/statistics-data/public-arrest-data/",
      " to find the current direct-CSV URL and pass it via `url = ...`."),
      call. = FALSE)
  }
  raw <- .morie_dataset_http_text(url)
  df <- utils::read.csv(text = raw, stringsAsFactors = FALSE,
                         check.names = FALSE)
  if (!is.null(max_features)) {
    df <- utils::head(df, as.integer(max_features))
  }
  df
}

# ---------------------------------------------------------------------------
# Discovery: external (non-Ontario) Socrata feeds morie wraps
# ---------------------------------------------------------------------------

#' List the external Socrata datasets wrapped by morie
#'
#' Sibling discovery helper to [morie_datasets_ontario_ckan_layers()],
#' covering the non-Ontario open-data Socrata portals morie ships
#' offline-mode fixtures + mocked live-mode dispatch for.
#'
#' Coverage:
#'   * City of Chicago "Crimes -- 2001 to Present" (`ijzp-q8t2`).
#'   * City of Chicago "Arrests" (`dpt3-jri9`; 3PP).
#'   * City of Chicago "Boundaries-Neighborhoods" (`y6yq-dbs2`).
#'   * City of Chicago "Boundaries-Police-Beats (current)"
#'     (`n9it-hstw`; 3PP+).
#'   * City of Chicago "Boundaries-Police-Districts (current)"
#'     (`24zt-jpfn`; 3PP+).
#'   * City of Chicago "Boundaries-Wards (2023-)"
#'     (`sp34-6z76`; 3UU, SODA3-only).
#'   * City of Chicago "Boundaries-Community-Areas (current)"
#'     (`cauq-8yn6`; 3UU, SODA3-only).
#'   * City of Chicago "IUCR Code Dictionary"
#'     (`c7ck-438e`; 3UU).
#'   * NYC OpenData NYPD Stop, Question and Frisk (SQF) microdata --
#'     three published years (2022 = `e4yi-bvqr`, 2023 = `rbed-zzin`,
#'     2024 = `7v9w-k82r`).
#'
#' All Chicago Socrata endpoints accept both the numeric/UUID
#' specifier (`/resource/<id>.json`) and the publisher's
#' human-readable alias (`/resource/<alias>.json`, e.g.
#' `/resource/arrests.json` or `/resource/crimes.json`). morie's
#' wrappers default to the UUID for stability; pass `resource_id =
#' "<alias>"` to exercise the alias path.
#'
#' @return A `data.frame` with columns `dataset_key`, `label`,
#'   `portal`, `resource_url`, `fixture`.
#' @export
morie_datasets_external_socrata_layers <- function() {
  rows <- list(
    list(dataset_key = "chicago_crime",
         label = "City of Chicago -- Crimes (2001 to Present)",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/ijzp-q8t2.json",
         fixture = "chicago_crime_synthetic.csv"),
    list(dataset_key = "chicago_arrests",
         label = "City of Chicago -- Arrests",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/dpt3-jri9.json",
         fixture = "chicago_arrests_dpt3_jri9_sample.csv"),
    list(dataset_key = "chicago_neighborhoods",
         label = "City of Chicago -- Boundaries-Neighborhoods (Office of Tourism)",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/y6yq-dbs2.json",
         fixture = "chicago_neighborhoods.csv"),
    list(dataset_key = "chicago_police_beats",
         label = "City of Chicago -- Boundaries-Police-Beats (current)",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/n9it-hstw.json",
         fixture = "chicago_police_beats.csv"),
    list(dataset_key = "chicago_police_districts",
         label = "City of Chicago -- Boundaries-Police-Districts (current)",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/24zt-jpfn.json",
         fixture = "chicago_police_districts.csv"),
    list(dataset_key = "chicago_wards",
         label = "City of Chicago -- Boundaries-Wards (2023-)",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/sp34-6z76.json",
         fixture = "chicago_wards.csv"),
    list(dataset_key = "chicago_community_areas",
         label = "City of Chicago -- Boundaries-Community-Areas (current)",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/cauq-8yn6.json",
         fixture = "chicago_community_areas.csv"),
    list(dataset_key = "chicago_iucr_codes",
         label = "City of Chicago -- IUCR Code Dictionary",
         portal = "data.cityofchicago.org",
         resource_url = "https://data.cityofchicago.org/resource/c7ck-438e.json",
         fixture = "chicago_iucr_codes.csv"),
    list(dataset_key = "nyc_sqf_2024",
         label = "NYPD Stop, Question and Frisk -- 2024",
         portal = "data.cityofnewyork.us",
         resource_url = "https://data.cityofnewyork.us/resource/7v9w-k82r.json",
         fixture = "nyc_sqf_synthetic.csv"),
    list(dataset_key = "nyc_sqf_2023",
         label = "NYPD Stop, Question and Frisk -- 2023",
         portal = "data.cityofnewyork.us",
         resource_url = "https://data.cityofnewyork.us/resource/rbed-zzin.json",
         fixture = "nyc_sqf_synthetic.csv"),
    list(dataset_key = "nyc_sqf_2022",
         label = "NYPD Stop, Question and Frisk -- 2022",
         portal = "data.cityofnewyork.us",
         resource_url = "https://data.cityofnewyork.us/resource/e4yi-bvqr.json",
         fixture = "nyc_sqf_synthetic.csv"))
  out <- do.call(rbind, lapply(rows, as.data.frame,
                                stringsAsFactors = FALSE))
  rownames(out) <- NULL
  out
}
