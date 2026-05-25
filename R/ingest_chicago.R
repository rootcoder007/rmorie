# SPDX-License-Identifier: AGPL-3.0-or-later
#
# City of Chicago / Socrata open-data ingest (R port).
#
# Chicago publishes its open data via a Socrata-powered portal
# (https://data.cityofchicago.org/). Every dataset is backed by a
# stable SoDA endpoint that speaks SoQL ($where / $select / $order /
# $limit / $offset) for server-side filtering and paging.
#
# The Socrata API shape is shared with NYC OpenData, data.seattle.gov,
# data.cityofnewyork.us, and many other municipal/state deployments,
# so the generic `morie_ingest_chicago_socrata()` helper works against
# any of them, while the `morie_ingest_chicago_crime()` convenience
# wrapper targets the documented Chicago "Crimes - 2001 to Present"
# feed (resource id `ijzp-q8t2`, verified 2026-05-13).
#
# HTTP: routes via .morie_dataset_http_text (3YY -> libcurl C++ backend with httr2 fallback). JSON: jsonlite::fromJSON(simplifyVector=FALSE).
# Optional BigQuery mirror: `bigquery-public-data.chicago_crime` via
# `morie_ingest_bigquery_table()` for analysts who prefer SQL on the
# full historical mirror; see ingest_bigquery.R.

.MORIE_CHICAGO_DEFAULT_UA <- "morie/r (+https://hadesllm.com)"
.MORIE_CHICAGO_DEFAULT_TIMEOUT <- 60

# Socrata server-side cap is 50,000 rows per response.
.MORIE_CHICAGO_DEFAULT_PAGE_SIZE <- 50000L

# Canonical Chicago Socrata resource ids.  Other resources can be
# added here as morie's analysis suite grows.
.MORIE_CHICAGO_RESOURCE_REGISTRY <- list(
  crime = "https://data.cityofchicago.org/resource/ijzp-q8t2.json"
)

#' Built-in Chicago / Socrata resource registry
#'
#' Returns the canonical Chicago open-data Socrata endpoints morie
#' ships with as a flat data.frame.  Useful for discovery and for the
#' CLI \code{--list} surface.
#'
#' @return A base R \code{data.frame} with columns \code{name},
#'   \code{url}.
#' @export
morie_ingest_chicago_resources <- function() {
  data.frame(
    name = names(.MORIE_CHICAGO_RESOURCE_REGISTRY),
    url  = unlist(.MORIE_CHICAGO_RESOURCE_REGISTRY, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

# Internal: a single Socrata SoQL GET against `resource_url`.
.morie_chicago_socrata_get <- function(resource_url,
                                       where = NULL,
                                       select = NULL,
                                       order = NULL,
                                       limit =
                                         .MORIE_CHICAGO_DEFAULT_PAGE_SIZE,
                                       offset = 0L,
                                       app_token = NULL,
                                       user_agent =
                                         .MORIE_CHICAGO_DEFAULT_UA,
                                       timeout =
                                         .MORIE_CHICAGO_DEFAULT_TIMEOUT) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package 'httr2' is required for morie_ingest_chicago_*(). ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }
  params <- list(
    `$limit`  = as.integer(limit),
    `$offset` = as.integer(offset)
  )
  if (!is.null(where) && nzchar(where)) params[["$where"]] <- where
  if (!is.null(select) && nzchar(select)) params[["$select"]] <- select
  if (!is.null(order) && nzchar(order)) params[["$order"]] <- order

  # 3YY: route through .morie_dataset_http_text (libcurl + httr2
  # fallback) + parse JSON with simplifyVector = FALSE to preserve
  # the list-of-records shape downstream code expects. app_token
  # rides as X-App-Token header.
  headers <- if (!is.null(app_token) && nzchar(app_token)) {
    paste0("X-App-Token: ", app_token)
  } else {
    character()
  }
  body <- tryCatch(
    .morie_dataset_http_text(resource_url,
                              query = params,
                              headers = headers,
                              timeout_s = as.integer(timeout)),
    error = function(e) {
      stop(
        "morie Chicago socrata GET failed (", resource_url, "): ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  payload <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  if (is.list(payload) && !is.null(payload$error)) {
    stop("morie Chicago socrata error: ",
      paste(utils::capture.output(str(payload)), collapse = " "),
      call. = FALSE
    )
  }
  if (!is.list(payload)) {
    stop(
      "morie Chicago socrata GET: unexpected payload type: ",
      class(payload)[1L],
      call. = FALSE
    )
  }
  payload
}

# Internal: bind a list-of-row-lists to a data.frame, tolerating
# heterogeneous JSON shapes (missing columns become NA).
.morie_chicago_rows_to_df <- function(rows) {
  if (length(rows) == 0L) {
    return(data.frame())
  }
  all_cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  do.call(rbind, lapply(rows, function(r) {
    vals <- lapply(all_cols, function(k) {
      v <- r[[k]]
      if (is.null(v)) NA else if (is.list(v)) {
        paste(unlist(v, use.names = FALSE), collapse = ";")
      } else {
        v
      }
    })
    names(vals) <- all_cols
    as.data.frame(vals, stringsAsFactors = FALSE)
  }))
}

#' Fetch every row from a Socrata SoDA JSON endpoint
#'
#' Pages transparently through \code{$offset} until either the server
#' returns fewer rows than \code{page_size} (the last page) or
#' \code{max_features} is reached.  Works against any Socrata-shaped
#' portal (Chicago, NYC, Seattle, etc.).
#'
#' @param resource_url Full Socrata resource URL ending in
#'   \code{.json}, e.g.
#'   \code{"https://data.cityofchicago.org/resource/ijzp-q8t2.json"}.
#' @param where,select,order SoQL clauses; see
#'   \url{https://dev.socrata.com/docs/queries/}.  \code{where} is the
#'   equivalent of SQL \code{WHERE}.
#' @param page_size Rows per request (capped at 50,000 server-side).
#' @param max_features Optional hard cap on total returned rows.
#' @param app_token Optional Socrata application token (anonymous
#'   calls share a throttled pool; tokens give per-app quotas).
#' @param user_agent,timeout Standard request knobs.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
#' df <- morie_ingest_chicago_socrata(
#'   "https://data.cityofnewyork.us/resource/uip8-fykc.json",
#'   where = "arrest_year = 2023",
#'   max_features = 5000L
#' )
#' }
#' @export
morie_ingest_chicago_socrata <- function(resource_url,
                                         where = NULL,
                                         select = NULL,
                                         order = NULL,
                                         page_size =
                                           .MORIE_CHICAGO_DEFAULT_PAGE_SIZE,
                                         max_features = NULL,
                                         app_token = NULL,
                                         user_agent =
                                           .MORIE_CHICAGO_DEFAULT_UA,
                                         timeout =
                                           .MORIE_CHICAGO_DEFAULT_TIMEOUT) {
  if (!is.character(resource_url) || length(resource_url) != 1L ||
    !nzchar(resource_url)) {
    stop("`resource_url` must be a single non-empty URL.", call. = FALSE)
  }
  page_size <- as.integer(page_size)
  rows <- list()
  offset <- 0L
  repeat {
    page <- .morie_chicago_socrata_get(
      resource_url,
      where = where, select = select, order = order,
      limit = page_size, offset = offset, app_token = app_token,
      user_agent = user_agent, timeout = timeout
    )
    if (length(page) == 0L) break
    rows <- c(rows, page)
    if (!is.null(max_features) && length(rows) >= max_features) {
      rows <- rows[seq_len(max_features)]
      break
    }
    if (length(page) < page_size) break
    offset <- offset + length(page)
  }
  if (length(rows) == 0L) {
    stop(
      "morie_ingest_chicago_socrata: returned zero rows (where=",
      if (is.null(where)) "NULL" else paste0("'", where, "'"), ")",
      call. = FALSE
    )
  }
  .morie_chicago_rows_to_df(rows)
}

#' Pull the City of Chicago "Crimes - 2001 to Present" feed
#'
#' Returns a data.frame with the documented Socrata schema (snake_case
#' column names preserved): \code{id}, \code{case_number},
#' \code{date}, \code{block}, \code{iucr}, \code{primary_type},
#' \code{description}, \code{location_description}, \code{arrest},
#' \code{domestic}, \code{beat}, \code{district}, \code{ward},
#' \code{community_area}, \code{fbi_code}, \code{x_coordinate},
#' \code{y_coordinate}, \code{year}, \code{updated_on},
#' \code{latitude}, \code{longitude}.
#'
#' @param year Optional reporting year (e.g. 2024); when set, applies
#'   the server-side SoQL filter \code{year = <year>}.
#' @param where Optional raw SoQL \code{$where} clause (overrides
#'   \code{year}).
#' @param max_features Optional hard cap on returned rows.
#' @param app_token Optional Socrata X-App-Token for higher rate
#'   limits.
#' @param user_agent,timeout Standard request knobs.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
#' df <- morie_ingest_chicago_crime(year = 2024, max_features = 10000L)
#' head(df)
#' }
#' @seealso \code{\link{morie_ingest_chicago_socrata}},
#'   \code{\link{morie_ingest_bigquery_table}} for the BigQuery
#'   public-data mirror (\code{bigquery-public-data.chicago_crime}).
#' @export
morie_ingest_chicago_crime <- function(year = NULL,
                                       where = NULL,
                                       max_features = NULL,
                                       app_token = NULL,
                                       user_agent =
                                         .MORIE_CHICAGO_DEFAULT_UA,
                                       timeout =
                                         .MORIE_CHICAGO_DEFAULT_TIMEOUT) {
  clause <- where
  if (is.null(clause) && !is.null(year)) {
    yr <- suppressWarnings(as.integer(year))
    if (is.na(yr)) {
      stop("`year` must be coercible to integer.", call. = FALSE)
    }
    clause <- sprintf("year = %d", yr)
  }
  morie_ingest_chicago_socrata(
    resource_url = .MORIE_CHICAGO_RESOURCE_REGISTRY$crime,
    where = clause,
    max_features = max_features,
    app_token = app_token,
    user_agent = user_agent,
    timeout = timeout
  )
}

#' BigQuery mirror of the Chicago crime feed
#'
#' Convenience wrapper around
#' \code{\link{morie_ingest_bigquery_table}} that pulls
#' \code{bigquery-public-data.chicago_crime.crime} - the Google
#' BigQuery public-data mirror of the Socrata feed served by
#' \code{\link{morie_ingest_chicago_crime}}.  Use this path when you
#' want SQL-side filtering or the full historical depth of the dataset
#' without paging through SoQL.
#'
#' Requires the optional \pkg{bigrquery} package and a billing project
#' (\code{billing_project} arg or \code{GCP_PROJECT} env var); public
#' datasets are billed to the caller's project, not the dataset
#' owner's.
#'
#' @param where Optional raw SQL \code{WHERE} clause (no leading
#'   \code{WHERE}).
#' @param year Convenience shortcut: when \code{where} is \code{NULL}
#'   and \code{year} is set, applies \code{year = <year>}.
#' @param limit Optional \code{LIMIT}.
#' @param select Projection list (default \code{"*"}).
#' @param billing_project,page_size,max_rows,quiet Forwarded to
#'   \code{\link{morie_ingest_bigquery_table}}.
#' @return A base R \code{data.frame}.
#' @seealso \code{\link{morie_ingest_chicago_crime}},
#'   \code{\link{morie_ingest_bigquery_table}}
#' @export
morie_ingest_chicago_crime_bigquery <- function(where = NULL,
                                                year = NULL,
                                                limit = NULL,
                                                select = "*",
                                                billing_project = NULL,
                                                page_size = 10000L,
                                                max_rows = Inf,
                                                quiet = TRUE) {
  clause <- where
  if (is.null(clause) && !is.null(year)) {
    yr <- suppressWarnings(as.integer(year))
    if (is.na(yr)) {
      stop("`year` must be coercible to integer.", call. = FALSE)
    }
    clause <- sprintf("year = %d", yr)
  }
  morie_ingest_bigquery_table(
    project = "bigquery-public-data",
    dataset = "chicago_crime",
    table   = "crime",
    where   = clause,
    limit   = limit,
    select  = select,
    billing_project = billing_project,
    page_size = page_size,
    max_rows  = max_rows,
    quiet     = quiet
  )
}
