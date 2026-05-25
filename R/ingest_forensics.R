# SPDX-License-Identifier: AGPL-3.0-or-later
#
# US forensic open-data ingestors (R port).
#
# Mirrors `morie.ingest.forensics` in Python: a unified thin client
# over three federal forensic-statistics open-data endpoints so morie
# can serve digital-forensics, missing-persons, and federal
# crime-reporting use cases.
#
# Sources covered
# ---------------
# * **FBI NIBRS** - National Incident-Based Reporting System, exposed
#   via the Crime Data Explorer API at
#   https://api.usa.gov/crime/fbi/cde/.  Requires an API key from
#   https://api.data.gov/signup/; pass via `api_key` or the
#   `FBI_CDE_API_KEY` environment variable.  Responses are nested
#   JSON; the client flattens to one row per offence-event using
#   dotted keys (`offense.code`, `victim.age`).
#
# * **NamUs** - National Missing and Unidentified Persons System,
#   https://www.namus.gov/api/CaseSets/NamUs/MissingPersons/Search.
#   POST-with-JSON-body Socrata-like; no key required.  Returns the
#   documented `NAMUS_MISSING_COLUMNS` schema (`case_number`,
#   `state`, `county`, `dlc_date`, ...).
#
# * **NIST RDS** - NIST Reference Datasets catalog metadata at
#   https://data.nist.gov/rmm/records.  The raw reference datasets
#   themselves (CSAFE, NSRL, ...) are multi-gigabyte and must be
#   downloaded out-of-band; this client returns the catalog records.
#
# HTTP: routes via .morie_dataset_http_text_with_status + .morie_dataset_http_post_json_with_status (3ZZ -> libcurl C++ backend with httr2 fallback). JSON: jsonlite::fromJSON(simplifyVector=FALSE). HTTP status codes inspected for NamUs 401/403 + 4xx custom error formatting.

.MORIE_FORENSICS_DEFAULT_UA <- "morie/r (+https://hadesllm.com)"
.MORIE_FORENSICS_DEFAULT_TIMEOUT <- 60

# Endpoints (verified 2026-05-13; subject to federal-portal reorg).
.MORIE_FBI_CDE_BASE <- "https://api.usa.gov/crime/fbi/cde"
.MORIE_FBI_CDE_SIGNUP_URL <- "https://api.data.gov/signup/"
.MORIE_FBI_CDE_API_KEY_ENV <- "FBI_CDE_API_KEY"

.MORIE_NAMUS_MISSING_BASE <-
  "https://www.namus.gov/api/CaseSets/NamUs/MissingPersons/Search"

.MORIE_NIST_RDS_BASE <- "https://data.nist.gov/rmm/records"

# Documented NamUs MissingPersons columns, kept here so the offline /
# synthetic fallback ships the same schema as the live API.
.MORIE_NAMUS_MISSING_COLUMNS <- c(
  "case_number", "state", "county", "dlc_date", "sex", "race",
  "age_min", "age_max", "height_cm_min", "height_cm_max",
  "weight_kg_min", "weight_kg_max", "first_name", "last_name",
  "city", "circumstances"
)

# Documented NIST RDS catalog columns; the raw records carry far more.
.MORIE_NIST_RDS_COLUMNS <- c(
  "dataset_id", "title", "description", "publisher", "issued",
  "modified", "keyword", "landing_page", "size_bytes", "license"
)

# Internal: resolve FBI CDE API key from arg -> env, or stop.
.morie_forensics_require_fbi_key <- function(api_key = NULL) {
  key <- if (!is.null(api_key) && nzchar(api_key)) {
    api_key
  } else {
    Sys.getenv(.MORIE_FBI_CDE_API_KEY_ENV, "")
  }
  if (!nzchar(key)) {
    stop(
      "FBI NIBRS / Crime Data Explorer requires an API key. ",
      "Sign up free at ", .MORIE_FBI_CDE_SIGNUP_URL,
      " and either pass api_key=... or export ",
      .MORIE_FBI_CDE_API_KEY_ENV, "=<key>.",
      call. = FALSE
    )
  }
  key
}

# Internal: flatten one nested NIBRS JSON record to a single row.
# Nested dicts become dotted keys; scalar lists are ";"-joined;
# nested lists are JSON-serialised.
.morie_forensics_flatten_nibrs <- function(rec) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required for NIBRS flattening. ",
      "install.packages('jsonlite')",
      call. = FALSE
    )
  }
  out <- list()
  for (k in names(rec)) {
    v <- rec[[k]]
    if (is.list(v) && !is.null(names(v))) {
      # named-list -> dotted child keys
      for (sk in names(v)) {
        out[[paste0(k, ".", sk)]] <- v[[sk]]
      }
    } else if (is.list(v) || (is.vector(v) && length(v) > 1L &&
      is.null(names(v)) && !is.character(v))) {
      # unnamed list / vector
      vv <- v
      is_scalarish <- function(x) {
        is.null(x) || (length(x) == 1L && !is.list(x))
      }
      if (all(vapply(vv, is_scalarish, logical(1L)))) {
        out[[k]] <- paste(
          vapply(vv, function(x) {
            if (is.null(x)) "" else as.character(x)
          }, character(1L)),
          collapse = ";"
        )
      } else {
        out[[k]] <- jsonlite::toJSON(vv, auto_unbox = TRUE, null = "null")
      }
    } else {
      out[[k]] <- if (is.null(v)) NA else v
    }
  }
  out
}

# Internal: shared GET + JSON parse with explicit auth checks.
# 3ZZ: routes through .morie_dataset_http_text_with_status (libcurl
# backend with httr2 fallback), inspects HTTP status code for
# 401/403 (auth) + 4xx (generic) custom error formatting.
.morie_forensics_get_json <- function(url,
                                      params = list(),
                                      headers = list(),
                                      timeout =
                                        .MORIE_FORENSICS_DEFAULT_TIMEOUT,
                                      user_agent =
                                        .MORIE_FORENSICS_DEFAULT_UA,
                                      auth_signup_url = NULL,
                                      label = "forensics") {
  # 3ZZ: route through .morie_dataset_http_text_with_status (libcurl
  # + httr2 fallback). The status_code is needed for the 401/403
  # API-key handling + general 4xx error formatting.
  # `headers` arrives as a named list in the existing call sites;
  # flatten to "Key: Value" strings for the helper's contract.
  hdr_vec <- if (length(headers) > 0L) {
    paste(names(headers), unlist(headers), sep = ": ")
  } else {
    character()
  }
  resp <- .morie_dataset_http_text_with_status(
    url, query = params, headers = hdr_vec,
    timeout_s = as.integer(timeout))
  status <- resp$status_code
  if (status == 401L || status == 403L) {
    stop(
      label, ": API rejected the request (HTTP ", status, ")",
      if (!is.null(auth_signup_url)) {
        paste0(". Verify your key at ", auth_signup_url, ".")
      } else {
        "."
      },
      call. = FALSE
    )
  }
  if (status >= 400L) {
    stop(
      label, " -> HTTP ", status, ": ",
      substr(resp$body, 1L, 200L),
      call. = FALSE
    )
  }
  jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
}

# Internal: rbind a list of named-list rows into a data.frame,
# tolerating heterogeneous columns (missing -> NA).
.morie_forensics_rows_to_df <- function(rows, columns = NULL) {
  if (length(rows) == 0L) {
    if (is.null(columns)) {
      return(data.frame())
    }
    return(data.frame(
      matrix(NA, nrow = 0L, ncol = length(columns),
        dimnames = list(NULL, columns)
      ),
      stringsAsFactors = FALSE
    ))
  }
  cols <- if (is.null(columns)) {
    unique(unlist(lapply(rows, names), use.names = FALSE))
  } else {
    columns
  }
  do.call(rbind, lapply(rows, function(r) {
    vals <- lapply(cols, function(k) {
      v <- r[[k]]
      if (is.null(v)) NA else if (is.list(v)) {
        paste(unlist(v, use.names = FALSE), collapse = ";")
      } else {
        v
      }
    })
    names(vals) <- cols
    as.data.frame(vals, stringsAsFactors = FALSE)
  }))
}

#' Pull FBI NIBRS offence-event records via Crime Data Explorer
#'
#' Queries the FBI Crime Data Explorer NIBRS endpoint
#' (\code{/crime/fbi/cde/nibrs/<state>/<offense>?year=...}).  Requires
#' an API key from \url{https://api.data.gov/signup/}; pass via
#' \code{api_key=} or set the \code{FBI_CDE_API_KEY} environment
#' variable.  Returns one row per offence-event with nested
#' sub-objects flattened using dotted keys
#' (\code{offense.code}, \code{victim.age}, ...).
#'
#' @param year Reporting year (e.g. 2023).  Required - CDE forces a
#'   year scope.
#' @param offense NIBRS offence slug (e.g. \code{"aggravated-assault"},
#'   \code{"burglary"}); \code{NULL} returns all offences.
#' @param state Two-letter US state code (e.g. \code{"GA"}).
#'   \code{NULL} returns the national feed (very large; use
#'   \code{max_features}).
#' @param api_key FBI CDE API key; falls back to
#'   \code{$FBI_CDE_API_KEY}.
#' @param max_features Optional hard cap on returned rows.
#' @param page_size CDE page size; server-side cap varies by endpoint.
#' @param timeout HTTP timeout in seconds.
#' @return A base R \code{data.frame}, one row per offence-event.
#' @examples
#' \dontrun{
#' df <- morie_ingest_forensics_nibrs(
#'   year = 2023, offense = "aggravated-assault", state = "GA",
#'   api_key = Sys.getenv("FBI_CDE_API_KEY"),
#'   max_features = 5000L
#' )
#' head(df)
#' }
#' @export
morie_ingest_forensics_nibrs <- function(year,
                                         offense = NULL,
                                         state = NULL,
                                         api_key = NULL,
                                         max_features = NULL,
                                         page_size = 500L,
                                         timeout =
                                           .MORIE_FORENSICS_DEFAULT_TIMEOUT) {
  if (missing(year) || is.null(year)) {
    stop("`year` is required for morie_ingest_forensics_nibrs().",
      call. = FALSE
    )
  }
  yr <- suppressWarnings(as.integer(year))
  if (is.na(yr)) {
    stop("`year` must be coercible to integer.", call. = FALSE)
  }
  key <- .morie_forensics_require_fbi_key(api_key)
  state_part <- if (!is.null(state) && nzchar(state)) {
    toupper(state)
  } else {
    "national"
  }
  offense_part <- if (!is.null(offense) && nzchar(offense)) {
    offense
  } else {
    "all"
  }
  url <- sprintf(
    "%s/nibrs/%s/%s",
    .MORIE_FBI_CDE_BASE, state_part, offense_part
  )

  rows <- list()
  offset <- 0L
  page_size <- as.integer(page_size)
  repeat {
    payload <- .morie_forensics_get_json(
      url = url,
      params = list(
        API_KEY = key, year = yr, from = offset, size = page_size
      ),
      timeout = timeout,
      auth_signup_url = .MORIE_FBI_CDE_SIGNUP_URL,
      label = "FBI CDE NIBRS"
    )
    batch <- payload$results
    if (is.null(batch)) batch <- payload$data
    if (is.null(batch)) batch <- list()
    if (length(batch) == 0L) break
    for (rec in batch) {
      rows[[length(rows) + 1L]] <- .morie_forensics_flatten_nibrs(rec)
    }
    if (!is.null(max_features) && length(rows) >= max_features) {
      rows <- rows[seq_len(max_features)]
      break
    }
    if (length(batch) < page_size) break
    offset <- offset + length(batch)
  }
  if (length(rows) == 0L) {
    stop(
      "morie_ingest_forensics_nibrs: zero records (year=", yr,
      ", state=", if (is.null(state)) "NULL" else paste0("'", state, "'"),
      ", offense=",
      if (is.null(offense)) "NULL" else paste0("'", offense, "'"), ").",
      call. = FALSE
    )
  }
  .morie_forensics_rows_to_df(rows)
}

# Internal: pull morie's documented columns out of one NamUs record.
.morie_forensics_flatten_namus <- function(rec) {
  sub_id <- rec$subjectIdentification
  if (is.null(sub_id)) sub_id <- list()
  sub_desc <- rec$subjectDescription
  if (is.null(sub_desc)) sub_desc <- list()
  sighting <- rec$sighting
  if (is.null(sighting)) sighting <- list()
  sighting_addr <- if (is.list(sighting) && !is.null(sighting$address)) {
    sighting$address
  } else {
    list()
  }
  pick <- function(x, k) {
    if (is.list(x) && !is.null(x[[k]])) x[[k]] else NA
  }
  pick_first <- function(...) {
    vs <- list(...)
    for (v in vs) {
      if (!is.null(v) && !identical(v, NA) && length(v) > 0L &&
        (!is.character(v) || nzchar(v))) {
        return(v)
      }
    }
    NA
  }
  circ <- rec$circumstances
  if (is.null(circ) || identical(circ, NA)) {
    cod <- rec$circumstancesOfDisappearance
    if (is.list(cod)) circ <- cod$circumstancesOfDisappearance
  }
  list(
    case_number   = pick_first(rec$caseNumber, rec$namUsCaseNumber),
    state         = pick_first(sighting_addr$state, rec$state),
    county        = pick_first(sighting_addr$county, rec$county),
    dlc_date      = pick(sighting, "date"),
    sex           = pick(sub_desc, "sex"),
    race          = pick_first(sub_desc$primaryEthnicity, sub_desc$race),
    age_min       = pick(sub_desc, "currentMinAge"),
    age_max       = pick(sub_desc, "currentMaxAge"),
    height_cm_min = pick(sub_desc, "heightFrom"),
    height_cm_max = pick(sub_desc, "heightTo"),
    weight_kg_min = pick(sub_desc, "weightFrom"),
    weight_kg_max = pick(sub_desc, "weightTo"),
    first_name    = pick(sub_id, "firstName"),
    last_name     = pick(sub_id, "lastName"),
    city          = pick(sighting_addr, "city"),
    circumstances = if (is.null(circ)) NA else circ
  )
}

#' Pull NamUs missing-persons case metadata
#'
#' Posts a JSON-body search request to the NamUs MissingPersons
#' endpoint (\code{/api/CaseSets/NamUs/MissingPersons/Search}) and
#' pages through the results.  Returns morie's documented schema -
#' \code{case_number}, \code{state}, \code{county}, \code{dlc_date}
#' (date last contact), \code{sex}, \code{race}, \code{age_min},
#' \code{age_max}, \code{height_cm_min}, \code{height_cm_max},
#' \code{weight_kg_min}, \code{weight_kg_max}, \code{first_name},
#' \code{last_name}, \code{city}, \code{circumstances}.
#'
#' @param state Two-letter US state code; \code{NULL} returns the
#'   national feed.
#' @param max_features Optional hard cap on returned rows.
#' @param page_size Records per request (default 200).
#' @param user_agent,timeout Standard request knobs.
#' @return A base R \code{data.frame}.
#' @examples
#' \dontrun{
#' df <- morie_ingest_forensics_namus_missing(state = "CA",
#'                                            max_features = 1000L)
#' head(df)
#' }
#' @export
morie_ingest_forensics_namus_missing <- function(
    state = NULL,
    max_features = NULL,
    page_size = 200L,
    user_agent = .MORIE_FORENSICS_DEFAULT_UA,
    timeout = .MORIE_FORENSICS_DEFAULT_TIMEOUT) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package 'httr2' is required for morie_ingest_forensics_*(). ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required for morie_ingest_forensics_*(). ",
      "install.packages('jsonlite')",
      call. = FALSE
    )
  }
  page_size <- as.integer(page_size)
  body <- list(
    take = page_size, skip = 0L,
    projections = list(),
    predicates = list()
  )
  if (!is.null(state) && nzchar(state)) {
    body$predicates <- list(
      list(field = "state", value = toupper(state), operator = "Is")
    )
  }

  rows <- list()
  repeat {
    # 3ZZ: route through .morie_dataset_http_post_json_with_status
    # (libcurl + httr2 fallback). NamUs requires Accept +
    # Content-Type headers; Content-Type is added by the helper
    # automatically so we only pass Accept.
    resp <- .morie_dataset_http_post_json_with_status(
      .MORIE_NAMUS_MISSING_BASE,
      body = body,
      headers = "Accept: application/json",
      timeout_s = as.integer(timeout),
      auto_unbox = TRUE)
    status <- resp$status_code
    if (status >= 400L) {
      stop(
        "NamUs MissingPersons -> HTTP ", status, ": ",
        substr(resp$body, 1L, 200L),
        call. = FALSE
      )
    }
    payload <- jsonlite::fromJSON(resp$body, simplifyVector = FALSE)
    batch <- payload$results
    if (is.null(batch)) batch <- payload$data
    if (is.null(batch) && is.list(payload) && is.null(names(payload))) {
      batch <- payload
    }
    if (is.null(batch)) batch <- list()
    if (length(batch) == 0L) break
    for (rec in batch) {
      rows[[length(rows) + 1L]] <- .morie_forensics_flatten_namus(rec)
    }
    if (!is.null(max_features) && length(rows) >= max_features) {
      rows <- rows[seq_len(max_features)]
      break
    }
    if (length(batch) < page_size) break
    body$skip <- body$skip + length(batch)
  }
  if (length(rows) == 0L) {
    stop(
      "morie_ingest_forensics_namus_missing: zero records (state=",
      if (is.null(state)) "NULL" else paste0("'", state, "'"), ").",
      call. = FALSE
    )
  }
  .morie_forensics_rows_to_df(rows, columns = .MORIE_NAMUS_MISSING_COLUMNS)
}

# Internal: pull morie's documented columns out of one NIST RDS record.
.morie_forensics_flatten_nist <- function(rec) {
  keyword <- rec$keyword
  if (is.null(keyword)) keyword <- rec$theme
  if (is.list(keyword) || (is.vector(keyword) && length(keyword) > 1L)) {
    keyword <- paste(vapply(keyword, function(k) {
      if (is.null(k)) "" else as.character(k)
    }, character(1L)), collapse = ";")
  }
  publisher <- rec$publisher
  if (is.list(publisher)) {
    publisher <- if (!is.null(publisher$name)) {
      publisher$name
    } else if (!is.null(publisher$`@id`)) {
      publisher$`@id`
    } else {
      NA
    }
  }
  license_ <- rec$license
  if (is.null(license_)) license_ <- rec$rights
  if (is.list(license_)) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      license_ <- jsonlite::toJSON(license_, auto_unbox = TRUE)
    } else {
      license_ <- paste(unlist(license_, use.names = FALSE),
        collapse = ";"
      )
    }
  }
  list(
    dataset_id   = {
      v <- rec$ediid
      if (is.null(v)) v <- rec$identifier
      if (is.null(v)) v <- rec$`@id`
      if (is.null(v)) NA else v
    },
    title        = if (is.null(rec$title)) NA else rec$title,
    description  = if (is.null(rec$description)) NA else rec$description,
    publisher    = if (is.null(publisher)) NA else publisher,
    issued       = if (is.null(rec$issued)) NA else rec$issued,
    modified     = if (is.null(rec$modified)) NA else rec$modified,
    keyword      = if (is.null(keyword)) NA else keyword,
    landing_page = {
      v <- rec$landingPage
      if (is.null(v)) v <- rec$landing_page
      if (is.null(v)) NA else v
    },
    size_bytes   = {
      v <- rec$size
      if (is.null(v)) v <- rec$byteSize
      if (is.null(v)) NA else v
    },
    license      = if (is.null(license_)) NA else license_
  )
}

#' Pull NIST Reference Datasets (RDS) catalog metadata
#'
#' The raw reference datasets (CSAFE bullets/cartridges, NSRL hash
#' library, ...) are multi-gigabyte and shipped on dedicated download
#' servers; this function returns only the catalog records so the
#' caller can pick what to download separately.
#'
#' @param dataset_id Specific NIST RDS / EDI id (e.g.
#'   \code{"ark:/88434/mds2-2418"}).  When set, returns a single-row
#'   frame.
#' @param query Free-text search over title / description / keyword.
#'   Ignored when \code{dataset_id} is set.
#' @param max_features Optional hard cap on returned rows.
#' @param page_size Records per request (default 50).
#' @param raw If \code{TRUE}, return the raw catalog JSON columns
#'   instead of morie's flattened schema.
#' @param timeout HTTP timeout in seconds.
#' @return A base R \code{data.frame}.
#' @export
morie_ingest_forensics_nist_rds <- function(
    dataset_id = NULL,
    query = NULL,
    max_features = NULL,
    page_size = 50L,
    raw = FALSE,
    timeout = .MORIE_FORENSICS_DEFAULT_TIMEOUT) {
  page_size <- as.integer(page_size)
  rows <- list()
  offset <- 0L
  repeat {
    params <- list(size = page_size, from = offset)
    if (!is.null(dataset_id) && nzchar(dataset_id)) {
      params[["@id"]] <- dataset_id
    } else if (!is.null(query) && nzchar(query)) {
      params$searchphrase <- query
    }
    payload <- .morie_forensics_get_json(
      url = .MORIE_NIST_RDS_BASE,
      params = params,
      headers = list(Accept = "application/json"),
      timeout = timeout,
      label = "NIST RDS"
    )
    batch <- payload$ResultData
    if (is.null(batch)) batch <- payload$results
    if (is.null(batch)) batch <- payload$data
    if (is.null(batch)) batch <- list()
    if (length(batch) == 0L) break
    for (rec in batch) {
      if (isTRUE(raw)) {
        rows[[length(rows) + 1L]] <- rec
      } else {
        rows[[length(rows) + 1L]] <- .morie_forensics_flatten_nist(rec)
      }
    }
    if (!is.null(dataset_id) && nzchar(dataset_id)) break
    if (!is.null(max_features) && length(rows) >= max_features) {
      rows <- rows[seq_len(max_features)]
      break
    }
    if (length(batch) < page_size) break
    offset <- offset + length(batch)
  }
  if (length(rows) == 0L) {
    stop(
      "morie_ingest_forensics_nist_rds: zero records (dataset_id=",
      if (is.null(dataset_id)) "NULL" else paste0("'", dataset_id, "'"),
      ", query=",
      if (is.null(query)) "NULL" else paste0("'", query, "'"), ").",
      call. = FALSE
    )
  }
  if (isTRUE(raw)) {
    .morie_forensics_rows_to_df(rows)
  } else {
    .morie_forensics_rows_to_df(rows, columns = .MORIE_NIST_RDS_COLUMNS)
  }
}
