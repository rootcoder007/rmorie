# SPDX-License-Identifier: AGPL-3.0-or-later
#
# CanLII REST API client (R port).
#
# The Canadian Legal Information Institute (https://www.canlii.org)
# publishes every Canadian court and most tribunals, which makes it
# the route to the decisions the A2AJ corpus does not yet carry (see
# morie_ingest_a2aj_gaps()): the Human Rights Tribunal of Ontario, the
# BC Human Rights Tribunal, the Ontario Superior Court, Alberta, Quebec
# and the remaining provinces. Its API (https://api.canlii.org/v1)
# returns metadata and the citation network, not full text; every
# record carries the canlii.org URL of the decision. A key is free on
# request from CanLII and is read from the CANLII_API_KEY environment
# variable when not passed explicitly.
#
# Mirrors the Python `morie.ingest.canlii` module:
#
#   * `morie_ingest_canlii_databases()`   - courts and tribunals
#   * `morie_ingest_canlii_cases()`       - decisions of one database
#   * `morie_ingest_canlii_case()`        - one decision's metadata
#   * `morie_ingest_canlii_citator()`     - cited / citing cases, laws
#   * `morie_ingest_canlii_legislation_databases()`
#   * `morie_ingest_canlii_legislations()`
#   * `morie_ingest_canlii_legislation()`
#   * `morie_ingest_canlii_case_id()`     - neutral citation -> ids
#
# HTTP routes through .morie_dataset_http_text_with_status (libcurl
# C++ backend with httr2 fallback); JSON through .morie_from_json.

.MORIE_CANLII_API <- "https://api.canlii.org/v1"
.MORIE_CANLII_DEFAULT_UA <- "morie/r (+https://github.com/rootcoder007/rmorie)"
.MORIE_CANLII_DEFAULT_TIMEOUT <- 60
.MORIE_CANLII_MAX_RESULTS <- 10000L

#' Internal helper: resolve the API key
#' @noRd
.morie_canlii_key <- function(api_key = NULL) {
  if (is.null(api_key) || !nzchar(api_key)) {
    api_key <- Sys.getenv("CANLII_API_KEY", "")
  }
  if (!nzchar(api_key)) {
    stop("A CanLII API key is required: pass api_key = or set the ",
      "CANLII_API_KEY environment variable. Keys are free on request ",
      "from https://www.canlii.org/en/feedback/feedback.html",
      call. = FALSE
    )
  }
  api_key
}

#' Internal helper: one CanLII API call, unwrapped to the parsed JSON
#' @noRd
.morie_canlii_call <- function(path, params = NULL, api_key = NULL,
                               timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                               user_agent = .MORIE_CANLII_DEFAULT_UA) {
  key <- .morie_canlii_key(api_key)
  url <- paste0(.MORIE_CANLII_API, "/", path)
  params <- c(list(api_key = key), params)
  res <- .morie_dataset_http_text_with_status(
    url,
    query = params,
    headers = paste0("User-Agent: ", user_agent),
    timeout_s = as.integer(timeout)
  )
  body <- res$body
  parsed <- tryCatch(
    .morie_from_json(body, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (res$status_code >= 400L) {
    detail <- if (is.list(parsed) && !is.null(parsed$message)) {
      parsed$message
    } else if (is.list(parsed) && !is.null(parsed$error)) {
      parsed$error
    } else {
      substr(body, 1L, 200L)
    }
    stop("CanLII ", path, " -> HTTP ", res$status_code, ": ", detail,
      call. = FALSE
    )
  }
  if (is.null(parsed)) {
    stop("CanLII ", path, " returned non-JSON: ", substr(body, 1L, 120L),
      call. = FALSE
    )
  }
  parsed
}

#' Internal helper: a list of CanLII records to a data.frame
#'
#' CanLII identifiers arrive as language-keyed objects
#' (\code{"caseId": {"en": "2008scc9"}}); those collapse to the single
#' string. Other nested values are kept as list columns.
#' @noRd
.morie_canlii_records_df <- function(records) {
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  records <- lapply(records, function(r) {
    for (k in c("caseId", "legislationId", "databaseId")) {
      v <- r[[k]]
      if (is.list(v) && length(v) >= 1L) r[[k]] <- as.character(v[[1L]])
    }
    r
  })
  .morie_a2aj_records_df(records)
}

#' Internal helper: optional YYYY-MM-DD parameters, validated
#' @noRd
.morie_canlii_date_params <- function(...) {
  args <- list(...)
  args <- args[!vapply(args, is.null, logical(1))]
  for (nm in names(args)) {
    v <- as.character(args[[nm]])
    if (length(v) != 1L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v)) {
      stop(nm, " must be one \"YYYY-MM-DD\" string", call. = FALSE)
    }
    args[[nm]] <- v
  }
  args
}

#' CanLII case-law databases
#'
#' Lists every court and tribunal CanLII carries, with the
#' \code{databaseId} that the other CanLII functions take.
#'
#' @param language \code{"en"} (default) or \code{"fr"}.
#' @param api_key CanLII API key; defaults to the \code{CANLII_API_KEY}
#'   environment variable.
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A data.frame with \code{databaseId}, \code{jurisdiction} and
#'   \code{name}.
#' @references CanLII API documentation,
#'   \url{https://github.com/canlii/API_documentation}.
#' @examples
#' \dontrun{
#' dbs <- morie_ingest_canlii_databases()
#' dbs[dbs$jurisdiction == "on", ]
#' }
#' @export
morie_ingest_canlii_databases <- function(language = c("en", "fr"),
                                          api_key = NULL,
                                          timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                          user_agent = .MORIE_CANLII_DEFAULT_UA) {
  language <- match.arg(language)
  res <- .morie_canlii_call(paste0("caseBrowse/", language, "/"),
    api_key = api_key, timeout = timeout, user_agent = user_agent
  )
  .morie_canlii_records_df(res$caseDatabases)
}

#' Decisions of one CanLII database
#'
#' Pages through the decisions of a court or tribunal, newest first,
#' optionally within publication, modification or decision-date
#' windows. CanLII returns at most 10,000 rows per call; use
#' \code{offset} to page.
#'
#' @param database_id CanLII database id, for example \code{"onhrt"}
#'   (Human Rights Tribunal of Ontario) or \code{"bcsc"}; see
#'   \code{\link{morie_ingest_canlii_databases}} and
#'   \code{\link{morie_ingest_a2aj_gaps}}.
#' @param offset Zero-based offset into the result list.
#' @param result_count Rows to return, at most 10,000.
#' @param published_before,published_after,modified_before,modified_after,changed_before,changed_after,decision_date_before,decision_date_after
#'   Optional \code{"YYYY-MM-DD"} filters.
#' @param language \code{"en"} (default) or \code{"fr"}.
#' @param api_key CanLII API key; defaults to the \code{CANLII_API_KEY}
#'   environment variable.
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A data.frame with \code{databaseId}, \code{caseId},
#'   \code{title} and \code{citation}.
#' @examples
#' \dontrun{
#' hrto <- morie_ingest_canlii_cases("onhrt",
#'   result_count = 200,
#'   decision_date_after = "2024-01-01"
#' )
#' head(hrto[, c("caseId", "title", "citation")])
#' }
#' @export
morie_ingest_canlii_cases <- function(database_id,
                                      offset = 0L,
                                      result_count = 100L,
                                      published_before = NULL,
                                      published_after = NULL,
                                      modified_before = NULL,
                                      modified_after = NULL,
                                      changed_before = NULL,
                                      changed_after = NULL,
                                      decision_date_before = NULL,
                                      decision_date_after = NULL,
                                      language = c("en", "fr"),
                                      api_key = NULL,
                                      timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                      user_agent = .MORIE_CANLII_DEFAULT_UA) {
  language <- match.arg(language)
  database_id <- .morie_canlii_id(database_id, "database_id")
  result_count <- as.integer(result_count)
  if (is.na(result_count) || result_count < 1L ||
    result_count > .MORIE_CANLII_MAX_RESULTS) {
    stop("result_count must be between 1 and ", .MORIE_CANLII_MAX_RESULTS,
      call. = FALSE
    )
  }
  params <- c(
    list(offset = as.integer(offset), resultCount = result_count),
    .morie_canlii_date_params(
      publishedBefore = published_before,
      publishedAfter = published_after,
      modifiedBefore = modified_before,
      modifiedAfter = modified_after,
      changedBefore = changed_before,
      changedAfter = changed_after,
      decisionDateBefore = decision_date_before,
      decisionDateAfter = decision_date_after
    )
  )
  res <- .morie_canlii_call(
    paste0("caseBrowse/", language, "/", database_id, "/"),
    params = params, api_key = api_key, timeout = timeout,
    user_agent = user_agent
  )
  .morie_canlii_records_df(res$cases)
}

#' Metadata of one CanLII decision
#'
#' @param database_id CanLII database id, for example \code{"bcsc"}.
#' @param case_id CanLII case id, for example \code{"2007bcsc1700"};
#'   \code{\link{morie_ingest_canlii_case_id}} derives it from a
#'   neutral citation.
#' @param language \code{"en"} (default) or \code{"fr"}.
#' @param api_key CanLII API key; defaults to the \code{CANLII_API_KEY}
#'   environment variable.
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A one-row data.frame with \code{databaseId}, \code{caseId},
#'   \code{url}, \code{title}, \code{citation}, \code{language},
#'   \code{docketNumber}, \code{decisionDate}, \code{keywords} and
#'   \code{concatenatedId}.
#' @examples
#' \dontrun{
#' # Issue 2 of the A2AJ tracker: a BC Supreme Court decision the corpus
#' # lacks. CanLII has it.
#' ids <- morie_ingest_canlii_case_id("2007 BCSC 1700")
#' morie_ingest_canlii_case(ids$database_id, ids$case_id)
#' }
#' @export
morie_ingest_canlii_case <- function(database_id, case_id,
                                     language = c("en", "fr"),
                                     api_key = NULL,
                                     timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                     user_agent = .MORIE_CANLII_DEFAULT_UA) {
  language <- match.arg(language)
  database_id <- .morie_canlii_id(database_id, "database_id")
  case_id <- .morie_canlii_id(case_id, "case_id")
  res <- .morie_canlii_call(
    paste0("caseBrowse/", language, "/", database_id, "/", case_id, "/"),
    api_key = api_key, timeout = timeout, user_agent = user_agent
  )
  .morie_canlii_records_df(list(res))
}

#' Citation network of one CanLII decision
#'
#' The cases a decision cites, the cases that cite it, or the
#' legislation it cites. CanLII serves the citator in English only.
#'
#' @param database_id CanLII database id.
#' @param case_id CanLII case id.
#' @param type \code{"citedCases"} (default), \code{"citingCases"} or
#'   \code{"citedLegislations"}.
#' @param api_key CanLII API key; defaults to the \code{CANLII_API_KEY}
#'   environment variable.
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A data.frame of the cited or citing records (\code{databaseId},
#'   \code{caseId} or \code{legislationId}, \code{title},
#'   \code{citation}), zero rows when there are none.
#' @examples
#' \dontrun{
#' morie_ingest_canlii_citator("csc-scc", "2008scc9", "citingCases")
#' }
#' @export
morie_ingest_canlii_citator <- function(database_id, case_id,
                                        type = c(
                                          "citedCases", "citingCases",
                                          "citedLegislations"
                                        ),
                                        api_key = NULL,
                                        timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                        user_agent = .MORIE_CANLII_DEFAULT_UA) {
  type <- match.arg(type)
  database_id <- .morie_canlii_id(database_id, "database_id")
  case_id <- .morie_canlii_id(case_id, "case_id")
  res <- .morie_canlii_call(
    paste0("caseCitator/en/", database_id, "/", case_id, "/", type),
    api_key = api_key, timeout = timeout, user_agent = user_agent
  )
  .morie_canlii_records_df(res[[type]])
}

#' CanLII legislation databases
#'
#' @inheritParams morie_ingest_canlii_databases
#' @return A data.frame with \code{databaseId}, \code{type},
#'   \code{jurisdiction} and \code{name}.
#' @examples
#' \dontrun{
#' morie_ingest_canlii_legislation_databases()
#' }
#' @export
morie_ingest_canlii_legislation_databases <- function(language = c("en", "fr"),
                                                      api_key = NULL,
                                                      timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                                      user_agent = .MORIE_CANLII_DEFAULT_UA) {
  language <- match.arg(language)
  res <- .morie_canlii_call(paste0("legislationBrowse/", language, "/"),
    api_key = api_key, timeout = timeout, user_agent = user_agent
  )
  .morie_canlii_records_df(res$legislationDatabases)
}

#' Statutes or regulations of one CanLII legislation database
#'
#' @param database_id CanLII legislation database id, for example
#'   \code{"cas"} (federal statutes) as listed by
#'   \code{\link{morie_ingest_canlii_legislation_databases}}.
#' @inheritParams morie_ingest_canlii_databases
#' @return A data.frame with \code{databaseId}, \code{legislationId},
#'   \code{title}, \code{citation} and \code{type}.
#' @examples
#' \dontrun{
#' morie_ingest_canlii_legislations("cas")
#' }
#' @export
morie_ingest_canlii_legislations <- function(database_id,
                                             language = c("en", "fr"),
                                             api_key = NULL,
                                             timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                             user_agent = .MORIE_CANLII_DEFAULT_UA) {
  language <- match.arg(language)
  database_id <- .morie_canlii_id(database_id, "database_id")
  res <- .morie_canlii_call(
    paste0("legislationBrowse/", language, "/", database_id, "/"),
    api_key = api_key, timeout = timeout, user_agent = user_agent
  )
  .morie_canlii_records_df(res$legislations)
}

#' Metadata of one CanLII statute or regulation
#'
#' @param database_id CanLII legislation database id.
#' @param legislation_id CanLII legislation id from
#'   \code{\link{morie_ingest_canlii_legislations}}.
#' @inheritParams morie_ingest_canlii_databases
#' @return A one-row data.frame with \code{legislationId}, \code{url},
#'   \code{title}, \code{citation}, \code{type}, \code{language},
#'   \code{dateScheme}, \code{startDate}, \code{endDate},
#'   \code{repealed} and \code{content}.
#' @examples
#' \dontrun{
#' morie_ingest_canlii_legislation("cas", "rsc-1985-c-c-46")
#' }
#' @export
morie_ingest_canlii_legislation <- function(database_id, legislation_id,
                                            language = c("en", "fr"),
                                            api_key = NULL,
                                            timeout = .MORIE_CANLII_DEFAULT_TIMEOUT,
                                            user_agent = .MORIE_CANLII_DEFAULT_UA) {
  language <- match.arg(language)
  database_id <- .morie_canlii_id(database_id, "database_id")
  legislation_id <- .morie_canlii_id(legislation_id, "legislation_id")
  res <- .morie_canlii_call(
    paste0(
      "legislationBrowse/", language, "/", database_id, "/",
      legislation_id, "/"
    ),
    api_key = api_key, timeout = timeout, user_agent = user_agent
  )
  .morie_canlii_records_df(list(res))
}

#' Internal helper: validate one path identifier
#' @noRd
.morie_canlii_id <- function(x, what) {
  if (!is.character(x) || length(x) != 1L || !grepl("^[a-z0-9-]+$", x)) {
    stop(what, " must be one lower-case CanLII identifier such as ",
      "\"bcsc\" or \"2007bcsc1700\"",
      call. = FALSE
    )
  }
  x
}

#' CanLII identifiers from a neutral citation
#'
#' A neutral citation such as \code{"2007 BCSC 1700"} names its court
#' (\code{BCSC}) and CanLII's case id is the citation in lower case
#' without spaces (\code{"2007bcsc1700"}). The Supreme Court of Canada
#' is the one court whose database id differs from its citation code
#' (\code{"csc-scc"}). Reporter-style citations
#' (\code{"[1959] SCR 121"}) have no derivable id and come back NA.
#'
#' @param citation Character vector of citations.
#' @return A data.frame with \code{citation}, \code{database_id} and
#'   \code{case_id}, NA where the citation is not neutral.
#' @examples
#' morie_ingest_canlii_case_id(c("2007 BCSC 1700", "2008 SCC 9", "[1959] SCR 121"))
#' @export
morie_ingest_canlii_case_id <- function(citation) {
  citation <- as.character(citation)
  m <- regmatches(
    citation,
    regexec("^[[:space:]]*([0-9]{4})[[:space:]]+([A-Za-z]+)[[:space:]]+([0-9]+)[[:space:]]*$", citation)
  )
  db <- vapply(m, function(x) {
    if (length(x) == 4L) tolower(x[3L]) else NA_character_
  }, character(1))
  id <- vapply(m, function(x) {
    if (length(x) == 4L) tolower(paste0(x[2L], x[3L], x[4L])) else NA_character_
  }, character(1))
  db[!is.na(db) & db == "scc"] <- "csc-scc"
  data.frame(
    citation = citation, database_id = db, case_id = id,
    stringsAsFactors = FALSE
  )
}
