# SPDX-License-Identifier: AGPL-3.0-or-later
#
# A2AJ Canadian Legal Data client (R port).
#
# Access to Algorithmic Justice (https://a2aj.ca) publishes Canadian
# court and tribunal decisions plus federal and provincial statutes
# and regulations through three open channels, documented at
# https://github.com/a2aj-ca/canadian-legal-data:
#
#   * a REST API at https://api.a2aj.ca (coverage / search / fetch);
#   * Hugging Face datasets `a2aj/canadian-case-law` and
#     `a2aj/canadian-laws`, one Parquet file per court or law type;
#   * the same Parquet files by direct download.
#
# Mirrors the Python `morie.ingest.a2aj` module:
#
#   * `morie_ingest_a2aj_coverage()`       - what the corpus holds
#   * `morie_ingest_a2aj_search()`         - full-text / name search
#   * `morie_ingest_a2aj_fetch()`          - one document by citation
#   * `morie_ingest_a2aj_download()`       - one court's Parquet file
#   * `morie_ingest_a2aj_load()`           - that file as a data.frame
#   * `morie_ingest_a2aj_citation_edges()` - citation network edges
#   * `morie_ingest_a2aj_gaps()`           - courts A2AJ does not carry
#
# HTTP routes through .morie_dataset_http_text_with_status (libcurl
# C++ backend with httr2 fallback); JSON through .morie_from_json.
# Parquet is read by the native reader, which decodes the LIST-typed
# citation columns, or by `arrow` when that is installed.
#
# Every text is an unofficial copy; each record carries an
# `upstream_license` field that may restrict commercial use.

.MORIE_A2AJ_API <- "https://api.a2aj.ca"
.MORIE_A2AJ_HF <- "https://huggingface.co/datasets/a2aj"
.MORIE_A2AJ_DEFAULT_UA <- "morie/r (+https://github.com/rootcoder007/rmorie)"
.MORIE_A2AJ_DEFAULT_TIMEOUT <- 60

#' Internal helper: one A2AJ API call, unwrapped to the parsed JSON
#' @noRd
.morie_a2aj_call <- function(endpoint, params = NULL,
                             timeout = .MORIE_A2AJ_DEFAULT_TIMEOUT,
                             user_agent = .MORIE_A2AJ_DEFAULT_UA) {
  url <- paste0(.MORIE_A2AJ_API, "/", endpoint)
  res <- .morie_dataset_http_text_with_status(
    url,
    query = params,
    headers = paste0("User-Agent: ", user_agent),
    timeout_s = as.integer(timeout)
  )
  if (res$status_code >= 400L) {
    stop("A2AJ ", endpoint, " -> HTTP ", res$status_code, ": ",
      substr(res$body, 1L, 200L),
      call. = FALSE
    )
  }
  if (!nzchar(trimws(res$body))) {
    stop("A2AJ ", endpoint, " returned an empty body", call. = FALSE)
  }
  tryCatch(
    .morie_from_json(res$body, simplifyVector = FALSE),
    error = function(e) {
      stop("A2AJ ", endpoint, " returned non-JSON: ",
        substr(res$body, 1L, 120L),
        call. = FALSE
      )
    }
  )
}

#' Internal helper: a list of JSON records to a data.frame
#'
#' Records may carry different keys; every key seen becomes a column,
#' missing values are NA. A key whose value is a JSON array in any
#' record becomes a list column (class AsIs) holding a character
#' vector per row, which is how the citation network fields arrive.
#' @noRd
.morie_a2aj_records_df <- function(records) {
  if (is.null(records) || length(records) == 0L) {
    return(data.frame())
  }
  keys <- unique(unlist(lapply(records, names)))
  cols <- lapply(keys, function(k) {
    vals <- lapply(records, function(r) r[[k]])
    is_vec <- vapply(vals, function(v) {
      is.list(v) || length(v) > 1L
    }, logical(1))
    if (any(is_vec)) {
      return(I(lapply(vals, function(v) {
        if (is.null(v)) {
          return(NULL)
        }
        out <- vapply(v, function(x) {
          if (is.null(x)) NA_character_ else as.character(x)
        }, character(1))
        as.character(out)
      })))
    }
    scalar <- lapply(vals, function(v) if (is.null(v)) NA else v)
    if (all(vapply(scalar, function(v) {
      is.numeric(v) || is.logical(v) && is.na(v)
    }, logical(1)))) {
      return(as.numeric(unlist(scalar)))
    }
    vapply(scalar, function(v) {
      if (is.na(v)[1L]) NA_character_ else as.character(v)
    }, character(1))
  })
  names(cols) <- keys
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

#' Internal helper: an ISO date string column to Date
#' @noRd
.morie_a2aj_as_date <- function(x) {
  as.Date(substr(as.character(x), 1L, 10L))
}

#' A2AJ corpus coverage
#'
#' Lists every court, tribunal or law collection in the A2AJ Canadian
#' Legal Data corpus with its date range and document count. This is
#' the same table the project's README carries, fetched live.
#'
#' @param doc_type \code{"cases"} (default) or \code{"laws"}.
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A data.frame with \code{dataset}, \code{description_en},
#'   \code{description_fr}, \code{earliest_document_date},
#'   \code{latest_document_date} (both \code{Date}) and
#'   \code{number_of_documents}.
#' @references Rehaag S, Wallace S, McCarten C (2025). A2AJ Canadian
#'   Legal Data. \url{https://github.com/a2aj-ca/canadian-legal-data}.
#' @seealso \code{\link{morie_ingest_a2aj_gaps}} for the courts the
#'   corpus does not carry.
#' @examples
#' \donttest{
#' cov <- try(morie_ingest_a2aj_coverage("cases"))
#' if (!inherits(cov, "try-error")) head(cov)
#' }
#' @export
morie_ingest_a2aj_coverage <- function(doc_type = c("cases", "laws"),
                                       timeout = .MORIE_A2AJ_DEFAULT_TIMEOUT,
                                       user_agent = .MORIE_A2AJ_DEFAULT_UA) {
  doc_type <- match.arg(doc_type)
  res <- .morie_a2aj_call("coverage", list(doc_type = doc_type),
    timeout = timeout, user_agent = user_agent
  )
  df <- .morie_a2aj_records_df(res$results)
  for (k in c("earliest_document_date", "latest_document_date")) {
    if (k %in% names(df)) df[[k]] <- .morie_a2aj_as_date(df[[k]])
  }
  if ("number_of_documents" %in% names(df)) {
    df$number_of_documents <- as.integer(df$number_of_documents)
  }
  df
}

#' Search A2AJ case law or legislation
#'
#' Full-text or title search over the A2AJ corpus. The API returns at
#' most 50 hits per call and does not page, so narrow with
#' \code{dataset} and the date window rather than asking for more.
#'
#' @param query Search string. The API accepts its own advanced syntax
#'   (quoted phrases, boolean operators); see
#'   \url{https://api.a2aj.ca/docs}.
#' @param search_type \code{"full_text"} (default) or \code{"name"}
#'   (titles only).
#' @param doc_type \code{"cases"} (default) or \code{"laws"}.
#' @param size Number of hits, 1 to 50.
#' @param search_language \code{"en"} (default) or \code{"fr"}.
#' @param sort_results \code{"default"} (relevance with the corpus's
#'   court weighting), \code{"newest_first"} or \code{"oldest_first"}.
#' @param dataset Optional character vector of dataset codes to
#'   restrict to (for example \code{c("SCC", "ONCA")}); see
#'   \code{\link{morie_ingest_a2aj_coverage}}.
#' @param start_date,end_date Optional \code{"YYYY-MM-DD"} bounds on
#'   the document date.
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A data.frame, one row per hit, with the record's metadata
#'   fields (\code{dataset}, \code{citation_en}, \code{name_en},
#'   \code{document_date_en}, \code{url_en}, \code{upstream_license},
#'   ...) and the search \code{score}. Zero rows when nothing matched.
#' @examples
#' \donttest{
#' hits <- try(morie_ingest_a2aj_search("roncarelli",
#'   search_type = "name", dataset = "SCC"
#' ))
#' if (!inherits(hits, "try-error")) hits[, c("citation_en", "name_en")]
#' }
#' @export
morie_ingest_a2aj_search <- function(query,
                                     search_type = c("full_text", "name"),
                                     doc_type = c("cases", "laws"),
                                     size = 10L,
                                     search_language = c("en", "fr"),
                                     sort_results = c(
                                       "default", "newest_first",
                                       "oldest_first"
                                     ),
                                     dataset = NULL,
                                     start_date = NULL,
                                     end_date = NULL,
                                     timeout = .MORIE_A2AJ_DEFAULT_TIMEOUT,
                                     user_agent = .MORIE_A2AJ_DEFAULT_UA) {
  if (!is.character(query) || length(query) != 1L || !nzchar(query)) {
    stop("query must be a single non-empty string", call. = FALSE)
  }
  search_type <- match.arg(search_type)
  doc_type <- match.arg(doc_type)
  search_language <- match.arg(search_language)
  sort_results <- match.arg(sort_results)
  size <- as.integer(size)
  if (is.na(size) || size < 1L || size > 50L) {
    stop("size must be between 1 and 50", call. = FALSE)
  }
  params <- list(
    query = query,
    search_type = search_type,
    doc_type = doc_type,
    size = size,
    search_language = search_language,
    sort_results = sort_results,
    dataset = if (length(dataset)) paste(dataset, collapse = ",") else NULL,
    start_date = start_date,
    end_date = end_date
  )
  res <- .morie_a2aj_call("search", params,
    timeout = timeout, user_agent = user_agent
  )
  df <- .morie_a2aj_records_df(res$results)
  if ("score" %in% names(df)) df$score <- as.numeric(df$score)
  df
}

#' Fetch one A2AJ document by citation
#'
#' Returns the unofficial full text (or a character window of it) of a
#' decision or law, with its metadata. For case law the citation
#' network fields (\code{cases_cited_*}, \code{cases_citing_*}) come
#' back as list columns when \code{include_citations = TRUE}.
#'
#' A citation the corpus does not hold returns \code{NULL}. For a court
#' A2AJ does not cover at all, see \code{\link{morie_ingest_a2aj_gaps}}
#' and the CanLII client, \code{\link{morie_ingest_canlii_case}}.
#'
#' @param citation Neutral or official citation, in English or French,
#'   for example \code{"2023 SCC 17"} or \code{"RSC 1985, c C-46"}.
#' @param doc_type \code{"cases"} (default) or \code{"laws"}.
#' @param output_language \code{"en"} (default), \code{"fr"} or
#'   \code{"both"}.
#' @param section Laws only: one section to return instead of the
#'   whole text.
#' @param start_char,end_char Character window of the text;
#'   \code{end_char = -1} (default) runs to the end. Ignored when
#'   \code{section} is given.
#' @param include_citations Cases only: also return the lists of cited
#'   and citing cases.
#' @param citations_limit,citations_offset Page through those lists
#'   (limit at most 1000).
#' @param timeout HTTP timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A one-row data.frame, or \code{NULL} when the citation is
#'   not in the corpus. Text is in \code{unofficial_text_en} and/or
#'   \code{unofficial_text_fr}.
#' @examples
#' \donttest{
#' doc <- try(morie_ingest_a2aj_fetch("2023 SCC 17", end_char = 400))
#' if (is.data.frame(doc)) doc$name_en
#' }
#' @export
morie_ingest_a2aj_fetch <- function(citation,
                                    doc_type = c("cases", "laws"),
                                    output_language = c("en", "fr", "both"),
                                    section = NULL,
                                    start_char = 0L,
                                    end_char = -1L,
                                    include_citations = FALSE,
                                    citations_limit = 100L,
                                    citations_offset = 0L,
                                    timeout = .MORIE_A2AJ_DEFAULT_TIMEOUT,
                                    user_agent = .MORIE_A2AJ_DEFAULT_UA) {
  if (!is.character(citation) || length(citation) != 1L ||
    !nzchar(citation)) {
    stop("citation must be a single non-empty string", call. = FALSE)
  }
  doc_type <- match.arg(doc_type)
  output_language <- match.arg(output_language)
  params <- list(
    citation = citation,
    doc_type = doc_type,
    output_language = output_language,
    section = if (doc_type == "laws") section else NULL,
    start_char = as.integer(start_char),
    end_char = as.integer(end_char),
    include_citations = if (doc_type == "cases" && isTRUE(include_citations)) {
      "true"
    } else {
      NULL
    },
    citations_limit = if (isTRUE(include_citations)) {
      as.integer(citations_limit)
    } else {
      NULL
    },
    citations_offset = if (isTRUE(include_citations)) {
      as.integer(citations_offset)
    } else {
      NULL
    }
  )
  res <- .morie_a2aj_call("fetch", params,
    timeout = timeout, user_agent = user_agent
  )
  if (is.null(res$results) || length(res$results) == 0L) {
    return(NULL)
  }
  df <- .morie_a2aj_records_df(res$results)
  if ("citing_cases_count" %in% names(df)) {
    df$citing_cases_count <- as.integer(df$citing_cases_count)
  }
  df
}

#' Parquet URL of one A2AJ dataset
#'
#' @param dataset Dataset code, for example \code{"SCC"} or
#'   \code{"LEGISLATION-FED"}.
#' @param doc_type \code{"cases"} (default) or \code{"laws"}.
#' @return The Hugging Face download URL of that dataset's
#'   \code{train.parquet}.
#' @examples
#' morie_ingest_a2aj_url("SCC")
#' morie_ingest_a2aj_url("REGULATIONS-ON", doc_type = "laws")
#' @export
morie_ingest_a2aj_url <- function(dataset, doc_type = c("cases", "laws")) {
  doc_type <- match.arg(doc_type)
  if (!is.character(dataset) || length(dataset) != 1L ||
    !grepl("^[A-Z0-9-]+$", dataset)) {
    stop("dataset must be one upper-case code such as \"SCC\" or ",
      "\"LEGISLATION-FED\"",
      call. = FALSE
    )
  }
  repo <- if (doc_type == "cases") "canadian-case-law" else "canadian-laws"
  sprintf("%s/%s/resolve/main/%s/train.parquet", .MORIE_A2AJ_HF, repo, dataset)
}

#' Download one A2AJ dataset's Parquet file
#'
#' Fetches the Hugging Face Parquet file for one court, tribunal or
#' law collection into a cache directory and returns its path. Files
#' range from a few megabytes (small tribunals) to several hundred
#' (the Supreme Court of Canada, the BC Supreme Court).
#'
#' @param dataset Dataset code, for example \code{"SCC"}.
#' @param doc_type \code{"cases"} (default) or \code{"laws"}.
#' @param cache_dir Directory to keep the file in. Defaults to a
#'   session-scoped \code{tempdir()} subdirectory; pass
#'   \code{morie_cache_dir("a2aj")} to keep it across sessions.
#' @param refresh Re-download even when the file is already cached.
#' @param quiet Passed to \code{\link[utils]{download.file}}.
#' @return The local path of the Parquet file, invisibly.
#' @examples
#' \donttest{
#' p <- try(morie_ingest_a2aj_download("SCT"))
#' if (!inherits(p, "try-error")) file.info(p)$size
#' }
#' @export
morie_ingest_a2aj_download <- function(dataset,
                                       doc_type = c("cases", "laws"),
                                       cache_dir = NULL,
                                       refresh = FALSE,
                                       quiet = TRUE) {
  doc_type <- match.arg(doc_type)
  url <- morie_ingest_a2aj_url(dataset, doc_type)
  if (is.null(cache_dir)) {
    cache_dir <- file.path(tempdir(), "morie-a2aj")
  }
  dir.create(file.path(cache_dir, doc_type),
    showWarnings = FALSE, recursive = TRUE
  )
  dest <- file.path(cache_dir, doc_type, paste0(dataset, ".parquet"))
  if (file.exists(dest) && !isTRUE(refresh)) {
    return(invisible(dest))
  }
  tmp <- paste0(dest, ".part")
  method <- if (isTRUE(capabilities("libcurl"))) "libcurl" else "auto"
  status <- utils::download.file(url, tmp,
    mode = "wb", quiet = quiet, method = method
  )
  if (!identical(as.integer(status), 0L) || !file.exists(tmp)) {
    unlink(tmp)
    stop("download of ", url, " failed (status ", status, ")",
      call. = FALSE
    )
  }
  file.rename(tmp, dest)
  invisible(dest)
}

#' Load one A2AJ dataset as a data.frame
#'
#' Downloads (or reuses) the dataset's Parquet file and decodes it. The
#' \code{unofficial_text_*} columns hold the full texts and dominate
#' the file, so pass \code{columns} to skip them when only metadata or
#' the citation network is needed.
#'
#' @param dataset Dataset code, for example \code{"SCC"}.
#' @param doc_type \code{"cases"} (default) or \code{"laws"}.
#' @param columns Optional character vector of columns to decode.
#' @param engine \code{"auto"} (default: \pkg{arrow} when installed,
#'   otherwise the native reader), \code{"native"} or \code{"arrow"}.
#'   The native reader is pure R and decodes the LIST-typed citation
#'   columns; it is fine for the smaller courts and slow for the
#'   largest files, which is where \pkg{arrow} earns its place.
#' @param cache_dir,refresh Passed to
#'   \code{\link{morie_ingest_a2aj_download}}.
#' @return A data.frame; \code{cases_cited_*} and \code{cases_citing_*}
#'   are list columns with one character vector per row.
#' @examples
#' \donttest{
#' sct <- try(morie_ingest_a2aj_load("SCT",
#'   columns = c("citation_en", "document_date_en", "cases_cited_en")
#' ))
#' if (is.data.frame(sct)) head(sct$citation_en)
#' }
#' @export
morie_ingest_a2aj_load <- function(dataset,
                                   doc_type = c("cases", "laws"),
                                   columns = NULL,
                                   engine = c("auto", "native", "arrow"),
                                   cache_dir = NULL,
                                   refresh = FALSE) {
  doc_type <- match.arg(doc_type)
  engine <- match.arg(engine)
  path <- morie_ingest_a2aj_download(dataset, doc_type,
    cache_dir = cache_dir, refresh = refresh
  )
  use_arrow <- switch(engine,
    auto = requireNamespace("arrow", quietly = TRUE),
    arrow = TRUE,
    native = FALSE
  )
  if (use_arrow) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("engine = \"arrow\" needs the 'arrow' package", call. = FALSE)
    }
    reader <- arrow::ParquetFileReader$create(path)
    if (is.null(columns)) {
      tb <- reader$ReadTable()
    } else {
      idx <- match(columns, names(reader$GetSchema()))
      if (anyNA(idx)) {
        stop("no such column(s) in ", path, ": ",
          paste(columns[is.na(idx)], collapse = ", "),
          call. = FALSE
        )
      }
      tb <- reader$ReadTable(idx - 1L)
    }
    return(as.data.frame(tb))
  }
  morie_read_parquet(path, columns = columns)
}

#' Citation network edges from A2AJ case records
#'
#' Turns the \code{cases_cited_<language>} list column of an A2AJ case
#' data.frame into an edge list, one row per (citing, cited) pair, in
#' the order the citations first appear in the decision. Feed the
#' result to any graph routine; \code{table(edges$to)} is the in-corpus
#' citation count of each cited decision.
#'
#' @param x A data.frame from \code{\link{morie_ingest_a2aj_load}} or
#'   \code{\link{morie_ingest_a2aj_fetch}} with
#'   \code{include_citations = TRUE}.
#' @param language \code{"en"} (default) or \code{"fr"}: which
#'   citation column and citation string to use.
#' @return A data.frame with character columns \code{from} (the citing
#'   decision's neutral citation) and \code{to} (the cited one).
#' @examples
#' x <- data.frame(citation_en = c("2020 SCC 1", "2021 ONCA 2"))
#' x$cases_cited_en <- I(list(c("2019 SCC 9", "2018 FCA 3"), character(0)))
#' morie_ingest_a2aj_citation_edges(x)
#' @export
morie_ingest_a2aj_citation_edges <- function(x, language = c("en", "fr")) {
  language <- match.arg(language)
  from_col <- paste0("citation_", language)
  to_col <- paste0("cases_cited_", language)
  if (!is.data.frame(x) || !all(c(from_col, to_col) %in% names(x))) {
    stop("x needs columns ", from_col, " and ", to_col, call. = FALSE)
  }
  cited <- x[[to_col]]
  from <- as.character(x[[from_col]])
  edges <- lapply(seq_len(nrow(x)), function(i) {
    to <- cited[[i]]
    to <- as.character(to[!is.na(to) & nzchar(to)])
    if (!length(to)) {
      return(NULL)
    }
    data.frame(from = rep(from[i], length(to)), to = to,
      stringsAsFactors = FALSE
    )
  })
  edges <- edges[!vapply(edges, is.null, logical(1))]
  if (!length(edges)) {
    return(data.frame(from = character(0), to = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, edges)
  rownames(out) <- NULL
  out
}

#' Courts and tribunals the A2AJ corpus does not carry
#'
#' A2AJ covers the federal courts, most federal tribunals and the
#' appellate and superior courts of British Columbia, Ontario (Court
#' of Appeal only), Nova Scotia and Yukon. Its open issues ask for the
#' Human Rights Tribunal of Ontario, the BC Human Rights Tribunal, the
#' Ontario Superior Court of Justice, Alberta, Quebec and the remaining
#' provinces. Until the corpus grows, those decisions are reachable
#' through CanLII's API (\code{\link{morie_ingest_canlii_cases}}),
#' which needs a free key; this table maps each gap to the CanLII
#' database id used on canlii.org.
#'
#' @return A data.frame with \code{code}, \code{name},
#'   \code{jurisdiction}, \code{canlii_database_id} and \code{issue}
#'   (the upstream issue number that requested it, or NA).
#' @references \url{https://github.com/a2aj-ca/canadian-legal-data/issues}
#' @examples
#' morie_ingest_a2aj_gaps()
#' @export
morie_ingest_a2aj_gaps <- function() {
  data.frame(
    code = c(
      "HRTO", "BCHRT", "ONSC", "ONCJ",
      "ABCA", "ABKB", "ABCJ",
      "QCCA", "QCCS", "QCCQ",
      "MBCA", "MBKB", "SKCA", "SKKB",
      "NBCA", "NBKB", "NLCA", "NLSC",
      "PECA", "PESC", "NTCA", "NTSC", "NUCJ", "YKSC"
    ),
    name = c(
      "Human Rights Tribunal of Ontario",
      "British Columbia Human Rights Tribunal",
      "Ontario Superior Court of Justice",
      "Ontario Court of Justice",
      "Court of Appeal of Alberta",
      "Court of King's Bench of Alberta",
      "Alberta Court of Justice",
      "Court of Appeal of Quebec",
      "Superior Court of Quebec",
      "Court of Quebec",
      "Court of Appeal of Manitoba",
      "Court of King's Bench of Manitoba",
      "Court of Appeal for Saskatchewan",
      "Court of King's Bench for Saskatchewan",
      "Court of Appeal of New Brunswick",
      "Court of King's Bench of New Brunswick",
      "Court of Appeal of Newfoundland and Labrador",
      "Supreme Court of Newfoundland and Labrador",
      "Prince Edward Island Court of Appeal",
      "Supreme Court of Prince Edward Island",
      "Court of Appeal for the Northwest Territories",
      "Supreme Court of the Northwest Territories",
      "Nunavut Court of Justice",
      "Supreme Court of Yukon"
    ),
    jurisdiction = c(
      "ON", "BC", "ON", "ON", "AB", "AB", "AB", "QC", "QC", "QC",
      "MB", "MB", "SK", "SK", "NB", "NB", "NL", "NL", "PE", "PE",
      "NT", "NT", "NU", "YT"
    ),
    canlii_database_id = c(
      "onhrt", "bchrt", "onsc", "oncj",
      "abca", "abkb", "abcj",
      "qcca", "qccs", "qccq",
      "mbca", "mbkb", "skca", "skkb",
      "nbca", "nbkb", "nlca", "nlsc",
      "peca", "pesctd", "ntca", "ntsc", "nucj", "yksc"
    ),
    issue = c(
      4L, 4L, 4L, NA, 3L, 3L, 3L, 1L, 1L, 1L,
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
    ),
    stringsAsFactors = FALSE
  )
}
