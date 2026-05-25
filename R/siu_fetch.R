# SPDX-License-Identifier: AGPL-3.0-or-later
#' Lightweight Ontario SIU Director's Reports scraper (R-native)
#'
#' On-demand scraper for the Ontario Special Investigations Unit (SIU)
#' Director's Reports index at \url{https://www.siu.on.ca/en/directors_reports.php}.
#' This is the R port of \code{morie.siu_fetch} -- the lightweight
#' \code{httr2}/\code{rvest} path that complements the C/C++ harvester
#' in \code{\link{morie_fetch_siu}}. Use this when:
#'
#' \itemize{
#'   \item you want a tiny R-only dependency footprint (no compiled code);
#'   \item you only need the header / index fields (case_number,
#'         police_service, incident date, decision date) -- not the
#'         full 64-column schema;
#'   \item you are running on a host where the C++ parser does not build.
#' }
#'
#' Distribution policy (2026-05): the scraped corpus is NOT shipped with
#' the package. Each user runs the scraper themselves, which is
#' unambiguously fair use of public oversight reports.
#'
#' The scraper is conservative: a 2-second delay between requests,
#' retries on 5xx, and a descriptive user-agent. The latest published
#' year as of release is 2023; \code{years = NULL} (the default) scrapes
#' the unfiltered index, which surfaces the most recent posts.
#'
#' @section Cache directory:
#' By default this writes \code{SIU.csv} under \code{\link[base]{tempdir}()}
#' so R cleans it up at end of session. Pass \code{cache_dir =
#' morie_cache_dir("siu")} explicitly to opt into a persistent cross-
#' session cache; see \code{\link{morie_cache_dir}} and
#' \code{\link{morie_cache_clear}} (no implicit writes to \code{~/.cache}).
#'
#' @name morie_siu_fetch
NULL


#' @rdname morie_siu_fetch
#' @export
morie_siu_index_url <- function() {
  "https://www.siu.on.ca/en/directors_reports.php"
}

# Internal: latest fiscal year of SIU reports we treat as "published".
# SIU keeps active a small rolling index; reports outside this year
# may or may not return a row. Keep this in sync with the Python
# `siu_fetch.SIU_LATEST_YEAR` if/when it lands upstream.
.siu_fetch_latest_year <- 2023L
.siu_fetch_user_agent <-
  "morie/0.9 (+https://github.com/rootcoder007/morie)"
.siu_fetch_rate_seconds <- 2.0


#' Cache-path helper for the lightweight SIU scraper
#'
#' Returns the path \code{<cache_dir>/SIU.csv}, creating
#' \code{cache_dir} if needed. Default is \code{file.path(tempdir(),
#' "morie", "siu")}; pass \code{morie_cache_dir("siu")} for persistent
#' caching.
#'
#' @param cache_dir Output directory.
#' @return Absolute path to \code{SIU.csv} (file may not exist yet).
#' @examples
#' p <- morie_siu_cache_path(tempfile("siu_demo_"))
#' p
#' @export
morie_siu_cache_path <- function(cache_dir = file.path(tempdir(), "morie", "siu")) {
  cache_dir <- path.expand(cache_dir)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(cache_dir, "SIU.csv")
}


# Internal: polite HTTP GET via httr2. Gated on the httr2 namespace so
# the package's base footprint stays light.
.siu_fetch_http_get <- function(url, timeout_s = 60L) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "morie_siu_fetch_cases() needs the 'httr2' package: ",
      "install.packages('httr2')",
      call. = FALSE
    )
  }
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, .siu_fetch_user_agent)
  req <- httr2::req_timeout(req, timeout_s)
  req <- httr2::req_retry(
    req,
    max_tries = 3L,
    is_transient = function(resp) httr2::resp_status(resp) >= 500L
  )
  resp <- httr2::req_perform(req)
  httr2::resp_body_string(resp, encoding = "UTF-8")
}


# Internal: pull (case_number, absolute_url) pairs out of an index page
# using a tag-aware xml2/rvest pass when available, falling back to a
# regex sweep that matches the Python implementation. Returns a
# character matrix with columns "case_number" and "url".
.siu_fetch_extract_links <- function(index_html, base_url) {
  if (requireNamespace("xml2", quietly = TRUE) &&
      requireNamespace("rvest", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(index_html), error = function(e) NULL)
    if (!is.null(doc)) {
      anchors <- rvest::html_elements(doc,
        "a[href*='case_summary_details.php']"
      )
      hrefs <- rvest::html_attr(anchors, "href")
      texts <- trimws(rvest::html_text2(anchors))
      ok <- !is.na(hrefs) & nzchar(hrefs)
      hrefs <- hrefs[ok]
      texts <- texts[ok]
      cn_pat <- "([A-Za-z]+-?[0-9]+|[0-9]+-[A-Z]+-[0-9]+)"
      m <- regmatches(texts, regexpr(cn_pat, texts))
      keep <- nzchar(m)
      if (any(keep)) {
        return(cbind(
          case_number = m[keep],
          url = .siu_fetch_resolve_url(hrefs[keep], base_url)
        ))
      }
    }
  }
  # Regex fallback (mirrors siu_fetch._extract_case_links).
  pat <- paste0(
    'href="(case_summary_details\\.php\\?[^"]+)"[^>]*>',
    "(?:\\s*<[^>]+>)*\\s*",
    "([A-Za-z\\-]+[0-9]+|[0-9]+-[A-Z]+-[0-9]+)"
  )
  m <- gregexpr(pat, index_html, perl = TRUE, ignore.case = TRUE)[[1L]]
  if (m[1L] < 0L) {
    return(matrix(character(0L), ncol = 2L,
      dimnames = list(NULL, c("case_number", "url"))))
  }
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  groups <- regmatches(index_html, regexec(pat, index_html,
    perl = TRUE, ignore.case = TRUE))[[1L]]
  # Single-shot regex only captures the first match through regexec;
  # iterate via substring + re-match for the rest.
  out <- list()
  pos <- 1L
  remaining <- index_html
  while (TRUE) {
    g <- regexec(pat, remaining, perl = TRUE, ignore.case = TRUE)
    mm <- regmatches(remaining, g)[[1L]]
    if (length(mm) < 3L) break
    out[[length(out) + 1L]] <- c(
      case_number = mm[3L],
      url = .siu_fetch_resolve_url(mm[2L], base_url)
    )
    hit <- regexpr(pat, remaining, perl = TRUE, ignore.case = TRUE)
    if (hit < 0L) break
    cut <- as.integer(hit) + attr(hit, "match.length")
    if (cut >= nchar(remaining)) break
    remaining <- substr(remaining, cut + 1L, nchar(remaining))
  }
  if (!length(out)) {
    return(matrix(character(0L), ncol = 2L,
      dimnames = list(NULL, c("case_number", "url"))))
  }
  do.call(rbind, out)
}


# Internal: resolve a relative URL against the SIU index base.
.siu_fetch_resolve_url <- function(rel, base_url) {
  vapply(rel, function(r) {
    if (grepl("^https?://", r, ignore.case = TRUE)) return(r)
    paste0(sub("/[^/]*$", "/", base_url), sub("^/", "", r))
  }, character(1L), USE.NAMES = FALSE)
}


# Internal: convert a "Month D, YYYY" string into ISO YYYY-MM-DD.
# Returns "" on any failure.
.siu_fetch_to_iso <- function(date_str) {
  if (!nzchar(date_str)) return("")
  parsed <- suppressWarnings(
    as.Date(trimws(date_str), format = "%B %d, %Y")
  )
  if (is.na(parsed)) return("")
  format(parsed, "%Y-%m-%d")
}


# Internal: parse one case detail page into a flat list with the
# canonical six-field schema. This is a deliberately conservative,
# regex-based parse that mirrors the Python `_parse_case_page`.
# TODO: a full structured extraction (witness counts, decision
# category, mental-health flags, ...) is intentionally NOT
# implemented here -- it belongs in the C++ parser
# (`morie_fetch_siu`). Users who need the 64-column schema should
# call the compiled harvester instead.
.siu_fetch_parse_case_page <- function(html, case_number, url) {
  rec <- list(
    case_number = case_number,
    police_service = "",
    incident_iso = "",
    notification_iso = "",
    decision_iso = "",
    director_decision_text = "",
    source_url = url
  )
  if (!nzchar(html)) return(rec)

  date_pats <- list(
    incident_iso = paste0(
      "(?:Incident|incident occurred on)\\s*[:\\-]?\\s*",
      "([A-Z][a-z]+\\s+\\d{1,2},\\s*\\d{4})"
    ),
    notification_iso = paste0(
      "(?:Notification|SIU was notified on)\\s*[:\\-]?\\s*",
      "([A-Z][a-z]+\\s+\\d{1,2},\\s*\\d{4})"
    ),
    decision_iso = paste0(
      "(?:Director'?s? [Dd]ecision)\\s*[:\\-]?\\s*",
      "([A-Z][a-z]+\\s+\\d{1,2},\\s*\\d{4})"
    )
  )
  for (k in names(date_pats)) {
    m <- regmatches(html, regexec(date_pats[[k]], html, perl = TRUE))[[1L]]
    if (length(m) >= 2L) rec[[k]] <- .siu_fetch_to_iso(m[2L])
  }
  svc_pat <- paste0(
    "(?:Police Service|Notifying Service)\\s*[:\\-]?\\s*",
    "([A-Z][A-Za-z' \\-]+(?:Police|Service))"
  )
  m <- regmatches(html, regexec(svc_pat, html, perl = TRUE,
    ignore.case = TRUE))[[1L]]
  if (length(m) >= 2L) rec$police_service <- trimws(m[2L])

  dec_pat <- paste0(
    "(?:no reasonable grounds|reasonable grounds|charge\\(s\\)? was|",
    "withdrawn|charges? were laid)"
  )
  m <- regmatches(html, regexpr(dec_pat, html, perl = TRUE,
    ignore.case = TRUE))
  if (length(m) >= 1L && nzchar(m[1L])) {
    rec$director_decision_text <- trimws(m[1L])
  }
  rec
}


#' Scrape Ontario SIU Director's Reports into a tidy CSV
#'
#' Pulls the SIU index, walks every linked case-detail page, and writes a
#' six-column CSV (\code{case_number}, \code{police_service},
#' \code{incident_iso}, \code{notification_iso}, \code{decision_iso},
#' \code{director_decision_text}, \code{source_url}) into
#' \code{cache_dir}.
#'
#' This is the lightweight R-only path. For the full 64-column corpus
#' use \code{\link{morie_fetch_siu}} (compiled C++ harvester).
#'
#' @param years Integer vector of fiscal years to scrape, or \code{NULL}
#'   (default) to scrape the unfiltered index. Years above
#'   \code{2023} (the latest published as of release) may return
#'   empty results.
#' @param cache_dir Output directory. Default
#'   \code{file.path(tempdir(), "morie", "siu")}; pass
#'   \code{morie_cache_dir("siu")} for persistent caching.
#' @param overwrite Logical; if \code{FALSE} and \code{SIU.csv} already
#'   exists, its path is returned without re-scraping.
#' @param progress Logical; print a one-line status per index / case
#'   fetch when \code{TRUE} (default).
#' @return Path to the written \code{SIU.csv}.
#' @examples
#' \dontrun{
#' # Network: scrapes the SIU index (~5-15 min at the polite rate).
#' csv <- morie_siu_fetch_cases(cache_dir = tempfile("siu_"))
#' utils::head(utils::read.csv(csv))
#' }
#' @export
morie_siu_fetch_cases <- function(
  years = NULL,
  cache_dir = file.path(tempdir(), "morie", "siu"),
  overwrite = FALSE,
  progress = TRUE
) {
  out_path <- morie_siu_cache_path(cache_dir)
  if (file.exists(out_path) && !overwrite) {
    return(out_path)
  }
  if (!is.null(years)) {
    years <- as.integer(years)
    if (any(!is.finite(years))) {
      stop("`years` must be a finite integer vector or NULL.",
        call. = FALSE)
    }
    too_new <- years[years > .siu_fetch_latest_year]
    if (length(too_new) && progress) {
      message(
        "morie_siu_fetch_cases: ", length(too_new),
        " requested year(s) exceed the latest published (",
        .siu_fetch_latest_year, "); ",
        "those years may return zero cases."
      )
    }
  }

  index_url <- morie_siu_index_url()
  year_list <- if (is.null(years)) list(NULL) else as.list(years)

  case_links <- list()
  for (y in year_list) {
    url <- if (is.null(y)) index_url else paste0(index_url, "?year=", y)
    if (progress) message("[siu] index: ", url)
    html <- tryCatch(.siu_fetch_http_get(url), error = function(e) {
      if (progress) message("[siu] index fetch failed: ", conditionMessage(e))
      ""
    })
    if (!nzchar(html)) next
    case_links[[length(case_links) + 1L]] <-
      .siu_fetch_extract_links(html, base_url = index_url)
    Sys.sleep(.siu_fetch_rate_seconds)
  }
  case_links <- do.call(rbind, case_links)
  if (is.null(case_links) || !nrow(case_links)) {
    stop(
      "Scraped 0 SIU index entries. Site layout may have changed; ",
      "check morie_siu_index_url() and the extractor regex.",
      call. = FALSE
    )
  }
  # Deduplicate by url
  case_links <- case_links[!duplicated(case_links[, "url"]), , drop = FALSE]

  records <- vector("list", nrow(case_links))
  n <- nrow(case_links)
  for (i in seq_len(n)) {
    cn <- case_links[i, "case_number"]
    u <- case_links[i, "url"]
    if (progress && (i %% 25L == 0L || i == n)) {
      message("[siu] case ", i, "/", n)
    }
    html <- tryCatch(.siu_fetch_http_get(u), error = function(e) "")
    records[[i]] <- .siu_fetch_parse_case_page(html, cn, u)
    Sys.sleep(.siu_fetch_rate_seconds)
  }
  records <- records[!vapply(records, is.null, logical(1L))]
  if (!length(records)) {
    stop(
      "Scraped 0 SIU cases. The site layout may have changed; ",
      "verify morie_siu_index_url() and the regexes in siu_fetch.R.",
      call. = FALSE
    )
  }

  fieldnames <- c(
    "case_number", "police_service", "incident_iso",
    "notification_iso", "decision_iso",
    "director_decision_text", "source_url"
  )
  df <- do.call(rbind, lapply(records, function(r) {
    as.data.frame(r[fieldnames], stringsAsFactors = FALSE)
  }))
  rownames(df) <- NULL
  utils::write.csv(df, out_path, row.names = FALSE, na = "")
  if (progress) {
    message("[siu] wrote ", nrow(df), " cases to ", out_path)
  }
  out_path
}


#' Scrape Ontario SIU Director's Reports and return a data frame
#'
#' Thin wrapper over \code{\link{morie_siu_fetch_cases}}, returning a
#' data frame instead of the CSV path. Mirrors the Python
#' \code{fetch_siu_dataframe()} adapter used by the dataset catalog.
#'
#' @param ... Forwarded to \code{morie_siu_fetch_cases}.
#' @return A data frame with the six-column SIU header schema.
#' @examples
#' \dontrun{
#' df <- morie_siu_fetch_dataframe(cache_dir = tempfile("siu_"))
#' utils::head(df)
#' }
#' @export
morie_siu_fetch_dataframe <- function(...) {
  utils::read.csv(morie_siu_fetch_cases(...),
    stringsAsFactors = FALSE,
    colClasses = "character"
  )
}
