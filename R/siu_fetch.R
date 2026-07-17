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
#' @return A character string.
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
#' Internal helper: Siu Fetch Http Get
#' @noRd
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
#' Internal helper: Siu Fetch Extract Links
#' @noRd
.siu_fetch_extract_links <- function(index_html, base_url) {
  # The SIU index is a table of <tr class="dr-item" id="DRID"> rows; each
  # row carries the case number in a <nobr> (e.g. "26-OCI-168") and a
  # "Read Full Text" link to directors_report_details.php?drid=DRID.
  cn_pat   <- "[0-9]{2,4}-[A-Z]{2,5}-[0-9]+"
  href_pat <- "directors_report_details\\.php\\?drid=[0-9]+"
  empty <- matrix(character(0L), ncol = 2L,
    dimnames = list(NULL, c("case_number", "url")))

  if (requireNamespace("xml2", quietly = TRUE) &&
      requireNamespace("rvest", quietly = TRUE)) {
    doc <- tryCatch(xml2::read_html(index_html), error = function(e) NULL)
    if (!is.null(doc)) {
      rows <- rvest::html_elements(doc, "tr.dr-item, .dr-item")
      out <- lapply(rows, function(row) {
        a <- rvest::html_element(row, "a[href*='directors_report_details']")
        href <- rvest::html_attr(a, "href")
        if (is.na(href) || !nzchar(href)) {
          return(NULL)
        }
        txt <- rvest::html_text2(row)
        cn <- regmatches(txt, regexpr(cn_pat, txt))
        if (!length(cn) || !nzchar(cn)) {
          cn <- sub(".*drid=([0-9]+).*", "drid-\\1", href)
        }
        c(case_number = cn, url = .siu_fetch_resolve_url(href, base_url))
      })
      out <- out[!vapply(out, is.null, logical(1L))]
      if (length(out)) {
        return(do.call(rbind, out))
      }
    }
  }
  # Regex fallback: split into dr-item rows, pull (case_number, drid) each.
  rows <- strsplit(index_html, '<tr[^>]*class="dr-item"', perl = TRUE)[[1L]]
  if (length(rows) > 1L) rows <- rows[-1L]
  out <- list()
  for (row in rows) {
    href <- regmatches(row, regexpr(href_pat, row, perl = TRUE))
    if (!length(href) || !nzchar(href)) next
    cn <- regmatches(row, regexpr(cn_pat, row, perl = TRUE))
    if (!length(cn) || !nzchar(cn)) {
      cn <- sub(".*drid=([0-9]+).*", "drid-\\1", href)
    }
    out[[length(out) + 1L]] <- c(
      case_number = cn,
      url = .siu_fetch_resolve_url(href, base_url)
    )
  }
  if (!length(out)) {
    return(empty)
  }
  do.call(rbind, out)
}


# Internal: resolve a relative URL against the SIU index base.
#' Internal helper: Siu Fetch Resolve Url
#' @noRd
.siu_fetch_resolve_url <- function(rel, base_url) {
  vapply(rel, function(r) {
    if (grepl("^https?://", r, ignore.case = TRUE)) return(r)
    if (startsWith(r, "/")) {
      host <- sub("^(https?://[^/]+).*", "\\1", base_url)
      return(paste0(host, r))
    }
    paste0(sub("/[^/]*$", "/", base_url), sub("^/", "", r))
  }, character(1L), USE.NAMES = FALSE)
}


# Internal: convert a "Month D, YYYY" string into ISO YYYY-MM-DD.
# Returns "" on any failure.
#' Internal helper: Siu Fetch To Iso
#' @noRd
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
#' Internal helper: Siu Fetch Parse Case Page
#' @noRd
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
  if (!nzchar(rec$police_service)) {
    # 2026-07 layout: no labelled field, but the prose wraps every
    # force mention in <abbr title="Waterloo Regional Police Service">.
    # A report can reference several forces (e.g. OPP assisting a
    # municipal service); the subject service is the one mentioned
    # most often, so take the modal abbr title.
    abbr_pat <- '<abbr title="([^"]*(?:Police|Provincial)[^"]*)"'
    hits <- regmatches(html, gregexpr(abbr_pat, html, perl = TRUE))[[1L]]
    if (length(hits)) {
      names_all <- vapply(regmatches(hits,
        regexec(abbr_pat, hits, perl = TRUE)),
        function(g) trimws(g[2L]), character(1L))
      tab <- sort(table(names_all), decreasing = TRUE)
      rec$police_service <- names(tab)[1L]
    }
  }

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

  # 2026-07 layout: the index renders ~29 <tr class="dr-item"> rows and
  # infinite-scrolls the rest through GET
  #   /ssi/get_more_drs.php?lang=en&lastCount=<rows so far>
  # (the ?year= query parameter is ignored server-side, so year
  # filtering happens below on the case-number prefix instead).
  if (progress) message("[siu] index: ", index_url)
  html <- tryCatch(.siu_fetch_http_get(index_url), error = function(e) {
    if (progress) message("[siu] index fetch failed: ", conditionMessage(e))
    ""
  })
  case_links <- list()
  total <- NA_integer_
  if (nzchar(html)) {
    tm <- regmatches(html, regexec(
      'id="total_drs"[^>]*value="([0-9]+)"', html))[[1L]]
    if (length(tm) == 2L) total <- as.integer(tm[2L])
    case_links[[1L]] <- .siu_fetch_extract_links(html, base_url = index_url)
    more_base <- sub("/en/directors_reports\\.php$",
                     "/ssi/get_more_drs.php", index_url)
    got <- if (length(case_links)) nrow(case_links[[1L]]) else 0L
    while (!is.na(total) && got < total) {
      page_url <- paste0(more_base, "?lang=en&lastCount=", got)
      if (progress) message("[siu] index page: lastCount=", got,
                            " of ", total)
      chunk <- tryCatch(.siu_fetch_http_get(page_url),
                        error = function(e) "")
      if (!nzchar(chunk)) break
      links <- .siu_fetch_extract_links(chunk, base_url = index_url)
      if (!nrow(links)) break
      case_links[[length(case_links) + 1L]] <- links
      got <- got + nrow(links)
      Sys.sleep(.siu_fetch_rate_seconds)
    }
  }
  case_links <- do.call(rbind, case_links)
  # Year filter via the case-number prefix ("26-OCI-168" -> 2026).
  if (!is.null(years) && !is.null(case_links) && nrow(case_links)) {
    yy <- sprintf("%02d", years %% 100L)
    pref <- substr(case_links[, "case_number"], 1L, 2L)
    case_links <- case_links[pref %in% yy, , drop = FALSE]
  }
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
