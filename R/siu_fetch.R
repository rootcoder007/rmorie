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
#' @examples
#' morie_siu_index_url()
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

  # Preferred path: the compiled SIU parser that lives in bricklayer
  # (>= 0.3.5) -- the ecosystem's C/C++ foundation. It fills the canonical
  # fields from the full 16-field schema parse; the conservative regex
  # passes below only top up whatever it could not state. Older bricklayer
  # (or none) falls straight through to the pure-R path.
  if (requireNamespace("rmoriebricklayer", quietly = TRUE) &&
      exists("bricklayer_parse_siu",
             envir = asNamespace("rmoriebricklayer"))) {
    bf <- tryCatch(rmoriebricklayer::bricklayer_parse_siu(html),
                   error = function(e) NULL)
    if (!is.null(bf)) {
      take <- function(k) if (!is.na(bf[k]) && nzchar(bf[[k]])) bf[[k]] else ""
      rec$police_service   <- take("police_service")
      rec$incident_iso     <- take("date_of_incident_iso")
      rec$notification_iso <- take("date_siu_notified_iso")
      rec$decision_iso     <- take("date_of_director_decision_iso")
    }
  }

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
    if (nzchar(rec[[k]])) next  # already filled by the compiled parser
    m <- regmatches(html, regexec(date_pats[[k]], html, perl = TRUE))[[1L]]
    if (length(m) >= 2L) rec[[k]] <- .siu_fetch_to_iso(m[2L])
  }
  # 2026-07 layout: the labelled Incident/Notification/Decision date
  # fields are gone -- the dates now live only in the report prose.
  # Narrative fallbacks (only fill fields the labels missed):
  plain <- gsub("\\s+", " ", gsub("<[^>]+>", " ", html))
  # "10:00 a.m." would break sentence-bounded [^.] matching -- drop
  # the abbreviation periods before scanning.
  plain <- gsub("([ap])\\.m\\.", "\\1m", plain)
  date_re <- "([A-Z][a-z]+ \\d{1,2}, \\d{4})"
  if (!nzchar(rec$decision_iso)) {
    # Approval footer: "Date: June 12, 2026 Electronically approved by
    # <name> Director".
    m <- regmatches(plain, regexec(paste0(
      "Date:\\s*", date_re,
      "(?=.{0,140}Director)"), plain, perl = TRUE))[[1L]]
    if (length(m) >= 2L) rec$decision_iso <- .siu_fetch_to_iso(m[2L])
  }
  if (!nzchar(rec$notification_iso)) {
    # "On <date>, ... contacted/notified the SIU" (or "the SIU was
    # notified"), i.e. the date in the same sentence as the SIU
    # notification wording.
    m <- regmatches(plain, regexec(paste0(
      "On\\s+", date_re, "[^.]{0,200}?",
      "(?:contacted|notified)[^.]{0,80}?SIU"), plain, perl = TRUE))[[1L]]
    if (length(m) < 2L) {
      m <- regmatches(plain, regexec(paste0(
        "SIU was notified[^.]{0,80}?on\\s+", date_re),
        plain, perl = TRUE))[[1L]]
    }
    if (length(m) >= 2L) rec$notification_iso <- .siu_fetch_to_iso(m[2L])
  }
  if (!nzchar(rec$incident_iso)) {
    # The incident is the EARLIEST calendar date mentioned in the
    # report body (narratives open with the event, later dates are
    # follow-ups, notification and approval).
    all_d <- regmatches(plain, gregexpr(date_re, plain, perl = TRUE))[[1L]]
    iso <- vapply(all_d, .siu_fetch_to_iso, character(1L))
    iso <- iso[nzchar(iso)]
    if (length(iso)) rec$incident_iso <- min(iso)
  }
  svc_pat <- paste0(
    "(?:Police Service|Notifying Service)\\s*[:\\-]?\\s*",
    "([A-Z][A-Za-z' \\-]+(?:Police|Service))"
  )
  m <- regmatches(html, regexec(svc_pat, html, perl = TRUE,
    ignore.case = TRUE))[[1L]]
  if (length(m) >= 2L && !nzchar(rec$police_service)) {
    rec$police_service <- trimws(m[2L])
  }
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
  # The mandate boilerplate near the top also says "reasonable
  # grounds"; the verdict lives in the Analysis and Director's
  # Decision section, so scan from the LAST such heading when present.
  scan <- plain
  hi <- regexpr("Analysis and Director.{1,3}s Decision(?!.*Analysis and Director)",
                plain, perl = TRUE)
  if (hi > 0L) scan <- substr(plain, hi, nchar(plain))
  m <- regmatches(scan, regexpr(dec_pat, scan, perl = TRUE,
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

  # Never fetch the LIVE corpus (2000+ paginated pages, one HTTP call per case)
  # under R CMD check / examples -- it would hang the example phase. Return the
  # cache if present, else a typed-empty placeholder, so self-tests, examples
  # and offline callers stay fast. Argument validation above still runs. A
  # mocked test (or a deliberate live run) sets options(morie.siu.allow_fetch =
  # TRUE) to bypass this and exercise the real fetch/parse pipeline.
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
      !isTRUE(getOption("morie.siu.allow_fetch"))) {
    if (file.exists(out_path)) {
      return(out_path)
    }
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    writeLines("case_number", out_path)  # 0-row corpus placeholder
    return(out_path)
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

#' Parse one SIU director's report into the full 16-field schema
#'
#' Delegates to the compiled parser in the ecosystem's C/C++ foundation
#' package, \pkg{rmoriebricklayer} (>= 0.3.5): the sixteen panel-reviewed
#' schema fields (dates, investigator counts, witness/subject-official
#' counts, injuries, legislation, ...) plus \code{_language}, extracted
#' deterministically and offline. This supersedes the conservative
#' six-field regex parse used by \code{morie_siu_fetch_cases()}, which
#' itself now routes through the same compiled parser when available.
#'
#' @param html A length-1 character vector of raw report HTML, or the
#'   path to a saved report file.
#' @return A named character vector: the 16 schema fields plus
#'   \code{_language}.
#' @examplesIf requireNamespace("rmoriebricklayer", quietly = TRUE) && exists("bricklayer_parse_siu", envir = asNamespace("rmoriebricklayer"))
#' f <- morie_siu_parse_report(system.file("extdata",
#'   "siu_synthetic_report.html", package = "rmoriebricklayer"))
#' f[["number_of_subject_officers"]]
#' @export
morie_siu_parse_report <- function(html) {
  if (!requireNamespace("rmoriebricklayer", quietly = TRUE) ||
      !exists("bricklayer_parse_siu",
              envir = asNamespace("rmoriebricklayer"))) {
    stop("morie_siu_parse_report() needs rmoriebricklayer >= 0.3.5 ",
         "(the compiled SIU parser). Install/update it with:\n",
         "  install.packages(\"rmoriebricklayer\", ",
         "repos = \"https://rootcoder007.r-universe.dev\")")
  }
  rmoriebricklayer::bricklayer_parse_siu(html)
}

#' SIU director's-reports corpus: reviewed data first, fetch only what's new
#'
#' The right way to get SIU data in the morie ecosystem. Loads the
#' panel-reviewed 65-column corpus bundled in \pkg{rmoriedata} (2,182
#' English reports, subject-official coverage 100 percent, built by a
#' multi-model reading panel plus deterministic residual resolution) --
#' nothing is re-fetched or re-parsed for reports already reviewed. With
#' \code{update = TRUE} it then discovers reports published AFTER the
#' corpus snapshot (report ids above the bundled maximum), fetches only
#' those few (through \pkg{rmoriebricklayer}'s live-plus-Wayback engine
#' when installed), fills the mechanical schema fields with the compiled
#' parser, and appends them flagged \code{panel_reviewed = FALSE} with the
#' judgment columns left \code{NA} -- run the reading panel (the
#' \code{siu} command-line tool against your own 'Ollama' server) to fill
#' those, exactly as the reviewed corpus was built.
#'
#' @param update Also fetch + parse reports newer than the bundled corpus
#'   (default \code{FALSE}: fully offline).
#' @param max_new Ceiling on how many new reports to fetch per call
#'   (default 25; a normal refresh sees 0-15).
#' @param quiet Suppress progress messages.
#' @return A data.frame in the 65-column reviewed-corpus schema. New rows
#'   (if any) carry \code{panel_reviewed = FALSE}.
#' @examplesIf requireNamespace("rmoriedata", quietly = TRUE)
#' df <- morie_siu_reports()
#' nrow(df)
#' table(df$panel_reviewed)
#' @export
morie_siu_reports <- function(update = FALSE, max_new = 25L, quiet = FALSE) {
  if (!requireNamespace("rmoriedata", quietly = TRUE)) {
    stop("morie_siu_reports() needs the rmoriedata package (it carries ",
         "the reviewed corpus). install.packages(\"rmoriedata\", ",
         "repos = \"https://rootcoder007.r-universe.dev\")")
  }
  corpus <- rmoriedata::load_siu_reports()
  if (!isTRUE(update)) return(corpus)

  max_drid <- suppressWarnings(max(as.integer(corpus$drid), na.rm = TRUE))
  latest <- tryCatch(.siu_discover_max_drid(default = max_drid),
                     error = function(e) max_drid)
  if (!is.finite(latest) || latest <= max_drid) {
    if (!quiet) message("siu: corpus is current (max drid ", max_drid, ")")
    return(corpus)
  }
  new_ids <- seq.int(max_drid + 1L, latest)
  if (length(new_ids) > max_new) new_ids <- new_ids[seq_len(max_new)]
  if (!quiet) {
    message("siu: ", length(new_ids), " report id(s) newer than the ",
            "reviewed corpus; fetching only those")
  }

  fetch_one <- function(id) {
    # bricklayer's live+Wayback fetch engine when available, else plain R.
    if (requireNamespace("rmoriebricklayer", quietly = TRUE)) {
      dest <- tempfile(fileext = ".html")
      on.exit(unlink(dest), add = TRUE)
      ok <- tryCatch(
        rmoriebricklayer::bricklayer_fetch_siu(id, dest),
        error = function(e) NULL)
      if (!is.null(ok) && file.exists(dest) && file.size(dest) > 0) {
        return(paste(readLines(dest, warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n"))
      }
      return(NULL)
    }
    url <- sprintf(
      "https://www.siu.on.ca/en/directors_report_details.php?drid=%d", id)
    tryCatch(.siu_fetch_http_get(url), error = function(e) NULL)
  }

  # parser field -> corpus column (the mechanical tier of the schema).
  fmap <- c(police_service = "police_service",
            date_of_incident_iso = "date_of_incident_iso",
            date_siu_notified_iso = "date_siu_notified_iso",
            date_of_director_decision_iso = "date_of_director_decision_iso",
            siu_investigators = "siu_investigators",
            siu_forensics_investigators = "siu_forensics_investigators",
            number_of_witness_officials = "number_of_witness_officials",
            number_of_civilian_witnesses = "number_of_civilian_witnesses",
            number_of_subject_officers = "number_of_subject_officials",
            age_affected = "age_affected",
            sex_gender_affected = "sex_gender_affected",
            charges_recommended = "charges_recommended",
            directors_name = "directors_name",
            location_of_call = "location_of_call",
            specific_injuries = "specific_injuries",
            relevant_legislation = "relevant_legislation",
            "_language" = "X_language")
  fmap <- fmap[fmap %in% names(corpus)]

  new_rows <- list()
  for (id in new_ids) {
    html <- fetch_one(id)
    if (is.null(html) || !nzchar(html)) next
    fields <- tryCatch(morie_siu_parse_report(html), error = function(e) NULL)
    if (is.null(fields)) next
    # A dead drid returns a page with no report body; require a date or a
    # service before treating it as a real report.
    if (!nzchar(fields[["police_service"]]) &&
        !nzchar(fields[["date_of_incident_iso"]])) next
    row <- corpus[NA_integer_, , drop = FALSE][1, ]
    row$drid <- as.character(id)
    row$source_url_report <- sprintf(
      "https://www.siu.on.ca/en/directors_report_details.php?drid=%d", id)
    for (k in names(fmap)) {
      v <- fields[[k]]
      if (!is.null(v) && nzchar(v)) row[[fmap[[k]]]] <- v
    }
    row$panel_reviewed <- FALSE
    new_rows[[length(new_rows) + 1L]] <- row
    if (!quiet) message("siu: added drid ", id, " (panel_reviewed = FALSE)")
  }
  if (!length(new_rows)) return(corpus)
  out <- rbind(corpus, do.call(rbind, new_rows))
  if (!quiet) {
    message("siu: ", length(new_rows), " new report(s) appended -- ",
            "mechanical fields parsed; run the reading panel (siu CLI + ",
            "your Ollama server) to fill the judgment columns")
  }
  out
}

#' Resolve a subject-official count: verified corpus first, rules second
#'
#' The correct order for the ecosystem: a report already in the
#' panel-reviewed corpus (\pkg{rmoriedata}) returns its VERIFIED count --
#' nothing re-derives an established answer. Only a report outside the
#' corpus falls through to the deterministic rule set compiled in
#' \pkg{rmoriebricklayer} (the foundation layer), whose rules were proven
#' zero-wrong against all 2,182 reviewed reports; where even the rules
#' cannot answer, the reading panel ([morie_siu_panel()]) decides.
#'
#' @param text Plain report text (needed only for unreviewed reports).
#' @param drid Report id; supply whenever known.
#' @return A list with `count` (integer, `NA` only when both corpus and
#'   rules are silent -- run the panel) and `reason`.
#' @examplesIf requireNamespace("rmoriedata", quietly = TRUE)
#' morie_siu_resolve_so(drid = 5038)
#' @export
morie_siu_resolve_so <- function(text = NULL, drid = NULL) {
  if (!is.null(drid) && requireNamespace("rmoriedata", quietly = TRUE)) {
    corpus <- tryCatch(rmoriedata::load_siu_reports(),
                       error = function(e) NULL)
    if (!is.null(corpus)) {
      hit <- corpus[corpus$drid == as.character(drid), , drop = FALSE]
      if (nrow(hit) == 1L) {
        n <- suppressWarnings(as.integer(hit$number_of_subject_officials))
        if (!is.na(n)) {
          return(list(count = n,
                      reason = "panel-reviewed corpus (verified)"))
        }
      }
    }
  }
  if (is.null(text)) {
    stop("report not in the reviewed corpus; supply `text` for the ",
         "rule-based resolution (rmoriebricklayer)")
  }
  if (!requireNamespace("rmoriebricklayer", quietly = TRUE)) {
    stop("rule-based resolution needs rmoriebricklayer")
  }
  rmoriebricklayer::bricklayer_siu_resolve_so(text)
}
