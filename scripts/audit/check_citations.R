#!/usr/bin/env Rscript
# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Block unverifiable citations from being committed.
#
# A citation is a factual claim about a document that exists. Written from
# recall it is wrong often enough to matter -- the Boyd shelf had 10 of 54
# section numbers wrong, one of them invented outright -- and a wrong citation
# is worse than none, because it looks like evidence.
#
# This gate refuses the shapes a reader CANNOT check:
#
#   E1  a reference with a venue and page numbers but NO title
#   E2  a bare "Author (year)" with no venue at all
#   E3  a leaked template placeholder, e.g. a literal {HYN}
#   E5  a ledger entry with no resolvable source (an invented ledger entry)
#   E6  --online only: a ledger entry that disagrees with the publisher
#
# E1-E5 are decided offline and always run. E6 resolves each DOI against
# Crossref, so the ledger itself cannot be filled in from memory: an entry
# whose year/volume/pages do not match the publisher record fails.
#
# The ledger (citations_ledger.json, next to this script) is append-only in
# practice -- confirm a citation is real, then add it. It is the record of
# what was actually looked up, not a list of what someone believed.
#
# Usage: Rscript scripts/audit/check_citations.R [--online] [ROOT ...]
# Scans R/*.R under each ROOT (default "."). Exit status 1 on any finding.

args <- commandArgs(trailingOnly = TRUE)
self <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
here <- if (length(self)) dirname(normalizePath(self[1L])) else file.path("scripts", "audit")
ledger_path <- file.path(here, "citations_ledger.json")

# "#' @references Author (1999). \emph{Journal}, 94(446), 496-509."  -- no title.
R_TITLELESS <- "\\(\\d{4}[a-z]?\\)\\.\\s*\\\\emph\\{[^}]+\\}[ ,]*\\s*\\d"
# "Verdinelli & Wasserman (1995); Dickey (1971)."  -- no venue at all.
BARE_AUTHOR_YEAR <- paste0("^\\s*(?:#'\\s*)?(?:@references\\s+)?",
                           "[A-Z][A-Za-z'\\-]+(?:[^.()]{0,60})\\(\\d{4}[a-z]?\\)\\s*[;.]\\s*$")
PLACEHOLDER <- "^\\s*(?:#'\\s*)?\\{[A-Z_][A-Z0-9_]*\\}\\s*$"
# Telling a journal locator from a book edition: a journal reads "\emph{Venue}, 94(446), 496-509"
# -- the italics then a volume and an issue or a page range; a book reads "\emph{Title}, 3rd ed.
# Publisher", where the italic run IS the title and nothing is missing. Anchoring on what FOLLOWS
# the italics is reliable; guessing from the words inside them is not.
EDITION <- "(?i)^\\s*,?\\s*\\d+(st|nd|rd|th)\\s+(ed|edn|edition)\\b"
JOURNAL_LOCATOR <- "^\\s*,?\\s*\\d+\\s*(\\(\\d+[^)]*\\))?\\s*,\\s*\\d"

has <- function(pattern, x) grepl(pattern, x, perl = TRUE)

load_ledger <- function() {
  if (!file.exists(ledger_path)) return(list())
  j <- jsonlite::fromJSON(ledger_path, simplifyVector = FALSE)
  if (is.null(j$entries)) list() else j$entries
}

# (line number, line) for the lines of each roxygen @references block
reference_blocks <- function(lines) {
  out <- list()
  for (i in seq_along(lines)) {
    if (!grepl("@references", lines[i], fixed = TRUE)) next
    out[[as.character(i)]] <- lines[i]
    j <- i + 1L
    while (j <= length(lines) && has("^\\s*#'\\s{2,}\\S", lines[j])) {
      out[[as.character(j)]] <- lines[j]
      j <- j + 1L
    }
  }
  out
}

judge <- function(entry, line) {
  e <- trimws(entry)
  if (!nzchar(e) || (startsWith(e, "@references") && nchar(e) < 14L)) return(character())
  e <- sub("^@references\\s*", "", e, perl = TRUE)
  out <- character()
  m <- regexpr(R_TITLELESS, e, perl = TRUE)
  if (m > 0L) {
    # anchor the venue to the reference that matched, not the first italics in the block:
    # otherwise a titled article followed by a second reference cross-matches
    sub_e <- substring(e, m)
    it <- regexpr("\\\\emph\\{([^}]+)\\}", sub_e, perl = TRUE)
    tail <- if (it > 0L) substring(sub_e, it + attr(it, "match.length")) else ""
    if (!has(EDITION, tail) && has(JOURNAL_LOCATOR, tail)) {
      out <- c(out, sprintf("%d: E1: citation has a venue and locator but NO title: %s", line,
                            substr(e, 1L, 88L)))
    }
  }
  if (has(BARE_AUTHOR_YEAR, entry) && !grepl("*", e, fixed = TRUE) &&
      !grepl("\\emph", e, fixed = TRUE)) {
    out <- c(out, sprintf("%d: E2: bare author-year, no venue: %s", line, substr(e, 1L, 88L)))
  }
  out
}

check_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  problems <- character()
  for (i in which(has(PLACEHOLDER, lines))) {
    problems <- c(problems, sprintf("%d: E3: leaked template placeholder: %s", i, trimws(lines[i])))
  }
  blocks <- reference_blocks(lines)
  keys <- sort(as.integer(names(blocks)))
  # stitch wrapped references so a title split across lines is not a false positive,
  # keeping the first line number for the report
  buf <- ""
  start <- NA_integer_
  for (k in keys) {
    ln <- blocks[[as.character(k)]]
    s <- sub("\\s+$", "", sub("^\\s*#'\\s*", "", ln, perl = TRUE), perl = TRUE)
    if (is.na(start)) {
      start <- k
      buf <- s
    } else if (has("^\\s{4,}\\S", ln) || has("^\\s*#'\\s{3,}\\S", ln)) {
      buf <- paste(buf, trimws(s))
    } else {
      problems <- c(problems, judge(buf, start))
      start <- k
      buf <- s
    }
  }
  if (!is.na(start)) problems <- c(problems, judge(buf, start))
  problems
}

# E5: a ledger entry must carry a source that could be resolved; otherwise the ledger is just
# another place to write things from memory, and the gate would certify its own guesses
check_ledger_shape <- function(ledger) {
  bad <- character()
  for (key in names(ledger)) {
    v <- ledger[[key]]
    src <- if (is.null(v$source)) "" else as.character(v$source)
    if (!(startsWith(src, "doi:") || startsWith(src, "http"))) {
      bad <- c(bad, sprintf("ledger[%s]: E5: no resolvable source (need doi: or http), got '%s'", key, src))
    }
    if (is.null(v$verified) || !nzchar(as.character(v$verified))) {
      bad <- c(bad, sprintf("ledger[%s]: E5: no `verified` date", key))
    }
    # an entry may not be both a certificate and a guess: unconfirmed pages record the start page
    if (isTRUE(v$pages_unverified) && grepl("-", as.character(v$pages %||% ""), fixed = TRUE)) {
      bad <- c(bad, sprintf(paste0("ledger[%s]: E5: marked pages_unverified but still records a RANGE ",
                                   "'%s'; record the confirmed start page only"), key, v$pages))
    }
  }
  bad
}

`%||%` <- function(a, b) if (is.null(a)) b else a

crossref <- function(doi, ua) {
  con <- url(paste0("https://api.crossref.org/works/", doi), headers = ua)
  on.exit(close(con))
  jsonlite::fromJSON(paste(readLines(con, warn = FALSE), collapse = "\n"), simplifyVector = FALSE)$message
}

# E6: resolve each DOI against Crossref and compare with what the ledger claims
check_ledger_online <- function(ledger) {
  bad <- character()
  ua <- c("User-Agent" = "rmorie-citation-gate (https://github.com/rootcoder007/rmorie)")
  for (key in sort(names(ledger))) {
    v <- ledger[[key]]
    src <- as.character(v$source %||% "")
    if (!startsWith(src, "doi:")) next
    msg <- tryCatch(crossref(substring(src, 5L), ua), error = function(e) e)
    if (inherits(msg, "error")) {
      bad <- c(bad, sprintf("ledger[%s]: E6: DOI did not resolve (%s)", key, conditionMessage(msg)))
      next
    }
    # the year of record: online-first ("issued") or the print issue -- a 2014 issue published
    # online in 2013 is cited as 2014, and either is the publisher's
    yr_of <- function(d) tryCatch(d$`date-parts`[[1]][[1]], error = function(e) NULL)
    years <- unlist(list(yr_of(msg$issued), yr_of(msg$`published-print`),
                         yr_of(msg$`journal-issue`$`published-print`)))
    for (field in c("year", "volume", "pages")) {
      got <- switch(field, year = if (length(years)) years[1] else NULL, volume = msg$volume, pages = msg$page)
      want <- v[[field]]
      if (is.null(want) || is.null(got)) next
      if (field == "year" && as.character(want) %in% as.character(years)) next
      w <- trimws(gsub("--", "-", as.character(want), fixed = TRUE))
      g <- trimws(gsub("--", "-", as.character(got), fixed = TRUE))
      if (w == g) next
      # older deposits (JSTOR-era DOIs) carry only the first page: a ledger range that begins
      # there is consistent, not contradictory
      if (field == "pages" && !grepl("-", g, fixed = TRUE) && strsplit(w, "-", fixed = TRUE)[[1]][1] == g) next
      bad <- c(bad, sprintf("ledger[%s]: E6: %s is '%s' at the publisher, ledger says '%s'",
                            key, field, g, w))
    }
    Sys.sleep(0.4)
  }
  bad
}

online <- "--online" %in% args
roots <- args[!startsWith(args, "--")]
if (!length(roots)) roots <- "."
files <- sort(unlist(lapply(roots, function(r) Sys.glob(file.path(r, "R", "*.R")))))
files <- sub("^\\./", "", files[!startsWith(basename(files), "_lazy_map")])
ledger <- load_ledger()
problems <- check_ledger_shape(ledger)
if (online) problems <- c(problems, check_ledger_online(ledger))
for (f in files) {
  p <- check_file(f)
  if (length(p)) problems <- c(problems, paste0(f, ":", p))
}
cat(sprintf("citation gate: %d files, %d ledger entries\n", length(files), length(ledger)))
if (length(problems)) {
  cat(sprintf("\n%d unverifiable citation(s):\n\n", length(problems)))
  cat(paste0("  ", utils::head(problems, 60L)), sep = "\n")
  if (length(problems) > 60L) cat(sprintf("  ... and %d more\n", length(problems) - 60L))
  cat(paste0("\nFix by supplying the missing title/venue from the publisher record, then record ",
             "it in scripts/audit/citations_ledger.json.\n"))
  quit(status = 1L)
}
cat("citation gate: OK\n")
