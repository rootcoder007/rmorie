# SPDX-License-Identifier: AGPL-3.0-or-later
#
# morie -- Multi-domain Open Research and Inferential Estimation
# Copyright (C) 2026 Vansh Singh Ruhela and morie contributors.
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. morie is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Affero General Public License for more details. You should have
# received a copy of the GNU Affero General Public License along with
# morie. If not, see <https://www.gnu.org/licenses/>.

#' Pure-R SIU director's-report parser (port of
#' \code{morie.siu._parser})
#'
#' Parses one SIU director's-report HTML page (or one news-release
#' page) into a structured row list. The production parser lives in
#' the Rcpp / C++ backend (\code{.siu_parse_report},
#' \code{.siu_parse_news}); this pure-R port is provided as a
#' reference implementation and as a fallback for environments
#' where the compiled \pkg{libmorie} backend is unavailable.
#'
#' \strong{Suggested dependencies.} These functions optionally use
#' \pkg{rvest} + \pkg{xml2} for DOM walking; without them, a
#' regex-based fallback over flat tag-stripped text is used. Either
#' way the parser is pure (no network) -- hand it a raw HTML string
#' and it returns a row dict matching \code{SIU_COLUMNS}.
#'
#' Hardened against the SIU page markup shifting over time by:
#' \itemize{
#'   \item looking for several label variants per field,
#'   \item falling back to regex on stripped text when DOM
#'     structure shifts,
#'   \item preserving the verbatim \code{narrative_full}
#'     regardless of parse success.
#' }
#'
#' @name morie_siu_parser
NULL


# Bump in lockstep with the Python parser when behaviour changes.
.SIU_R_PARSER_VERSION <- "0.1.0"


# ---------------------------------------------------------------------------
# .siu_p_has_rvest -- gate rvest/xml2 use (suggested, not required).
# ---------------------------------------------------------------------------
.siu_p_has_rvest <- function() {
  requireNamespace("rvest", quietly = TRUE) &&
    requireNamespace("xml2", quietly = TRUE)
}


# ---------------------------------------------------------------------------
# .siu_p_blank_row -- minimal SIU_COLUMNS row template. Keys match the
# Python BLANK_ROW; extra keys appearing only here are populated by
# this parser specifically.
# ---------------------------------------------------------------------------
.siu_p_blank_row <- function() {
  list(
    parser_version                       = .SIU_R_PARSER_VERSION,
    source_url_report                    = NULL,
    source_url_news                      = NULL,
    drid                                 = NA_integer_,
    nrid                                 = NA_integer_,
    case_number                          = NA_character_,
    police_service                       = NA_character_,
    number_of_officers_involved          = NA_integer_,
    number_of_subject_officials          = NA_integer_,
    number_of_witness_officials          = NA_integer_,
    number_of_civilian_witnesses         = NA_integer_,
    siu_investigators                    = NA_character_,
    siu_forensics_investigators          = NA_character_,
    subject_official_interviewed_or_notes = NA_character_,
    location_of_call                     = NA_character_,
    reason_for_interaction               = NA_character_,
    date_of_incident_iso                 = NA_character_,
    date_of_incident_raw                 = NA_character_,
    date_siu_notified_iso                = NA_character_,
    date_siu_notified_raw                = NA_character_,
    notifying_party                      = NA_character_,
    date_of_director_decision_iso        = NA_character_,
    date_of_director_decision_raw        = NA_character_,
    injuries_sustained                   = NA_character_,
    specific_injuries                    = NA_character_,
    sex_gender_affected                  = NA_character_,
    age_affected                         = NA_character_,
    relevant_legislation                 = NA_character_,
    charges_recommended                  = NA,
    directors_decision_reasonable        = NA_character_,
    mental_health_or_race_indications    = NA_character_,
    supplemental_materials               = NA_character_,
    narrative_full                       = NA_character_,
    narrative_summary                    = NA_character_,
    `_language`                          = NA_character_
  )
}


# ---------------------------------------------------------------------------
# Stripped-text + body-slice primitives.
# ---------------------------------------------------------------------------
.siu_p_stripped_text <- function(html) {
  if (.siu_p_has_rvest()) {
    doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
    if (!is.null(doc)) {
      for (s in xml2::xml_find_all(
        doc, ".//script | .//style | .//noscript")) {
        xml2::xml_remove(s)
      }
      txt <- rvest::html_text2(doc)
      txt <- gsub("[ \\t]+", " ", txt, perl = TRUE)
      txt <- gsub("\\
{3,}", "\
\
", txt, perl = TRUE)
      return(trimws(txt))
    }
  }
  # Regex fallback (mirrors siu.R's .siu_html_to_text)
  h <- html
  h <- gsub("(?is)<script\\b[^>]*>.*?</script>", " ", h, perl = TRUE)
  h <- gsub("(?is)<style\\b[^>]*>.*?</style>",   " ", h, perl = TRUE)
  h <- gsub("<[^>]+>", " ", h, perl = TRUE)
  ents <- c(
    "&amp;"   = "&",  "&lt;"    = "<",  "&gt;"    = ">",
    "&quot;"  = "\"", "&apos;"  = "'",  "&#39;"   = "'",
    "&nbsp;"  = " ",  "&rsquo;" = "'",  "&lsquo;" = "'",
    "&ldquo;" = "\"", "&rdquo;" = "\"", "&ndash;" = "-",
    "&mdash;" = "-",  "&hellip;" = "..."
  )
  for (k in names(ents)) h <- gsub(k, ents[[k]], h, fixed = TRUE)
  h <- gsub("\\s+", " ", h, perl = TRUE)
  trimws(h)
}


.siu_p_trim_to_body <- function(text) {
  if (!is.character(text) || length(text) == 0L || !nzchar(text)) return(text)
  m <- tryCatch(gregexpr("(?:^|\\
)\\s*Mandate of the SIU\\s*\\
",
                text, perl = TRUE)[[1L]],
                error = function(e) -1L)
  if (length(m) >= 2L && isTRUE(m[1L] != -1L)) {
    return(substr(text, m[2L], nchar(text)))
  }
  if (length(m) == 1L && isTRUE(m[1L] != -1L)) {
    return(substr(text, m[1L], nchar(text)))
  }
  m2 <- tryCatch(gregexpr("(?:^|\\
)\\s*The Investigation\\s*\\
",
                 text, perl = TRUE)[[1L]],
                 error = function(e) -1L)
  if (length(m2) >= 2L && isTRUE(m2[1L] != -1L)) {
    return(substr(text, m2[2L], nchar(text)))
  }
  text
}


# ---------------------------------------------------------------------------
# Label / value primitives.
# ---------------------------------------------------------------------------
.siu_p_label_value <- function(text, label) {
  pat <- paste0(.siu_p_re_escape(label),
                "\\s*[:\\-]?\\s*(.{1,200}?)(?=\\
|$)")
  m <- regmatches(text, regexpr(pat, text, perl = TRUE,
                                ignore.case = TRUE))
  if (length(m) == 0L) return(NULL)
  cap <- regmatches(
    text, regexec(pat, text, perl = TRUE, ignore.case = TRUE))[[1L]]
  if (length(cap) < 2L) return(NULL)
  val <- trimws(cap[2L])
  val <- sub(",|;|:$", "", val)
  if (!nzchar(val)) NULL else val
}

.siu_p_label_int <- function(text, label) {
  raw <- .siu_p_label_value(text, label)
  if (is.null(raw)) return(NA_integer_)
  m <- regmatches(raw, regexpr("\\d+", raw, perl = TRUE))
  if (length(m) == 0L) NA_integer_ else as.integer(m)
}

.siu_p_re_escape <- function(s) {
  gsub("([\\\\.^$|()\\[\\]{}*+?])", "\\\\\\1", s, perl = TRUE)
}


# ---------------------------------------------------------------------------
# Section slicer -- pulls text between `header` and the first
# end-marker. Pure-R equivalent of _section_text.
# ---------------------------------------------------------------------------
.siu_p_section_text <- function(text, header, end_markers = character()) {
  pat <- paste0("(?:^|\\
)\\s*",
                .siu_p_re_escape(header), "\\s*\\
")
  m <- regexpr(pat, text, perl = TRUE)
  if (m[1L] == -1L) return("")
  start <- m[1L] + attr(m, "match.length")
  end <- nchar(text) + 1L
  for (em in end_markers) {
    em_pat <- paste0("(?:^|\\
)\\s*",
                     .siu_p_re_escape(em), "\\s*\\
")
    em_m <- regexpr(em_pat, substr(text, start, nchar(text)),
                    perl = TRUE)
    if (em_m[1L] != -1L) {
      cand <- start + em_m[1L] - 1L
      if (cand < end) end <- cand
    }
  }
  substr(text, start, end - 1L)
}


# ---------------------------------------------------------------------------
# Police-service vocabulary (mirrors _POLICE_SERVICES from
# _parser.py). Compiled from data/datasets/vsr/SIU1a.xlsx as of
# 2026-05-06; multi-service joins are handled at match-time.
# ---------------------------------------------------------------------------
.SIU_P_POLICE_SERVICES <- c(
  "Toronto Police Service", "RCMP", "Royal Canadian Mounted Police",
  "Ontario Provincial Police", "Peel Regional Police",
  "Vancouver Police Department", "Niagara Regional Police Service",
  "Hamilton Police Service", "Ottawa Police Service",
  "York Regional Police", "London Police Service",
  "Durham Regional Police Service", "Windsor Police Service",
  "Waterloo Regional Police Service", "Halton Regional Police Service",
  "Brantford Police Service", "Barrie Police Service",
  "Greater Sudbury Police Service",
  "Victoria/Esquimalt Police Department", "Kingston Police Service",
  "Guelph Police Service", "Peterborough Police Service",
  "Thunder Bay Police Service", "Chatham-Kent Police Service",
  "North Bay Police Service", "Sarnia Police Service",
  "Sault Ste. Marie Police Service", "Belleville Police Service",
  "Timmins Police Service", "Abbotsford Police Department",
  "Cornwall Community Police Service", "Cornwall Police Service",
  "St. Thomas Police Service", "Hanover Police Service",
  "Stratford Police Service", "Kawartha Lakes Police Service",
  "Saanich Police Department", "South Simcoe Police Service",
  "Owen Sound Police Service", "New Westminster Police Service",
  "Cobourg Police Service", "Niagara Parks Commission",
  "Dryden Police Service", "Brockville Police Service",
  "Delta Police Department", "West Vancouver Police Department",
  "Port Moody Police Department", "Gananoque Police Service",
  "Smith Falls Police Service", "Saugeen Shores Police Service",
  "Nelson Police Department", "Surrey Police Department",
  "Woodstock Police Service", "Port Hope Police Service",
  "Stl'atl'imx Tribal Police", "West Grey Police Service",
  "Anishinabek Police Service", "Six Nations Police",
  "Akwesasne Mohawk Police Service", "Nishnawbe Aski Police Service"
)

.SIU_P_POLICE_ABBR <- c(
  OPP   = "Ontario Provincial Police",
  TPS   = "Toronto Police Service",
  RCMP  = "RCMP",
  PRP   = "Peel Regional Police",
  VPD   = "Vancouver Police Department",
  NRPS  = "Niagara Regional Police Service",
  HPS   = "Hamilton Police Service",
  YRP   = "York Regional Police",
  LPS   = "London Police Service",
  DRPS  = "Durham Regional Police Service",
  WPS   = "Windsor Police Service",
  WRPS  = "Waterloo Regional Police Service",
  HRPS  = "Halton Regional Police Service",
  BPS   = "Brantford Police Service",
  GSPS  = "Greater Sudbury Police Service",
  TBPS  = "Thunder Bay Police Service",
  SSMPS = "Sault Ste. Marie Police Service",
  GPS   = "Guelph Police Service",
  CKPS  = "Chatham-Kent Police Service",
  NBPS  = "North Bay Police Service"
)

# Pre-strip the NPC jurisdictional boilerplate before vocab scan, so
# the SIU "About" footer doesn't false-tag every modern report as a
# Niagara Parks Commission case.
.SIU_P_NPC_RE <- paste0(
  "(?:special\\s+constables?\\s+of\\s+the\\s+|",
  "a\\s+special\\s+constable\\s+of\\s+the\\s+)",
  "Niagara\\s+Parks\\s+Commission",
  "(?:\\s+(?:and|or)\\s+(?:a\\s+)?peace\\s+officers?\\s+",
  "(?:with|under)\\s+(?:the\\s+)?Legisla[a-z]*[^.]{0,120})?"
)


.siu_p_detect_police_service <- function(text) {
  if (!is.character(text) || length(text) == 0L ||
      is.na(text[1L]) || !nzchar(text)) return(NA_character_)
  cleaned <- gsub(.SIU_P_NPC_RE, "", text,
                  perl = TRUE, ignore.case = TRUE)
  if (!nzchar(cleaned)) return(NA_character_)
  counts <- integer(0L)
  for (nm in .SIU_P_POLICE_SERVICES) {
    gm <- tryCatch(gregexpr(.siu_p_re_escape(nm), cleaned,
                            perl = TRUE)[[1L]],
                   error = function(e) -1L)
    if (length(gm) == 0L || is.na(gm[1L])) next
    c_ <- length(gm)
    ml <- attr(gm, "match.length")
    if (c_ > 0L && !is.null(ml) && !is.na(ml[1L]) &&
        isTRUE(ml[1L] != -1L)) {
      counts[nm] <- c_
    }
  }
  if (length(counts) > 0L) {
    # RCMP collapse (mirrors python behaviour)
    if (all(c("Royal Canadian Mounted Police", "RCMP") %in% names(counts))) {
      counts <- counts[setdiff(names(counts),
                               "Royal Canadian Mounted Police")]
    } else if ("Royal Canadian Mounted Police" %in% names(counts) &&
               !"RCMP" %in% names(counts)) {
      counts["RCMP"] <- counts[["Royal Canadian Mounted Police"]]
      counts <- counts[setdiff(names(counts),
                               "Royal Canadian Mounted Police")]
    }
    # Highest count wins; ties broken by longer name (more specific)
    ord <- order(-as.integer(counts),
                 -nchar(names(counts)))
    return(names(counts)[ord[1L]])
  }
  # Fall back to standalone abbreviations
  abbr_counts <- integer(0L)
  for (abbr in names(.SIU_P_POLICE_ABBR)) {
    full <- .SIU_P_POLICE_ABBR[[abbr]]
    pat <- paste0("\\b", .siu_p_re_escape(abbr), "\\b")
    hits <- gregexpr(pat, text, perl = TRUE)[[1L]]
    if (hits[1L] != -1L) {
      abbr_counts[full] <-
        (if (is.null(abbr_counts[full]) ||
             is.na(abbr_counts[full])) 0L
         else abbr_counts[full]) + length(hits)
    }
  }
  if (length(abbr_counts) > 0L) {
    return(names(abbr_counts)[which.max(abbr_counts)])
  }
  NA_character_
}


# ---------------------------------------------------------------------------
# Language detection (mirrors _EN_MARKERS / _FR_MARKERS).
# ---------------------------------------------------------------------------
.SIU_P_FR_MARKERS <- c(
  "L'enquete", "Exercice du mandat", "Elements de preuve",
  "Dispositions legislatives pertinentes", "Temoins civils",
  "Agents impliques", "Mandat de l'UES"
)
.SIU_P_EN_MARKERS <- c(
  "The Investigation", "Notification of the SIU",
  "Mandate engaged", "Civilian Witnesses",
  "Witness Officers", "Subject Officers",
  "Analysis and Director's Decision"
)

.siu_p_detect_language <- function(text) {
  # Crude ASCII fold for matching FR markers without accents
  norm <- iconv(text, to = "ASCII//TRANSLIT")
  if (is.na(norm)) norm <- text
  en <- sum(vapply(.SIU_P_EN_MARKERS,
                   function(m) grepl(m, text, fixed = TRUE),
                   logical(1L)))
  fr <- sum(vapply(.SIU_P_FR_MARKERS,
                   function(m) grepl(m, norm, fixed = TRUE),
                   logical(1L)))
  if (en >= 2L && en > fr) return("en")
  if (fr >= 2L && fr > en) return("fr")
  "unknown"
}


# ---------------------------------------------------------------------------
# URL helpers (drid / nrid extractors).
# ---------------------------------------------------------------------------
.siu_p_parse_drid_from_url <- function(url) {
  if (is.null(url) || !nzchar(url)) return(NA_integer_)
  m <- regmatches(url, regexpr("drid=(\\d+)", url, perl = TRUE))
  if (length(m) == 0L) return(NA_integer_)
  as.integer(sub("drid=", "", m))
}

.siu_p_parse_nrid_from_url <- function(url) {
  if (is.null(url) || !nzchar(url)) return(NA_integer_)
  m <- regmatches(url, regexpr("nrid=(\\d+)", url, perl = TRUE))
  if (length(m) == 0L) return(NA_integer_)
  as.integer(sub("nrid=", "", m))
}


# ---------------------------------------------------------------------------
# Normalisation helpers (mirror morie.siu._normalize).
# ---------------------------------------------------------------------------
.siu_p_normalise_sex <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(NA_character_)
  low <- tolower(trimws(s))
  if (low %in% c("woman", "female", "girl", "f")) return("Female")
  if (low %in% c("man", "male", "boy", "m"))      return("Male")
  if (low %in% c("non-binary", "nonbinary", "x")) return("Non-binary")
  s
}

.siu_p_normalise_yes_no <- function(v) {
  if (is.null(v) || is.na(v) || !nzchar(v)) return(NA)
  low <- tolower(trimws(v))
  if (low %in% c("yes", "y", "true", "t", "1")) return(TRUE)
  if (low %in% c("no", "n", "false", "f", "0"))  return(FALSE)
  NA
}

.siu_p_parse_date <- function(raw) {
  if (is.null(raw) || is.na(raw) || !nzchar(raw)) {
    return(list(iso = NA_character_, raw = NA_character_))
  }
  r <- gsub(",", "", trimws(raw))
  # "Month DD YYYY"
  d <- suppressWarnings(as.Date(r, format = "%B %d %Y"))
  if (is.na(d)) d <- suppressWarnings(as.Date(r, format = "%b %d %Y"))
  if (is.na(d)) d <- suppressWarnings(as.Date(r, format = "%d %B %Y"))
  if (is.na(d)) d <- suppressWarnings(as.Date(r, format = "%Y-%m-%d"))
  list(iso = if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d"),
       raw = raw)
}

.siu_p_find_case_number <- function(text) {
  # SIU case numbers: 2 digits, optional hyphen, 3-4 letters, hyphen,
  # 3 digits.
  m <- regmatches(text,
                  regexpr("\\b\\d{2}-[A-Z]{3,4}-\\d{3}\\b",
                          text, perl = TRUE))
  if (length(m) == 0L) NA_character_ else m
}


# ---------------------------------------------------------------------------
# Narrative + supplemental helpers.
# ---------------------------------------------------------------------------
.SIU_P_NARRATIVE_START_RE <-
  "(?:Mandate engaged|The Investigation|Notification of the SIU)"
.SIU_P_NARRATIVE_END_RE <- paste0(
  "(?:Endnotes|News Releases for this Case|",
  "Note:\\s*\\
*The signed English|",
  "^\\s*THE UNIT\\s*$|^\\s*PROGRAMS AND SERVICES\\s*$)"
)


.siu_p_extract_narrative_full <- function(html, text) {
  if (.siu_p_has_rvest()) {
    doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
    if (!is.null(doc)) {
      for (sel in c("div.report-body", "div.article-body",
                    "article", "div#main-content")) {
        node <- rvest::html_element(doc, sel)
        if (!inherits(node, "xml_missing") &&
            !is.na(node)) {
          out <- tryCatch(rvest::html_text2(node),
                          error = function(e) NA_character_)
          if (!is.na(out) && nzchar(out)) return(out)
        }
      }
    }
  }
  if (!is.character(text) || length(text) == 0L || is.na(text[1L]) ||
      !nzchar(text)) return(text)
  s_m <- tryCatch(regexpr(.SIU_P_NARRATIVE_START_RE, text,
                          perl = TRUE, ignore.case = TRUE),
                  error = function(e) -1L)
  e_m <- tryCatch(regexpr(.SIU_P_NARRATIVE_END_RE, text,
                          perl = TRUE, ignore.case = TRUE),
                  error = function(e) -1L)
  s <- if (length(s_m) == 0L || is.na(s_m[1L]) ||
           isTRUE(s_m[1L] == -1L)) 1L else as.integer(s_m[1L])
  e <- if (length(e_m) == 0L || is.na(e_m[1L]) ||
           isTRUE(e_m[1L] == -1L)) nchar(text) else as.integer(e_m[1L]) - 1L
  if (is.na(s) || is.na(e)) return(text)
  if (e > s) substr(text, s, e) else text
}


.siu_p_extract_summary <- function(text) {
  paras <- strsplit(text, "\
\
", fixed = TRUE)[[1L]]
  paras <- paras[nchar(paras) > 80L]
  if (!length(paras)) return(NA_character_)
  substr(paras[1L], 1L, 1500L)
}


.SIU_P_MH_RACE_KEYWORDS <- c(
  "mental health", "mental illness", "psychiatric", "schizophren",
  "bipolar", "depression", "psychosis", "psychotic", "suicidal",
  "self-harm", "delusion", "crisis intervention", "EDP",
  "emotionally disturbed", "MCIT",
  "Black", "African", "Indigenous", "First Nations", "Metis",
  "Inuit", "racialised", "racialized", "racial", "anti-Black",
  "anti-black", "ethnic", "South Asian", "racism"
)


.siu_p_scan_mh_race <- function(narrative) {
  if (is.null(narrative) || is.na(narrative) ||
      !nzchar(narrative)) return("")
  low <- tolower(narrative)
  hits <- character(0L)
  for (kw in .SIU_P_MH_RACE_KEYWORDS) {
    if (grepl(tolower(kw), low, fixed = TRUE) && !kw %in% hits) {
      hits <- c(hits, kw)
    }
  }
  paste(hits, collapse = "; ")
}


.siu_p_find_news_release_link <- function(html, source_url) {
  if (.siu_p_has_rvest()) {
    doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
    if (!is.null(doc)) {
      hrefs <- rvest::html_attr(
        rvest::html_elements(doc, "a"), "href")
      hrefs <- hrefs[!is.na(hrefs)]
      hit <- hrefs[grepl("news_template.php", hrefs, fixed = TRUE) &
                     grepl("nrid=", hrefs, fixed = TRUE)]
      if (length(hit) > 0L) {
        href <- hit[1L]
        if (!startsWith(href, "http") && !is.null(source_url)) {
          href <- paste0(sub("/[^/]*$", "/", source_url), href)
        }
        return(href)
      }
    }
  }
  m <- regmatches(html,
                  regexpr("news_template\\.php\\?nrid=\\d+",
                          html, perl = TRUE))
  if (length(m) == 0L) return(NA_character_)
  if (!is.null(source_url) && !startsWith(m, "http")) {
    m <- paste0(sub("/[^/]*$", "/", source_url), m)
  }
  m
}


# ---------------------------------------------------------------------------
# Public entry points.
# ---------------------------------------------------------------------------

#' Parse a SIU director's-report HTML page (pure-R)
#'
#' @param html Raw HTML response body.
#' @param drid Optional integer drid from the request URL -- useful
#'   when the page itself doesn't echo it.
#' @param source_url Optional canonical URL -- used to derive drid
#'   and recorded as \code{source_url_report}.
#' @return A list with every \code{SIU_COLUMNS} key (NAs for
#'   unfound fields).
#' @export
morie_siu_parse_html <- function(html, drid = NA_integer_,
                                 source_url = NULL) {
  text_full <- .siu_p_stripped_text(html)
  text <- .siu_p_trim_to_body(text_full)
  row <- .siu_p_blank_row()
  row$source_url_report <- source_url
  row$case_number <- .siu_p_find_case_number(text_full)

  lang <- .siu_p_detect_language(text)
  row$`_language` <- lang

  if (is.na(drid) && !is.null(source_url)) {
    drid <- .siu_p_parse_drid_from_url(source_url)
  }
  row$drid <- drid

  nrid_link <- .siu_p_find_news_release_link(html, source_url)
  if (!is.na(nrid_link)) {
    row$nrid <- .siu_p_parse_nrid_from_url(nrid_link)
    row$source_url_news <- nrid_link
  }

  row$narrative_full <- .siu_p_extract_narrative_full(html, text)
  row$narrative_summary <- .siu_p_extract_summary(text)

  if (identical(lang, "fr")) {
    # French-only page: skip the English-only structured extraction.
    return(row)
  }

  row$police_service <- .siu_p_detect_police_service(text)

  # Date fields
  raw_inc <- .siu_p_label_value(text, "Date of Incident") %||%
    .siu_p_label_value(text, "Incident Date")
  d_inc <- .siu_p_parse_date(raw_inc)
  row$date_of_incident_iso <- d_inc$iso
  row$date_of_incident_raw <- d_inc$raw

  raw_not <- .siu_p_label_value(text, "Date SIU was Notified") %||%
    .siu_p_label_value(text, "SIU Notified")
  d_not <- .siu_p_parse_date(raw_not)
  row$date_siu_notified_iso <- d_not$iso
  row$date_siu_notified_raw <- d_not$raw

  raw_dec <- .siu_p_label_value(
    text, "Date of SIU Director's Decision") %||%
    .siu_p_label_value(text, "Decision Date")
  if (is.null(raw_dec)) {
    m <- regmatches(text,
                    regexpr(
                      "Date:\\s*([A-Z][a-z]+\\s+\\d{1,2},?\\s+\\d{4})",
                      text, perl = TRUE))
    if (length(m) > 0L) raw_dec <- sub("Date:\\s*", "", m)
  }
  d_dec <- .siu_p_parse_date(raw_dec)
  row$date_of_director_decision_iso <- d_dec$iso
  row$date_of_director_decision_raw <- d_dec$raw

  # Counts (best-effort labels; full prose-mining lives in the
  # Rcpp/C++ parser).
  row$number_of_officers_involved <-
    .siu_p_label_int(text, "Number of Officers")

  # Location / reason
  row$location_of_call <- .siu_p_label_value(text, "Location")
  row$reason_for_interaction <-
    .siu_p_label_value(text, "Reason for Interaction")

  # Charges
  row$charges_recommended <- .siu_p_normalise_yes_no(
    .siu_p_label_value(text, "Charges Recommended") %||%
      .siu_p_label_value(text, "Charges Laid")
  )

  # MH / race signal
  row$mental_health_or_race_indications <-
    .siu_p_scan_mh_race(row$narrative_full %||% "")

  # Sex/age via "<n>-year-old <woman|man|...>"
  m <- regmatches(
    text,
    regexpr(paste0("\\b(\\d{1,3})[\\s-]year[\\s-]old\\s+",
                   "(woman|man|female|male|girl|boy|person|",
                   "individual|youth|child|adult)\\b"),
            text, perl = TRUE, ignore.case = TRUE))
  if (length(m) > 0L) {
    pieces <- regmatches(
      m, regexec(paste0("(\\d{1,3})[\\s-]year[\\s-]old\\s+",
                        "(\\w+)"),
                 m, perl = TRUE, ignore.case = TRUE))[[1L]]
    if (length(pieces) >= 3L) {
      row$age_affected <- pieces[2L]
      row$sex_gender_affected <- .siu_p_normalise_sex(pieces[3L])
    }
  }

  row
}


#' Parse a SIU news-release HTML page (pure-R)
#'
#' News-release pages live at \code{news_template.php?nrid=<N>} and
#' have a different layout than the director's reports -- a single
#' headline, short summary paragraph, signed-by-Director line.
#'
#' @param html Raw HTML response body.
#' @param nrid Optional integer nrid from the request URL.
#' @param source_url Optional canonical URL.
#' @return A list with \code{nrid}, \code{source_url_news},
#'   \code{news_release_title}, \code{news_release_date_iso},
#'   \code{news_release_date_raw}, \code{news_release_summary},
#'   \code{case_number}, and \code{directors_name}.
#' @export
morie_siu_parse_news_html <- function(html, nrid = NA_integer_,
                                      source_url = NULL) {
  text_full <- .siu_p_stripped_text(html)
  out <- list(
    nrid                  = nrid,
    source_url_news       = source_url,
    news_release_title    = NA_character_,
    news_release_date_iso = NA_character_,
    news_release_date_raw = NA_character_,
    news_release_summary  = NA_character_,
    case_number           = .siu_p_find_case_number(text_full),
    directors_name        = NA_character_
  )

  # Headline: line after the second-occurring "News Release" marker.
  matches <- gregexpr("\\
\\s*News Release\\s*\\
",
                      text_full, perl = TRUE)[[1L]]
  if (matches[1L] != -1L) {
    for (start in matches) {
      after <- substr(text_full,
                      start + attr(matches, "match.length")[1L],
                      nchar(text_full))
      nl <- regexpr("\\
", after, perl = TRUE)
      if (nl[1L] == -1L) next
      candidate <- trimws(substr(after, 1L, nl[1L] - 1L))
      if (!nzchar(candidate)) next
      if (tolower(candidate) %in% c("media centre",
                                    "news releases",
                                    "case numbers")) next
      if (nchar(candidate) < 10L || nchar(candidate) > 200L) next
      out$news_release_title <- candidate
      break
    }
  }

  # Release date: "<City>, ON (DD Month, YYYY)"
  rd_re <- paste0(
    "([A-Za-z\\.]+(?:[ \\-][A-Za-z\\.]+)?,\\s*ON)\\s*",
    "\\(\\s*(\\d{1,2}\\s+[A-Z][a-z]+,?\\s+\\d{4})\\s*\\)"
  )
  m <- regmatches(text_full,
                  regexec(rd_re, text_full,
                          perl = TRUE, ignore.case = TRUE))[[1L]]
  if (length(m) >= 3L) {
    raw <- trimws(m[3L])
    d <- .siu_p_parse_date(raw)
    out$news_release_date_iso <- d$iso
    out$news_release_date_raw <- raw
    # Summary: chunk immediately after the date stamp.
    after <- substr(text_full,
                    attr(regexpr(rd_re, text_full, perl = TRUE,
                                 ignore.case = TRUE),
                         "match.length")[1L] + 1L,
                    nchar(text_full))
    after <- sub("^\\s*-+\\s*", "", after, perl = TRUE)
    chunks <- strsplit(after, "\\
\\s*\\
", perl = TRUE)[[1L]]
    if (length(chunks) > 0L) {
      out$news_release_summary <- substr(trimws(chunks[1L]),
                                         1L, 1500L)
    }
  }

  # Director name
  dr_re <- paste0(
    "Director of the Special Investigations Unit,\\s*",
    "([A-Z][A-Za-z'\\-]+\\s+[A-Z][A-Za-z'\\-]+)"
  )
  m2 <- regmatches(text_full,
                   regexec(dr_re, text_full, perl = TRUE))[[1L]]
  if (length(m2) >= 2L) out$directors_name <- trimws(m2[2L])
  out
}


# Local null-coalesce used inside this file (no operator export).
