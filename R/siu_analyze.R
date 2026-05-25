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

#' SIU descriptive analyses (R port of \code{morie.siu.analyze})
#'
#' Turns a scraped SIU\code{_by_case.csv} (the canonical output of
#' \code{\link{morie_fetch_siu}}) into a set of \code{RichResult}-style
#' descriptive analyses. Each callable accepts either a path to the
#' CSV or a data frame directly and returns a structured list with
#' \code{title}, \code{summary_lines}, \code{tables},
#' \code{interpretation}, \code{warnings}, and \code{payload}
#' fields. The structure mirrors the Python analyze.py surfaces so
#' that the same multi-section render works through morie's
#' rich-output dispatch.
#'
#' These analyses are deliberately distinct from the published-table
#' replicators in \code{\link{morie_siu_sprott_doob_feb2021}} and the
#' Mandela classifier in \code{\link{morie_siu_classify_mandela}}:
#' those work from CSC SIU person-stay data, whereas these analyses
#' work from the Ontario SIU \emph{director's-report} corpus.
#'
#' @name morie_siu_analyze
#' @seealso \code{\link{morie_fetch_siu}},
#'   \code{\link{morie_siu_classify_mandela}},
#'   \code{\link{mrm_siu_per_service_rate}}.
NULL


# ---------------------------------------------------------------------------
# .siu_an_load -- accept a data frame, a path, or NULL (defaults).
# ---------------------------------------------------------------------------
.siu_an_load <- function(x = NULL) {
  if (is.data.frame(x)) {
    return(x)
  }
  default_csv <- file.path(
    tempdir(), "morie", "siu", "SIU_by_case.csv"
  )
  p <- if (is.null(x)) default_csv else as.character(x)
  if (!file.exists(p)) {
    stop(
      "SIU dataset not found at '", p, "'. Run morie_fetch_siu() ",
      "first, or pass the CSV path explicitly.",
      call. = FALSE
    )
  }
  utils::read.csv(p, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# .siu_an_rich -- thin RichResult constructor mirroring sprott_doob.R's
# .morie_siu_rich; reproduced here to keep this file self-contained.
# ---------------------------------------------------------------------------
.siu_an_rich <- function(title, summary_lines = list(),
                         tables = list(),
                         interpretation = "",
                         warnings = character(),
                         payload = list()) {
  structure(
    list(
      title          = title,
      summary_lines  = summary_lines,
      tables         = tables,
      interpretation = interpretation,
      warnings       = warnings,
      payload        = payload
    ),
    class = c("morie_rich_result", "list")
  )
}


# ---------------------------------------------------------------------------
# Truthy / falsy counters tolerant of CSV-roundtripped booleans.
# ---------------------------------------------------------------------------
.siu_an_truthy <- function(v) {
  if (is.logical(v)) return(sum(v %in% TRUE, na.rm = TRUE))
  s <- tolower(trimws(as.character(v)))
  sum(s %in% c("true", "yes", "1", "t"), na.rm = TRUE)
}

.siu_an_falsy <- function(v) {
  if (is.logical(v)) return(sum(v %in% FALSE, na.rm = TRUE))
  s <- tolower(trimws(as.character(v)))
  sum(s %in% c("false", "no", "0", "f"), na.rm = TRUE)
}


#' SIU cases by police service
#'
#' Per-police-service tabulation of case counts, charges-recommended
#' counts, and the implied charge rate. Sorted by case count
#' (descending), capped at the top 30 in the rendered table.
#'
#' @param data Either a data frame (e.g. the output of
#'   \code{morie_fetch_siu()} read in) or a path to
#'   \code{SIU_by_case.csv}. \code{NULL} (the default) looks in
#'   \code{file.path(tempdir(), "morie", "siu", "SIU_by_case.csv")}.
#' @return A \code{morie_rich_result} list with summary lines,
#'   one table, and a payload of raw counts.
#' @export
morie_siu_by_police_service <- function(data = NULL) {
  df <- .siu_an_load(data)
  if (!"police_service" %in% names(df)) {
    stop("Column 'police_service' missing from SIU_by_case.csv.",
         call. = FALSE)
  }
  if (!"charges_recommended" %in% names(df)) {
    df$charges_recommended <- NA
  }

  svc <- ifelse(is.na(df$police_service) | df$police_service == "",
                "(unknown)", as.character(df$police_service))
  rows <- list()
  total_charged <- 0L
  total_nocharge <- 0L
  uniq <- unique(svc)
  counts <- integer(length(uniq))
  names(counts) <- uniq
  for (s in uniq) {
    sub <- df[svc == s, , drop = FALSE]
    n <- nrow(sub)
    counts[[s]] <- n
    c_t <- .siu_an_truthy(sub$charges_recommended)
    c_f <- .siu_an_falsy(sub$charges_recommended)
    total_charged <- total_charged + c_t
    total_nocharge <- total_nocharge + c_f
    rate_str <- if ((c_t + c_f) > 0L) {
      sprintf("%.1f%%", 100 * c_t / (c_t + c_f))
    } else {
      "--"
    }
    rows[[length(rows) + 1L]] <- list(
      service     = substr(s, 1L, 50L),
      n           = n,
      charged     = c_t,
      no_charges  = c_f,
      charge_rate = rate_str
    )
  }

  # Sort by case count descending
  rows <- rows[order(vapply(rows, function(r) -as.integer(r$n),
                            integer(1)))]
  top5 <- vapply(rows[seq_len(min(5L, length(rows)))],
                 function(r) r$service, character(1))

  table_rows <- lapply(rows[seq_len(min(30L, length(rows)))],
                       function(r) {
    list(r$service, r$n, r$charged, r$no_charges, r$charge_rate)
  })

  .siu_an_rich(
    title = "SIU cases by police service",
    summary_lines = list(
      list("Unique police services", length(uniq)),
      list("Total cases", nrow(df)),
      list("With charges_recommended True", total_charged),
      list("With charges_recommended False", total_nocharge)
    ),
    tables = list(list(
      title   = "By police service (top 30):",
      headers = c("Police service", "Cases",
                  "Charged", "No charges", "Charge rate"),
      rows    = table_rows
    )),
    interpretation = paste0(
      "Top services by case count: ",
      paste(top5, collapse = ", "), ". ",
      "Services with low charge rate may indicate either truly ",
      "justified force or systematic under-charging -- ",
      "context-dependent interpretation."
    ),
    payload = list(
      counts     = as.list(counts),
      charged    = total_charged,
      no_charges = total_nocharge
    )
  )
}


#' SIU cases by year
#'
#' Year-over-year case volume plus charges-rate, parsed from the
#' first four characters of \code{date_of_incident_iso}.
#'
#' @inheritParams morie_siu_by_police_service
#' @return A \code{morie_rich_result} list.
#' @export
morie_siu_by_year <- function(data = NULL) {
  df <- .siu_an_load(data)
  if (!"date_of_incident_iso" %in% names(df)) {
    stop("Column 'date_of_incident_iso' missing.", call. = FALSE)
  }
  if (!"charges_recommended" %in% names(df)) {
    df$charges_recommended <- NA
  }
  yrs <- suppressWarnings(
    as.integer(substr(as.character(df$date_of_incident_iso), 1L, 4L))
  )
  valid <- !is.na(yrs)
  vdf <- df[valid, , drop = FALSE]
  vyr <- yrs[valid]

  uniq_yrs <- sort(unique(vyr))
  rows <- list()
  by_year_payload <- list()
  for (y in uniq_yrs) {
    sub <- vdf[vyr == y, , drop = FALSE]
    n <- nrow(sub)
    c_t <- .siu_an_truthy(sub$charges_recommended)
    c_f <- .siu_an_falsy(sub$charges_recommended)
    rate <- if ((c_t + c_f) > 0L) {
      sprintf("%.1f%%", 100 * c_t / (c_t + c_f))
    } else {
      "--"
    }
    rows[[length(rows) + 1L]] <- list(y, n, c_t, c_f, rate)
    by_year_payload[[as.character(y)]] <- n
  }

  span <- if (length(uniq_yrs)) {
    paste0(min(uniq_yrs), "-", max(uniq_yrs))
  } else {
    "n/a"
  }

  .siu_an_rich(
    title = "SIU cases by year",
    summary_lines = list(
      list("Years covered", span),
      list("Total cases with parseable date", nrow(vdf)),
      list("Cases with no parseable date", nrow(df) - nrow(vdf))
    ),
    tables = list(list(
      title   = "By year:",
      headers = c("Year", "Cases", "Charged", "No charges",
                  "Charge rate"),
      rows    = rows
    )),
    payload = list(by_year = by_year_payload)
  )
}


#' SIU per-case team-size distribution
#'
#' Distribution (n, mean, median, min, max) of subject-officials,
#' witness-officials, civilian-witnesses, and officers-involved.
#'
#' @inheritParams morie_siu_by_police_service
#' @return A \code{morie_rich_result} list.
#' @export
morie_siu_case_counts <- function(data = NULL) {
  df <- .siu_an_load(data)
  fields <- list(
    c("number_of_subject_officials",  "#Subject officials"),
    c("number_of_witness_officials",  "#Witness officials"),
    c("number_of_civilian_witnesses", "#Civilian witnesses"),
    c("number_of_officers_involved",  "#Officers involved")
  )
  rows <- list()
  for (f in fields) {
    col <- f[1L]
    lab <- f[2L]
    if (!col %in% names(df)) {
      rows[[length(rows) + 1L]] <-
        list(lab, "n/a", "n/a", "n/a", "n/a", "n/a")
      next
    }
    v <- suppressWarnings(as.numeric(df[[col]]))
    v <- v[!is.na(v)]
    if (length(v) == 0L) {
      rows[[length(rows) + 1L]] <-
        list(lab, "n/a", "n/a", "n/a", "n/a", "n/a")
    } else {
      rows[[length(rows) + 1L]] <- list(
        lab, length(v),
        sprintf("%.2f", mean(v)),
        sprintf("%.0f", stats::median(v)),
        sprintf("%.0f", min(v)),
        sprintf("%.0f", max(v))
      )
    }
  }
  .siu_an_rich(
    title = "SIU case-team size distribution",
    summary_lines = list(list("Total cases", nrow(df))),
    tables = list(list(
      title   = "Per-case team / witness counts:",
      headers = c("Field", "n parsed",
                  "Mean", "Median", "Min", "Max"),
      rows    = rows
    ))
  )
}


#' Affected-person demographics
#'
#' Sex/gender frequency table and age-distribution summary.
#'
#' @inheritParams morie_siu_by_police_service
#' @return A \code{morie_rich_result} list.
#' @export
morie_siu_demographics <- function(data = NULL) {
  df <- .siu_an_load(data)
  sex_col <- if ("sex_gender_affected" %in% names(df)) {
    ifelse(is.na(df$sex_gender_affected) |
             df$sex_gender_affected == "",
           "unknown", as.character(df$sex_gender_affected))
  } else {
    rep("unknown", nrow(df))
  }
  sex_tab <- sort(table(sex_col), decreasing = TRUE)
  sex_rows <- lapply(names(sex_tab), function(k) {
    list(k, as.integer(sex_tab[[k]]),
         sprintf("%.1f%%", 100 * sex_tab[[k]] / sum(sex_tab)))
  })

  age <- if ("age_affected" %in% names(df)) {
    suppressWarnings(as.numeric(df$age_affected))
  } else {
    numeric(0)
  }
  age <- age[!is.na(age)]

  mean_age <- if (length(age)) mean(age) else NA_real_
  med_age  <- if (length(age)) stats::median(age) else NA_real_
  age_rng <- if (length(age)) {
    paste0(as.integer(min(age)), "-", as.integer(max(age)))
  } else {
    "n/a"
  }

  .siu_an_rich(
    title = "Affected-person demographics",
    summary_lines = list(
      list("Total cases", nrow(df)),
      list("Cases with parseable age", length(age)),
      list("Mean age", mean_age),
      list("Median age", med_age),
      list("Age range", age_rng)
    ),
    tables = list(list(
      title   = "By sex/gender:",
      headers = c("Sex/gender", "Count", "Percent"),
      rows    = sex_rows
    ))
  )
}


#' Mental-health / race keyword indicators
#'
#' Frequency table of the semicolon-delimited keyword tags written by
#' the parser into \code{mental_health_or_race_indications}. The
#' parser scans narratives against a closed vocabulary
#' (see \code{morie.siu._parser._MH_RACE_KEYWORDS}); this analysis
#' tallies those tags across the case corpus.
#'
#' @inheritParams morie_siu_by_police_service
#' @return A \code{morie_rich_result} list, including a warning
#'   noting that keyword-presence is a signal, not a verdict.
#' @export
morie_siu_mental_health_race_indicators <- function(data = NULL) {
  df <- .siu_an_load(data)
  if (!"mental_health_or_race_indications" %in% names(df)) {
    return(.siu_an_rich(
      title = "Mental-health / race indicators in SIU narratives",
      warnings = "Column 'mental_health_or_race_indications' missing."
    ))
  }
  sigs <- as.character(df$mental_health_or_race_indications)
  sigs <- sigs[!is.na(sigs) & nzchar(trimws(sigs))]
  nonempty <- length(sigs)
  counts <- list()
  for (s in sigs) {
    for (kw in strsplit(s, ";", fixed = TRUE)[[1L]]) {
      kw <- trimws(kw)
      if (!nzchar(kw)) next
      prev <- if (is.null(counts[[kw]])) 0L else counts[[kw]]
      counts[[kw]] <- prev + 1L
    }
  }
  ord <- order(unlist(counts, use.names = FALSE), decreasing = TRUE)
  kws <- names(counts)[ord]
  vals <- as.integer(unlist(counts, use.names = FALSE))[ord]
  topn <- min(25L, length(kws))
  rows <- if (topn == 0L) list() else mapply(
    function(k, v) list(k, v),
    kws[seq_len(topn)], vals[seq_len(topn)],
    SIMPLIFY = FALSE
  )

  pct <- if (nrow(df) > 0L) {
    sprintf("%.1f%%", 100 * nonempty / nrow(df))
  } else {
    "n/a"
  }

  .siu_an_rich(
    title = "Mental-health / race indicators in SIU narratives",
    summary_lines = list(
      list("Total cases", nrow(df)),
      list("Cases with >= 1 indicator", nonempty),
      list("Distinct keywords matched", length(counts))
    ),
    tables = list(list(
      title   = "Top keywords:",
      headers = c("Keyword", "Cases mentioning"),
      rows    = rows
    )),
    warnings = paste(
      "Keyword-presence is a SIGNAL not a verdict. A case mentioning",
      "'mental health' may discuss it briefly without being primarily",
      "about MH. Read narratives in SIU_narratives.jsonl for context."
    ),
    interpretation = paste0(
      nonempty, "/", nrow(df), " cases (", pct, ") have at least one ",
      "MH or race keyword in the narrative. The distribution by ",
      "keyword is shown above; see also `morie_siu_by_police_service` ",
      "for service-by-service patterns."
    )
  )
}


# ---------------------------------------------------------------------------
# .siu_an_interval -- day-delta summary helper used by decision_timing.
# ---------------------------------------------------------------------------
.siu_an_interval <- function(label, a_iso, b_iso) {
  a <- suppressWarnings(as.Date(a_iso))
  b <- suppressWarnings(as.Date(b_iso))
  d <- as.numeric(b - a)
  d <- d[!is.na(d) & is.finite(d)]
  if (length(d) == 0L) {
    return(list(label, "n/a", "n/a", "n/a", "n/a", "n/a"))
  }
  list(label, length(d),
       sprintf("%.1f", mean(d)),
       sprintf("%.0f", stats::median(d)),
       sprintf("%.0f", min(d)),
       sprintf("%.0f", max(d)))
}


#' SIU decision-timing distributions
#'
#' Day-delta distributions for the three SIU process intervals:
#' \describe{
#'   \item{Incident -> SIU notified}{From
#'     \code{date_of_incident_iso} to \code{date_siu_notified_iso}.}
#'   \item{Notified -> director's decision}{From
#'     \code{date_siu_notified_iso} to
#'     \code{date_of_director_decision_iso}.}
#'   \item{Incident -> decision (total)}{The end-to-end interval.}
#' }
#'
#' @inheritParams morie_siu_by_police_service
#' @return A \code{morie_rich_result} list.
#' @export
morie_siu_decision_timing <- function(data = NULL) {
  df <- .siu_an_load(data)
  required <- c(
    "date_of_incident_iso",
    "date_siu_notified_iso",
    "date_of_director_decision_iso"
  )
  for (r in required) {
    if (!r %in% names(df)) df[[r]] <- NA_character_
  }
  rows <- list(
    .siu_an_interval("Incident -> SIU notified",
                     df$date_of_incident_iso,
                     df$date_siu_notified_iso),
    .siu_an_interval("Notified -> director's decision",
                     df$date_siu_notified_iso,
                     df$date_of_director_decision_iso),
    .siu_an_interval("Incident -> decision (total)",
                     df$date_of_incident_iso,
                     df$date_of_director_decision_iso)
  )
  .siu_an_rich(
    title = "SIU decision timing (days)",
    summary_lines = list(list("Total cases", nrow(df))),
    tables = list(list(
      title   = "Interval distributions (days):",
      headers = c("Interval", "n parsed",
                  "Mean", "Median", "Min", "Max"),
      rows    = rows
    ))
  )
}


#' Chi-square: charges-recommended independent of year?
#'
#' Cross-tabulates \code{charges_recommended} against incident-year
#' and tests independence with a Pearson chi-square (\code{
#' stats::chisq.test}; p-value via \code{stats::pchisq}). Years with
#' zero charge-decided cases are dropped. Complements
#' \code{\link{morie_siu_verify_chi2}} in \code{sprott_doob.R}, which
#' tests specific published 2x2 tables; this is a generic "did the
#' charge rate move?" probe over the harvested SIU corpus.
#'
#' @inheritParams morie_siu_by_police_service
#' @return A \code{morie_rich_result} list including the contingency
#'   table, statistic, df, and p-value.
#' @export
morie_siu_charges_by_year_chi2 <- function(data = NULL) {
  df <- .siu_an_load(data)
  yrs <- suppressWarnings(
    as.integer(substr(as.character(df$date_of_incident_iso), 1L, 4L))
  )
  ch_s <- tolower(trimws(as.character(df$charges_recommended)))
  charge_flag <- ifelse(
    ch_s %in% c("true", "yes", "1", "t"), "charged",
    ifelse(ch_s %in% c("false", "no", "0", "f"), "no_charges", NA)
  )
  ok <- !is.na(yrs) & !is.na(charge_flag)
  if (sum(ok) < 5L) {
    return(.siu_an_rich(
      title = "Charges-by-year chi-square",
      warnings = "Fewer than 5 usable rows; skipping test."
    ))
  }
  tab <- table(year = yrs[ok], charge = charge_flag[ok])
  keep <- apply(tab, 1L, function(r) all(r > 0L))
  tab <- tab[keep, , drop = FALSE]
  if (nrow(tab) < 2L) {
    return(.siu_an_rich(
      title = "Charges-by-year chi-square",
      warnings = "Fewer than 2 usable years after dropping zero rows."
    ))
  }
  tst <- suppressWarnings(stats::chisq.test(tab))
  stat <- as.numeric(tst$statistic)
  df_  <- as.integer(tst$parameter)
  pval <- as.numeric(
    stats::pchisq(stat, df = df_, lower.tail = FALSE)
  )
  .siu_an_rich(
    title = "SIU charges-recommended vs. year (chi-square)",
    summary_lines = list(
      list("Years included", nrow(tab)),
      list("Pearson chi-square", sprintf("%.3f", stat)),
      list("Degrees of freedom", df_),
      list("p-value", sprintf("%.4g", pval))
    ),
    tables = list(list(
      title   = "Contingency table (year x charge_flag):",
      headers = c("Year", "Charged", "No charges"),
      rows    = lapply(rownames(tab), function(y) {
        list(y,
             as.integer(tab[y, "charged"]),
             as.integer(tab[y, "no_charges"]))
      })
    )),
    interpretation = paste0(
      "H0: charge decision is independent of incident year. ",
      "p = ", sprintf("%.4g", pval),
      if (pval < 0.05) {
        " (reject H0 at alpha=0.05: charge rate moves over time)."
      } else {
        " (do not reject H0: no evidence of year-over-year shift)."
      }
    ),
    payload = list(statistic = stat, df = df_, p_value = pval)
  )
}


#' Run every SIU descriptive analysis in this module
#'
#' Convenience wrapper that runs each of the analysis surfaces and
#' optionally writes a \code{.txt} dump (and \code{.json} payload if
#' \pkg{jsonlite} is available) per result. Failures are captured
#' per-analysis: one bad surface does not stop the rest.
#'
#' @inheritParams morie_siu_by_police_service
#' @param out_dir Optional directory; if non-\code{NULL}, each result
#'   is written as \code{siu_analysis_<name>.txt}.
#' @return A named list of \code{morie_rich_result} objects.
#' @export
morie_siu_all_analyses <- function(data = NULL, out_dir = NULL) {
  surfaces <- list(
    by_police_service  = morie_siu_by_police_service,
    by_year            = morie_siu_by_year,
    case_counts        = morie_siu_case_counts,
    demographics       = morie_siu_demographics,
    mh_race_indicators = morie_siu_mental_health_race_indicators,
    decision_timing    = morie_siu_decision_timing,
    charges_year_chi2  = morie_siu_charges_by_year_chi2
  )
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  results <- list()
  for (nm in names(surfaces)) {
    r <- tryCatch(
      surfaces[[nm]](data),
      error = function(e) .siu_an_rich(
        title = paste0("siu.", nm, " (failed)"),
        warnings = paste0(class(e)[1L], ": ", conditionMessage(e))
      )
    )
    results[[nm]] <- r
    if (!is.null(out_dir)) {
      txt_path <- file.path(out_dir,
                            paste0("siu_analysis_", nm, ".txt"))
      tryCatch(
        writeLines(utils::capture.output(print(r)), txt_path),
        error = function(e) invisible(NULL)
      )
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        json_path <- file.path(out_dir,
                               paste0("siu_analysis_", nm, ".json"))
        tryCatch(
          writeLines(
            jsonlite::toJSON(r$payload, auto_unbox = TRUE,
                             pretty = TRUE, null = "null"),
            json_path
          ),
          error = function(e) invisible(NULL)
        )
      }
    }
  }
  results
}
