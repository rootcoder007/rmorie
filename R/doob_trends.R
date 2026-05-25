# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) morie contributors
#
# This file is part of morie. morie is free software: you can
# redistribute it and/or modify it under the terms of the GNU Affero
# General Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later
# version. See LICENSE for the full text.

#' Doob Federal Court Affidavit — national-aggregate trend analyses
#'
#' Replicates the analytical contribution of Prof. Anthony N. Doob's
#' expert-witness affidavit in *Canadian Civil Liberties Association
#' et al. v. The Attorney General of Canada* (Federal Court file
#' T-539-20, Application Record Vol. 3 of 5, pp. 778-795).
#'
#' Doob's national-aggregate analyses (Figures 1-4 + Tables 1-3) sit
#' ALONGSIDE the per-row MRM modules on OTIS provincial data and the
#' MRM chi-square family on aggregate contingency tables.
#'
#' @name doob_trends
#' @references
#' Doob, A. N. (2020). Affidavit (T-539-20) of Anthony Doob — Federal
#' Court of Canada, Application Record Vol. 3 of 5. CCLA et al. v.
#' Attorney General of Canada.
NULL


# -- Table 1: 5-year average annual releases (CCRSO 2013/14-2017/18) --

#' CCRSO Table 1 — 5-year average annual conditional releases
#'
#' @export
CCRSO_TABLE1_RELEASES <- data.frame(
  type                   = c("Day Parole", "Full Parole", "Statutory Release"),
  revoke_violent         = c(4.8, 5.0, 84.6),
  revoke_violent_pct     = c(0.14, 0.49, 1.5),
  revoke_non_violent     = c(33.8, 28.2, 452.8),
  revoke_non_violent_pct = c(1.0, 2.8, 7.8),
  revoke_breach          = c(268.6, 89.4, 1556.0),
  revoke_breach_pct      = c(7.9, 8.7, 26.7),
  success                = c(3085, 901.2, 3735.6),
  success_pct            = c(90.9, 88.0, 64.1),
  total                  = c(3392.2, 1023.8, 5829.0),
  stringsAsFactors       = FALSE
)


# -- Table 2: prisoner flow 2013/14-2017/18 (CCRSO) -------------------

#' CCRSO Table 2 — prisoner flow 2013/14-2017/18
#'
#' @export
CCRSO_TABLE2_FLOW <- data.frame(
  year                 = c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18"),
  avg_count            = c(15342, 14886, 14712, 14159, 14092),
  admissions           = c(5071, 4818, 4891, 4908, 4718),
  deaths               = c(48, 67, 65, 47, NA),
  full_parole_releases = c(163, 185, 178, 166, 208),
  statutory_releases   = c(5636, 5373, 5309, 4888, 4427),
  stringsAsFactors     = FALSE
)


# -- Table 3: 2018 age distribution (CCRSO + StatsCan) ----------------

#' CCRSO/StatsCan Table 3 — 2018 age distribution
#'
#' @export
CCRSO_TABLE3_AGE <- data.frame(
  age_group               = c("18-49", "50-59", "60+"),
  canada_adult_pop        = c(15770626, 5305888, 8811720),
  canada_adult_pop_pct    = c(52.8, 17.8, 29.5),
  csc_in_custody          = c(10544, 2236, 1312),
  csc_in_custody_pct      = c(74.8, 15.9, 9.3),
  csc_admissions_2017_18  = c(3920, 548, 250),
  csc_admissions_pct      = c(83.1, 11.6, 5.3),
  stringsAsFactors        = FALSE
)


# -- Helper: build a Rich-style named-list result --------------------

.doob_result <- function(title, summary_lines, tables = list(),
                          interpretation = NULL, payload = list(),
                          warnings = character()) {
  structure(
    list(
      title           = title,
      summary_lines   = summary_lines,
      tables          = tables,
      interpretation  = interpretation,
      payload         = payload,
      warnings        = warnings
    ),
    class = c("morie_result", "list")
  )
}


# -- Table 1 analysis -------------------------------------------------

#' Analyse Doob Affidavit Table 1 — 5-year average annual releases
#'
#' Renders Table 1 and computes overall success / revocation rates.
#'
#' @return A `morie_result` named-list with title, summary_lines,
#'   tables, interpretation, and payload.
#' @export
analyze_doob_table1_releases <- function() {
  tbl <- CCRSO_TABLE1_RELEASES
  rows <- lapply(seq_len(nrow(tbl)), function(i) {
    r <- tbl[i, ]
    c(r$type,
      sprintf("%.1f (%.2f%%)", r$revoke_violent, r$revoke_violent_pct),
      sprintf("%.1f (%.1f%%)", r$revoke_non_violent, r$revoke_non_violent_pct),
      sprintf("%.1f (%.1f%%)", r$revoke_breach, r$revoke_breach_pct),
      sprintf("%.1f (%.1f%%)", r$success, r$success_pct),
      sprintf("%.1f", r$total))
  })
  total_pop <- sum(tbl$total)
  total_violent_revokes <- sum(tbl$revoke_violent)
  overall_violent_revoke_pct <-
    if (total_pop > 0) 100 * total_violent_revokes / total_pop else 0

  .doob_result(
    title = paste0("Doob Affidavit Table 1 -- Successful & unsuccessful ",
                   "conditional releases (5-year avg, CCRSO 2013/14-2017/18)"),
    summary_lines = list(
      c("Source", "CCRSO 2018 pp.94-98, Doob Affidavit Exhibit B"),
      c("Total annual releases (5-yr avg)", round(total_pop, 1)),
      c("Total annual revoke-violent (5-yr avg)", round(total_violent_revokes, 1)),
      c("Overall violent-revocation rate",
        sprintf("%.3f%%", overall_violent_revoke_pct)),
      c("Doob's headline point",
        paste0(">= 99.4% of releases are successful or non-violent-",
               "revoked -- the violent-revocation rate is < 1%"))
    ),
    tables = list(list(
      title   = paste0("Table 1: Average annual releases by type, ",
                       "with revocation breakdowns:"),
      headers = c("Type", "Revoke (violent)", "Revoke (non-violent)",
                  "Revoke (breach)", "Successful", "Total"),
      rows    = rows
    )),
    interpretation = paste0(
      "Reproduces Doob's Federal Court Table 1. The headline finding: ",
      "violent revocations are very rare -- < 1% of all releases ",
      "(across day parole, full parole, statutory) result in revocation ",
      "due to a violent offence. The majority of unsuccessful releases ",
      "are breach-of-condition (curfew, drug/alcohol restrictions, ",
      "location restrictions), not new criminal conduct."
    ),
    payload = list(
      table1                     = tbl,
      total_pop_per_year         = total_pop,
      violent_revocation_rate_pct = overall_violent_revoke_pct
    )
  )
}


# -- Table 2 analysis -------------------------------------------------

#' Analyse Doob Affidavit Table 2 — prisoner flow
#'
#' Renders Table 2 plus year-over-year changes and 5-year averages.
#'
#' @return A `morie_result` named-list.
#' @export
analyze_doob_table2_flow <- function() {
  tbl <- CCRSO_TABLE2_FLOW
  rows <- lapply(seq_len(nrow(tbl)), function(i) {
    r <- tbl[i, ]
    c(r$year, r$avg_count, r$admissions,
      if (is.na(r$deaths)) "n/a" else r$deaths,
      r$full_parole_releases, r$statutory_releases)
  })
  n_years          <- nrow(tbl)
  avg_count        <- sum(tbl$avg_count) / n_years
  avg_admissions   <- sum(tbl$admissions) / n_years
  avg_full_parole  <- sum(tbl$full_parole_releases) / n_years
  avg_stat_release <- sum(tbl$statutory_releases) / n_years

  rows[[length(rows) + 1]] <- c(
    "Average", round(avg_count, 1), round(avg_admissions, 1),
    45,  # Doob's stated 5-yr average for deaths
    round(avg_full_parole, 1), round(avg_stat_release, 1)
  )
  monthly_releases <- avg_stat_release / 12

  .doob_result(
    title = paste0("Doob Affidavit Table 2 -- Flow of prisoners into and out ",
                   "of penitentiaries (CCRSO 2013/14-2017/18)"),
    summary_lines = list(
      c("Source", "CCRSO 2018, Doob Affidavit Exhibit B"),
      c("Average annual count (5-yr)", round(avg_count, 1)),
      c("Average annual admissions (5-yr)", round(avg_admissions, 1)),
      c("Average statutory releases / year (5-yr)", round(avg_stat_release, 1)),
      c("Average statutory releases / month", round(monthly_releases, 1)),
      c("Doob's headline point",
        paste0("About 427 statutory releases per month -- releasing ",
               "prisoners 6 months early would empty ~17.5% of the ",
               "penitentiary system in one tranche."))
    ),
    tables = list(list(
      title   = "Table 2: Annual prisoner flow + 5-year average:",
      headers = c("Year", "Avg count", "Admissions", "Deaths",
                  "Full parole rel.", "Statutory rel."),
      rows    = rows
    )),
    interpretation = paste0(
      "Reproduces Doob's Federal Court Table 2. The flow shows remarkable ",
      "stability: counts within 14k-15k, admissions and statutory ",
      "releases each ~5k/year. This stability is what enables Doob's ",
      "projection that releasing prisoners 6 months early would steady-",
      "state at ~427 fewer prisoners per month, depopulating ~17.5% of ",
      "the system."
    ),
    payload = list(
      table2           = tbl,
      avg_count        = avg_count,
      avg_admissions   = avg_admissions,
      monthly_releases = monthly_releases
    )
  )
}


# -- Table 3 analysis -------------------------------------------------

#' Analyse Doob Affidavit Table 3 — age over-/under-representation
#'
#' Renders Table 3 plus age-group IRRs for CSC custody and admissions
#' vs Canadian adult population.
#'
#' @return A `morie_result` named-list.
#' @export
analyze_doob_table3_age_overrepresentation <- function() {
  tbl <- CCRSO_TABLE3_AGE
  rows <- vector("list", nrow(tbl))
  irrs <- vector("list", nrow(tbl))
  for (i in seq_len(nrow(tbl))) {
    r <- tbl[i, ]
    irr_custody    <- r$csc_in_custody_pct  / r$canada_adult_pop_pct
    irr_admissions <- r$csc_admissions_pct  / r$canada_adult_pop_pct
    rows[[i]] <- c(
      r$age_group,
      format(r$canada_adult_pop, big.mark = ","),
      sprintf("%.1f%%", r$canada_adult_pop_pct),
      format(r$csc_in_custody, big.mark = ","),
      sprintf("%.1f%%", r$csc_in_custody_pct),
      format(r$csc_admissions_2017_18, big.mark = ","),
      sprintf("%.1f%%", r$csc_admissions_pct),
      sprintf("%.2f", irr_custody),
      sprintf("%.2f", irr_admissions)
    )
    irrs[[i]] <- list(age_group = r$age_group,
                      irr_custody = irr_custody,
                      irr_admissions = irr_admissions)
  }
  .doob_result(
    title = paste0("Doob Affidavit Table 3 -- Prisoner age distribution: ",
                   "Canada adult population vs CSC in-custody / admissions"),
    summary_lines = list(
      c("Source", "CCRSO 2018 + StatsCan CANSIM, Doob Affidavit Exhibits A,C,D"),
      c("Age 50+ as % of Canada adult population", "47.3%"),
      c("Age 50+ as % of CSC in-custody", "25.2%"),
      c("Age 50+ as % of CSC admissions (2017-18)", "16.9%"),
      c("Doob's headline point",
        paste0("Older adults are dramatically under-represented in CSC ",
               "custody (25.2% vs 47.3%) and even more so in admissions ",
               "(16.9%) -- the prison population skews young."))
    ),
    tables = list(list(
      title   = paste0("Table 3: Age distribution + over-/under-",
                       "representation IRR (CSC%/Canada%):"),
      headers = c("Age", "Adult pop", "Adult %", "In custody", "Custody %",
                  "Admissions", "Admissions %", "IRR custody",
                  "IRR admissions"),
      rows    = rows
    )),
    interpretation = paste0(
      "Reproduces Doob's Federal Court Table 3 plus over-/under-",
      "representation IRRs. Age 18-49 has IRR_custody ~ 1.42 ",
      "(over-represented) and IRR_admissions ~ 1.57; ages 50-59 ",
      "near-balanced (IRR ~ 0.89/0.65); ages 60+ severely under-",
      "represented (IRR ~ 0.32/0.18). This supports Doob's argument ",
      "that older prisoners are a low-risk group appropriate for ",
      "conditional release."
    ),
    payload = list(table3 = tbl, irrs = irrs)
  )
}


# -- Pettitt change-point detection ----------------------------------

#' Pettitt non-parametric change-point detection
#'
#' Pettitt's (1979) test for a single change-point in a time series.
#' Returns the change-point index, the test statistic, and an
#' approximate p-value.
#'
#' @param series Numeric vector / time series.
#' @return Named list with `change_point_index`, `U_max`, `p_value`,
#'   `note`.
#' @references Pettitt (1979). A non-parametric approach to the
#'   change-point problem. *J. R. Stat. Soc. C*, 28(2), 126--135.
#' @export
pettitt_changepoint <- function(series) {
  arr <- as.numeric(series)
  arr <- arr[is.finite(arr)]
  n <- length(arr)
  if (n < 5) {
    return(list(change_point_index = NA_integer_, U_max = NA_real_,
                 p_value = NA_real_,
                 note = "n < 5; Pettitt test not applicable"))
  }
  U <- numeric(n)
  # Mann-Whitney U-like statistic accumulated up to each split-point.
  # At t=n-1, the b slice is arr[(n+1):n] which is reversed/oob in R
  # (Python's slice arr[t+1:] returns empty). Guard against this.
  for (t in seq_len(n - 1)) {
    a <- arr[seq_len(t + 1)]
    if (t + 2 > n) {
      U[t + 1] <- 0
      next
    }
    b <- arr[(t + 2):n]
    U[t + 1] <- sum(sign(outer(a, b, "-")))
  }
  abs_U <- abs(U)
  # Python's argmax returns the FIRST max; R's which.max does the same.
  k_max <- which.max(abs_U) - 1L  # 0-indexed to match Python output
  U_max <- abs_U[k_max + 1L]
  # Pettitt 1979 eq. 11 approximate p-value.
  p_approx <- 2 * exp(-6 * U_max^2 / (n^3 + n^2))
  list(change_point_index = as.integer(k_max),
       U_max              = as.numeric(U_max),
       p_value            = min(p_approx, 1),
       note               = paste0("Pettitt 1979 approximate p-value; ",
                                   "single-change-point assumption"))
}


# -- Decoupling test --------------------------------------------------

#' Doob decoupling test
#'
#' Tests Doob's central thesis that imprisonment is decoupled from
#' crime by computing the Pearson correlation between the two time
#' series and (optionally) running Pettitt change-point on each.
#'
#' @param crime_series Numeric vector of per-period crime rates.
#' @param imprisonment_series Numeric vector of per-period
#'   imprisonment rates, same length as `crime_series`.
#' @param years Optional integer vector of period labels.
#' @return A `morie_result` named-list.
#' @export
decoupling_test <- function(crime_series, imprisonment_series,
                              years = NULL) {
  crime <- as.numeric(crime_series)
  imp   <- as.numeric(imprisonment_series)
  if (length(crime) != length(imp)) {
    return(.doob_result(
      title = "Doob decoupling test",
      summary_lines = list(),
      warnings = sprintf("length mismatch: crime=%d, imp=%d",
                          length(crime), length(imp))
    ))
  }
  n <- length(crime)
  if (n < 5) {
    return(.doob_result(
      title = "Doob decoupling test",
      summary_lines = list(),
      warnings = sprintf("only %d years; need >= 5", n)
    ))
  }
  r_pearson <- as.numeric(cor(crime, imp))
  if (abs(r_pearson) >= 1) {
    p <- 0
  } else {
    z <- 0.5 * log((1 + r_pearson) / (1 - r_pearson))
    p <- 2 * (1 - pnorm(abs(z) * sqrt(n - 3)))
  }
  pcp_crime <- pettitt_changepoint(crime)
  pcp_imp   <- pettitt_changepoint(imp)
  yrs_str <- if (!is.null(years)) {
    sprintf("%d-%d", min(years), max(years))
  } else {
    sprintf("n=%d years", n)
  }
  .doob_result(
    title = paste0("Doob decoupling test -- Pearson(crime rate, ",
                   "imprisonment rate) over time"),
    summary_lines = list(
      c("Years", yrs_str),
      c("n", n),
      c("Pearson r", round(r_pearson, 4)),
      c("Two-sided p (Fisher z)", round(p, 6)),
      c("Pettitt change-point: crime",
        ifelse(is.na(pcp_crime$change_point_index), "n/a",
                as.character(pcp_crime$change_point_index))),
      c("Pettitt change-point: imprisonment",
        ifelse(is.na(pcp_imp$change_point_index), "n/a",
                as.character(pcp_imp$change_point_index))),
      c("Doob's thesis",
        paste0("If r is small / sign-mismatched / non-significant, ",
               "imprisonment is decoupled from crime -- supports the ",
               "affidavit's central claim."))
    ),
    interpretation = paste0(
      "Tests Doob's central thesis from Federal Court Affidavit ",
      "T-539-20 paras 16-21: imprisonment rates and crime rates are ",
      "not consistently correlated across time. r ~ 0 with non-",
      "significant p ==> decoupling confirmed. r non-zero with small ",
      "magnitude ==> weak coupling. Pettitt change-points highlight ",
      "whether either series has structural breaks."
    ),
    payload = list(
      r_pearson      = r_pearson,
      p_value        = p,
      n              = n,
      pettitt_crime  = pcp_crime,
      pettitt_imp    = pcp_imp
    )
  )
}


# -- Full-affidavit roll-up -------------------------------------------

#' Replicate Doob's full Federal Court affidavit (Tables 1-3)
#'
#' Renders Tables 1, 2 and 3 in a single roll-up `morie_result`.
#' Figures 1-4 (time-series CANSIM data) are out-of-scope here; pass
#' your own series to `decoupling_test()` once they are available.
#'
#' @return A `morie_result` named-list.
#' @export
analyze_doob_full_affidavit <- function() {
  t1 <- analyze_doob_table1_releases()
  t2 <- analyze_doob_table2_flow()
  t3 <- analyze_doob_table3_age_overrepresentation()
  sections <- list()
  labels <- c("Sect Table 1", "Sect Table 2", "Sect Table 3")
  results <- list(t1, t2, t3)
  for (i in seq_along(results)) {
    r <- results[[i]]
    if (length(r$tables) > 0) {
      sections[[length(sections) + 1]] <- list(
        title   = sprintf("%s -- %s", labels[i], r$title),
        headers = r$tables[[1]]$headers,
        rows    = r$tables[[1]]$rows
      )
    }
  }
  .doob_result(
    title = paste0("Doob Federal Court Affidavit (T-539-20) -- ",
                   "aggregate national-level replication"),
    summary_lines = list(
      c("Source", "Doob Affidavit T-539-20 Vol 3, pp. 778-795"),
      c("Tables replicated", "1, 2, 3"),
      c("Figures 1-4 (time series)",
        paste0("require StatsCan CANSIM data; use ",
               "decoupling_test(crime, imp) once series available")),
      c("Companion analyses in morie",
        paste0("MRM modules on OTIS provincial data; MRM chi^2 on ",
               "c/d-series; SIU IAP federal context"))
    ),
    tables = sections,
    interpretation = paste0(
      "Replicates the three CCRSO tables Doob used to argue (a) ",
      "violent revocation is rare (< 1% of releases), (b) prisoner ",
      "flow is steady-state (~427 statutory releases/month), and (c) ",
      "older adults are under-represented in CSC custody. These ",
      "national-aggregate analyses sit alongside the MRM modules on ",
      "OTIS from federal aggregates down to provincial individual-",
      "level evidence."
    ),
    payload = list(
      table1 = CCRSO_TABLE1_RELEASES,
      table2 = CCRSO_TABLE2_FLOW,
      table3 = CCRSO_TABLE3_AGE
    )
  )
}
