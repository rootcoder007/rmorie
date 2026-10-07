# SPDX-License-Identifier: AGPL-3.0-or-later
#' Comprehensive per-dataset analyses for ALL 28 OTIS public-release files
#'
#' R port of \code{morie.otis_all_analyze}. Pairs with the OTIS loaders
#' (see \code{?morie_otis} and the b01/c-series/d-series CSV files
#' under \code{data/datasets/OTIS/}) and chains the existing MRM-OTIS
#' callables in \code{mrm_otis.R} the same way
#' \code{morie_arsau_analyze_*} (in \code{mrm_arsau.R}) chains the
#' generic MRM-UoF callables.
#'
#' For every dataset id (\code{b01}..\code{d07}) this module exposes
#' \code{morie_otis_analyze_<id>(data)}. Each analyzer returns a named
#' \code{list} with class \eqn{c("morie_otis_analysis_result",
#' "morie_rich_result", "list")} containing
#' \code{title} / \code{summary_lines} / \code{tables} /
#' \code{interpretation} / \code{warnings} / \code{payload}, mirroring
#' the Python \code{RichResult} shape used in
#' \code{src/morie/otis_all_analyze.py}.
#'
#' Cross-year invariant: \code{UniqueIndividual_ID} is reassigned
#' every fiscal year (see \code{variable_taxonomy.R}). Every analyzer
#' that touches that column is within-year only.
#'
#' @name morie_otis_all_analyze
NULL


# ---------------------------------------------------------------------------
# Internal helpers (mirror _summary_lines, _crosstab, _year_trend, _to_int)
# ---------------------------------------------------------------------------

#' Internal helper: Otis Year Col
#' @noRd
#' @keywords internal
.otis_emit <- function(res, out_dir, id) {
  # Mirrors the Python arm: a text rendering and the JSON payload, one
  # pair per analysis. Writing is best-effort -- a failure to write must
  # not destroy a result that took a DML fit to produce -- but it is
  # reported, because a silent write failure is how a caller ends up
  # believing an empty directory is an empty analysis.
  if (is.null(out_dir)) return(res)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  tryCatch(
    writeLines(paste(utils::capture.output(print(res)), collapse = "\n"),
               file.path(out_dir, sprintf("otis_%s.txt", id))),
    error = function(e)
      warning("could not write ", id, ".txt: ", conditionMessage(e),
              call. = FALSE))
  tryCatch(
    writeLines(.morie_to_json(res$payload, auto_unbox = TRUE),
               file.path(out_dir, sprintf("otis_%s.json", id))),
    error = function(e)
      warning("could not write ", id, ".json: ", conditionMessage(e),
              call. = FALSE))
  res
}

#' .otis_year_col
#'
#' Internal helper in otis_all_analyze.R; see the file header for
#' the source the module follows.
#'
#' @param df Passed to \code{names}.
#' @return Nothing; called for its effect.
#' @export
#' @keywords internal
.otis_year_col <- function(df) {
  for (c in c("EndFiscalYear", "Year")) {
    if (c %in% names(df)) return(c)
  }
  NULL
}

#' Internal helper: Otis To Int
#' @noRd
.otis_to_int <- function(x) {
  v <- suppressWarnings(as.integer(x))
  if (length(v) == 0L || is.na(v)) 0L else v
}

#' Internal helper: Otis Is Truthy
#' @noRd
.otis_is_truthy <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) return(as.integer(x == 1))
  as.integer(tolower(trimws(as.character(x))) %in% c("yes", "true", "1"))
}

#' Internal helper: Otis Summary Lines
#' @noRd
.otis_summary_lines <- function(df, ds_id, description = NULL,
                                  series = NULL, primary_metric = NULL) {
  yc <- .otis_year_col(df)
  out <- list()
  out[["Dataset"]] <- if (!is.null(description)) {
    sprintf("%s -- %s", ds_id, description)
  } else ds_id
  if (!is.null(series)) out[["Series"]] <- series
  out[["Rows"]] <- nrow(df)
  out[["Columns"]] <- ncol(df)
  if (!is.null(yc) && nrow(df) > 0L) {
    yrs <- suppressWarnings(as.numeric(df[[yc]]))
    yrs <- yrs[is.finite(yrs)]
    if (length(yrs) > 0L) {
      out[["Years covered"]] <- sprintf("%d-%d", as.integer(min(yrs)),
                                          as.integer(max(yrs)))
    }
  }
  if (!is.null(primary_metric) && primary_metric %in% names(df)) {
    v <- suppressWarnings(as.numeric(df[[primary_metric]]))
    out[[sprintf("Total %s", primary_metric)]] <-
      as.integer(sum(v, na.rm = TRUE))
  }
  out
}

#' Internal helper: Otis Crosstab
#' @noRd
.otis_crosstab <- function(df, row, col, value,
                            aggfunc = c("sum", "max"),
                            top_rows = 20L) {
  aggfunc <- match.arg(aggfunc)
  if (!all(c(row, col, value) %in% names(df))) return(NULL)
  v <- suppressWarnings(as.numeric(df[[value]]))
  ok <- !is.na(v)
  if (!any(ok)) return(NULL)
  agg <- if (aggfunc == "max") max else sum
  # Python pivot_table(aggfunc=, fill_value=0): empty cells are 0, and
  # aggfunc applies per cell (xtabs could only sum).
  m <- tapply(v[ok], list(df[[row]][ok], df[[col]][ok]), agg)
  m[is.na(m)] <- 0
  totals <- rowSums(m)
  ord <- order(-totals)[seq_len(min(top_rows, nrow(m)))]
  m <- m[ord, , drop = FALSE]
  totals <- totals[ord]
  list(
    title = sprintf("%s by %s x %s:", value, row, col),
    headers = c(row, colnames(m), "TOTAL"),
    rows = lapply(seq_len(nrow(m)), function(i) {
      c(rownames(m)[i], as.integer(m[i, ]), as.integer(totals[i]))
    })
  )
}

#' Internal helper: Otis Year Trend
#' @noRd
.otis_year_trend <- function(df, value, year_col = NULL) {
  yc <- year_col %||% .otis_year_col(df)
  if (is.null(yc) || !(value %in% names(df))) return(NULL)
  v <- suppressWarnings(as.numeric(df[[value]]))
  g <- stats::aggregate(v, by = list(year = df[[yc]]),
                          FUN = sum, na.rm = TRUE)
  g <- g[order(g$year), , drop = FALSE]
  list(
    title = sprintf("%s by %s:", value, yc),
    headers = c(yc, value),
    rows = lapply(seq_len(nrow(g)), function(i) {
      c(as.integer(g$year[i]), as.integer(g$x[i]))
    })
  )
}

# Local `%||%` (R < 4.4)
`%||%` <- function(a, b) if (is.null(a)) b else a


#' Internal helper: Otis Wrap
#' @noRd
.otis_wrap <- function(title, summary_lines, tables = list(),
                        interpretation = NULL, warnings = character(0),
                        payload = NULL) {
  tables <- Filter(Negate(is.null), tables)
  out <- list(
    title = title,
    summary_lines = summary_lines,
    tables = tables,
    warnings = warnings,
    interpretation = interpretation %||% "",
    payload = payload
  )
  class(out) <- c("morie_otis_analysis_result", "morie_rich_result",
                  "list")
  out
}


# Indicator helpers (parallel to Python _female_indicator etc.)
#' Internal helper: Otis Female Indicator
#' @noRd
.otis_female_indicator <- function(x) {
  as.integer(tolower(as.character(x)) == "female")
}

#' Internal helper: Otis Toronto Indicator
#' @noRd
.otis_toronto_indicator <- function(x) {
  as.integer(tolower(as.character(x)) == "toronto")
}

#' Internal helper: Otis Age 50plus Indicator
#' @noRd
.otis_age_50plus_indicator <- function(x) {
  as.integer(grepl("50", as.character(x)))
}

#' Internal helper: Otis Indigenous Indicator
#' @noRd
.otis_indigenous_indicator <- function(x) {
  as.integer(tolower(as.character(x)) == "indigenous")
}

#' Internal helper: Otis Minority Religion Indicator
#' @noRd
.otis_minority_religion_indicator <- function(x) {
  excluded <- c("christian", "no religion", "unknown or not reported")
  as.integer(!(tolower(trimws(as.character(x))) %in% excluded))
}


# ---------------------------------------------------------------------------
# b-series analyses (placements / durations / alerts)
# ---------------------------------------------------------------------------

#' Person-level segregation-placement analysis (b01)
#'
#' @param data b01 data.frame (76,934 rows in the public release).
#' @return A \code{morie_otis_analysis_result} list with reason / alert /
#'   year-trend tables. Within-year only -- \code{UniqueIndividual_ID}
#'   is not cross-year-safe.
#' @examples
#' otis_b01 <- morie_synth_otis("b01", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b01(otis_b01)
#' class(res)
#' @export
morie_otis_analyze_b01 <- function(data) {
  stopifnot(is.data.frame(data))
  s <- .otis_summary_lines(data, "b01",
    description = "Segregation placements (person-level detail)")
  if ("UniqueIndividual_ID" %in% names(data)) {
    s[["Unique individuals"]] <-
      length(unique(data[["UniqueIndividual_ID"]]))
  }
  dur <- "NumberConsecutiveDays_Segregation"
  if (dur %in% names(data)) {
    d <- suppressWarnings(as.numeric(data[[dur]]))
    s[["Mean consecutive days"]] <- round(mean(d, na.rm = TRUE), 2)
    s[["Median consecutive days"]] <- stats::median(d, na.rm = TRUE)
    s[["Max consecutive days"]] <- as.integer(max(d, na.rm = TRUE))
  }

  reason_cols <- grep("^SegReason_", names(data), value = TRUE)
  reason_rows <- lapply(reason_cols, function(r) {
    n_yes <- sum(.otis_is_truthy(data[[r]]))
    if (n_yes == 0L) return(NULL)
    list(reason = sub("^SegReason_", "", r),
         count = n_yes,
         pct = sprintf("%.1f%%", 100 * n_yes / nrow(data)))
  })
  reason_rows <- Filter(Negate(is.null), reason_rows)
  reason_rows <- reason_rows[order(-vapply(reason_rows,
    function(r) r$count, integer(1)))]

  alert_rows <- lapply(c("MentalHealth_Alert", "SuicideRisk_Alert",
                          "SuicideWatch_Alert"), function(a) {
    if (!(a %in% names(data))) return(NULL)
    n_yes <- sum(.otis_is_truthy(data[[a]]))
    list(alert = a, count = n_yes,
         pct = sprintf("%.1f%%", 100 * n_yes / nrow(data)))
  })
  alert_rows <- Filter(Negate(is.null), alert_rows)

  tables <- list(
    list(title = "Reasons for placement (count, % of rows):",
         headers = c("Reason", "Count", "Percent"),
         rows = lapply(reason_rows, function(r)
           c(r$reason, r$count, r$pct))),
    list(title = "Alert flags on placements:",
         headers = c("Alert", "Count", "Percent"),
         rows = lapply(alert_rows, function(r)
           c(r$alert, r$count, r$pct))),
    .otis_year_trend(data, "Number_Of_Placements")
  )

  .otis_wrap(
    title = "b01 -- Segregation placements (person-level detail)",
    summary_lines = s,
    tables = tables,
    payload = list(
      n_rows = nrow(data),
      n_individuals = if ("UniqueIndividual_ID" %in% names(data))
        length(unique(data[["UniqueIndividual_ID"]])) else NA_integer_
    )
  )
}

#' Aggregate segregation days per person per year (b02)
#' @param data b02 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b02 <- morie_synth_otis("b02", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b02(otis_b02)
#' class(res)
#' @export
morie_otis_analyze_b02 <- function(data) {
  s <- .otis_summary_lines(data, "b02",
    description = "Segregation total days per person per fiscal year")
  daycol <- "TotalAggregatedDays_Segregation"
  if (daycol %in% names(data)) {
    d <- suppressWarnings(as.numeric(data[[daycol]]))
    s[["Mean total days"]] <- round(mean(d, na.rm = TRUE), 2)
    s[["Median total days"]] <- stats::median(d, na.rm = TRUE)
    s[["Max total days"]] <- as.integer(max(d, na.rm = TRUE))
  }
  .otis_wrap(
    "b02 -- Segregation total days per person per fiscal year",
    s,
    list(
      .otis_year_trend(data, daycol),
      .otis_crosstab(data, "Gender", "Region_MostRecentPlacement",
                     daycol)
    )
  )
}

#' Segregation placements by alert x institution (b03)
#' @param data b03 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b03 <- morie_synth_otis("b03", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b03(otis_b03)
#' class(res)
#' @export
morie_otis_analyze_b03 <- function(data) {
  s <- .otis_summary_lines(data, "b03",
    description = "Placements by alert/hold flag x institution")
  .otis_wrap(
    "b03 -- Segregation placements by alert/hold flag x institution",
    s,
    list(
      .otis_crosstab(data, "Alert_Type", "Alert_Presence",
                     "Number_SegregationPlacements"),
      .otis_crosstab(data, "Region_AtTimeOfPlacement", "Alert_Type",
                     "Number_SegregationPlacements")
    )
  )
}

#' Placement durations by region & gender (b04)
#' @param data b04 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b04 <- morie_synth_otis("b04", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b04(otis_b04)
#' class(res)
#' @export
morie_otis_analyze_b04 <- function(data) {
  s <- .otis_summary_lines(data, "b04",
    description = "Placement durations (max/median/mode) by region & gender")
  .otis_wrap(
    "b04 -- Placement durations by region & gender",
    s,
    list(.otis_crosstab(data, "Region_AtTimeOfPlacement", "Measure",
                        "NumberConsecutiveDays_Segregation",
                        aggfunc = "max"))
  )
}

#' Distribution of placements by binned duration (b05)
#' @param data b05 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b05 <- morie_synth_otis("b05", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b05(otis_b05)
#' class(res)
#' @export
morie_otis_analyze_b05 <- function(data) {
  s <- .otis_summary_lines(data, "b05",
    description = "Distribution by binned duration")
  .otis_wrap(
    "b05 -- Distribution of placements by binned duration",
    s,
    list(.otis_crosstab(data, "Consecutive_Duration", "EndFiscalYear",
                        "Number_SegregationPlacements"))
  )
}

#' Reasons for placement x institution x gender (b06)
#' @param data b06 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b06 <- morie_synth_otis("b06", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b06(otis_b06)
#' class(res)
#' @export
morie_otis_analyze_b06 <- function(data) {
  s <- .otis_summary_lines(data, "b06",
    description = "Reasons for placement by institution & gender")
  .otis_wrap(
    "b06 -- Reasons for placement by institution & gender",
    s,
    list(
      .otis_crosstab(data, "Reason", "EndFiscalYear",
                     "Number_SegregationPlacements"),
      .otis_crosstab(data, "Reason", "Gender",
                     "Number_SegregationPlacements")
    )
  )
}

#' Alerts x gender (b07)
#' @param data b07 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b07 <- morie_synth_otis("b07", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b07(otis_b07)
#' class(res)
#' @export
morie_otis_analyze_b07 <- function(data) {
  s <- .otis_summary_lines(data, "b07",
    description = "Placements with/without alert x gender")
  with_col <- "Number_Segregation_Placements_With_Alert"
  wo_col   <- "Number_Segregation_Placements_Without_Alert"
  rows <- list()
  if (all(c(with_col, wo_col, "EndFiscalYear", "Alert_Type",
            "Gender") %in% names(data))) {
    rows <- lapply(seq_len(nrow(data)), function(i) {
      w  <- .otis_to_int(data[[with_col]][i])
      wo <- .otis_to_int(data[[wo_col]][i])
      tot <- w + wo
      rate <- if (tot > 0L) 100 * w / tot else 0
      c(.otis_to_int(data[["EndFiscalYear"]][i]),
        as.character(data[["Alert_Type"]][i]),
        as.character(data[["Gender"]][i]),
        w, wo, sprintf("%.1f%%", rate))
    })
  }
  .otis_wrap(
    "b07 -- Segregation placements with/without alert x gender",
    s,
    list(list(title = "By alert x gender x year:",
              headers = c("Year", "Alert_Type", "Gender",
                          "With_Alert", "Without_Alert",
                          "% with alert"),
              rows = rows))
  )
}

#' Durations by institution & gender (b08)
#' @param data b08 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b08 <- morie_synth_otis("b08", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b08(otis_b08)
#' class(res)
#' @export
morie_otis_analyze_b08 <- function(data) {
  s <- .otis_summary_lines(data, "b08",
    description = "Placement durations by institution & gender")
  .otis_wrap(
    "b08 -- Placement durations by institution & gender",
    s,
    list(.otis_crosstab(data, "Institution_AtTimeOfPlacement", "Measure",
                        "NumberConsecutiveDays_Segregation",
                        aggfunc = "max", top_rows = 15L))
  )
}

#' Individuals by number of placements x gender (b09)
#' @param data b09 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_b09 <- morie_synth_otis("b09", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_b09(otis_b09)
#' class(res)
#' @export
morie_otis_analyze_b09 <- function(data) {
  s <- .otis_summary_lines(data, "b09",
    description = "Individuals by number of placements x gender")
  .otis_wrap(
    "b09 -- Individuals by number of placements x gender",
    s,
    list(.otis_crosstab(data, "NumberPlacements_Segregation", "Gender",
                        "NumberIndividuals_Segregation"))
  )
}


# ---------------------------------------------------------------------------
# c-series analyses (custody / RC / seg counts + demographics)
# ---------------------------------------------------------------------------

#' Total individuals x custody/RC/seg x gender (c01)
#' @param data c01 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c01 <- morie_synth_otis("c01", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c01(otis_c01)
#' class(res)
#' @export
morie_otis_analyze_c01 <- function(data) {
  s <- .otis_summary_lines(data, "c01",
    description = "Total individuals x custody/RC/seg x gender")
  rate_rows <- lapply(seq_len(nrow(data)), function(i) {
    cust <- .otis_to_int(data[["NumberIndividuals_InCustody"]][i])
    rc   <- .otis_to_int(data[["NumberIndividuals_RestrictiveConfinement"]][i])
    seg  <- .otis_to_int(data[["NumberIndividuals_Segregation"]][i])
    c(.otis_to_int(data[["EndFiscalYear"]][i]),
      as.character(data[["Gender"]][i]),
      cust, rc, seg,
      sprintf("%.1f%%", if (cust > 0) 100 * rc / cust else 0),
      sprintf("%.1f%%", if (cust > 0) 100 * seg / cust else 0))
  })
  .otis_wrap(
    "c01 -- Total individuals x custody/RC/seg x gender",
    s,
    list(list(title = "Cohort sizes + ratios:",
              headers = c("Year", "Gender", "Custody", "RC", "Seg",
                          "RC/custody", "Seg/custody"),
              rows = rate_rows))
  )
}

#' Individuals in RC/seg by institution (c02)
#' @param data c02 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c02 <- morie_synth_otis("c02", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c02(otis_c02)
#' class(res)
#' @export
morie_otis_analyze_c02 <- function(data) {
  s <- .otis_summary_lines(data, "c02",
    description = "Individuals in RC/seg by institution x region x gender")
  .otis_wrap(
    "c02 -- Individuals in RC/seg by institution x region x gender",
    s,
    list(.otis_crosstab(data, "Institution_MostRecentPlacement",
                        "EndFiscalYear",
                        "NumberIndividuals_RestrictiveConfinement",
                        top_rows = 15L))
  )
}

#' Individuals x race x gender (c03)
#' @param data c03 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c03 <- morie_synth_otis("c03", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c03(otis_c03)
#' class(res)
#' @export
morie_otis_analyze_c03 <- function(data) {
  s <- .otis_summary_lines(data, "c03",
    description = "Individuals x race x gender")
  cols <- c("NumberIndividuals_InCustody",
            "NumberIndividuals_RestrictiveConfinement",
            "NumberIndividuals_Segregation")
  rows <- list()
  if (all(c("Race", cols) %in% names(data))) {
    agg <- stats::aggregate(
      data[, cols, drop = FALSE],
      by = list(Race = data[["Race"]]),
      FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
    )
    agg <- agg[order(-agg[[cols[1]]]), , drop = FALSE]
    rows <- lapply(seq_len(nrow(agg)), function(i) {
      cust <- .otis_to_int(agg[[cols[1]]][i])
      rc   <- .otis_to_int(agg[[cols[2]]][i])
      seg  <- .otis_to_int(agg[[cols[3]]][i])
      c(as.character(agg$Race[i]), cust, rc, seg,
        sprintf("%.1f%%", if (cust > 0) 100 * rc / cust else 0),
        sprintf("%.1f%%", if (cust > 0) 100 * seg / cust else 0))
    })
  }
  .otis_wrap(
    "c03 -- Individuals x race x gender",
    s,
    list(list(title = "By race (totals across years/genders):",
              headers = c("Race", "Custody", "RC", "Seg",
                          "RC/custody", "Seg/custody"),
              rows = rows)),
    interpretation = paste(
      "Race x confinement disparities are visible in the RC/custody",
      "and Seg/custody ratios. Compare across race categories --",
      "the gap between Indigenous and White rates is a documented",
      "Ontario-corrections finding."
    )
  )
}

#' Internal helper: Otis C Simple
#' @noRd
.otis_c_simple <- function(ds_id, description, row_col,
                            col_col = "Region_MostRecentPlacement",
                            value_col = "NumberIndividuals_RestrictiveConfinement") {
  function(data) {
    s <- .otis_summary_lines(data, ds_id, description = description)
    .otis_wrap(
      sprintf("%s -- %s", ds_id, description),
      s,
      list(.otis_crosstab(data, row_col, col_col, value_col))
    )
  }
}

#' Individuals in RC/seg by race x region (c04)
#' @param data c04 data.frame from OTIS.
#' @return RichResult with summary + race-by-region crosstab.
#' @examples
#' res <- morie_otis_analyze_c04(morie_synth_otis("c04", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_c04 <- .otis_c_simple(
  "c04", "Individuals in RC/seg x race x region", "Race")
#' Individuals in RC/seg by religion x region (c05)
#' @param data c05 data.frame from OTIS.
#' @return RichResult with summary + religion-by-region crosstab.
#' @examples
#' res <- morie_otis_analyze_c05(morie_synth_otis("c05", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_c05 <- .otis_c_simple(
  "c05", "Individuals in RC/seg x religion x region", "Religion")
#' Individuals in RC/seg by age category x region (c06)
#' @param data c06 data.frame from OTIS.
#' @return RichResult with summary + age-by-region crosstab.
#' @examples
#' res <- morie_otis_analyze_c06(morie_synth_otis("c06", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_c06 <- .otis_c_simple(
  "c06", "Individuals in RC/seg x age category x region", "Age_Category")

#' Individuals x alerts x gender (c07)
#' @param data c07 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c07 <- morie_synth_otis("c07", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c07(otis_c07)
#' class(res)
#' @export
morie_otis_analyze_c07 <- function(data) {
  s <- .otis_summary_lines(data, "c07",
    description = "Individuals in custody/RC/seg x alert type x gender")
  .otis_wrap(
    "c07 -- Individuals in custody/RC/seg x alert type x gender",
    s,
    list(
      .otis_crosstab(data, "Alert_Type", "Gender",
                     "NumberIndividuals_RestrictiveConfinement"),
      .otis_crosstab(data, "Alert_Type", "EndFiscalYear",
                     "NumberIndividuals_Segregation")
    )
  )
}

#' Individuals by religion x gender (c08)
#' @param data c08 data.frame from OTIS.
#' @return RichResult with summary + religion-by-gender crosstab.
#' @examples
#' res <- morie_otis_analyze_c08(morie_synth_otis("c08", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_c08 <- .otis_c_simple(
  "c08", "Individuals x religion x gender", "Religion", "Gender")
#' Individuals by age category x gender (c09)
#' @param data c09 data.frame from OTIS.
#' @return RichResult with summary + age-by-gender crosstab.
#' @examples
#' res <- morie_otis_analyze_c09(morie_synth_otis("c09", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_c09 <- .otis_c_simple(
  "c09", "Individuals x age category x gender", "Age_Category", "Gender")

#' RC/seg aggregate durations by institution (c10)
#' @param data c10 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c10 <- morie_synth_otis("c10", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c10(otis_c10)
#' class(res)
#' @export
morie_otis_analyze_c10 <- function(data) {
  s <- .otis_summary_lines(data, "c10",
    description = "RC/seg aggregate durations by institution")
  .otis_wrap(
    "c10 -- RC/seg aggregate durations by institution",
    s,
    list(.otis_crosstab(data, "Institution_MostRecentPlacement", "Measure",
                        "TotalAggregatedDays_RestrictiveConfinement",
                        aggfunc = "max", top_rows = 15L))
  )
}

#' Individuals by aggregate-duration bin (c11)
#' @param data c11 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c11 <- morie_synth_otis("c11", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c11(otis_c11)
#' class(res)
#' @export
morie_otis_analyze_c11 <- function(data) {
  s <- .otis_summary_lines(data, "c11",
    description = "Individuals by binned aggregate duration")
  .otis_wrap(
    "c11 -- Individuals by binned aggregate duration",
    s,
    list(.otis_crosstab(data, "Aggregate_Duration", "EndFiscalYear",
                        "NumberIndividuals_RestrictiveConfinement"))
  )
}

#' RC/seg aggregate durations by region & gender (c12)
#' @param data c12 data.frame.
#' @return An object of class \code{"morie_otis_analysis_result"}.
#' @examples
#' otis_c12 <- morie_synth_otis("c12", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_c12(otis_c12)
#' class(res)
#' @export
morie_otis_analyze_c12 <- function(data) {
  s <- .otis_summary_lines(data, "c12",
    description = "RC/seg aggregate durations by region & gender")
  .otis_wrap(
    "c12 -- RC/seg aggregate durations by region & gender",
    s,
    list(.otis_crosstab(data, "Region_MostRecentPlacement", "Measure",
                        "TotalAggregatedDays_RestrictiveConfinement",
                        aggfunc = "max"))
  )
}


# ---------------------------------------------------------------------------
# d-series analyses (custodial deaths)
# ---------------------------------------------------------------------------

#' Person-level custodial deaths (d01)
#' @param data d01 data.frame.
#' @return A logical scalar.
#' @examples
#' otis_d01 <- morie_synth_otis("d01", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_d01(otis_d01)
#' class(res)
#' @export
morie_otis_analyze_d01 <- function(data) {
  s <- .otis_summary_lines(data, "d01",
    description = "Custodial deaths (person-level)")
  if ("UniqueIndividual_ID" %in% names(data)) {
    s[["Distinct individuals"]] <-
      length(unique(data[["UniqueIndividual_ID"]]))
  }
  cause_col <- if ("MedicalCauseofDeath" %in% names(data))
    "MedicalCauseofDeath" else "MedicalCauseOfDeath"
  means_col <- if ("MeansofDeath" %in% names(data))
    "MeansofDeath" else "MeansOfDeath"
  .tab <- function(col, title, hkey) {
    if (!(col %in% names(data))) return(NULL)
    tab <- sort(table(data[[col]]), decreasing = TRUE)
    list(title = title, headers = c(hkey, "Deaths"),
         rows = lapply(seq_along(tab),
           function(i) c(names(tab)[i], as.integer(tab[i]))))
  }
  .otis_wrap(
    "d01 -- Custodial deaths (person-level)", s,
    list(.tab("Region_AtTimeOfDeath", "By region:", "Region"),
         .tab("HousingUnit_Type", "By housing unit type:",
              "HousingUnit"),
         .tab(cause_col, "By medical cause:", "Cause"),
         .tab(means_col, "By means of death:", "Means"))
  )
}

#' Internal helper: Otis D Simple
#' @noRd
.otis_d_simple <- function(ds_id, description, by) {
  function(data) {
    s <- .otis_summary_lines(data, ds_id, description = description)
    .otis_wrap(
      sprintf("%s -- %s", ds_id, description),
      s,
      list(.otis_year_trend(data, "Number_CustodialDeaths"),
           .otis_crosstab(data, by, "Year", "Number_CustodialDeaths"))
    )
  }
}

#' Custodial deaths by gender (d02)
#' @param data d02 data.frame from OTIS.
#' @return RichResult with summary + deaths-by-gender crosstab.
#' @examples
#' res <- morie_otis_analyze_d02(morie_synth_otis("d02", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_d02 <- .otis_d_simple(
  "d02", "Custodial deaths x gender", "Gender")
#' Custodial deaths by race (d03)
#' @param data d03 data.frame from OTIS.
#' @return RichResult with summary + deaths-by-race crosstab.
#' @examples
#' res <- morie_otis_analyze_d03(morie_synth_otis("d03", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_d03 <- .otis_d_simple(
  "d03", "Custodial deaths x race", "Race")
#' Custodial deaths by religion (d04)
#' @param data d04 data.frame from OTIS.
#' @return RichResult with summary + deaths-by-religion crosstab.
#' @examples
#' res <- morie_otis_analyze_d04(morie_synth_otis("d04", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_d04 <- .otis_d_simple(
  "d04", "Custodial deaths x religion", "Religion")
#' Custodial deaths by age category (d05)
#' @param data d05 data.frame from OTIS.
#' @return RichResult with summary + deaths-by-age crosstab.
#' @examples
#' res <- morie_otis_analyze_d05(morie_synth_otis("d05", n = 120L, seed = 1L))
#' class(res)
#' @export
morie_otis_analyze_d05 <- .otis_d_simple(
  "d05", "Custodial deaths x age category", "Age_Category")

#' Custodial deaths by alert x medical cause (d06)
#' @param data d06 data.frame from OTIS.
#' @return RichResult with summary + medical-cause-by-alert crosstab.
#' @examples
#' otis_d06 <- morie_synth_otis("d06", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_d06(otis_d06)
#' class(res)
#' @export
morie_otis_analyze_d06 <- function(data) {
  s <- .otis_summary_lines(data, "d06",
    description = "Custodial deaths x alert x medical cause")
  .otis_wrap(
    "d06 -- Custodial deaths x alert x medical cause", s,
    list(.otis_crosstab(data, "MedicalCauseOfDeath", "Alert_Type",
                        "Number_CustodialDeaths"))
  )
}

#' Custodial deaths by alert x housing unit (d07)
#' @param data d07 data.frame from OTIS.
#' @return RichResult with summary + housing-unit-by-alert crosstab.
#' @examples
#' otis_d07 <- morie_synth_otis("d07", n = 120L, seed = 1L)
#' res <- morie_otis_analyze_d07(otis_d07)
#' class(res)
#' @export
morie_otis_analyze_d07 <- function(data) {
  s <- .otis_summary_lines(data, "d07",
    description = "Custodial deaths x alert x housing unit")
  .otis_wrap(
    "d07 -- Custodial deaths x alert x housing unit", s,
    list(.otis_crosstab(data, "HousingUnit_Type", "Alert_Type",
                        "Number_CustodialDeaths"))
  )
}


# ---------------------------------------------------------------------------
# Master driver
# ---------------------------------------------------------------------------

#' Registry of dataset-id -> analyzer
#'
#' Mirrors \code{_ANALYSES} in
#' \code{src/morie/otis_all_analyze.py}.
#' @return A named list with elements \code{b01}, \code{b02},
#'   \code{b03}, \code{b04}, \code{b05}, \code{b06}, \code{b07},
#'   \code{b08}, \code{b09}, \code{c01}, \code{c02}, \code{c03},
#'   \code{c04}, \code{c05}, \code{c06}, \code{c07}, \code{c08},
#'   \code{c09}, \code{c10}, \code{c11}, \code{c12}, \code{d01},
#'   \code{d02}, \code{d03}, \code{d04}, \code{d05}, \code{d06},
#'   \code{d07}.
#' @examples
#' res <- morie_otis_analyzers()
#' head(names(res))
#' @export
morie_otis_analyzers <- function() {
  list(
    b01 = morie_otis_analyze_b01, b02 = morie_otis_analyze_b02,
    b03 = morie_otis_analyze_b03, b04 = morie_otis_analyze_b04,
    b05 = morie_otis_analyze_b05, b06 = morie_otis_analyze_b06,
    b07 = morie_otis_analyze_b07, b08 = morie_otis_analyze_b08,
    b09 = morie_otis_analyze_b09,
    c01 = morie_otis_analyze_c01, c02 = morie_otis_analyze_c02,
    c03 = morie_otis_analyze_c03, c04 = morie_otis_analyze_c04,
    c05 = morie_otis_analyze_c05, c06 = morie_otis_analyze_c06,
    c07 = morie_otis_analyze_c07, c08 = morie_otis_analyze_c08,
    c09 = morie_otis_analyze_c09, c10 = morie_otis_analyze_c10,
    c11 = morie_otis_analyze_c11, c12 = morie_otis_analyze_c12,
    d01 = morie_otis_analyze_d01, d02 = morie_otis_analyze_d02,
    d03 = morie_otis_analyze_d03, d04 = morie_otis_analyze_d04,
    d05 = morie_otis_analyze_d05, d06 = morie_otis_analyze_d06,
    d07 = morie_otis_analyze_d07
  )
}

#' Run every OTIS analyzer against a named list of datasets
#'
#' Counterpart to Python \code{analyze_all()}.
#'
#' @param datasets Named list \code{list(b01 = <df>, c01 = <df>, ...)}.
#'   IDs absent from the list are skipped silently.
#' @param out_dir Optional directory to write per-dataset
#'   \code{overlay_<id>.rds} files. \code{NULL} means in-memory only.
#' @return Named list of \code{morie_otis_analysis_result}s.
#' @examples
#' ds <- morie_synth_otis_all(n = 80L, seed = 2L)
#' res <- try(morie_otis_analyze_all(ds))
#' if (!inherits(res, "try-error")) names(res)
#' @export
morie_otis_analyze_all <- function(datasets, out_dir = NULL) {
  stopifnot(is.list(datasets))
  out <- list()
  for (id in names(datasets)) {
    fn <- morie_otis_analyzers()[[id]]
    if (is.null(fn)) next
    out[[id]] <- tryCatch(
      fn(datasets[[id]]),
      error = function(e) .otis_wrap(
        title = sprintf("%s (failed)", id),
        summary_lines = list(error = conditionMessage(e)),
        warnings = conditionMessage(e)
      )
    )
    if (!is.null(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(out[[id]], file.path(out_dir,
                                    sprintf("otis_%s.rds", id)))
    }
  }
  out
}

#' Print method for \code{morie_otis_analysis_result} objects
#'
#' @param x A \code{morie_otis_analysis_result} object.
#' @param ... Ignored; accepted for S3 consistency.
#' @return \code{x}, invisibly.
#' @examples
#' otis_b01 <- morie_synth_otis("b01", n = 120L, seed = 1L)
#' print(morie_otis_analyze_b01(otis_b01))
#' @export
print.morie_otis_analysis_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    lw <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      cat(sprintf("  %-*s  %s\
", lw, nms[i],
                  format(x$summary_lines[[i]])))
    }
    cat("\
")
  }
  for (w in x$warnings) cat("Warning:", w, "\
")
  if (nzchar(x$interpretation)) cat(x$interpretation, "\
")
  invisible(x)
}


# ===========================================================================
# CAUSAL / Ruhela-formulation / DLRM ANALYZERS
# ===========================================================================
#
# The estimator stack (IPW, AIPW, g-computation, PSM, IRM-DML, PLR,
# SuperLearner, ATC, balance) lives in otis_causal.R.

#' Internal helper: the result an aggregate analysis returns when its table
#' lacks columns it needs (a result, not an error, so analyze_all carries on)
#' @noRd
.otis_missing_columns <- function(fn_name, data, need) {
  miss <- paste(setdiff(need, names(data)), collapse = ", ")
  msg <- sprintf("%s: missing required columns: %s", fn_name, miss)
  .otis_wrap(
    title = sprintf("%s -- missing columns", fn_name),
    summary_lines = list(status = "missing columns", missing = miss),
    warnings = msg,
    interpretation = msg
  )
}


# ---------------------------------------------------------------------------
# High-level wrapper: analyze_a01 (causal pipeline)
# ---------------------------------------------------------------------------

#' OTIS a01 high-level causal analysis (MatchIt + IRM-DML)
#'
#' Wraps the full causal pipeline for the canonical
#' Restrictive Confinement Detailed Dataset: 8-state alert-combo
#' encoding -> MatchIt 1:1 NN PSM -> IRM-DML with RF nuisances ->
#' multi-way clustered SE.
#'
#' @param data a01 data.frame (loaded from
#'   \code{a01_restrictive_confinement_detailed_dataset.csv}). Pass
#'   \code{NULL} to indicate "use registered loader" -- the R port
#'   requires you supply the data because we don't ship the loader
#'   side-effect from R.
#' @param out_dir Optional output directory.
#' @return A \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_a01 <- morie_synth_otis("a01", n = 120L, seed = 1L)
#' morie_otis_analyze_a01(otis_a01)
#' }
morie_otis_analyze_a01 <- function(data = NULL, out_dir = NULL) {
  pair <- if (is.null(data))
    morie_otis_make_pair_alert_to_volatility_a01()
  else
    morie_otis_make_pair_alert_to_volatility_ruhela(data)
  fit <- morie_otis_irm_dml(
    pair$data, treatment = pair$T, outcome = pair$Y,
    covariates = pair$covariates,
    cluster_cols = "UniqueIndividual_ID",
    match_first = TRUE
  )
  summary <- list(
    "Source file" = "a01_restrictive_confinement_detailed_dataset.csv",
    "Person-years (after MatchIt)" = fit$n,
    "ATE" = round(fit$ate, 4),
    "ATE 95% CI" = sprintf("[%.4f, %.4f]",
                            fit$ate_ci95[1], fit$ate_ci95[2]),
    "ATTE" = round(fit$atte, 4),
    "ATTE 95% CI" = sprintf("[%.4f, %.4f]",
                              fit$atte_ci95[1], fit$atte_ci95[2]),
    "Standard error type" = fit$se_kind
  )
  .otis_emit(
    .otis_wrap(
      title = paste0("OTIS a01 -- high alert complexity (ac >= 2) ",
                      "-> regional volatility (vm count)"),
      summary_lines = summary,
      interpretation = paste(
        "MatchIt-then-IRM-DML reproduction of the published",
        "res_pool finding on the canonical Restrictive",
        "Confinement Detailed Dataset."
      ),
      payload = fit
    ),
    out_dir, "a01")
}


# ---------------------------------------------------------------------------
# Ruhela formulations: full DLRM driver
# ---------------------------------------------------------------------------

#' Internal helper: 0/1 indicators the alternative formulations treat on
#' @noRd
.otis_female <- function(x) as.integer(!is.na(x) & tolower(as.character(x)) == "female")
#' @noRd
.otis_toronto <- function(x) as.integer(!is.na(x) & tolower(as.character(x)) == "toronto")
#' @noRd
.otis_age50 <- function(x) as.integer(!is.na(x) & grepl("50", as.character(x), fixed = TRUE))

#' Internal helper: the caller's OTIS table, or the published one by id
#' @noRd
.otis_ruhela_input <- function(data, id) {
  if (!is.null(data)) return(data)
  tryCatch(
    if (id == "a01") morie_otis_load() else morie_load_dataset(paste0("otis", id)),
    error = function(e) stop(sprintf(
      "pass data = the OTIS %s table; it could not be loaded here (%s)",
      id, conditionMessage(e)), call. = FALSE))
}

#' Internal helper: stop naming the columns a table lacks
#' @noRd
.otis_require_cols <- function(df, cols, id) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) {
    stop(sprintf("the OTIS %s table is missing column(s): %s", id,
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Internal helper: the ten-estimator DLRM on one Ruhela formulation
#'
#' Mirrors python \code{_ruhela_formulations_on}: an explicit
#' (treatment, outcome, covariates) triple, or by default the canonical
#' alert-complexity -> regional-volatility pair with its Naive arm.
#' Each estimator that fails records its error in its table row.
#' @noRd
.otis_ruhela_formulations_on <- function(df, ds_id, source_label, title,
                                         interpretation,
                                         cluster_col = "EndFiscalYear",
                                         treatment = NULL, outcome = NULL,
                                         covariates = NULL) {
  naive <- NULL
  if (!is.null(treatment)) {
    cols <- c(treatment, outcome, covariates)
    .otis_require_cols(df, cols, ds_id)
    data <- df[stats::complete.cases(df[, cols, drop = FALSE]), cols, drop = FALSE]
    label <- sprintf("%s: T=%s, Y=%s", ds_id, treatment, outcome)
  } else {
    both <- morie_otis_make_pair_alert_to_volatility_all(df)
    data <- both$ruhela$data
    treatment <- both$ruhela$T
    outcome <- both$ruhela$Y
    covariates <- both$ruhela$covariates
    naive <- both$naive
    label <- sprintf("%s: T_high_ac -> Y_vm_count (canonical)", ds_id)
  }
  cl <- if (cluster_col %in% names(data)) cluster_col else NULL
  fit <- function(f, ...) f(data, treatment, outcome, covariates, ...)
  ci <- function(v) sprintf("[%+.3f, %+.3f]", v[1L], v[2L])
  err_row <- function(idx, e) c(idx, "--", "err", substr(conditionMessage(e), 1L, 30L),
                                "--", "--", "--")
  rows <- list()
  ensemble <- list()
  add <- function(idx, name, estimand, f) {
    est <- tryCatch(f(), error = function(e) e)
    if (inherits(est, "error")) {
      rows[[length(rows) + 1L]] <<- err_row(idx, est)
      return(invisible(NULL))
    }
    notes <- unlist(est$notes)
    rows[[length(rows) + 1L]] <<- c(
      idx, estimand, sprintf("%+.4f", est$ate), sprintf("%.4f", est$ate_se),
      ci(est$ate_ci95), sprintf("%.2e", est$ate_pval),
      if (length(notes)) paste(utils::head(notes, 2L), collapse = ", ") else "--")
    ensemble[[length(ensemble) + 1L]] <<- list(
      estimator = name, estimand = estimand, estimate = est$ate,
      se = est$ate_se, p = est$ate_pval, n = est$n, notes = notes)
  }
  add("1. IPW (Hajek)", "IPW", "ATE", function() fit(morie_otis_ipw_ate))
  add("2. AIPW (RRZ DR)", "AIPW", "ATE", function() fit(morie_otis_aipw_ate))
  add("3. g-computation", "g-computation", "ATE",
      function() fit(morie_otis_gcomputation, n_bootstrap = 200L))
  add("4. PSM 1:1 NN", "PSM-NN", "ATT", function() fit(morie_otis_psm, k = 1L))
  add("5. PSM subclass (5)", "PSM-subclass", "ATE",
      function() fit(morie_otis_psm_subclass, n_strata = 5L))

  irm <- tryCatch(fit(morie_otis_irm_dml, cluster_cols = cl), error = function(e) e)
  if (inherits(irm, "error")) {
    rows[[length(rows) + 1L]] <- err_row("6. IRM-DML", irm)
    irm <- NULL
  } else {
    for (k in c("ate", "atte", "atc")) {
      rows[[length(rows) + 1L]] <- c(
        "6. IRM-DML", toupper(k), sprintf("%+.4f", irm[[k]]),
        sprintf("%.4f", irm[[paste0(k, "_se")]]), ci(irm[[paste0(k, "_ci95")]]),
        sprintf("%.2e", irm[[paste0(k, "_pval")]]),
        if (k == "ate") paste0("cluster=", irm$se_kind) else "--")
    }
    ensemble[[length(ensemble) + 1L]] <- c(list(estimator = "IRM-DML"),
      irm[c("ate", "ate_se", "atte", "atte_se", "atc", "atc_se", "n")])
  }
  mf <- tryCatch(fit(morie_otis_irm_dml, cluster_cols = cl, match_first = TRUE),
                 error = function(e) e)
  if (inherits(mf, "error")) {
    rows[[length(rows) + 1L]] <- err_row("7. PSM->IRM-DML", mf)
    mf <- NULL
  } else {
    rows[[length(rows) + 1L]] <- c(
      "7. PSM->IRM-DML (match_first)", "ATE", sprintf("%+.4f", mf$ate),
      sprintf("%.4f", mf$ate_se), ci(mf$ate_ci95), sprintf("%.2e", mf$ate_pval),
      sprintf("matched_n=%d", mf$n))
  }
  add("8. ATC (AIPW)", "ATC-AIPW", "ATC", function() fit(morie_otis_atc))
  add("9. PLR (Chernozhukov 2018)", "PLR", "theta", function() fit(morie_otis_plr))
  add("10. SuperLearner AIPW", "SuperLearner-AIPW", "ATE",
      function() fit(morie_otis_aipw_superlearner))

  # Same IRM-DML point estimate, four standard errors
  multi_se <- list()
  if (!is.null(irm)) {
    flavours <- list("pooled (iid)" = character(0),
                     "cluster: EndFiscalYear" = "EndFiscalYear",
                     "cluster: UniqueIndividual_ID" = "UniqueIndividual_ID",
                     "multi-way: yr x id" = c("EndFiscalYear", "UniqueIndividual_ID"))
    for (lab in names(flavours)) {
      cc <- flavours[[lab]]
      if (!all(cc %in% names(data))) {
        multi_se[[length(multi_se) + 1L]] <- c(lab, "--", "n/a (col missing)", "--", "--")
        next
      }
      f <- tryCatch(fit(morie_otis_irm_dml, cluster_cols = if (length(cc)) cc),
                    error = function(e) e)
      multi_se[[length(multi_se) + 1L]] <- if (inherits(f, "error")) {
        c(lab, "--", "err", substr(conditionMessage(f), 1L, 30L), "--")
      } else {
        c(lab, sprintf("%+.4f", f$ate), sprintf("%.4f", f$ate_se), ci(f$ate_ci95),
          sprintf("%.2e", f$ate_pval))
      }
    }
  }

  naive_rows <- list()
  if (!is.null(naive)) {
    runs <- list(IPW = morie_otis_ipw_ate, AIPW = morie_otis_aipw_ate,
                 "g-comp" = morie_otis_gcomputation)
    for (lab in names(runs)) {
      r <- tryCatch(runs[[lab]](naive$data, naive$T, naive$Y, naive$covariates),
                    error = function(e) e)
      naive_rows[[length(naive_rows) + 1L]] <- if (inherits(r, "error")) {
        c(lab, "err", substr(conditionMessage(r), 1L, 30L), "--", "--")
      } else {
        c(lab, sprintf("%+.4f", r$ate), sprintf("%.4f", r$ate_se), ci(r$ate_ci95),
          sprintf("%.2e", r$ate_pval))
      }
    }
  }

  balance <- tryCatch(morie_otis_balance(data, treatment, covariates),
                      error = function(e) e)
  balance_summary <- if (inherits(balance, "error")) {
    paste("err:", conditionMessage(balance))
  } else {
    sprintf("max |SMD| = %.3f", max(abs(balance$smd_raw), na.rm = TRUE))
  }

  tables <- list(list(
    title = sprintf("Ruhela formulations (full DLRM) on %s -- %s:", ds_id, label),
    headers = c("Estimator", "Estimand", "Estimate", "SE", "95% CI", "p-value", "Notes"),
    rows = rows))
  if (length(multi_se)) {
    tables[[length(tables) + 1L]] <- list(
      title = paste("IRM-DML SE comparison: pooled vs clustered on EndFiscalYear,",
                    "UniqueIndividual_ID, multi-way (year x id):"),
      headers = c("SE flavour", "ATE", "SE", "95% CI", "p-value"),
      rows = multi_se)
  }
  if (length(naive_rows)) {
    tables[[length(tables) + 1L]] <- list(
      title = paste("Naive-arm sensitivity (any-flag, vm-binary): concordance",
                    "with Ruhela in sign, smaller magnitude"),
      headers = c("Estimator", "ATE", "SE", "95% CI", "p-value"),
      rows = naive_rows)
  }
  .otis_wrap(
    title = title,
    summary_lines = list(
      "Source file" = source_label,
      "Dataset id" = ds_id,
      "Formulation" = label,
      "Cluster axis (primary)" = cluster_col,
      "n" = nrow(data),
      "Treated prevalence" = sprintf("%.1f%%", 100 * mean(.otis_binarise(data[[treatment]]))),
      "Pre-balance (covariate SMD)" = balance_summary,
      "Propensity calibration" = "none"),
    tables = tables,
    interpretation = interpretation,
    payload = list(
      ensemble = ensemble,
      match_first = mf,
      multi_se = multi_se,
      naive_arm = naive_rows,
      balance = if (!inherits(balance, "error")) balance,
      formulation = list(label = label, treatment = treatment, outcome = outcome,
                         covariates = covariates, n_rows = nrow(data)),
      ds_id = ds_id, cluster_col = cluster_col))
}

#' OTIS a01 Ruhela formulations (full DLRM)
#'
#' Runs the ten-estimator DLRM (IPW, AIPW, g-computation, PSM 1:1, PSM
#' subclassification, IRM-DML with ATE/ATTE/ATC, PSM-then-IRM-DML, ATC-AIPW,
#' PLR, SuperLearner AIPW) on the canonical a01 formulation, T = alert-state
#' complexity >= 2, Y = regional-volatility count, with an IRM-DML standard
#' error comparison (pooled, by year, by person, two-way) and the Naive-arm
#' sensitivity (any-flag treatment, binary outcome).
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_formulations(a01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_a01_ruhela_formulations <- function(data = NULL, out_dir = NULL) {
  work <- .otis_ruhela_input(data, "a01")
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "a01",
    source_label = "a01_restrictive_confinement_detailed_dataset.csv",
    title = paste0(
      "OTIS a01 -- Ruhela formulations (full DLRM) on the canonical ",
      "formulation (T = ac >= 2 -> Y = vm count)"),
    interpretation = paste0(
      "Ruhela formulations on the canonical OTIS-RC dataset (a01). ",
      "The Ruhela formulation (T = ac >= 2 alert-state complexity, ",
      "Y = vm regional-volatility count) is the published ",
      "Goffmanian contrast in notez1a.qmd section res_pool. ",
      "Concordance across IPW (single-robust on propensity), ",
      "g-computation (single-robust on outcome), AIPW ",
      "(doubly-robust), PSM-NN (ATT, nonparametric), PSM-subclass ",
      "(ATE, nonparametric), IRM-DML and the MatchIt-then-DoubleML ",
      "pipeline is the strongest practical signal of ",
      "identification. Naive-arm sensitivity (any-flag, vm-binary) ",
      "shows the same Goffmanian sign at smaller magnitude."))
  .otis_emit(res, out_dir, "a01")
}

#' OTIS b01 Ruhela formulations (full DLRM)
#'
#' The DLRM of \code{\link{morie_otis_analyze_a01_ruhela_formulations}} on the
#' b01 segregation placements.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_formulations(b01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b01_ruhela_formulations <- function(data = NULL, out_dir = NULL) {
  work <- .otis_ruhela_input(data, "b01")
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b01",
    source_label = "b01_segregation_detailed_dataset.csv",
    title = paste0(
      "OTIS b01 -- Ruhela formulations (full DLRM) ",
      "(segregation-detailed complement)"),
    interpretation = paste0(
      "Ruhela formulations on b01 segregation-placement records, ",
      "complementing the a01 RC analysis. Same Ruhela formulation ",
      "and DLRM ensemble; the larger ATE magnitude (~3x a01) ",
      "reflects segregation being a stronger institutional contrast ",
      "than restrictive confinement, and per-placement rather than ",
      "per-day units of analysis."))
  .otis_emit(res, out_dir, "b01")
}

#' OTIS b02 Ruhela formulations: T=Female -> seg-day count
#'
#' b02 has no alert columns, so the formulation tests gender disparity in
#' segregation-day burden: T = Female, Y = TotalAggregatedDays_Segregation,
#' adjusting for region, age band and fiscal year. Same DLRM ensemble; no
#' Naive arm.
#'
#' @param data The OTIS b02 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b02 <- morie_synth_otis("b02", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b02_ruhela_formulations(b02)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b02_ruhela_formulations <- function(data = NULL, out_dir = NULL) {
  work <- .otis_ruhela_input(data, "b02")
  need <- c("Gender", "TotalAggregatedDays_Segregation",
            "Region_MostRecentPlacement", "Age_Category", "EndFiscalYear")
  .otis_require_cols(work, need, "b02")
  work <- work[stats::complete.cases(work[, need, drop = FALSE]), , drop = FALSE]
  work$T_female <- .otis_female(work$Gender)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b02",
    source_label = "b02_segregation_detailed_total_days.csv",
    title = paste0(
      "OTIS b02 -- Ruhela formulations: T=Female -> Y=Total seg ",
      "days within FY"),
    interpretation = paste0(
      "Ruhela formulation on b02 testing gender disparity in ",
      "segregation-day burden. Per-person-year aggregated days in ",
      "segregation as the count outcome; Female indicator as the ",
      "treatment. Same DLRM ensemble as a01/b01; no Naive arm ",
      "because b02 lacks the alert columns to construct one."),
    treatment = "T_female",
    outcome = "TotalAggregatedDays_Segregation",
    covariates = c("Region_MostRecentPlacement", "Age_Category", "EndFiscalYear"))
  .otis_emit(res, out_dir, "b02")
}

# DLRM short aliases
#' @rdname morie_otis_analyze_a01_ruhela_formulations
#' @export
morie_otis_analyze_a01_dlrm <- morie_otis_analyze_a01_ruhela_formulations
#' @rdname morie_otis_analyze_b01_ruhela_formulations
#' @export
morie_otis_analyze_b01_dlrm <- morie_otis_analyze_b01_ruhela_formulations
#' @rdname morie_otis_analyze_b02_ruhela_formulations
#' @export
morie_otis_analyze_b02_dlrm <- morie_otis_analyze_b02_ruhela_formulations


# ---------------------------------------------------------------------------
# Per-year Ruhela formulations
# ---------------------------------------------------------------------------

#' Per-fiscal-year full-DLRM Ruhela formulation driver
#'
#' Runs the DLRM estimator battery separately in each fiscal year through
#' \code{\link{morie_otis_per_year_irm_dml}} with \code{full_battery = TRUE}
#' and tabulates one row per (year, estimator). Stable signs across
#' estimators within a year argue against misspecification; stable
#' magnitudes across years argue for temporal stationarity.
#'
#' @param data Data frame holding the treatment, outcome, covariates and
#'   year column.
#' @param ds_id Dataset id label.
#' @param treatment Treatment column name.
#' @param outcome Outcome column name.
#' @param covariates Character vector of covariate column names.
#' @param year_col Year column (default \code{"EndFiscalYear"}).
#' @param cluster_col Cluster axis for the IRM-DML SE, or \code{NULL}.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}; \code{payload$by_year} holds
#'   the per-year fits.
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 400L
#' x <- rnorm(n)
#' d <- rbinom(n, 1, plogis(0.4 * x))
#' df <- data.frame(T = d, Y = 0.5 * d + x + rnorm(n), x = x,
#'                  EndFiscalYear = rep(2023:2024, each = n / 2))
#' r <- morie_otis_analyze_ruhela_per_year(df, ds_id = "demo", treatment = "T",
#'                                         outcome = "Y", covariates = "x",
#'                                         cluster_col = NULL)
#' head(r$tables[[1L]]$rows, 3L)
#' }
morie_otis_analyze_ruhela_per_year <- function(data, ds_id,
                                                 treatment, outcome,
                                                 covariates,
                                                 year_col = "EndFiscalYear",
                                                 cluster_col = "EndFiscalYear",
                                                 out_dir = NULL) {
  .otis_require_cols(data, c(treatment, outcome, covariates, year_col), ds_id)
  by_year <- morie_otis_per_year_irm_dml(
    data, treatment, outcome, covariates, year_col = year_col,
    cluster_cols = cluster_col, full_battery = TRUE)
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- c(...)
  ci <- function(v, s) sprintf("[%+.3f, %+.3f]", v - 1.96 * s, v + 1.96 * s)
  for (yr in names(by_year)) {
    res <- by_year[[yr]]
    for (k in c("ipw", "aipw", "gcomp", "psm_nn", "psm_subclass", "atc", "plr",
                "superlearner")) {
      e <- res[[k]]
      if (!is.null(e$error)) {
        add(yr, toupper(k), "err", "--", "--", "--", substr(e$error, 1L, 30L))
      } else {
        add(yr, toupper(k), sprintf("%+.4f", e$ate), sprintf("%.4f", e$se),
            sprintf("[%+.3f, %+.3f]", e$ci95[1L], e$ci95[2L]),
            sprintf("%.2e", e$p), sprintf("n=%d", e$n))
      }
    }
    irm <- res$irm_dml
    if (!is.null(irm$error)) {
      add(yr, "IRM-DML", "err", "--", "--", "--", substr(irm$error, 1L, 30L))
    } else {
      for (k in c("ate", "atte", "atc")) {
        v <- irm[[k]]
        s <- irm[[paste0(k, "_se")]]
        add(yr, paste("IRM-DML", toupper(k)), sprintf("%+.4f", v), sprintf("%.4f", s),
            ci(v, s), "--", sprintf("n=%d %s", irm$n, irm$se_kind))
      }
    }
    mf <- res$match_first
    if (!is.null(mf$error)) {
      add(yr, "PSM->IRM-DML", "err", "--", "--", "--", substr(mf$error, 1L, 30L))
    } else {
      add(yr, "PSM->IRM-DML", sprintf("%+.4f", mf$ate), sprintf("%.4f", mf$ate_se),
          ci(mf$ate, mf$ate_se), "--", sprintf("matched_n=%d", mf$n))
    }
  }
  res <- .otis_wrap(
    title = sprintf(paste("OTIS %s -- per-fiscal-year full DLRM-on-Ruhela-formulations",
                          "suite (10 estimators x N years)"), ds_id),
    summary_lines = list(
      "Dataset id" = ds_id,
      "Treatment" = treatment,
      "Outcome" = outcome,
      "Year column" = year_col,
      "Cluster axis" = if (is.null(cluster_col)) "iid" else cluster_col,
      "Years analysed" = length(by_year),
      "Estimators per year" = 10L),
    tables = list(list(
      title = paste("Per-year x estimator: ATE / SE / 95% CI / p / n. Triangulation",
                    "across IPW + AIPW + g-comp + PSM (NN+subclass) + IRM-DML",
                    "(ATE+ATTE+ATC) + match_first + ATC-AIPW + PLR + SuperLearner."),
      headers = c("FY", "Estimator", "Estimate", "SE", "95% CI", "p", "Notes"),
      rows = rows)),
    interpretation = paste(
      "Per-year x per-estimator triangulation. Stable signs across estimators",
      "within each FY = no-misspecification robustness; stable magnitudes",
      "across FYs = temporal stationarity. Use this to identify FYs where",
      "some estimators disagree -- typical sign of finite-sample fragility",
      "or genuine effect heterogeneity."),
    payload = list(by_year = by_year, ds_id = ds_id, treatment = treatment,
                   outcome = outcome))
  .otis_emit(res, out_dir, paste0(ds_id, "-per-year"))
}

#' Per-year full-DLRM on the a01 canonical formulation
#'
#' \code{\link{morie_otis_analyze_ruhela_per_year}} on the a01 person-year
#' cell frame (T = alert complexity >= 2, Y = regional-volatility count),
#' clustered on fiscal year.
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 800L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_per_year(a01)
#' r$summary_lines$`Years analysed`
#' }
morie_otis_analyze_a01_ruhela_per_year <- function(data = NULL,
                                                     out_dir = NULL) {
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "a01"))
  morie_otis_analyze_ruhela_per_year(pair$data, ds_id = "a01", treatment = pair$T,
                                      outcome = pair$Y, covariates = pair$covariates,
                                      out_dir = out_dir)
}

#' Per-year full-DLRM on the b01 canonical formulation
#'
#' As \code{\link{morie_otis_analyze_a01_ruhela_per_year}}, on b01.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 800L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_per_year(b01)
#' r$summary_lines$`Years analysed`
#' }
morie_otis_analyze_b01_ruhela_per_year <- function(data = NULL,
                                                     out_dir = NULL) {
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "b01"))
  morie_otis_analyze_ruhela_per_year(pair$data, ds_id = "b01", treatment = pair$T,
                                      outcome = pair$Y, covariates = pair$covariates,
                                      out_dir = out_dir)
}

# ---------------------------------------------------------------------------
# Aggregate Ruhela formulations -- Poisson + NB GLM IRR
# ---------------------------------------------------------------------------
#
# These are FAITHFULLY ported. The math is base-R glm() + MASS::glm.nb.
# GEE clustering runs the native Liang-Zeger estimator (Poisson +
# exchangeable working correlation, sandwich covariance).

#' Internal helper: Otis Aggregate Glm
#' @noRd
.otis_aggregate_glm <- function(work, treatment, outcome,
                                  covariates = character(0),
                                  year_col = "EndFiscalYear",
                                  cluster_group = NULL,
                                  ds_id, source_label, title,
                                  interpretation) {
  cov <- as.character(covariates %||% character(0))
  needed <- unique(c(treatment, outcome, year_col, cov, cluster_group))
  needed <- needed[needed %in% names(work)]
  work <- work[, needed, drop = FALSE]
  work <- work[stats::complete.cases(work), , drop = FALSE]
  work[[outcome]] <- suppressWarnings(as.integer(work[[outcome]]))
  work <- work[!is.na(work[[outcome]]), , drop = FALSE]

  if (nrow(work) == 0L || sum(work[[outcome]], na.rm = TRUE) == 0L) {
    return(.otis_wrap(
      title = title,
      summary_lines = list("Source file" = source_label,
                              "Dataset id" = ds_id,
                              status = "empty cell-table or zero outcome"),
      warnings = "empty cell-table or zero outcome count"
    ))
  }

  # Build formula with factor() wrappers for non-numeric covs
  parts <- sprintf("factor(`%s`)", treatment)
  for (c in cov) parts <- c(parts, sprintf("factor(`%s`)", c))
  parts <- c(parts, sprintf("factor(`%s`)", year_col))
  fml <- stats::as.formula(sprintf("`%s` ~ %s", outcome,
                                     paste(parts, collapse = " + ")))

  rows <- list()
  aic_pois <- NA_real_
  aic_nb <- NA_real_
  estimands <- list()

  # Poisson + Negative Binomial (NB requires MASS).
  # 3MMM.28: suppressWarnings around the call so the per-fit
  # "glm.fit: algorithm did not converge" / "alternation limit
  # reached" messages don't spam the test log on small synthetic
  # OTIS panels. The callers below bump maxit; convergence status is
  # available via out$converged if needed.
  fit_one <- function(fam_label, fitter) {
    out <- suppressWarnings(tryCatch(fitter(), error = function(e) e))
    if (inherits(out, "error")) {
      return(list(label = fam_label, fail = TRUE,
                  msg = conditionMessage(out)))
    }
    co <- stats::coef(summary(out))
    rn <- rownames(co)
    t_idx <- grep(treatment, rn, fixed = TRUE)
    if (length(t_idx) == 0L) {
      return(list(label = fam_label, fail = TRUE,
                  msg = "no coef matched treatment"))
    }
    t_key <- rn[t_idx[1]]
    beta <- co[t_key, "Estimate"]
    se   <- co[t_key, "Std. Error"]
    pval <- co[t_key, ncol(co)]
    irr <- exp(beta)
    ci_lo <- exp(beta - 1.96 * se)
    ci_hi <- exp(beta + 1.96 * se)
    aic_v <- stats::AIC(out)
    list(label = fam_label, fail = FALSE,
         t_key = t_key, beta = beta, se = se, pval = pval,
         irr = irr, ci_lo = ci_lo, ci_hi = ci_hi, aic = aic_v)
  }

  res_p <- fit_one("Poisson", function()
    stats::glm(fml, data = work, family = stats::poisson(),
                control = stats::glm.control(maxit = 200L)))
  if (!res_p$fail) {
    rows[[length(rows) + 1L]] <- c(res_p$label, res_p$t_key,
      round(res_p$irr, 4),
      sprintf("[%.3f, %.3f]", res_p$ci_lo, res_p$ci_hi),
      sprintf("%.2e", res_p$pval),
      round(res_p$aic, 1),
      sprintf("n=%d, total_y=%d", nrow(work),
              as.integer(sum(work[[outcome]]))))
    aic_pois <- res_p$aic
    estimands[[length(estimands) + 1L]] <- res_p
  } else {
    rows[[length(rows) + 1L]] <- c("Poisson", treatment, "fit failed",
                                     res_p$msg, "--", "--", "--")
  }

  {
    res_nb <- fit_one("NB", function()
      morie_glm_nb(fml, data = work,
                    control = stats::glm.control(maxit = 200L)))
    if (!res_nb$fail) {
      rows[[length(rows) + 1L]] <- c(res_nb$label, res_nb$t_key,
        round(res_nb$irr, 4),
        sprintf("[%.3f, %.3f]", res_nb$ci_lo, res_nb$ci_hi),
        sprintf("%.2e", res_nb$pval),
        round(res_nb$aic, 1),
        sprintf("n=%d, total_y=%d", nrow(work),
                as.integer(sum(work[[outcome]]))))
      aic_nb <- res_nb$aic
      estimands[[length(estimands) + 1L]] <- res_nb
    } else {
      rows[[length(rows) + 1L]] <- c("NB", treatment, "fit failed",
                                       res_nb$msg, "--", "--", "--")
    }
  }

  # GEE clustered fits
  if (!is.null(cluster_group) && cluster_group %in% names(work)) {
    n_groups <- length(unique(work[[cluster_group]]))
    {
      # Order by cluster for reproducible cluster blocks.
      work_g <- work[order(work[[cluster_group]]), , drop = FALSE]
      for (gee_label in c("GEE-Poisson")) {
        # Native GEE restricts to the Poisson family (as geepack did
        # here). Integer factor codes keep cluster ids unambiguous.
        id_int <- as.integer(factor(work_g[[cluster_group]]))
        # Native GEE (Liang-Zeger 1986), Poisson + exchangeable working
        # correlation with sandwich covariance -- the geepack::geeglm
        # surface this loop consumed, cross-validated in tests.
        res_g <- tryCatch({
          mf <- stats::model.frame(fml, data = work_g)
          Xg <- stats::model.matrix(fml, mf)
          yg <- stats::model.response(mf)
          .morie_gee_poisson_exch(Xg, yg, id_int)
        }, error = function(e) e)
        if (inherits(res_g, "error")) {
          rows[[length(rows) + 1L]] <- c(
            sprintf("%s (cluster:%s)", gee_label, cluster_group),
            treatment, "fit failed",
            conditionMessage(res_g), "--", "--", "--")
          next
        }
        # Validate the sandwich estimator before letting summary()
        # take sqrt of it. On small/degenerate data the robust
        # covariance matrix's diagonal can be negative or NaN, which
        # silently produces NaN standard errors. Detect + report
        # explicitly instead of letting NaN propagate into the table.
        vb <- tryCatch(res_g$vbeta, error = function(e) NULL)
        if (is.null(vb) || any(!is.finite(diag(vb))) ||
            any(diag(vb) < 0)) {
          rows[[length(rows) + 1L]] <- c(
            sprintf("%s (cluster:%s)", gee_label, cluster_group),
            treatment, "degenerate covariance",
            paste0("GEE sandwich variance matrix is non-PSD ",
                   "(diag(vbeta) has NaN or negative entries); ",
                   "likely small clusters, collinear factor levels, ",
                   "or near-zero outcome counts. Use a stratified ",
                   "non-GEE model or pool clusters."),
            "--", "--", "--")
          next
        }
        rn <- colnames(Xg)
        t_idx <- grep(treatment, rn, fixed = TRUE)
        if (length(t_idx) == 0L) {
          rows[[length(rows) + 1L]] <- c(
            sprintf("%s (cluster:%s)", gee_label, cluster_group),
            treatment, "no coef", "--", "--", "--", "--")
          next
        }
        t_key <- rn[t_idx[1]]
        beta <- res_g$coefficients[t_idx[1]]
        se   <- sqrt(diag(vb))[t_idx[1]]
        pval <- 2 * stats::pnorm(-abs(beta / se))
        irr <- exp(beta)
        rows[[length(rows) + 1L]] <- c(
          sprintf("%s (cluster:%s, Exch)", gee_label, cluster_group),
          t_key, round(irr, 4),
          sprintf("[%.3f, %.3f]",
                  exp(beta - 1.96 * se), exp(beta + 1.96 * se)),
          sprintf("%.2e", pval), "--",
          sprintf("groups=%d, n_obs=%d", n_groups, nrow(work)))
      }
    }
  }

  overdisp <- if (is.finite(aic_pois) && is.finite(aic_nb))
    round(aic_pois - aic_nb, 1) else "n/a"

  summary <- list(
    "Source file" = source_label,
    "Dataset id" = ds_id,
    "Treatment column" = treatment,
    "Outcome (count)" = outcome,
    "Year FE" = year_col,
    "Covariate FEs" = if (length(cov) > 0L)
      paste(cov, collapse = ", ") else "none",
    "Cluster group (GEE)" = cluster_group %||% "--",
    "Cells" = nrow(work),
    "Total outcome count" = as.integer(sum(work[[outcome]])),
    "Overdispersion (Poisson AIC - NB AIC)" = overdisp
  )

  .otis_wrap(
    title = title,
    summary_lines = summary,
    tables = list(list(
      title = paste0("Aggregate Ruhela formulation -- ",
                       "Poisson + NB IRR",
                       if (!is.null(cluster_group))
                         " + GEE clustered" else "",
                       " for treatment effect on count outcome:"),
      headers = c("Family", "Coefficient", "IRR", "95% CI",
                    "p-value", "AIC", "Notes"),
      rows = rows
    )),
    interpretation = paste0(
      interpretation,
      "\
\
IRR > 1 ==> treatment increases the count rate; IRR < 1 ",
      "==> treatment decreases it. Concordance between Poisson and NB ",
      "indicates equidispersion; large gap (Poisson AIC > NB AIC) ",
      "indicates overdispersion -- trust NB."
    ),
    payload = list(formula = format(fml),
                   ds_id = ds_id, treatment = treatment,
                   outcome = outcome,
                   cluster_group = cluster_group,
                   n_rows = nrow(work),
                   estimands = estimands)
  )
}


# Aggregate Ruhela formulation analyzers (b03..d05). Each one drops NA
# on the relevant columns, constructs the treatment indicator, and
# delegates to .otis_aggregate_glm.

#' b03 aggregate Ruhela: Alert presence -> seg placements
#' @param data b03 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b03 <- morie_synth_otis("b03", n = 120L, seed = 1L)
#' morie_otis_analyze_b03_ruhela_aggregate(otis_b03)
#' }
morie_otis_analyze_b03_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("Alert_Presence", "Number_SegregationPlacements")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b03_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_alert <- as.integer(tolower(trimws(
    as.character(work$Alert_Presence))) %in%
    c("yes", "y", "true", "1"))
  .otis_aggregate_glm(work, treatment = "T_alert",
    outcome = "Number_SegregationPlacements",
    covariates = c("Alert_Type", "Region_AtTimeOfPlacement"),
    cluster_group = "Institution_AtTimeOfPlacement",
    ds_id = "b03",
    source_label = paste0("b03_segregation_placements_alerts_and_",
                            "hold_flags_by_institution.csv"),
    title = paste0("OTIS b03 -- Aggregate Ruhela formulation: ",
                     "Alert presence -> Number of segregation placements"),
    interpretation = paste(
      "Aggregate Ruhela formulation on b03 testing whether",
      "alert-flagged person-cells receive more segregation",
      "placements than no-alert cells."
    ))
}

#' b04 aggregate Ruhela: Female -> median seg duration
#' @param data b04 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b04 <- morie_synth_otis("b04", n = 120L, seed = 1L)
#' morie_otis_analyze_b04_ruhela_aggregate(otis_b04)
#' }
morie_otis_analyze_b04_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Region_AtTimeOfPlacement", "Gender",
            "Measure", "NumberConsecutiveDays_Segregation")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b04_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work <- work[trimws(as.character(work$Measure)) == "Median", ,
               drop = FALSE]
  if (nrow(work) == 0L)
    return(.otis_wrap("b04 aggregate Ruhela", list(),
                      warnings = "b04 has no Median rows after filter"))
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "NumberConsecutiveDays_Segregation",
    covariates = "Region_AtTimeOfPlacement",
    ds_id = "b04",
    source_label = paste0("b04_segregation_placements_consecutive",
                            "_durations_by_region.csv"),
    title = paste0("OTIS b04 -- Aggregate Ruhela formulation: ",
                     "Female -> median consecutive seg duration"),
    interpretation = paste(
      "Aggregate RF on b04 testing gender disparity in the MEDIAN",
      "consecutive duration of segregation placements, by region."
    ))
}

#' b05 aggregate Ruhela: schema-no-demographic guard
#'
#' OTIS b05 (segregation placements by consecutive duration) does
#' not carry a demographic treatment variable -- the published
#' schema is just \eqn{EndFiscalYear, Consecutive_Duration,
#' Number_SegregationPlacements}. The "Ruhela formulation" presumes
#' a binary treatment column (typically Gender, Race, or alert
#' status) for the aggregate RF test, so b05 has no meaningful
#' aggregate Ruhela analysis on its own. Returns a structured
#' "not applicable" wrapper rather than erroring so dispatcher
#' loops over the b03..b09 family stay green.
#'
#' @param data b05 data.frame.
#' @param out_dir Optional output directory (unused, accepted for
#'   parity with sibling aggregators).
#' @return \code{morie_otis_analysis_result} carrying a "not
#'   applicable" note in \code{warnings}.
#' @export
#' @examples
#' \donttest{
#' otis_b05 <- morie_synth_otis("b05", n = 120L, seed = 1L)
#' morie_otis_analyze_b05_ruhela_aggregate(otis_b05)
#' }
morie_otis_analyze_b05_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Consecutive_Duration",
            "Number_SegregationPlacements")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b05_ruhela_aggregate", data, need))
  .otis_emit(
    .otis_wrap(
      "b05 aggregate Ruhela", list(),
      warnings = paste(
        "b05 has no demographic treatment column (Gender / Race /",
        "Alert) in the published schema, so the Ruhela aggregate RF",
        "formulation is not applicable here. Use the b05 panel for",
        "consecutive-duration histograms instead."
      ),
      interpretation = paste(
        "OTIS b05 -- aggregate Ruhela formulation: not applicable.",
        "b05 publishes Consecutive_Duration counts only, with no",
        "demographic treatment column to contrast against."
      )
    ),
    out_dir, "b05_ruhela_aggregate")
}

#' b06 aggregate Ruhela: Disciplinary reason -> seg placements
#' @param data b06 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b06 <- morie_synth_otis("b06", n = 120L, seed = 1L)
#' morie_otis_analyze_b06_ruhela_aggregate(otis_b06)
#' }
morie_otis_analyze_b06_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Reason", "Gender",
            "Region_AtTimeOfPlacement",
            "Institution_AtTimeOfPlacement",
            "Number_SegregationPlacements")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b06_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_disciplinary <- as.integer(grepl(
    "disciplinary", tolower(as.character(work$Reason)), fixed = TRUE))
  .otis_aggregate_glm(work, treatment = "T_disciplinary",
    outcome = "Number_SegregationPlacements",
    covariates = c("Gender", "Region_AtTimeOfPlacement"),
    cluster_group = "Institution_AtTimeOfPlacement",
    ds_id = "b06",
    source_label = paste0("b06_segregation_placements_reason_for_",
                            "placement_by_institution.csv"),
    title = paste0("OTIS b06 -- Aggregate Ruhela formulation: ",
                     "Disciplinary reason -> seg placements"),
    interpretation = paste(
      "Aggregate RF on b06 testing whether disciplinary segregation",
      "reasons produce more placements than non-disciplinary."
    ))
}

#' b07 aggregate Ruhela (pivot to long): With-alert -> seg placements
#' @param data b07 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b07 <- morie_synth_otis("b07", n = 120L, seed = 1L)
#' morie_otis_analyze_b07_ruhela_aggregate(otis_b07)
#' }
morie_otis_analyze_b07_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Alert_Type", "Gender",
            "Number_Segregation_Placements_With_Alert",
            "Number_Segregation_Placements_Without_Alert")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b07_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  long_with <- data.frame(
    EndFiscalYear = work$EndFiscalYear,
    Alert_Type = work$Alert_Type,
    Gender = work$Gender,
    n_placements = work$Number_Segregation_Placements_With_Alert,
    T_alert = 1L,
    stringsAsFactors = FALSE
  )
  long_without <- data.frame(
    EndFiscalYear = work$EndFiscalYear,
    Alert_Type = work$Alert_Type,
    Gender = work$Gender,
    n_placements = work$Number_Segregation_Placements_Without_Alert,
    T_alert = 0L,
    stringsAsFactors = FALSE
  )
  long_df <- rbind(long_with, long_without)
  .otis_aggregate_glm(long_df, treatment = "T_alert",
    outcome = "n_placements",
    covariates = c("Alert_Type", "Gender"),
    ds_id = "b07",
    source_label = paste0("b07_segregation_placements_alerts_and_",
                            "hold_flags_by_gender.csv"),
    title = paste0("OTIS b07 -- Aggregate Ruhela formulation: ",
                     "With-alert vs without-alert x gender -> ",
                     "Number of segregation placements"),
    interpretation = paste(
      "Aggregate RF on b07 testing alert-flagged vs no-alert",
      "placements in the same gender x alert-type stratum."
    ))
}

#' b08 aggregate Ruhela: Female -> median seg duration (institution-clustered)
#' @param data b08 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b08 <- morie_synth_otis("b08", n = 120L, seed = 1L)
#' morie_otis_analyze_b08_ruhela_aggregate(otis_b08)
#' }
morie_otis_analyze_b08_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Region_AtTimeOfPlacement",
            "Institution_AtTimeOfPlacement", "Gender",
            "Measure", "NumberConsecutiveDays_Segregation")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b08_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work <- work[trimws(as.character(work$Measure)) == "Median", ,
               drop = FALSE]
  if (nrow(work) == 0L)
    return(.otis_wrap("b08 aggregate Ruhela", list(),
                      warnings = "b08 has no Median rows after filter"))
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "NumberConsecutiveDays_Segregation",
    covariates = "Region_AtTimeOfPlacement",
    cluster_group = "Institution_AtTimeOfPlacement",
    ds_id = "b08",
    source_label = paste0("b08_segregation_placements_consecutive_",
                            "durations_by_institution.csv"),
    title = paste0("OTIS b08 -- Aggregate Ruhela formulation: ",
                     "Female -> median seg duration"),
    interpretation = paste(
      "Aggregate RF on b08 testing gender disparity in median",
      "segregation duration; institution as GEE cluster group."
    ))
}

#' b09 aggregate Ruhela: Female -> individuals in segregation
#' @param data b09 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b09 <- morie_synth_otis("b09", n = 120L, seed = 1L)
#' morie_otis_analyze_b09_ruhela_aggregate(otis_b09)
#' }
morie_otis_analyze_b09_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "NumberPlacements_Segregation", "Gender",
            "NumberIndividuals_Segregation")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("b09_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "NumberIndividuals_Segregation",
    covariates = "NumberPlacements_Segregation",
    ds_id = "b09",
    source_label = paste0("b09_individuals_in_segregation_number_of_",
                            "times_in_segregation.csv"),
    title = paste0("OTIS b09 -- Aggregate Ruhela formulation: ",
                     "Female -> Number of individuals in segregation"),
    interpretation = paste(
      "Aggregate RF on b09 testing gender disparity in the",
      "individuals-in-segregation count distribution."
    ))
}

#' c01 aggregate Ruhela: Female -> RC count
#' @param data c01 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c01 <- morie_synth_otis("c01", n = 120L, seed = 1L)
#' morie_otis_analyze_c01_ruhela_aggregate(otis_c01)
#' }
morie_otis_analyze_c01_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c01_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    ds_id = "c01",
    source_label = paste0("c01_individuals_in_segregation_and_",
                            "restrictive_confinement_total_individuals.csv"),
    title = paste0("OTIS c01 -- Aggregate Ruhela formulation: ",
                     "Female -> Individuals in RC"),
    interpretation = "Aggregate RF on c01: gender disparity in RC totals.")
}

#' c01 region-cluster variant (year-clustered GEE)
#' @param data c01 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c01 <- morie_synth_otis("c01", n = 120L, seed = 1L)
#' morie_otis_analyze_c01_ruhela_aggregate_region_cluster(otis_c01)
#' }
morie_otis_analyze_c01_ruhela_aggregate_region_cluster <- function(data,
                                                          out_dir = NULL) {
  need <- c("EndFiscalYear", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c01_ruhela_aggregate_region_cluster", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    cluster_group = "EndFiscalYear",
    ds_id = "c01-RC",
    source_label = paste0("c01 (year-clustered variant)"),
    title = paste0("OTIS c01 -- Aggregate RF, year-clustered: ",
                     "Female -> Individuals in RC"),
    interpretation = paste(
      "Year-clustered GEE variant of c01. With only 3 fiscal years",
      "(= 3 clusters) the GEE inference is unreliable; shipped",
      "for transparency, not as the primary inference."
    ))
}

#' c02 aggregate Ruhela: Female -> RC (institution GEE)
#' @param data c02 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c02 <- morie_synth_otis("c02", n = 120L, seed = 1L)
#' morie_otis_analyze_c02_ruhela_aggregate(otis_c02)
#' }
morie_otis_analyze_c02_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Region_MostRecentPlacement",
            "Institution_MostRecentPlacement", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c02_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Region_MostRecentPlacement",
    cluster_group = "Institution_MostRecentPlacement",
    ds_id = "c02",
    source_label = paste0("c02_individuals_in_segregation_and_",
                            "restrictive_confinement_by_institution.csv"),
    title = paste0("OTIS c02 -- Aggregate RF, institution-clustered: ",
                     "Female -> Individuals in RC"),
    interpretation = paste(
      "Aggregate RF on c02 with institution as GEE cluster group.",
      "Marginal-model analogue of glmmTMB nbinom2 random-intercept."
    ))
}

#' c03 aggregate Ruhela: Indigenous -> RC
#' @param data c03 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c03 <- morie_synth_otis("c03", n = 120L, seed = 1L)
#' morie_otis_analyze_c03_ruhela_aggregate(otis_c03)
#' }
morie_otis_analyze_c03_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Race", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c03_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_indigenous <- .otis_indigenous_indicator(work$Race)
  .otis_aggregate_glm(work, treatment = "T_indigenous",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Gender",
    ds_id = "c03",
    source_label = paste0("c03_individuals_in_segregation_and_",
                            "restrictive_confinement_race_by_gender.csv"),
    title = paste0("OTIS c03 -- Aggregate RF: ",
                     "Indigenous -> Individuals in RC"),
    interpretation = paste(
      "Aggregate RF on c03 testing Indigenous overrepresentation",
      "in RC; quantifies the population-level IRR."
    ))
}

#' c04 aggregate Ruhela: Indigenous -> RC (by region)
#' @param data c04 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c04 <- morie_synth_otis("c04", n = 120L, seed = 1L)
#' morie_otis_analyze_c04_ruhela_aggregate(otis_c04)
#' }
morie_otis_analyze_c04_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Race", "Region_MostRecentPlacement",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c04_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_indigenous <- .otis_indigenous_indicator(work$Race)
  .otis_aggregate_glm(work, treatment = "T_indigenous",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Region_MostRecentPlacement",
    ds_id = "c04",
    source_label = paste0("c04_individuals_in_segregation_and_",
                            "restrictive_confinement_race_by_region.csv"),
    title = paste0("OTIS c04 -- Aggregate RF: ",
                     "Indigenous -> Individuals in RC, by region"),
    interpretation = paste(
      "Aggregate RF on c04 testing within-region Indigenous",
      "overrepresentation in RC."
    ))
}

#' c04 region-cluster variant
#' @param data c04 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c04 <- morie_synth_otis("c04", n = 120L, seed = 1L)
#' morie_otis_analyze_c04_ruhela_aggregate_region_cluster(otis_c04)
#' }
morie_otis_analyze_c04_ruhela_aggregate_region_cluster <- function(data,
                                                          out_dir = NULL) {
  need <- c("EndFiscalYear", "Race", "Region_MostRecentPlacement",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c04_ruhela_aggregate_region_cluster", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_indigenous <- .otis_indigenous_indicator(work$Race)
  .otis_aggregate_glm(work, treatment = "T_indigenous",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    cluster_group = "Region_MostRecentPlacement",
    ds_id = "c04-RC",
    source_label = "c04 (region-clustered variant)",
    title = paste0("OTIS c04 -- Aggregate RF, region-clustered: ",
                     "Indigenous -> Individuals in RC"),
    interpretation = "Region-clustered GEE variant of c04.")
}

#' c05 aggregate Ruhela: non-majority religion -> RC
#' @param data c05 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c05 <- morie_synth_otis("c05", n = 120L, seed = 1L)
#' morie_otis_analyze_c05_ruhela_aggregate(otis_c05)
#' }
morie_otis_analyze_c05_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Religion", "Region_MostRecentPlacement",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c05_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_minority_religion <- .otis_minority_religion_indicator(
    work$Religion)
  .otis_aggregate_glm(work, treatment = "T_minority_religion",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Region_MostRecentPlacement",
    ds_id = "c05",
    source_label = paste0("c05_individuals_in_segregation_and_",
                            "restrictive_confinement_religion_by_region.csv"),
    title = paste0("OTIS c05 -- Aggregate RF: non-majority religion ",
                     "-> Individuals in RC"),
    interpretation = paste(
      "Aggregate RF on c05 testing religious-minority overrepresentation",
      "in RC, controlling for region and year."
    ))
}

#' c06 aggregate Ruhela: Age 50+ -> RC
#' @param data c06 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c06 <- morie_synth_otis("c06", n = 120L, seed = 1L)
#' morie_otis_analyze_c06_ruhela_aggregate(otis_c06)
#' }
morie_otis_analyze_c06_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Age_Category", "Region_MostRecentPlacement",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c06_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_50plus <- .otis_age_50plus_indicator(work$Age_Category)
  .otis_aggregate_glm(work, treatment = "T_50plus",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Region_MostRecentPlacement",
    ds_id = "c06",
    source_label = paste0("c06_individuals_in_segregation_and_",
                            "restrictive_confinement_age_category_by_region.csv"),
    title = paste0("OTIS c06 -- Aggregate RF: Age 50+ -> ",
                     "Individuals in RC, by region"),
    interpretation = "Aggregate RF on c06: age-50+ overrepresentation by region.")
}

#' c07 aggregate Ruhela: Alert presence x Gender -> RC
#' @param data c07 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c07 <- morie_synth_otis("c07", n = 120L, seed = 1L)
#' morie_otis_analyze_c07_ruhela_aggregate(otis_c07)
#' }
morie_otis_analyze_c07_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Alert_Type", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c07_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  no_alert_strs <- c("no alert", "none", "no_alert", "no")
  work$T_alert_present <- as.integer(!(tolower(trimws(
    as.character(work$Alert_Type))) %in% no_alert_strs))
  if (sum(work$T_alert_present) == 0L ||
      sum(1L - work$T_alert_present) == 0L) {
    return(.otis_wrap("c07 aggregate Ruhela", list(),
                      warnings = paste(
                        "c07 has no no-alert reference rows --",
                        "treatment indicator is degenerate")))
  }
  .otis_aggregate_glm(work, treatment = "T_alert_present",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = c("Alert_Type", "Gender"),
    ds_id = "c07",
    source_label = paste0("c07_individuals_in_segregation_and_",
                            "restrictive_confinement_alerts_and_hold_flags.csv"),
    title = paste0("OTIS c07 -- Aggregate RF: Alert presence x ",
                     "Gender -> Individuals in RC"),
    interpretation = paste(
      "Aggregate RF on c07 -- population-level counterpart of the",
      "canonical (T_high_ac, vm) per-row formulation on a01/b01."
    ))
}

#' c08 aggregate Ruhela: non-majority religion x gender -> RC
#' @param data c08 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c08 <- morie_synth_otis("c08", n = 120L, seed = 1L)
#' morie_otis_analyze_c08_ruhela_aggregate(otis_c08)
#' }
morie_otis_analyze_c08_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Religion", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c08_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_minority_religion <- .otis_minority_religion_indicator(
    work$Religion)
  .otis_aggregate_glm(work, treatment = "T_minority_religion",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Gender",
    ds_id = "c08",
    source_label = paste0("c08_individuals_in_segregation_and_",
                            "restrictive_confinement_religion_by_gender.csv"),
    title = paste0("OTIS c08 -- Aggregate RF: non-majority religion ",
                     "x Gender -> Individuals in RC"),
    interpretation = "Aggregate RF on c08, parallel to c05 with gender control.")
}

#' c09 aggregate Ruhela: Age 50+ x gender -> RC
#' @param data c09 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c09 <- morie_synth_otis("c09", n = 120L, seed = 1L)
#' morie_otis_analyze_c09_ruhela_aggregate(otis_c09)
#' }
morie_otis_analyze_c09_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Age_Category", "Gender",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c09_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_50plus <- .otis_age_50plus_indicator(work$Age_Category)
  .otis_aggregate_glm(work, treatment = "T_50plus",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    covariates = "Gender",
    ds_id = "c09",
    source_label = paste0("c09_individuals_in_segregation_and_",
                            "restrictive_confinement_age_category_by_gender.csv"),
    title = paste0("OTIS c09 -- Aggregate RF: Age 50+ -> ",
                     "Individuals in RC, by gender"),
    interpretation = "Aggregate RF on c09: age-50+ overrepresentation by gender.")
}

#' c10 aggregate Ruhela: Female -> median RC days (institution GEE)
#' @param data c10 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c10 <- morie_synth_otis("c10", n = 120L, seed = 1L)
#' morie_otis_analyze_c10_ruhela_aggregate(otis_c10)
#' }
morie_otis_analyze_c10_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Region_MostRecentPlacement",
            "Institution_MostRecentPlacement", "Gender", "Measure",
            "TotalAggregatedDays_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c10_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work <- work[trimws(as.character(work$Measure)) == "Median", ,
               drop = FALSE]
  if (nrow(work) == 0L)
    return(.otis_wrap("c10 aggregate Ruhela", list(),
                      warnings = "c10 has no Median rows after filter"))
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "TotalAggregatedDays_RestrictiveConfinement",
    covariates = "Region_MostRecentPlacement",
    cluster_group = "Institution_MostRecentPlacement",
    ds_id = "c10",
    source_label = paste0("c10_segregation_and_restrictive_",
                            "confinement_aggregate_durations_by_institution.csv"),
    title = paste0("OTIS c10 -- Aggregate RF: Female -> ",
                     "median RC days, institution-clustered"),
    interpretation = "Aggregate RF on c10: gender disparity in median RC days.")
}

#' c11 aggregate Ruhela: long-duration bin (>=16 days) -> RC
#' @param data c11 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c11 <- morie_synth_otis("c11", n = 120L, seed = 1L)
#' morie_otis_analyze_c11_ruhela_aggregate(otis_c11)
#' }
morie_otis_analyze_c11_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Aggregate_Duration",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c11_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  long_bins <- c("16 to 20 days", "21 to 25 days", "26 to 30 days",
                 "Greater than 30 days")
  work$T_long_duration <- as.integer(
    as.character(work$Aggregate_Duration) %in% long_bins)
  .otis_aggregate_glm(work, treatment = "T_long_duration",
    outcome = "NumberIndividuals_RestrictiveConfinement",
    ds_id = "c11",
    source_label = paste0("c11_individuals_in_segregation_and_",
                            "restrictive_confinement_aggregate_lengths.csv"),
    title = paste0("OTIS c11 -- Aggregate RF: Long-duration bin ",
                     "(>=16 days) -> Individuals in RC"),
    interpretation = paste(
      "Aggregate RF on c11 testing whether long-duration bins",
      "(>=16 days) concentrate more individuals in RC."
    ))
}

#' c12 aggregate Ruhela: Female -> median RC days (by region)
#' @param data c12 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c12 <- morie_synth_otis("c12", n = 120L, seed = 1L)
#' morie_otis_analyze_c12_ruhela_aggregate(otis_c12)
#' }
morie_otis_analyze_c12_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("EndFiscalYear", "Region_MostRecentPlacement", "Gender",
            "Measure", "TotalAggregatedDays_RestrictiveConfinement")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("c12_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work <- work[trimws(as.character(work$Measure)) == "Median", ,
               drop = FALSE]
  if (nrow(work) == 0L)
    return(.otis_wrap("c12 aggregate Ruhela", list(),
                      warnings = "c12 has no Median rows after filter"))
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "TotalAggregatedDays_RestrictiveConfinement",
    covariates = "Region_MostRecentPlacement",
    ds_id = "c12",
    source_label = paste0("c12_segregation_and_restrictive_",
                            "confinement_aggregate_durations_by_region.csv"),
    title = paste0("OTIS c12 -- Aggregate RF: Female -> ",
                     "median RC days, by region"),
    interpretation = "Aggregate RF on c12: region-level companion to c10.")
}

#' d02 aggregate Ruhela: Female -> custodial deaths
#' @param data d02 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_d02 <- morie_synth_otis("d02", n = 120L, seed = 1L)
#' morie_otis_analyze_d02_ruhela_aggregate(otis_d02)
#' }
morie_otis_analyze_d02_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("Year", "Gender", "Number_CustodialDeaths")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("d02_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_female <- .otis_female_indicator(work$Gender)
  .otis_aggregate_glm(work, treatment = "T_female",
    outcome = "Number_CustodialDeaths", year_col = "Year",
    ds_id = "d02",
    source_label = "d02_deaths_in_custody_gender.csv",
    title = paste0("OTIS d02 -- Aggregate RF: Female -> ",
                     "Number of custodial deaths"),
    interpretation = paste(
      "Aggregate RF on d02: gender disparity in custodial death",
      "counts (small N -- expect wide CI)."
    ))
}

#' d03 aggregate Ruhela: Indigenous -> custodial deaths
#' @param data d03 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_d03 <- morie_synth_otis("d03", n = 120L, seed = 1L)
#' morie_otis_analyze_d03_ruhela_aggregate(otis_d03)
#' }
morie_otis_analyze_d03_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("Year", "Race", "Number_CustodialDeaths")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("d03_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_indigenous <- .otis_indigenous_indicator(work$Race)
  .otis_aggregate_glm(work, treatment = "T_indigenous",
    outcome = "Number_CustodialDeaths", year_col = "Year",
    ds_id = "d03",
    source_label = "d03_deaths_in_custody_race.csv",
    title = paste0("OTIS d03 -- Aggregate RF: Indigenous -> ",
                     "Number of custodial deaths"),
    interpretation = paste(
      "Aggregate RF on d03: Indigenous overrepresentation in custodial",
      "deaths. Small-sample warning."
    ))
}

#' d04 aggregate Ruhela: non-majority religion -> custodial deaths
#' @param data d04 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_d04 <- morie_synth_otis("d04", n = 120L, seed = 1L)
#' morie_otis_analyze_d04_ruhela_aggregate(otis_d04)
#' }
morie_otis_analyze_d04_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("Year", "Religion", "Number_CustodialDeaths")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("d04_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_minority_religion <- .otis_minority_religion_indicator(
    work$Religion)
  .otis_aggregate_glm(work, treatment = "T_minority_religion",
    outcome = "Number_CustodialDeaths", year_col = "Year",
    ds_id = "d04",
    source_label = "d04_deaths_in_custody_religion.csv",
    title = paste0("OTIS d04 -- Aggregate RF: Non-majority religion ",
                     "-> Number of custodial deaths"),
    interpretation = "Aggregate RF on d04: religious-minority death counts.")
}

#' d05 aggregate Ruhela: Age 50+ -> custodial deaths
#' @param data d05 data.frame.
#' @param out_dir Optional.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_d05 <- morie_synth_otis("d05", n = 120L, seed = 1L)
#' morie_otis_analyze_d05_ruhela_aggregate(otis_d05)
#' }
morie_otis_analyze_d05_ruhela_aggregate <- function(data, out_dir = NULL) {
  need <- c("Year", "Age_Category", "Number_CustodialDeaths")
  if (!all(need %in% names(data)))
    return(.otis_missing_columns("d05_ruhela_aggregate", data, need))
  work <- data[stats::complete.cases(data[, need, drop = FALSE]), ,
               drop = FALSE]
  work$T_50plus <- .otis_age_50plus_indicator(work$Age_Category)
  .otis_aggregate_glm(work, treatment = "T_50plus",
    outcome = "Number_CustodialDeaths", year_col = "Year",
    ds_id = "d05",
    source_label = "d05_deaths_in_custody_age_category.csv",
    title = paste0("OTIS d05 -- Aggregate RF: Age 50+ -> ",
                     "Number of custodial deaths"),
    interpretation = paste(
      "Aggregate RF on d05: age-50+ overrepresentation in custodial",
      "death counts; well-documented old-age mortality in custody."
    ))
}


# ---------------------------------------------------------------------------
# Alt-T per-row Ruhela formulations (a01 / b01 / b02)
# ---------------------------------------------------------------------------

#' a01 alt-T Ruhela: Female -> vm count
#'
#' The a01 person-year cell frame with T = Female and Y = regional-volatility
#' count, adjusting for age band and fiscal year; full DLRM ensemble.
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_alt_gender(a01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_a01_ruhela_alt_gender <- function(data = NULL, out_dir = NULL) {
  work <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "a01"))$data
  work$T_female <- .otis_female(work$Gender)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "a01-altG",
    source_label = paste0(
      "a01_restrictive_confinement_detailed_dataset.csv (alt-T: ",
      "Gender)"),
    title = paste0(
      "OTIS a01 -- Alt-T Ruhela formulation: T=Female -> Y=vm count ",
      "(regional volatility)"),
    interpretation = paste0(
      "Alternative-treatment Ruhela formulation on the a01 cell ",
      "frame. Same Y (vm count) as the canonical formulation; T ",
      "swapped to Female indicator. Tests whether gender alone ",
      "drives intra-year regional volatility."),
    treatment = "T_female",
    outcome = "Y_vm_count",
    covariates = c("Age_Category", "EndFiscalYear"))
  .otis_emit(res, out_dir, "a01-altG")
}

#' a01 alt-T Ruhela: Age 50+ -> vm count
#'
#' T = age band containing 50, Y = regional-volatility count, adjusting for
#' gender and fiscal year; full DLRM ensemble.
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_alt_age(a01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_a01_ruhela_alt_age <- function(data = NULL, out_dir = NULL) {
  work <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "a01"))$data
  work$T_50plus <- .otis_age50(work$Age_Category)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "a01-altA",
    source_label = paste0(
      "a01_restrictive_confinement_detailed_dataset.csv (alt-T: Age ",
      "50+)"),
    title = paste0(
      "OTIS a01 -- Alt-T Ruhela formulation: T=Age 50+ -> Y=vm ",
      "count"),
    interpretation = paste0(
      "Alternative-treatment Ruhela formulation: age 50+ as the ",
      "binary treatment. Tests whether older adults experience ",
      "different intra-year regional churn than younger adults."),
    treatment = "T_50plus",
    outcome = "Y_vm_count",
    covariates = c("Gender", "EndFiscalYear"))
  .otis_emit(res, out_dir, "a01-altA")
}

#' a01 alt-T Ruhela: Toronto region -> vm count
#'
#' T = placement region Toronto, Y = regional-volatility count, adjusting for
#' gender, age band and fiscal year; full DLRM ensemble.
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_alt_toronto(a01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_a01_ruhela_alt_toronto <- function(data = NULL, out_dir = NULL) {
  work <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "a01"))$data
  work$T_toronto <- .otis_toronto(work$regA)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "a01-altT",
    source_label = paste0(
      "a01_restrictive_confinement_detailed_dataset.csv (alt-T: ",
      "Toronto region)"),
    title = paste0(
      "OTIS a01 -- Alt-T Ruhela formulation: T=Toronto region -> ",
      "Y=vm count"),
    interpretation = paste0(
      "Alternative-treatment Ruhela formulation: Toronto-region ",
      "placement as the binary treatment. Tests whether the Toronto ",
      "region's institutional density translates into different ",
      "intra-year regional churn."),
    treatment = "T_toronto",
    outcome = "Y_vm_count",
    covariates = c("Gender", "Age_Category", "EndFiscalYear"))
  .otis_emit(res, out_dir, "a01-altT")
}

#' b01 alt-T Ruhela: Female -> vm count
#'
#' As \code{\link{morie_otis_analyze_a01_ruhela_alt_gender}}, on the b01 cell frame.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_alt_gender(b01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b01_ruhela_alt_gender <- function(data = NULL, out_dir = NULL) {
  work <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "b01"))$data
  work$T_female <- .otis_female(work$Gender)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b01-altG",
    source_label = "b01_segregation_detailed_dataset.csv (alt-T: Gender)",
    title = paste0(
      "OTIS b01 -- Alt-T Ruhela formulation: T=Female -> Y=vm count ",
      "(regional volatility)"),
    interpretation = paste0(
      "Alternative-treatment Ruhela formulation on the b01 cell ",
      "frame. Same Y (vm count) as canonical; T = Female indicator. ",
      "Tests whether gender alone drives intra-year regional ",
      "volatility on segregation records (vs the per-day RC records ",
      "of a01)."),
    treatment = "T_female",
    outcome = "Y_vm_count",
    covariates = c("Age_Category", "EndFiscalYear"))
  .otis_emit(res, out_dir, "b01-altG")
}

#' b01 alt-T Ruhela: Age 50+ -> vm count
#'
#' As \code{\link{morie_otis_analyze_a01_ruhela_alt_age}}, on the b01 cell frame.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_alt_age(b01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b01_ruhela_alt_age <- function(data = NULL, out_dir = NULL) {
  work <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "b01"))$data
  work$T_50plus <- .otis_age50(work$Age_Category)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b01-altA",
    source_label = "b01_segregation_detailed_dataset.csv (alt-T: Age 50+)",
    title = paste0(
      "OTIS b01 -- Alt-T Ruhela formulation: T=Age 50+ -> Y=vm ",
      "count"),
    interpretation = paste0(
      "Alt-T RF on b01: age 50+ as binary treatment. Tests whether ",
      "older adults experience different intra-year regional churn ",
      "than younger adults on segregation records."),
    treatment = "T_50plus",
    outcome = "Y_vm_count",
    covariates = c("Gender", "EndFiscalYear"))
  .otis_emit(res, out_dir, "b01-altA")
}

#' b01 alt-T Ruhela: Toronto region -> vm count
#'
#' As \code{\link{morie_otis_analyze_a01_ruhela_alt_toronto}}, on the b01 cell frame.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_alt_toronto(b01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b01_ruhela_alt_toronto <- function(data = NULL, out_dir = NULL) {
  work <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "b01"))$data
  work$T_toronto <- .otis_toronto(work$regA)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b01-altT",
    source_label = "b01_segregation_detailed_dataset.csv (alt-T: Toronto region)",
    title = paste0(
      "OTIS b01 -- Alt-T Ruhela formulation: T=Toronto region -> ",
      "Y=vm count"),
    interpretation = paste0(
      "Alt-T RF on b01: Toronto-region placement as binary ",
      "treatment. Tests whether Toronto's institutional density ",
      "translates into different intra-year regional churn on ",
      "segregation records."),
    treatment = "T_toronto",
    outcome = "Y_vm_count",
    covariates = c("Gender", "Age_Category", "EndFiscalYear"))
  .otis_emit(res, out_dir, "b01-altT")
}

#' b02 alt-T Ruhela: Toronto region -> seg-day count
#'
#' T = most recent placement region Toronto, Y = TotalAggregatedDays_Segregation,
#' adjusting for gender, age band and fiscal year; full DLRM ensemble.
#'
#' @param data The OTIS b02 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b02 <- morie_synth_otis("b02", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b02_ruhela_alt_region(b02)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b02_ruhela_alt_region <- function(data = NULL, out_dir = NULL) {
  work <- .otis_ruhela_input(data, "b02")
  need <- c("Gender", "TotalAggregatedDays_Segregation",
            "Region_MostRecentPlacement", "Age_Category", "EndFiscalYear")
  .otis_require_cols(work, need, "b02")
  work <- work[stats::complete.cases(work[, need, drop = FALSE]), , drop = FALSE]
  work$T_toronto <- .otis_toronto(work$Region_MostRecentPlacement)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b02-altR",
    source_label = paste0(
      "b02_segregation_detailed_total_days.csv (alt-T: Toronto ",
      "region)"),
    title = paste0(
      "OTIS b02 -- Alt-T Ruhela formulation: T=Toronto region -> ",
      "Y=Total seg days within FY"),
    interpretation = paste0(
      "Alt-T RF on b02: Toronto-region indicator. Tests whether ",
      "Toronto's institutional density translates into different ",
      "seg-day burden than other regions, controlling for gender, ",
      "age, and year."),
    treatment = "T_toronto",
    outcome = "TotalAggregatedDays_Segregation",
    covariates = c("Gender", "Age_Category", "EndFiscalYear"))
  .otis_emit(res, out_dir, "b02-altR")
}

#' b02 alt-T Ruhela: Age 50+ -> seg-day count
#'
#' T = age band containing 50, Y = TotalAggregatedDays_Segregation, adjusting
#' for gender, region and fiscal year; full DLRM ensemble.
#'
#' @param data The OTIS b02 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b02 <- morie_synth_otis("b02", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b02_ruhela_alt_age(b02)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b02_ruhela_alt_age <- function(data = NULL, out_dir = NULL) {
  work <- .otis_ruhela_input(data, "b02")
  need <- c("Gender", "TotalAggregatedDays_Segregation",
            "Region_MostRecentPlacement", "Age_Category", "EndFiscalYear")
  .otis_require_cols(work, need, "b02")
  work <- work[stats::complete.cases(work[, need, drop = FALSE]), , drop = FALSE]
  work$T_50plus <- .otis_age50(work$Age_Category)
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b02-altA",
    source_label = "b02_segregation_detailed_total_days.csv (alt-T: Age 50+)",
    title = paste0(
      "OTIS b02 -- Alt-T Ruhela formulation: T=Age 50+ -> Y=Total ",
      "seg days within FY"),
    interpretation = paste0(
      "Alt-T RF on b02: Age 50+ indicator. Tests whether older ",
      "adults experience different seg-day burden than younger ",
      "adults, controlling for gender, region, and year."),
    treatment = "T_50plus",
    outcome = "TotalAggregatedDays_Segregation",
    covariates = c("Gender", "Region_MostRecentPlacement", "EndFiscalYear"))
  .otis_emit(res, out_dir, "b02-altA")
}

# ---------------------------------------------------------------------------
# Subgroup Ruhela formulations (effect heterogeneity by gender)
# ---------------------------------------------------------------------------

#' a01 subgroup Ruhela: Female-only cell frame
#'
#' The canonical formulation (T = alert complexity >= 2, Y = vm count) on
#' female person-years only, without the gender covariate. Fewer than 100
#' female person-years returns a result carrying that warning instead.
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_subgroup_female(a01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_a01_ruhela_subgroup_female <- function(data = NULL, out_dir = NULL) {
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "a01"))
  work <- pair$data[.otis_female(pair$data$Gender) == 1L, , drop = FALSE]
  if (nrow(work) < 100L) {
    return(.otis_wrap(title = "a01 subgroup Female Ruhela", summary_lines = list(),
                      warnings = sprintf("only %d female cells; too few", nrow(work))))
  }
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "a01-subF",
    source_label = paste0(
      "a01_restrictive_confinement_detailed_dataset.csv (subgroup: ",
      "Female)"),
    title = paste0(
      "OTIS a01 -- Subgroup Ruhela formulation: Female-only cell ",
      "frame, T_high_ac -> vm count"),
    interpretation = paste0(
      "Subgroup analysis: canonical Ruhela formulation restricted ",
      "to female cells. Compare to ",
      "morie_otis_analyze_a01_ruhela_subgroup_male for ",
      "effect-heterogeneity-by-gender. If the female ATE is smaller ",
      "/ larger than the male ATE, alert-complexity-driven regional ",
      "volatility is gender-conditional."),
    treatment = pair$T,
    outcome = pair$Y,
    covariates = setdiff(pair$covariates, "Gender"))
  .otis_emit(res, out_dir, "a01-subF")
}

#' a01 subgroup Ruhela: Male-only cell frame
#'
#' The canonical formulation on male (non-female) person-years only, without
#' the gender covariate.
#'
#' @param data The OTIS a01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_a01_ruhela_subgroup_male(a01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_a01_ruhela_subgroup_male <- function(data = NULL, out_dir = NULL) {
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "a01"))
  work <- pair$data[.otis_female(pair$data$Gender) == 0L, , drop = FALSE]
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "a01-subM",
    source_label = paste0(
      "a01_restrictive_confinement_detailed_dataset.csv (subgroup: ",
      "Male)"),
    title = paste0(
      "OTIS a01 -- Subgroup Ruhela formulation: Male-only cell ",
      "frame, T_high_ac -> vm count"),
    interpretation = paste0(
      "Subgroup analysis: canonical Ruhela formulation restricted ",
      "to male cells. Companion to ",
      "morie_otis_analyze_a01_ruhela_subgroup_female for ",
      "effect-heterogeneity-by-gender."),
    treatment = pair$T,
    outcome = pair$Y,
    covariates = setdiff(pair$covariates, "Gender"))
  .otis_emit(res, out_dir, "a01-subM")
}

#' b01 subgroup Ruhela: Female-only cell frame
#'
#' As \code{\link{morie_otis_analyze_a01_ruhela_subgroup_female}}, on b01.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_subgroup_female(b01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b01_ruhela_subgroup_female <- function(data = NULL, out_dir = NULL) {
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "b01"))
  work <- pair$data[.otis_female(pair$data$Gender) == 1L, , drop = FALSE]
  if (nrow(work) < 100L) {
    return(.otis_wrap(title = "b01 subgroup Female Ruhela", summary_lines = list(),
                      warnings = sprintf("only %d female cells; too few", nrow(work))))
  }
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b01-subF",
    source_label = "b01_segregation_detailed_dataset.csv (subgroup: Female)",
    title = paste0(
      "OTIS b01 -- Subgroup Ruhela formulation: Female-only cell ",
      "frame, T_high_ac -> vm count"),
    interpretation = paste0(
      "Subgroup analysis on b01 (segregation records, ",
      "per-placement). Compare to ",
      "morie_otis_analyze_a01_ruhela_subgroup_female to see whether ",
      "the gender-conditional pattern in alert-complexity -> vm ",
      "holds across the two unit-of-analysis variants (per-day RC ",
      "vs per-placement seg)."),
    treatment = pair$T,
    outcome = pair$Y,
    covariates = setdiff(pair$covariates, "Gender"))
  .otis_emit(res, out_dir, "b01-subF")
}

#' b01 subgroup Ruhela: Male-only cell frame
#'
#' As \code{\link{morie_otis_analyze_a01_ruhela_subgroup_male}}, on b01.
#'
#' @param data The OTIS b01 table; \code{NULL} loads it.
#' @param out_dir Optional directory for the text and JSON rendering.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' b01 <- morie_synth_otis("b01", n = 300L, seed = 1L)
#' r <- morie_otis_analyze_b01_ruhela_subgroup_male(b01)
#' r$tables[[1L]]$rows[[1L]]
#' }
morie_otis_analyze_b01_ruhela_subgroup_male <- function(data = NULL, out_dir = NULL) {
  pair <- morie_otis_make_pair_alert_to_volatility_ruhela(.otis_ruhela_input(data, "b01"))
  work <- pair$data[.otis_female(pair$data$Gender) == 0L, , drop = FALSE]
  res <- .otis_ruhela_formulations_on(
    work,
    ds_id = "b01-subM",
    source_label = "b01_segregation_detailed_dataset.csv (subgroup: Male)",
    title = paste0(
      "OTIS b01 -- Subgroup Ruhela formulation: Male-only cell ",
      "frame, T_high_ac -> vm count"),
    interpretation = paste0(
      "Male-only subgroup analysis on b01. Companion to ",
      "morie_otis_analyze_b01_ruhela_subgroup_female for ",
      "effect-heterogeneity-by-gender on segregation records."),
    treatment = pair$T,
    outcome = pair$Y,
    covariates = setdiff(pair$covariates, "Gender"))
  .otis_emit(res, out_dir, "b01-subM")
}

# ---------------------------------------------------------------------------
# Mandela classification (Mandela-RF)
# ---------------------------------------------------------------------------
#
# Faithfully ported -- pure data-shaping, no estimator stack required.

.otis_mandela_solitary_bins <- c(
  "1 day", "2 days", "3 days", "4 days", "5 days",
  "6 to 10 days", "11 to 15 days"
)
.otis_mandela_torture_bins <- c(
  "16 to 20 days", "21 to 25 days", "26 to 30 days",
  "Greater than 30 days"
)

#' Internal helper: Otis Classify Bin
#' @noRd
.otis_classify_bin <- function(x) {
  s <- as.character(x)
  out <- rep("Unknown", length(s))
  out[s %in% .otis_mandela_solitary_bins] <- "Solitary Confinement (<=15d)"
  out[s %in% .otis_mandela_torture_bins]  <- "Torture (>=16d)"
  out
}

#' Mandela-RF on b05 -- per-placement Mandela classification by year
#'
#' Applies the Sprott-Doob 15-day Mandela threshold to OTIS b05
#' (Ontario provincial segregation placement counts by binned
#' duration).
#'
#' @param data b05 data.frame.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b05 <- morie_synth_otis("b05", n = 120L, seed = 1L)
#' morie_otis_analyze_b05_mandela_classification(otis_b05)
#' }
morie_otis_analyze_b05_mandela_classification <- function(data,
                                                            out_dir = NULL) {
  need <- c("EndFiscalYear", "Consecutive_Duration",
            "Number_SegregationPlacements")
  if (!all(need %in% names(data))) {
    return(.otis_wrap("b05 Mandela classification", list(),
      warnings = paste("b05 missing required columns:",
                        paste(setdiff(need, names(data)),
                              collapse = ", "))))
  }
  d <- data
  d$mandela_class <- .otis_classify_bin(d$Consecutive_Duration)
  d[["Number_SegregationPlacements"]] <- suppressWarnings(
    as.numeric(d[["Number_SegregationPlacements"]]))

  years <- sort(unique(suppressWarnings(
    as.integer(d$EndFiscalYear))))
  rows <- list()
  for (yr in years) {
    if (is.na(yr)) next
    sub <- d[suppressWarnings(as.integer(d$EndFiscalYear)) == yr, ,
            drop = FALSE]
    n_sol <- as.integer(sum(sub[["Number_SegregationPlacements"]][
      sub$mandela_class == "Solitary Confinement (<=15d)"],
      na.rm = TRUE))
    n_tor <- as.integer(sum(sub[["Number_SegregationPlacements"]][
      sub$mandela_class == "Torture (>=16d)"], na.rm = TRUE))
    total <- n_sol + n_tor
    if (total == 0L) next
    sol_pct <- 100 * n_sol / total
    tor_pct <- 100 * n_tor / total
    rows[[length(rows) + 1L]] <- c(yr, n_sol,
      sprintf("%.1f%%", sol_pct), n_tor,
      sprintf("%.1f%%", tor_pct), total)
  }

  sd_fed_sol_pct <- 28.4
  sd_fed_tor_pct <- 9.9
  sd_fed_n <- 1960
  prov_tor_pct <- if (length(rows) > 0L) {
    as.numeric(sub("%", "", rows[[length(rows)]][5]))
  } else NA_real_

  .otis_emit(
    .otis_wrap(
      title = paste0("Mandela-RF on OTIS b05 -- per-placement Mandela ",
                       "classification (Ontario provincial segregation, ",
                       "by fiscal year)"),
      summary_lines = list(
        "Source" = "OTIS b05 (segregation placement counts x duration)",
        "Mandela threshold" = paste("Rule 43 -- 15 days (<=15 = solitary;",
                                        ">=16 = torture)"),
        "Caveat" = paste("Duration-only proxy (no hours-out-of-cell in",
                            "OTIS); treat as upper bound"),
        "Federal SD-2021 reference (CSC SIUs N=1960)" = sprintf(
          "Solitary %s%%, Torture %s%%",
          sd_fed_sol_pct, sd_fed_tor_pct),
        "Latest provincial torture rate" = if (is.na(prov_tor_pct))
          "n/a" else sprintf("%.2f%% -- vs federal %s%%; %s",
                              prov_tor_pct, sd_fed_tor_pct,
                              if (prov_tor_pct > sd_fed_tor_pct)
                                "higher" else "lower")
      ),
      tables = list(list(
        title = paste0("OTIS b05 Mandela-class by fiscal year ",
                         "(placements as the unit):"),
        headers = c("Fiscal year", "Solitary N", "Solitary %",
                      "Torture N", "Torture %", "Total"),
        rows = rows
      )),
      interpretation = paste(
        "Applies the Sprott-Doob Mandela-Rules duration threshold",
        "(15 days, Rule 43) to OTIS provincial segregation placements.",
        "The 'torture' bin (>=16 days) counts placements crossing the",
        "prolonged-solitary line."
      ),
      payload = list(
        rows = rows,
        federal_reference = list(solitary_pct = sd_fed_sol_pct,
                                  torture_pct = sd_fed_tor_pct,
                                  n = sd_fed_n)
      )
    ),
    out_dir, "b05_mandela_classification")
}

#' Mandela-RF on c11 -- per-individual Mandela classification by year
#'
#' Applies the 15-day threshold to OTIS c11 (Ontario provincial counts
#' of INDIVIDUALS by binned aggregate duration). Reports both
#' restrictive-confinement and segregation-only views.
#'
#' @param data c11 data.frame.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c11 <- morie_synth_otis("c11", n = 120L, seed = 1L)
#' morie_otis_analyze_c11_mandela_classification(otis_c11)
#' }
morie_otis_analyze_c11_mandela_classification <- function(data,
                                                            out_dir = NULL) {
  need <- c("EndFiscalYear", "Aggregate_Duration",
            "NumberIndividuals_Segregation",
            "NumberIndividuals_RestrictiveConfinement")
  if (!all(need %in% names(data))) {
    return(.otis_wrap("c11 Mandela classification", list(),
      warnings = paste("c11 missing required columns:",
                        paste(setdiff(need, names(data)),
                              collapse = ", "))))
  }
  d <- data
  d$mandela_class <- .otis_classify_bin(d$Aggregate_Duration)
  d[["NumberIndividuals_Segregation"]] <- suppressWarnings(
    as.numeric(d[["NumberIndividuals_Segregation"]]))
  d[["NumberIndividuals_RestrictiveConfinement"]] <- suppressWarnings(
    as.numeric(d[["NumberIndividuals_RestrictiveConfinement"]]))

  years <- sort(unique(suppressWarnings(
    as.integer(d$EndFiscalYear))))
  rows <- list()
  kind_specs <- list(
    list(col = "NumberIndividuals_Segregation",
         lbl = "Segregation"),
    list(col = "NumberIndividuals_RestrictiveConfinement",
         lbl = "Restrictive confinement"))
  for (yr in years) {
    if (is.na(yr)) next
    sub <- d[suppressWarnings(as.integer(d$EndFiscalYear)) == yr, ,
            drop = FALSE]
    for (ks in kind_specs) {
      n_sol <- as.integer(sum(sub[[ks$col]][
        sub$mandela_class == "Solitary Confinement (<=15d)"],
        na.rm = TRUE))
      n_tor <- as.integer(sum(sub[[ks$col]][
        sub$mandela_class == "Torture (>=16d)"], na.rm = TRUE))
      total <- n_sol + n_tor
      if (total == 0L) next
      sol_pct <- 100 * n_sol / total
      tor_pct <- 100 * n_tor / total
      rows[[length(rows) + 1L]] <- c(yr, ks$lbl,
        n_sol, sprintf("%.1f%%", sol_pct),
        n_tor, sprintf("%.1f%%", tor_pct), total)
    }
  }

  .otis_emit(
    .otis_wrap(
      title = paste0("Mandela-RF on OTIS c11 -- per-individual Mandela ",
                       "classification (Ontario provincial restrictive-",
                       "confinement & segregation, by fiscal year)"),
      summary_lines = list(
        "Source" = "OTIS c11 (individuals x aggregate duration)",
        "Mandela threshold" = paste("Rule 43 -- 15 days",
                                        "(<=15 = solitary; >=16 = torture)"),
        "Two views" = paste("Segregation (closer match to federal SIU);",
                                "Restrictive Confinement (broader Ontario)"),
        "Caveat" = "Duration-only proxy -- no hours-out-of-cell in OTIS",
        "Federal SD-2021 reference (N=1960)" =
          "Solitary 28.4%, Torture 9.9%"
      ),
      tables = list(list(
        title = paste0("OTIS c11 Mandela-class by year x confinement ",
                         "type (individuals as the unit):"),
        headers = c("Year", "Type", "Solitary N", "Solitary %",
                      "Torture N", "Torture %", "Total"),
        rows = rows
      )),
      interpretation = paste(
        "Per-individual Mandela classification on OTIS c11. The",
        "'Segregation' view is most directly comparable to the",
        "Sprott-Doob federal SIU classification (28.4% solitary,",
        "9.9% torture across N=1960)."
      ),
      payload = list(rows = rows)
    ),
    out_dir, "c11_mandela_classification")
}

#' Mandela-RF cross-comparison: Ontario provincial vs federal SIU
#'
#' Cross-references the c11 Mandela classification against the
#' Sprott-Doob Feb 2021 federal SIU figures (Table 19, N=1960).
#'
#' @param data c11 data.frame (used to derive the provincial figures).
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c11 <- morie_synth_otis("c11", n = 120L, seed = 1L)
#' morie_otis_analyze_otis_mandela_provincial_vs_federal(otis_c11)
#' }
morie_otis_analyze_otis_mandela_provincial_vs_federal <- function(
        data, out_dir = NULL) {
  c11_r <- morie_otis_analyze_c11_mandela_classification(data, out_dir)
  if (!is.null(c11_r$warnings) && length(c11_r$warnings) > 0L
        && is.null(c11_r$payload$rows)) {
    return(c11_r)
  }
  fed_sol_pct <- 28.4
  fed_tor_pct <- 9.9
  fed_n <- 1960

  rows <- list()
  rows[[length(rows) + 1L]] <- c(
    "Federal SIUs (CSC, Sprott-Doob T19)",
    "Nov 2019 - Sept 2020", "Person-stays",
    sprintf("%s%%", fed_sol_pct), sprintf("%s%%", fed_tor_pct),
    fed_n)

  all_rows <- c11_r$payload$rows %||% list()
  seg_rows <- Filter(function(r) r[2] == "Segregation", all_rows)
  rc_rows  <- Filter(function(r) r[2] == "Restrictive confinement",
                     all_rows)
  for (r in seg_rows) {
    rows[[length(rows) + 1L]] <- c("Ontario Provincial Segregation",
      as.character(r[1]), "Individuals",
      r[4], r[6], r[7])
  }
  for (r in rc_rows) {
    rows[[length(rows) + 1L]] <- c("Ontario Provincial RC (broader)",
      as.character(r[1]), "Individuals",
      r[4], r[6], r[7])
  }

  if (length(seg_rows) > 0L) {
    tor_pcts <- vapply(seg_rows, function(r)
      as.numeric(sub("%", "", r[6])), numeric(1))
    max_tor_pct <- max(tor_pcts, na.rm = TRUE)
    max_tor_year <- seg_rows[[which(abs(tor_pcts - max_tor_pct) < 0.01)[1]]][1]
    gap_pp <- max_tor_pct - fed_tor_pct
  } else {
    max_tor_pct <- NA_real_
    max_tor_year <- NA
    gap_pp <- NA_real_
  }

  .otis_wrap(
    title = paste0("Mandela-RF cross-comparison -- Ontario provincial ",
                     "(OTIS) vs federal SIUs (Sprott-Doob T19)"),
    summary_lines = list(
      "Federal SIU reference" = sprintf(
        "Solitary %s%%, Torture %s%% (N=%d)",
        fed_sol_pct, fed_tor_pct, fed_n),
      "Provincial peak torture-rate (Segregation)" =
        if (is.na(max_tor_pct)) "n/a" else
          sprintf("%.1f%% in %s", max_tor_pct, max_tor_year),
      "Provincial-vs-federal gap (peak)" =
        if (is.na(gap_pp)) "n/a" else
          sprintf("%+.1f percentage points (%s provincially)",
                  gap_pp, if (gap_pp > 0) "higher" else "lower"),
      "Caveat" = paste("Federal: person-stays + full Mandela",
                          "operationalization; Provincial: individuals",
                          "+ duration only.")
    ),
    tables = list(list(
      title = "Federal vs Provincial Mandela classification:",
      headers = c("Source", "Period", "Unit",
                    "Solitary %", "Torture %", "N"),
      rows = rows
    )),
    interpretation = paste(
      "Cross-comparison of Mandela-Rules classifications -- federal",
      "SIUs (9.9% torture headline) vs Ontario provincial (OTIS c11).",
      "Note unit + operationalization differences; the comparison is",
      "directionally informative but not perfectly apples-to-apples."
    ),
    payload = list(
      federal = list(solitary_pct = fed_sol_pct,
                       torture_pct = fed_tor_pct, n = fed_n),
      max_provincial_torture_pct = max_tor_pct,
      max_provincial_torture_year = max_tor_year,
      gap_pp = gap_pp
    )
  )
}


# ---------------------------------------------------------------------------
# MRM chi-square family (analyze_c_chi2 / analyze_d_chi2)
# ---------------------------------------------------------------------------
#
# Faithfully ported via stats::chisq.test (+ Cramer's V).

#' Internal helper: Otis Chi2 Cramer
#' @noRd
.otis_chi2_cramer <- function(tbl) {
  if (length(tbl) == 0L || sum(tbl) == 0L)
    return(list(chi2 = NA_real_, dof = 0L, pvalue = NA_real_,
                cramer_v = NA_real_, n = 0L, min_cell = 0L))
  n <- sum(tbl)
  min_cell <- as.integer(min(tbl))
  ct <- tryCatch(
    suppressWarnings(stats::chisq.test(tbl)),
    error = function(e) e
  )
  if (inherits(ct, "error")) {
    return(list(chi2 = NA_real_, dof = 0L, pvalue = NA_real_,
                cramer_v = NA_real_, n = as.integer(n),
                min_cell = min_cell,
                error = conditionMessage(ct)))
  }
  denom <- n * (min(dim(tbl)) - 1L)
  v <- if (denom > 0L) sqrt(as.numeric(ct$statistic) / denom) else NA_real_
  min_exp <- if (!is.null(ct$expected)) min(ct$expected) else NA_real_
  list(chi2 = as.numeric(ct$statistic),
       dof = as.integer(ct$parameter),
       pvalue = as.numeric(ct$p.value),
       cramer_v = v,
       n = as.integer(n),
       min_cell = min_cell,
       min_expected = min_exp)
}

#' Internal helper: Otis Contingency Chi2
#' @noRd
.otis_contingency_chi2 <- function(df, row, col, value) {
  if (!all(c(row, col, value) %in% names(df)))
    return(list(table = NULL,
                stats = list(error = "missing columns",
                              chi2 = NA_real_, dof = 0L,
                              pvalue = NA_real_, cramer_v = NA_real_,
                              n = 0L, min_cell = 0L)))
  g <- df[!is.na(df[[row]]) & !is.na(df[[col]]) &
          !is.na(df[[value]]), , drop = FALSE]
  g[[value]] <- suppressWarnings(as.numeric(g[[value]]))
  g <- g[!is.na(g[[value]]), , drop = FALSE]
  if (nrow(g) == 0L)
    return(list(table = NULL,
                stats = .otis_chi2_cramer(matrix(0, 1, 1))))
  pivot <- stats::xtabs(
    stats::as.formula(sprintf("v ~ `%s` + `%s`", row, col)),
    data = data.frame(g[, c(row, col), drop = FALSE], v = g[[value]]))
  tbl <- as.matrix(pivot)
  list(table = tbl, stats = .otis_chi2_cramer(tbl))
}

#' MRM chi-square family on c-series
#'
#' Pearson chi-square + Cramer's V on every meaningful 2-way slice of
#' the c-series datasets. Honour to Prof. Doob's chi-square tradition
#' in Canadian corrections research.
#'
#' @param datasets Named list of c-series data.frames
#'   (e.g. \code{list(c03 = otis_c03, c04 = otis_c04, ...)}).
#' @param contingency_value Count column to pivot on
#'   (default \code{"NumberIndividuals_RestrictiveConfinement"}).
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_c03 <- morie_synth_otis("c03", n = 120L, seed = 1L)
#' otis_c04 <- morie_synth_otis("c04", n = 120L, seed = 1L)
#' morie_otis_analyze_c_chi2(list(c03 = otis_c03, c04 = otis_c04))
#' }
morie_otis_analyze_c_chi2 <- function(datasets,
        contingency_value = "NumberIndividuals_RestrictiveConfinement",
        out_dir = NULL) {
  stopifnot(is.list(datasets))
  slices <- list(
    list(ds = "c03", row = "Race", col = "Gender"),
    list(ds = "c04", row = "Race", col = "Region_MostRecentPlacement"),
    list(ds = "c05", row = "Religion",
         col = "Region_MostRecentPlacement"),
    list(ds = "c06", row = "Age_Category",
         col = "Region_MostRecentPlacement"),
    list(ds = "c07", row = "Alert_Type", col = "Gender"),
    list(ds = "c08", row = "Religion", col = "Gender"),
    list(ds = "c09", row = "Age_Category", col = "Gender")
  )
  rows <- list()
  payloads <- list()
  for (sl in slices) {
    df <- datasets[[sl$ds]]
    if (is.null(df)) {
      rows[[length(rows) + 1L]] <- c(sl$ds,
        sprintf("%s x %s", sl$row, sl$col),
        0L, "n/a", 0L, "n/a", "n/a", 0L)
      next
    }
    cc <- .otis_contingency_chi2(df, row = sl$row, col = sl$col,
                                  value = contingency_value)
    st <- cc$stats
    rows[[length(rows) + 1L]] <- c(sl$ds,
      sprintf("%s x %s", sl$row, sl$col),
      as.integer(st$n %||% 0L),
      if (is.finite(st$chi2 %||% NA_real_))
        round(st$chi2, 3) else "n/a",
      as.integer(st$dof %||% 0L),
      if (is.finite(st$pvalue %||% NA_real_))
        sprintf("%.2e", st$pvalue) else "n/a",
      if (is.finite(st$cramer_v %||% NA_real_))
        round(st$cramer_v, 4) else "n/a",
      as.integer(st$min_cell %||% 0L))
    payloads[[sl$ds]] <- list(row = sl$row, col = sl$col, stats = st)
  }

  .otis_emit(
    .otis_wrap(
      title = paste0("OTIS c-series -- chi^2 + Cramer's V family on ",
                       "demographic contingency tables"),
      summary_lines = list(
        "Contingency value column" = contingency_value,
        "Slices tested" = length(rows)
      ),
      tables = list(list(
        title = sprintf(
          "Contingency chi^2 and Cramer's V on c-series slices (value = %s):",
          contingency_value),
        headers = c("Dataset", "Slice", "n",
                      "chi^2", "dof", "p", "Cramer's V", "min cell"),
        rows = rows
      )),
      interpretation = paste(
        "Cramer's V is the canonical effect-size measure for chi^2",
        "independence on 2-way contingency tables: V~0 -> independence,",
        "V->1 -> strong association. p<.05 with V<0.1 ==>",
        "statistically detectable but practically tiny."
      ),
      payload = list(slices = payloads,
                     contingency_value = contingency_value)
    ),
    out_dir, "c_chi2")
}

#' MRM chi-square family on d-series
#'
#' Yearly trend (d01 Poisson CIs) + Alert x Cause / Housing
#' contingency chi^2 + Cramer's V on d06 / d07.
#'
#' @param datasets Named list with \code{d01}, \code{d06}, \code{d07}
#'   data.frames.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_d01 <- morie_synth_otis("d01", n = 120L, seed = 1L)
#' otis_d06 <- morie_synth_otis("d06", n = 120L, seed = 1L)
#' otis_d07 <- morie_synth_otis("d07", n = 120L, seed = 1L)
#' morie_otis_analyze_d_chi2(list(d01 = otis_d01,
#'                                  d06 = otis_d06, d07 = otis_d07))
#' }
morie_otis_analyze_d_chi2 <- function(datasets, out_dir = NULL) {
  stopifnot(is.list(datasets))
  d01 <- datasets$d01
  d06 <- datasets$d06
  d07 <- datasets$d07

  yearly_rows <- list()
  rr_text <- "n/a"
  yearly_payload <- list()
  yoy <- list()
  if (!is.null(d01) && "Year" %in% names(d01)) {
    y <- suppressWarnings(as.integer(d01$Year))
    y <- y[!is.na(y)]
    if (length(y) > 0L) {
      tab <- table(y)
      yrs <- as.integer(names(tab))
      for (i in seq_along(yrs)) {
        n_i <- as.integer(tab[i])
        lo <- if (n_i > 0L) stats::qchisq(0.025, 2 * n_i) / 2 else 0
        hi <- stats::qchisq(0.975, 2 * (n_i + 1)) / 2
        yearly_rows[[length(yearly_rows) + 1L]] <- c(
          yrs[i], n_i, sprintf("[%.1f, %.1f]", lo, hi))
        yearly_payload[[as.character(yrs[i])]] <- n_i
      }
      if (length(yrs) >= 2L) {
        first_y <- min(yrs)
        last_y <- max(yrs)
        n_first <- as.integer(tab[as.character(first_y)])
        n_last  <- as.integer(tab[as.character(last_y)])
        if (n_first > 0L) {
          rr <- n_last / n_first
          log_rr <- log(max(rr, 1e-9))
          se_log_rr <- sqrt(1 / max(n_first, 1L) +
                             1 / max(n_last, 1L))
          ci_lo <- exp(log_rr - 1.96 * se_log_rr)
          ci_hi <- exp(log_rr + 1.96 * se_log_rr)
          rr_text <- sprintf(
            "%.3f (%d/%d; n=%d/%d; 95%% CI [%.3f, %.3f])",
            rr, last_y, first_y, n_last, n_first, ci_lo, ci_hi)
          yoy <- list(first_year = first_y, last_year = last_y,
                       n_first = n_first, n_last = n_last,
                       rr = rr, ci95_low = ci_lo, ci95_high = ci_hi)
        }
      }
    }
  }

  stats_d06 <- if (!is.null(d06))
    .otis_contingency_chi2(d06, "Alert_Type", "MedicalCauseOfDeath",
                            "Number_CustodialDeaths")$stats
    else list(chi2 = NA_real_, dof = 0L, pvalue = NA_real_,
              cramer_v = NA_real_, n = 0L, min_cell = 0L)
  stats_d07 <- if (!is.null(d07))
    .otis_contingency_chi2(d07, "Alert_Type", "Housing_Type",
                            "Number_CustodialDeaths")$stats
    else list(chi2 = NA_real_, dof = 0L, pvalue = NA_real_,
              cramer_v = NA_real_, n = 0L, min_cell = 0L)

  chi_rows <- list()
  for (sp in list(list(ds = "d06", lbl = "Alert x MedicalCause",
                         st = stats_d06),
                  list(ds = "d07", lbl = "Alert x Housing_Type",
                         st = stats_d07))) {
    st <- sp$st
    chi_rows[[length(chi_rows) + 1L]] <- c(sp$ds, sp$lbl,
      as.integer(st$n %||% 0L),
      if (is.finite(st$chi2 %||% NA_real_))
        round(st$chi2, 3) else "n/a",
      as.integer(st$dof %||% 0L),
      if (is.finite(st$pvalue %||% NA_real_))
        sprintf("%.2e", st$pvalue) else "n/a",
      if (is.finite(st$cramer_v %||% NA_real_))
        round(st$cramer_v, 4) else "n/a",
      as.integer(st$min_cell %||% 0L))
  }

  total_n <- if (length(yearly_payload) > 0L)
    sum(unlist(yearly_payload)) else 0L
  year_range <- if (length(yearly_payload) > 0L)
    sprintf("%d-%d", min(as.integer(names(yearly_payload))),
            max(as.integer(names(yearly_payload)))) else "n/a"

  .otis_emit(
    .otis_wrap(
      title = paste0("OTIS d-series -- yearly death counts + ",
                       "Alert x Cause/Housing chi^2"),
      summary_lines = list(
        "d01 total deaths" = total_n,
        "d01 year range" = year_range,
        "Year-over-year RR" = rr_text,
        "d06 (Alert x MedicalCause) Cramer's V" =
          if (is.finite(stats_d06$cramer_v %||% NA_real_))
            round(stats_d06$cramer_v, 4) else "n/a",
        "d07 (Alert x Housing_Type) Cramer's V" =
          if (is.finite(stats_d07$cramer_v %||% NA_real_))
            round(stats_d07$cramer_v, 4) else "n/a"
      ),
      tables = list(
        list(title = "d01 yearly custodial-death counts (Poisson 95% CI):",
             headers = c("Year", "Deaths", "95% CI"),
             rows = yearly_rows),
        list(title = paste0("d06 / d07 Alert-flag x outcome contingency ",
                              "chi^2 + Cramer's V:"),
             headers = c("Dataset", "Slice", "n",
                           "chi^2", "dof", "p", "Cramer's V", "min cell"),
             rows = chi_rows)
      ),
      interpretation = paste(
        "d-series carries no per-individual alert columns -- the Ruhela",
        "alert-complexity dual is structurally impossible. Natural",
        "alternatives are: (1) yearly death-count trends, and (2)",
        "Cramer's V on d06 / d07 aggregate contingency tables."
      ),
      payload = list(d01_yearly_counts = yearly_payload,
                     yoy_rate_ratio = yoy,
                     d06_chi2 = stats_d06,
                     d07_chi2 = stats_d07)
    ),
    out_dir, "d_chi2")
}


# ---------------------------------------------------------------------------
# Ruhela grid + master orchestrator
# ---------------------------------------------------------------------------

#' Aggregate Ruhela grid: one-page IRR comparison across analyzers
#'
#' Runs every aggregate Ruhela formulation analyzer against the
#' supplied named datasets list and presents a single primary-IRR
#' comparison table (GEE cluster-robust > NB GLM > Poisson GLM).
#'
#' @param datasets Named list keyed by dataset id (b03..d05).
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' otis_b03 <- morie_synth_otis("b03", n = 120L, seed = 1L)
#' otis_c01 <- morie_synth_otis("c01", n = 120L, seed = 1L)
#' morie_otis_analyze_ruhela_grid(list(b03 = otis_b03, c01 = otis_c01))
#' }
morie_otis_analyze_ruhela_grid <- function(datasets,
                                             out_dir = NULL) {
  stopifnot(is.list(datasets))
  analyzers <- list(
    b03 = morie_otis_analyze_b03_ruhela_aggregate,
    b04 = morie_otis_analyze_b04_ruhela_aggregate,
    b06 = morie_otis_analyze_b06_ruhela_aggregate,
    b07 = morie_otis_analyze_b07_ruhela_aggregate,
    b08 = morie_otis_analyze_b08_ruhela_aggregate,
    b09 = morie_otis_analyze_b09_ruhela_aggregate,
    c01 = morie_otis_analyze_c01_ruhela_aggregate,
    `c01-RC` = morie_otis_analyze_c01_ruhela_aggregate_region_cluster,
    c02 = morie_otis_analyze_c02_ruhela_aggregate,
    c03 = morie_otis_analyze_c03_ruhela_aggregate,
    c04 = morie_otis_analyze_c04_ruhela_aggregate,
    `c04-RC` = morie_otis_analyze_c04_ruhela_aggregate_region_cluster,
    c05 = morie_otis_analyze_c05_ruhela_aggregate,
    c06 = morie_otis_analyze_c06_ruhela_aggregate,
    c07 = morie_otis_analyze_c07_ruhela_aggregate,
    c08 = morie_otis_analyze_c08_ruhela_aggregate,
    c09 = morie_otis_analyze_c09_ruhela_aggregate,
    c10 = morie_otis_analyze_c10_ruhela_aggregate,
    c11 = morie_otis_analyze_c11_ruhela_aggregate,
    c12 = morie_otis_analyze_c12_ruhela_aggregate,
    d02 = morie_otis_analyze_d02_ruhela_aggregate,
    d03 = morie_otis_analyze_d03_ruhela_aggregate,
    d04 = morie_otis_analyze_d04_ruhela_aggregate,
    d05 = morie_otis_analyze_d05_ruhela_aggregate
  )

  rows <- list()
  for (ds_id in names(analyzers)) {
    src_id <- sub("-RC", "", ds_id, fixed = TRUE)
    data <- datasets[[src_id]] %||% datasets[[ds_id]]
    if (is.null(data)) {
      rows[[length(rows) + 1L]] <- c(ds_id, "--", "missing data",
                                       "--", "--", "--", "--")
      next
    }
    r <- tryCatch(analyzers[[ds_id]](data),
                  error = function(e) {
                    list(warnings = conditionMessage(e),
                         tables = list())
                  })
    if (length(r$warnings) > 0L && length(r$tables) == 0L) {
      rows[[length(rows) + 1L]] <- c(ds_id, "--", "warn",
        substr(r$warnings[[1]], 1, 50), "--", "--", "--")
      next
    }
    if (length(r$tables) == 0L || length(r$tables[[1]]$rows) == 0L) {
      rows[[length(rows) + 1L]] <- c(ds_id, "--", "no rows",
                                       "--", "--", "--", "--")
      next
    }
    tab_rows <- r$tables[[1]]$rows
    .find <- function(substr_label) {
      for (row in tab_rows) {
        if (grepl(substr_label, as.character(row[1]), fixed = TRUE) &&
            !identical(row[3], "fit failed")) return(row)
      }
      NULL
    }
    primary <- .find("GEE-NB")
    primary_type <- if (!is.null(primary)) "GEE-NB (cluster)" else NULL
    if (is.null(primary)) {
      primary <- .find("GEE-Poisson")
      primary_type <- if (!is.null(primary))
        "GEE-Poisson (cluster)" else NULL
    }
    if (is.null(primary)) {
      for (row in tab_rows) {
        if (identical(as.character(row[1]), "NB") &&
            !identical(row[3], "fit failed")) {
          primary <- row
          primary_type <- "NB GLM"
          break
        }
      }
    }
    if (is.null(primary)) {
      for (row in tab_rows) {
        if (identical(as.character(row[1]), "Poisson") &&
            !identical(row[3], "fit failed")) {
          primary <- row
          primary_type <- "Poisson GLM"
          break
        }
      }
    }
    title <- substr(r$title %||% "", 1, 60)
    if (!is.null(primary)) {
      rows[[length(rows) + 1L]] <- c(ds_id, primary_type, title,
        as.character(primary[3]), as.character(primary[4]),
        as.character(primary[5]),
        if (length(primary) >= 7L)
          substr(as.character(primary[7]), 1, 30) else "--")
    } else {
      rows[[length(rows) + 1L]] <- c(ds_id, "all failed", title,
                                       "--", "--", "--", "--")
    }
  }

  .otis_emit(
    .otis_wrap(
      title = paste0("OTIS Ruhela formulations grid summary -- one-page ",
                       "IRR/ATE comparison across all aggregate RF analyzers"),
      summary_lines = list(
        "Aggregate datasets covered" = length(analyzers),
        "Per-row datasets covered" = paste(
          "see analyze_a01/b01/b02_ruhela_formulations + alt-T variants"),
        "MRM chi^2 datasets (c, d)" =
          "see analyze_c_chi2 / analyze_d_chi2",
        "Primary estimator priority" =
          "GEE-NB > GEE-Poisson > NB GLM > Poisson GLM"
      ),
      tables = list(list(
        title = paste0("Aggregate Ruhela formulations -- primary IRR per ",
                         "dataset (GEE cluster-robust > NB GLM):"),
        headers = c("DS", "Type", "Formulation", "IRR",
                      "95% CI", "p", "Notes"),
        rows = rows
      )),
      interpretation = paste(
        "One-page comparison of every aggregate Ruhela formulation",
        "currently shipped. Primary IRR is the GEE cluster-robust",
        "estimate when available; otherwise the NB GLM estimate."
      ),
      payload = list(n_aggregates = length(analyzers))
    ),
    out_dir, "ruhela_grid")
}

#' Paper-ready master report -- every Ruhela formulation in one result
#'
#' Sections:
#' \enumerate{
#'   \item Aggregate Ruhela formulations grid
#'   \item (Optional) per-row Ruhela formulations on a01/b01/b02
#'   \item MRM chi-square family on c-series + d-series
#'   \item Mandela-RF cross-comparison (provincial vs federal)
#' }
#'
#' @param datasets Named list of OTIS data.frames.
#' @param include_per_row Logical; if \code{TRUE} also runs the slow
#'   per-row RFs. Default \code{FALSE}.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' datasets_list <- list(b03 = morie_synth_otis("b03", n = 120L, seed = 1L),
#'                       c01 = morie_synth_otis("c01", n = 120L, seed = 1L))
#' morie_otis_analyze_ruhela_master(datasets_list)
#' }
morie_otis_analyze_ruhela_master <- function(datasets,
                                               include_per_row = FALSE,
                                               out_dir = NULL) {
  stopifnot(is.list(datasets))
  sections <- list()

  grid <- morie_otis_analyze_ruhela_grid(datasets, out_dir)
  if (length(grid$tables) > 0L) {
    sections[[length(sections) + 1L]] <- list(
      title = paste0("section 1 Aggregate Ruhela formulations -- ",
                       "primary IRR per dataset:"),
      headers = grid$tables[[1]]$headers,
      rows = grid$tables[[1]]$rows
    )
  }

  per_row_rows <- list()
  if (isTRUE(include_per_row)) {
    arms <- list(
      list(id = "a01",
           fn = morie_otis_analyze_a01_ruhela_formulations,
           data = datasets$a01),
      list(id = "b01",
           fn = morie_otis_analyze_b01_ruhela_formulations,
           data = datasets$b01),
      list(id = "b02",
           fn = morie_otis_analyze_b02_ruhela_formulations,
           data = datasets$b02))
    for (arm in arms) {
      r <- tryCatch(arm$fn(arm$data),
                    error = function(e) list(warnings =
                                              conditionMessage(e),
                                              tables = list()))
      if (length(r$warnings) > 0L) {
        per_row_rows[[length(per_row_rows) + 1L]] <- c(arm$id, "warn",
          substr(r$warnings[[1]], 1, 50), "--", "--", "--")
        next
      }
      if (length(r$tables) == 0L) {
        per_row_rows[[length(per_row_rows) + 1L]] <- c(arm$id,
          "no rows", "--", "--", "--", "--")
        next
      }
      per_row_rows[[length(per_row_rows) + 1L]] <- c(arm$id,
        "see full table", "--", "--", "--", "--")
    }
  }
  if (length(per_row_rows) > 0L) {
    sections[[length(sections) + 1L]] <- list(
      title = paste0("section 2 Per-row Ruhela formulations ",
                       "(canonical T_high_ac -> vm_count):"),
      headers = c("DS", "Estimator", "ATE", "SE", "95% CI", "p"),
      rows = per_row_rows
    )
  }

  c_chi <- tryCatch(morie_otis_analyze_c_chi2(datasets, out_dir = out_dir),
                    error = function(e) NULL)
  d_chi <- tryCatch(morie_otis_analyze_d_chi2(datasets, out_dir = out_dir),
                    error = function(e) NULL)
  chi_rows <- list()
  for (pair in list(list(lbl = "c-series", r = c_chi),
                    list(lbl = "d-series", r = d_chi))) {
    if (is.null(pair$r)) next
    if (length(pair$r$tables) > 0L) {
      for (row in utils::head(pair$r$tables[[1]]$rows, 3L)) {
        chi_rows[[length(chi_rows) + 1L]] <- c(pair$lbl,
          utils::head(row, 5L))
      }
    }
  }
  if (length(chi_rows) > 0L) {
    sections[[length(sections) + 1L]] <- list(
      title = paste0("section 3 MRM chi-square family -- Pearson chi^2",
                       " + Cramer's V on aggregate tables:"),
      headers = c("Series", "Slice/measure", "chi^2", "dof",
                    "p", "Cramer's V"),
      rows = chi_rows
    )
  }

  if (!is.null(datasets$c11)) {
    cmp <- tryCatch(
      morie_otis_analyze_otis_mandela_provincial_vs_federal(
        datasets$c11), error = function(e) NULL)
    if (!is.null(cmp) && length(cmp$tables) > 0L) {
      sections[[length(sections) + 1L]] <- list(
        title = paste0("section 4 Mandela-RF -- Ontario provincial ",
                         "vs federal SIU cross-comparison:"),
        headers = cmp$tables[[1]]$headers,
        rows = cmp$tables[[1]]$rows
      )
    }
  }

  .otis_wrap(
    title = paste0("OTIS Ruhela formulations -- paper-ready master ",
                     "report (provincial + federal-aggregate companion)"),
    summary_lines = list(
      "Sections" = length(sections),
      "Aggregate RFs" = paste("24 (b03-b09 + c01-c12 + d02-d05 +",
                                  "region-cluster variants)"),
      "Per-row RFs included" = include_per_row,
      "MRM chi-square" = "c-series + d-series families",
      "Methodology attribution" =
        "MRM (Multilevel Reconciliation Methodology)",
      "Acknowledgements (separate)" = "Jauregui, A. Laniyonu"
    ),
    tables = sections,
    interpretation = paste(
      "Master report compiling every Ruhela formulation analyzer",
      "shipped in morie. Sections: aggregate IRR grid (cluster-robust",
      "where high-cardinality grouping applies); per-row IRM-DML ATE",
      "on a01/b01/b02 (when include_per_row=TRUE); MRM chi^2 on",
      "c-series + d-series; Mandela-RF cross-comparison."
    ),
    payload = list(include_per_row = include_per_row,
                   n_sections = length(sections))
  )
}


# ---------------------------------------------------------------------------
# CSI context (analyze_a01_with_csi_context)
# ---------------------------------------------------------------------------

#' OTIS a01 causal pipeline + Toronto Crime Severity Index context
#'
#' Runs \code{\link{morie_otis_analyze_a01}} (alert complexity -> regional
#' volatility, MatchIt then IRM-DML) and sets it beside Toronto's Crime
#' Severity Index for the OTIS years (2023-2025), computed with
#' \code{\link{morie_tps_csi_per_year}} from TPS incident tables. The
#' question it frames: did the OTIS effect arise while Toronto crime
#' severity was rising or falling?
#'
#' @param data Optional a01 data frame (\code{NULL} loads it).
#' @param variant CSI variant: \code{"total"} or \code{"violent"}.
#' @param rebase_to_year Anchor year for the CSI index column
#'   (default 2023). Use \code{NULL} to skip rebasing.
#' @param tps_data Optional named list of TPS incident data frames, one per
#'   CSI category (names from \code{MORIE_TPS_CSI_CATEGORIES()}), each with an
#'   \code{OCC_YEAR} column. \code{NULL} loads them with
#'   \code{\link{morie_tps_load_dataset}}; categories that fail to load are
#'   listed in the warnings.
#' @param out_dir Optional output directory.
#' @return \code{morie_otis_analysis_result}.
#' @export
#' @examples
#' \donttest{
#' a01 <- morie_synth_otis("a01", n = 300L, seed = 1L)
#' cats <- MORIE_TPS_CSI_CATEGORIES()
#' tps <- stats::setNames(lapply(seq_along(cats), function(i)
#'   data.frame(OCC_YEAR = rep(2022:2025, times = 10 * i))), cats)
#' r <- morie_otis_analyze_a01_with_csi_context(a01, tps_data = tps)
#' r$tables[[1L]]$rows
#' }
morie_otis_analyze_a01_with_csi_context <- function(data = NULL,
                                                     variant = "total",
                                                     rebase_to_year = 2023L,
                                                     tps_data = NULL,
                                                     out_dir = NULL) {
  variant <- match.arg(variant, c("total", "violent"))
  otis <- morie_otis_analyze_a01(.otis_ruhela_input(data, "a01"))
  cats <- MORIE_TPS_CSI_CATEGORIES()
  load_errors <- character(0)
  if (is.null(tps_data)) {
    tps_data <- list()
    for (cat in cats) {
      d <- tryCatch(morie_tps_load_dataset(cat), error = function(e) e)
      if (inherits(d, "error")) {
        load_errors <- c(load_errors, sprintf("%s: %s", cat, conditionMessage(d)))
      } else {
        tps_data[[cat]] <- d
      }
    }
  }
  counts <- list()
  for (cat in intersect(names(tps_data), cats)) {
    y <- suppressWarnings(as.integer(tps_data[[cat]][["OCC_YEAR"]]))
    tab <- table(y[!is.na(y)])
    for (yk in names(tab)) {
      if (is.null(counts[[yk]])) counts[[yk]] <- list()
      counts[[yk]][[cat]] <- as.integer(tab[[yk]])
    }
  }
  if (!length(counts)) {
    otis$warnings <- c(otis$warnings, "CSI context unavailable: no TPS tables loaded",
                       load_errors)
    return(.otis_emit(otis, out_dir, "a01-csi"))
  }
  by_year <- morie_tps_csi_per_year(counts, variant = variant,
                                    rebase_to_year = rebase_to_year)
  overlap <- by_year[by_year$year %in% 2023:2025, , drop = FALSE]
  rows <- lapply(seq_len(nrow(overlap)), function(i) {
    r <- overlap[i, ]
    as.character(c(as.integer(r$year), round(r$raw_weighted_sum), as.integer(r$total_count),
                   round(r$csi_per_capita, 2),
                   if ("csi_index" %in% names(r) && !is.na(r$csi_index)) round(r$csi_index, 2) else "--"))
  })
  res <- .otis_wrap(
    title = "OTIS a01 (alert->vm) + Toronto Crime Severity Index context",
    summary_lines = c(otis$summary_lines, list(
      "-- CSI context (StatsCan), variant" = variant,
      "-- Rebase year" = if (is.null(rebase_to_year)) "none" else rebase_to_year,
      "-- Years with CSI data" = paste(sort(by_year$year), collapse = ", "))),
    tables = list(list(
      title = sprintf("Toronto CSI by year (variant=%s, rebased to %s=100):", variant,
                      if (is.null(rebase_to_year)) "none" else rebase_to_year),
      headers = c("year", "weighted_sum", "incidents", "csi_per_capita", "csi_index"),
      rows = rows)),
    interpretation = paste0(
      otis$interpretation, "\n\nCSI context: Toronto's Crime Severity Index (",
      variant, ") is shown alongside the OTIS estimate. The OTIS-observed ",
      "regional volatility effect happens inside this broader Toronto ",
      "crime-severity environment; rising CSI alongside a rising vm-effect ",
      "would suggest a coupling, falling CSI with a stable vm-effect would ",
      "suggest the OTIS dynamic is internal to corrections rather than a ",
      "reflection of street crime trends."),
    warnings = c(otis$warnings, load_errors),
    payload = list(otis = otis$payload, csi_by_year = by_year,
                   variant = variant, rebase_to_year = rebase_to_year))
  .otis_emit(res, out_dir, "a01-csi")
}

# ---------------------------------------------------------------------------
# MRM-prefixed aliases (renamed 2026-05-10)
# ---------------------------------------------------------------------------
#' @rdname morie_otis_analyze_a01_ruhela_formulations
#' @export
morie_otis_analyze_a01_mrm <- morie_otis_analyze_a01_ruhela_formulations
#' @rdname morie_otis_analyze_b01_ruhela_formulations
#' @export
morie_otis_analyze_b01_mrm <- morie_otis_analyze_b01_ruhela_formulations
#' @rdname morie_otis_analyze_b02_ruhela_formulations
#' @export
morie_otis_analyze_b02_mrm <- morie_otis_analyze_b02_ruhela_formulations
#' @rdname morie_otis_analyze_a01_ruhela_alt_gender
#' @export
morie_otis_analyze_a01_mrm_alt_gender <- morie_otis_analyze_a01_ruhela_alt_gender
#' @rdname morie_otis_analyze_a01_ruhela_alt_age
#' @export
morie_otis_analyze_a01_mrm_alt_age <- morie_otis_analyze_a01_ruhela_alt_age
#' @rdname morie_otis_analyze_a01_ruhela_alt_toronto
#' @export
morie_otis_analyze_a01_mrm_alt_toronto <- morie_otis_analyze_a01_ruhela_alt_toronto
#' @rdname morie_otis_analyze_b01_ruhela_alt_gender
#' @export
morie_otis_analyze_b01_mrm_alt_gender <- morie_otis_analyze_b01_ruhela_alt_gender
#' @rdname morie_otis_analyze_b01_ruhela_alt_age
#' @export
morie_otis_analyze_b01_mrm_alt_age <- morie_otis_analyze_b01_ruhela_alt_age
#' @rdname morie_otis_analyze_b01_ruhela_alt_toronto
#' @export
morie_otis_analyze_b01_mrm_alt_toronto <- morie_otis_analyze_b01_ruhela_alt_toronto
#' @rdname morie_otis_analyze_b02_ruhela_alt_region
#' @export
morie_otis_analyze_b02_mrm_alt_region <- morie_otis_analyze_b02_ruhela_alt_region
#' @rdname morie_otis_analyze_b02_ruhela_alt_age
#' @export
morie_otis_analyze_b02_mrm_alt_age <- morie_otis_analyze_b02_ruhela_alt_age
#' @rdname morie_otis_analyze_a01_ruhela_subgroup_female
#' @export
morie_otis_analyze_a01_mrm_subgroup_female <- morie_otis_analyze_a01_ruhela_subgroup_female
#' @rdname morie_otis_analyze_a01_ruhela_subgroup_male
#' @export
morie_otis_analyze_a01_mrm_subgroup_male <- morie_otis_analyze_a01_ruhela_subgroup_male
#' @rdname morie_otis_analyze_b01_ruhela_subgroup_female
#' @export
morie_otis_analyze_b01_mrm_subgroup_female <- morie_otis_analyze_b01_ruhela_subgroup_female
#' @rdname morie_otis_analyze_b01_ruhela_subgroup_male
#' @export
morie_otis_analyze_b01_mrm_subgroup_male <- morie_otis_analyze_b01_ruhela_subgroup_male
#' @rdname morie_otis_analyze_a01_ruhela_per_year
#' @export
morie_otis_analyze_a01_mrm_per_year <- morie_otis_analyze_a01_ruhela_per_year
#' @rdname morie_otis_analyze_b01_ruhela_per_year
#' @export
morie_otis_analyze_b01_mrm_per_year <- morie_otis_analyze_b01_ruhela_per_year
#' @rdname morie_otis_analyze_b03_ruhela_aggregate
#' @export
morie_otis_analyze_b03_mrm_aggregate <- morie_otis_analyze_b03_ruhela_aggregate
#' @rdname morie_otis_analyze_b04_ruhela_aggregate
#' @export
morie_otis_analyze_b04_mrm_aggregate <- morie_otis_analyze_b04_ruhela_aggregate
#' @rdname morie_otis_analyze_b06_ruhela_aggregate
#' @export
morie_otis_analyze_b06_mrm_aggregate <- morie_otis_analyze_b06_ruhela_aggregate
#' @rdname morie_otis_analyze_b07_ruhela_aggregate
#' @export
morie_otis_analyze_b07_mrm_aggregate <- morie_otis_analyze_b07_ruhela_aggregate
#' @rdname morie_otis_analyze_b08_ruhela_aggregate
#' @export
morie_otis_analyze_b08_mrm_aggregate <- morie_otis_analyze_b08_ruhela_aggregate
#' @rdname morie_otis_analyze_b09_ruhela_aggregate
#' @export
morie_otis_analyze_b09_mrm_aggregate <- morie_otis_analyze_b09_ruhela_aggregate
#' @rdname morie_otis_analyze_c01_ruhela_aggregate
#' @export
morie_otis_analyze_c01_mrm_aggregate <- morie_otis_analyze_c01_ruhela_aggregate
#' @rdname morie_otis_analyze_c01_ruhela_aggregate_region_cluster
#' @export
morie_otis_analyze_c01_mrm_aggregate_region_cluster <- morie_otis_analyze_c01_ruhela_aggregate_region_cluster
#' @rdname morie_otis_analyze_c02_ruhela_aggregate
#' @export
morie_otis_analyze_c02_mrm_aggregate <- morie_otis_analyze_c02_ruhela_aggregate
#' @rdname morie_otis_analyze_c03_ruhela_aggregate
#' @export
morie_otis_analyze_c03_mrm_aggregate <- morie_otis_analyze_c03_ruhela_aggregate
#' @rdname morie_otis_analyze_c04_ruhela_aggregate
#' @export
morie_otis_analyze_c04_mrm_aggregate <- morie_otis_analyze_c04_ruhela_aggregate
#' @rdname morie_otis_analyze_c04_ruhela_aggregate_region_cluster
#' @export
morie_otis_analyze_c04_mrm_aggregate_region_cluster <- morie_otis_analyze_c04_ruhela_aggregate_region_cluster
#' @rdname morie_otis_analyze_c05_ruhela_aggregate
#' @export
morie_otis_analyze_c05_mrm_aggregate <- morie_otis_analyze_c05_ruhela_aggregate
#' @rdname morie_otis_analyze_c06_ruhela_aggregate
#' @export
morie_otis_analyze_c06_mrm_aggregate <- morie_otis_analyze_c06_ruhela_aggregate
#' @rdname morie_otis_analyze_c07_ruhela_aggregate
#' @export
morie_otis_analyze_c07_mrm_aggregate <- morie_otis_analyze_c07_ruhela_aggregate
#' @rdname morie_otis_analyze_c08_ruhela_aggregate
#' @export
morie_otis_analyze_c08_mrm_aggregate <- morie_otis_analyze_c08_ruhela_aggregate
#' @rdname morie_otis_analyze_c09_ruhela_aggregate
#' @export
morie_otis_analyze_c09_mrm_aggregate <- morie_otis_analyze_c09_ruhela_aggregate
#' @rdname morie_otis_analyze_c10_ruhela_aggregate
#' @export
morie_otis_analyze_c10_mrm_aggregate <- morie_otis_analyze_c10_ruhela_aggregate
#' @rdname morie_otis_analyze_c11_ruhela_aggregate
#' @export
morie_otis_analyze_c11_mrm_aggregate <- morie_otis_analyze_c11_ruhela_aggregate
#' @rdname morie_otis_analyze_c12_ruhela_aggregate
#' @export
morie_otis_analyze_c12_mrm_aggregate <- morie_otis_analyze_c12_ruhela_aggregate
#' @rdname morie_otis_analyze_d02_ruhela_aggregate
#' @export
morie_otis_analyze_d02_mrm_aggregate <- morie_otis_analyze_d02_ruhela_aggregate
#' @rdname morie_otis_analyze_d03_ruhela_aggregate
#' @export
morie_otis_analyze_d03_mrm_aggregate <- morie_otis_analyze_d03_ruhela_aggregate
#' @rdname morie_otis_analyze_d04_ruhela_aggregate
#' @export
morie_otis_analyze_d04_mrm_aggregate <- morie_otis_analyze_d04_ruhela_aggregate
#' @rdname morie_otis_analyze_d05_ruhela_aggregate
#' @export
morie_otis_analyze_d05_mrm_aggregate <- morie_otis_analyze_d05_ruhela_aggregate
