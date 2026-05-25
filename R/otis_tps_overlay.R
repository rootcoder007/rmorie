# SPDX-License-Identifier: AGPL-3.0-or-later
#' Cross-link OTIS (Ontario corrections) with TPS (Toronto police) data
#'
#' R port of \code{morie.otis_tps_overlay}. Both feeds touch Toronto,
#' so three overlay analyses are meaningful:
#'
#' \itemize{
#'   \item \code{morie_otis_tps_yoy_correlation()} -- year-over-year
#'     Pearson r between OTIS Toronto-region segregation placements
#'     and TPS incident counts (per category).
#'   \item \code{morie_otis_tps_per_region_rollup()} -- OTIS seg/RC
#'     totals per region x year, with the Toronto row flagged for
#'     overlay use.
#'   \item \code{morie_otis_tps_composite_overlay()} -- alias for
#'     \code{morie_otis_tps_yoy_correlation()} (preserves the Python
#'     name, same body).
#' }
#'
#' All three return a \code{morie_otis_analysis_result} (the shared
#' RichResult-shaped list from \code{otis_all_analyze.R}).
#'
#' Cross-year invariants
#' ---------------------
#' \itemize{
#'   \item OTIS \code{UniqueIndividual_ID} is reassigned every fiscal
#'     year (see \code{variable_taxonomy.R}); the overlay therefore
#'     joins at the \emph{year} grain, never at the person grain.
#'   \item OTIS uses fiscal-year (\code{EndFiscalYear}); TPS uses
#'     calendar \code{OCC_YEAR} or \code{REPORT_YEAR}. The Pearson r
#'     here is computed on the year-aligned intersection -- there is
#'     a small fiscal/calendar misalignment that is documented in the
#'     interpretation but not corrected. Toronto OTIS data covers only
#'     2023-2025, so common-year samples are necessarily small.
#' }
#'
#' @name morie_otis_tps_overlay
NULL


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.otis_tps_toronto_seg_by_year <- function(df_b01) {
  stopifnot(is.data.frame(df_b01))
  if (!("EndFiscalYear" %in% names(df_b01))) {
    return(stats::setNames(integer(0), character(0)))
  }
  if ("Region_AtTimeOfPlacement" %in% names(df_b01)) {
    df_b01 <- df_b01[df_b01[["Region_AtTimeOfPlacement"]] == "Toronto",
                      , drop = FALSE]
  }
  if (nrow(df_b01) == 0L) {
    return(stats::setNames(integer(0), character(0)))
  }
  tab <- table(df_b01[["EndFiscalYear"]])
  yrs <- suppressWarnings(as.integer(names(tab)))
  keep <- !is.na(yrs)
  out <- as.integer(tab[keep])
  names(out) <- as.character(yrs[keep])
  out
}

.otis_tps_incidents_by_year <- function(df_tps) {
  stopifnot(is.data.frame(df_tps))
  yc <- if ("OCC_YEAR" %in% names(df_tps)) "OCC_YEAR" else
        if ("REPORT_YEAR" %in% names(df_tps)) "REPORT_YEAR" else NULL
  if (is.null(yc)) {
    return(stats::setNames(integer(0), character(0)))
  }
  tab <- table(df_tps[[yc]])
  yrs <- suppressWarnings(as.integer(names(tab)))
  keep <- !is.na(yrs) & yrs >= 1990L & yrs <= 2030L
  out <- as.integer(tab[keep])
  names(out) <- as.character(yrs[keep])
  out
}

# Wrap a list as a morie_otis_analysis_result (lazy reference to the
# constructor defined in otis_all_analyze.R -- both files ship in the
# same R/ collation order, so this resolves at package-load time).
.otis_overlay_wrap <- function(title, summary_lines,
                                tables = list(),
                                interpretation = "",
                                warnings = character(0),
                                payload = NULL) {
  tables <- Filter(Negate(is.null), tables)
  out <- list(
    title = title,
    summary_lines = summary_lines,
    tables = tables,
    warnings = warnings,
    interpretation = interpretation,
    payload = payload
  )
  class(out) <- c("morie_otis_analysis_result", "morie_rich_result",
                  "list")
  out
}


# ---------------------------------------------------------------------------
# 1. Year-over-year correlation
# ---------------------------------------------------------------------------

#' Year-over-year correlation between OTIS Toronto-region segregation
#' placements and TPS incident counts (per category)
#'
#' @param otis_b01 OTIS b01 data.frame
#'   (e.g. \code{read.csv("b01_segregation_detailed_dataset.csv")}).
#' @param tps_datasets A named list of TPS data.frames, one per
#'   category (e.g. \code{list(assault = <df>, robbery = <df>)}). Each
#'   data.frame must have \code{OCC_YEAR} or \code{REPORT_YEAR}.
#' @return A \code{morie_otis_analysis_result} with a per-category
#'   Pearson r table.
#' @export
#' @examples
#' if (FALSE) {
#'   b01 <- read.csv("b01_segregation_detailed_dataset.csv")
#'   tps <- list(
#'     assault = read.csv("Assault_Open_Data.csv"),
#'     robbery = read.csv("Robbery_Open_Data.csv")
#'   )
#'   morie_otis_tps_yoy_correlation(b01, tps)
#' }
morie_otis_tps_yoy_correlation <- function(otis_b01, tps_datasets) {
  stopifnot(is.data.frame(otis_b01))
  stopifnot(is.list(tps_datasets))
  seg <- .otis_tps_toronto_seg_by_year(otis_b01)
  if (length(seg) == 0L) {
    return(.otis_overlay_wrap(
      title = "OTIS x TPS -- YoY correlation",
      summary_lines = list(),
      warnings = "OTIS b01 has no Toronto-region data"
    ))
  }

  cats <- names(tps_datasets)
  rows <- lapply(cats, function(cat) {
    out <- tryCatch({
      tps <- .otis_tps_incidents_by_year(tps_datasets[[cat]])
      common <- intersect(names(seg), names(tps))
      if (length(common) < 3L) {
        return(c(cat, length(common), "n/a", "n/a", "n/a"))
      }
      x <- as.numeric(seg[common])
      y <- as.numeric(tps[common])
      r <- if (stats::sd(x) > 0 && stats::sd(y) > 0) {
        stats::cor(x, y)
      } else NA_real_
      n <- length(common)
      p <- if (is.finite(r) && n > 3L && abs(r) < 1) {
        z <- 0.5 * log((1 + r) / (1 - r))
        2 * (1 - stats::pnorm(abs(z) * sqrt(n - 3)))
      } else NA_real_
      yrs <- as.integer(common)
      c(cat, n, round(r, 4), signif(p, 3),
        sprintf("%d-%d", min(yrs), max(yrs)))
    }, error = function(e) c(cat, "err", substr(conditionMessage(e), 1, 30),
                              "n/a", "n/a"))
    out
  })

  # Sort by abs(r) descending; non-numeric rows sort last
  abs_r <- vapply(rows, function(r) {
    v <- suppressWarnings(as.numeric(r[3]))
    if (is.na(v)) -1 else abs(v)
  }, numeric(1))
  rows <- rows[order(-abs_r)]

  .otis_overlay_wrap(
    title = "OTIS x TPS -- year-by-year correlation (Toronto region)",
    summary_lines = list(
      `OTIS source` = "b01 -- segregation placements (Toronto region)",
      `OTIS years` = sprintf("%d-%d",
                              as.integer(min(names(seg))),
                              as.integer(max(names(seg)))),
      `TPS categories tested` = length(cats)
    ),
    tables = list(list(
      title = paste("Pearson r between OTIS Toronto-region segregation",
                     "count and TPS incident count (year-aligned):"),
      headers = c("TPS category", "Common years", "Pearson r",
                  "p-value", "Year range"),
      rows = rows
    )),
    interpretation = paste(
      "Positive r = years with more Toronto-region segregation",
      "placements in OTIS coincide with more reported TPS incidents",
      "in that category. This is association, NOT causation. OTIS",
      "uses fiscal-year (EndFiscalYear); TPS uses calendar year",
      "(OCC_YEAR / REPORT_YEAR) -- year axes are intersected as-is.",
      "Toronto OTIS data covers only 2023-2025, so common-year",
      "samples are small and CIs on r are wide."
    ),
    payload = list(seg_by_year = as.list(seg))
  )
}


# ---------------------------------------------------------------------------
# 2. Per-region rollup
# ---------------------------------------------------------------------------

#' OTIS seg/RC totals per region x year (Toronto row flagged for
#' TPS-overlay use)
#'
#' @param otis_b01 OTIS b01 data.frame.
#' @return A \code{morie_otis_analysis_result} with a year x region
#'   count matrix.
#' @export
morie_otis_tps_per_region_rollup <- function(otis_b01) {
  stopifnot(is.data.frame(otis_b01))
  if (!("Region_AtTimeOfPlacement" %in% names(otis_b01))) {
    return(.otis_overlay_wrap(
      title = "OTIS region rollup",
      summary_lines = list(),
      warnings = "region column missing"
    ))
  }
  if (!("EndFiscalYear" %in% names(otis_b01))) {
    return(.otis_overlay_wrap(
      title = "OTIS region rollup",
      summary_lines = list(),
      warnings = "EndFiscalYear column missing"
    ))
  }
  tab <- table(otis_b01[["EndFiscalYear"]],
               otis_b01[["Region_AtTimeOfPlacement"]])
  m <- as.matrix(tab)
  yrs <- suppressWarnings(as.integer(rownames(m)))
  ord <- order(yrs)
  m <- m[ord, , drop = FALSE]
  yrs <- yrs[ord]

  rows <- lapply(seq_len(nrow(m)), function(i)
    c(yrs[i], as.integer(m[i, ])))
  toronto_total <- if ("Toronto" %in% colnames(m))
    sum(m[, "Toronto"]) else 0L

  .otis_overlay_wrap(
    title = "OTIS -- segregation placements per region x year",
    summary_lines = list(
      Years = sprintf("%d-%d", min(yrs), max(yrs)),
      Regions = paste(colnames(m), collapse = ", "),
      `Total placements` = as.integer(sum(m)),
      `Toronto region total` = as.integer(toronto_total)
    ),
    tables = list(list(
      title = "Counts by year x region:",
      headers = c("Year", colnames(m)),
      rows = rows
    )),
    payload = list(by_region = m,
                   note = paste("EndFiscalYear preserves the cross-year",
                                "invariant; UniqueIndividual_ID is NOT",
                                "joined across years."))
  )
}


# ---------------------------------------------------------------------------
# 3. Composite overlay (alias)
# ---------------------------------------------------------------------------

#' Composite overlay (alias for the YoY correlation)
#'
#' Same body as \code{\link{morie_otis_tps_yoy_correlation}}; the alias
#' preserves the Python entry-point name.
#'
#' @inheritParams morie_otis_tps_yoy_correlation
#' @export
morie_otis_tps_composite_overlay <- function(otis_b01, tps_datasets) {
  morie_otis_tps_yoy_correlation(otis_b01, tps_datasets)
}


# ---------------------------------------------------------------------------
# 4. Master driver
# ---------------------------------------------------------------------------

#' Run all OTIS-TPS overlay analyses
#'
#' @param otis_b01 OTIS b01 data.frame.
#' @param tps_datasets Named list of TPS data.frames (one per category).
#' @param out_dir Optional output directory for \code{overlay_<name>.rds}.
#' @return Named list of \code{morie_otis_analysis_result}s
#'   (\code{region_rollup}, \code{yoy_correlation}).
#' @export
morie_otis_tps_analyze_all <- function(otis_b01, tps_datasets,
                                         out_dir = NULL) {
  out <- list(
    region_rollup = tryCatch(
      morie_otis_tps_per_region_rollup(otis_b01),
      error = function(e) .otis_overlay_wrap(
        title = "overlay region_rollup (failed)",
        summary_lines = list(),
        warnings = sprintf("%s: %s",
                            class(e)[1], conditionMessage(e)))),
    yoy_correlation = tryCatch(
      morie_otis_tps_yoy_correlation(otis_b01, tps_datasets),
      error = function(e) .otis_overlay_wrap(
        title = "overlay yoy_correlation (failed)",
        summary_lines = list(),
        warnings = sprintf("%s: %s",
                            class(e)[1], conditionMessage(e))))
  )
  if (!is.null(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    for (nm in names(out)) {
      saveRDS(out[[nm]],
              file.path(out_dir, sprintf("overlay_%s.rds", nm)))
    }
  }
  out
}
