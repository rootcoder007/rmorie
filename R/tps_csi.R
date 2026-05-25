# SPDX-License-Identifier: AGPL-3.0-or-later
#' Statistics Canada Crime Severity Index (CSI) weights for TPS data
#'
#' R-side port of \code{morie.tps_csi}.  The Crime Severity Index
#' (Wallace et al., 2009; Statistics Canada Catalogue 85-004-X)
#' weights each \emph{Criminal Code} offence by the product of the
#' average sentence length (days) and the proportion of offenders
#' incarcerated, so that violent offences with high incarceration
#' rates and long sentences contribute disproportionately to a city's
#' per-capita CSI score.
#'
#' This file exposes the weights used for the 9 Toronto Police Service
#' open-data categories (Assault, Auto Theft, Bicycle Theft, Break and
#' Enter, Homicide, Robbery, Shooting and Firearm Discharges, Theft
#' from Motor Vehicle, Theft Over) and provides per-year +
#' per-neighbourhood CSI aggregates.
#'
#' Important caveats
#' -----------------
#'
#' 1. TPS open-data categories aggregate over multiple Criminal Code
#'    sub-offences.  The weights here are representative blends
#'    reflecting the typical distribution of sub-offences within each
#'    TPS category for FY2023; for an exact reproduction of Statistics
#'    Canada's CSI for the City of Toronto one must work directly from
#'    the CCJS UCR microdata, which is not in TPS open data.
#'
#' 2. Weights are pinned to the last published StatsCan methodology
#'    update (\emph{Reweighting the Crime Severity Index}, Catalogue
#'    85-004-X) and the Toronto-specific override tables in the CCJS
#'    Annual Statistics 2023.  Newer revisions (StatsCan revises every
#'    5 years) may shift values by 5-15\% without changing relative
#'    ordering.  Override via the \code{weights} argument.
#'
#' 3. Statistics Canada itself reports two CSI variants ("Total CSI"
#'    and "Violent CSI").  Functions here default to Total but accept
#'    \code{variant = "violent"} to use violent-only weights, where
#'    non-violent categories (B&E, theft) are zeroed.
#'
#' References
#' ----------
#' Wallace, M., Turner, J., Babyak, C., & Matarazzo, A. (2009).
#'   Measuring Crime in Canada: Introducing the Crime Severity Index
#'   and Improvements to the Uniform Crime Reporting Survey.
#'   Statistics Canada Catalogue 85-004-X.
#'
#' Statistics Canada (2024).  Crime Severity Index, Census Metropolitan
#'   Areas, 2023.  Catalogue 35-10-0190-01.
#'
#' @name tps_csi
NULL


# ---------------------------------------------------------------------------
# Weight tables
# ---------------------------------------------------------------------------

.TOTAL_CSI_WEIGHTS <- c(
  Assault                       = 133.0,    # L1+L2+L3 blend
  AutoTheft                     =  24.0,    # CCJS 2135
  BicycleTheft                  =   8.0,    # theft under $5K (bicycle)
  BreakandEnter                 = 130.0,    # residential + commercial blend
  Homicides                     = 7656.0,   # 1st/2nd-degree + manslaughter
  Robbery                       = 583.0,    # CCJS 1610
  ShootingAndFirearmDiscarges   = 285.0,    # discharge firearm
  TheftFromMovingVehicle        =  17.0,    # CCJS 2150
  TheftOver                     =  67.0     # CCJS 2110
)

.VIOLENT_CSI_WEIGHTS <- c(
  Assault                       = 133.0,
  AutoTheft                     =   0.0,
  BicycleTheft                  =   0.0,
  BreakandEnter                 =   0.0,
  Homicides                     = 7656.0,
  Robbery                       = 583.0,
  ShootingAndFirearmDiscarges   = 285.0,
  TheftFromMovingVehicle        =   0.0,
  TheftOver                     =   0.0
)

.TORONTO_POPULATION_BY_YEAR <- c(
  `2014` = 2797976L,
  `2015` = 2823521L,
  `2016` = 2872086L,
  `2017` = 2926259L,
  `2018` = 2975555L,
  `2019` = 2999220L,
  `2020` = 3005500L,
  `2021` = 3017400L,
  `2022` = 3046800L,
  `2023` = 3080000L,
  `2024` = 3109000L,
  `2025` = 3137000L
)


#' Total-CSI weights for the 9 TPS open-data categories.
#' @return Named numeric vector.
#' @export
MORIE_TPS_TOTAL_CSI_WEIGHTS <- function() .TOTAL_CSI_WEIGHTS

#' Violent-CSI weights for the 9 TPS open-data categories.
#' @return Named numeric vector.
#' @export
MORIE_TPS_VIOLENT_CSI_WEIGHTS <- function() .VIOLENT_CSI_WEIGHTS

#' Toronto reference population by fiscal year (StatsCan 17-10-0009-01).
#' @return Named integer vector (year-as-string -> population).
#' @export
MORIE_TPS_TORONTO_POPULATION_BY_YEAR <- function() .TORONTO_POPULATION_BY_YEAR

#' Canonical CSI category names (the 9 TPS open-data feeds).
#' @return Character vector.
#' @export
MORIE_TPS_CSI_CATEGORIES <- function() names(.TOTAL_CSI_WEIGHTS)


# ---------------------------------------------------------------------------
# Scalar weight lookup
# ---------------------------------------------------------------------------

#' Return the CSI weight for a TPS open-data category.
#'
#' @param category TPS category name (e.g. "Assault", "Homicides").
#' @param variant One of "total" or "violent".
#' @param weights Optional named numeric vector overriding the built-in
#'   tables.  When supplied, takes precedence over \code{variant}.
#' @return Numeric scalar (0 if unknown).
#' @export
morie_tps_csi_weight <- function(category, variant = c("total", "violent"),
                                   weights = NULL) {
  variant <- match.arg(variant)
  if (!is.null(weights)) {
    v <- if (category %in% names(weights)) weights[[category]] else NULL
    return(if (is.null(v)) 0.0 else as.numeric(v))
  }
  tbl <- if (variant == "total") .TOTAL_CSI_WEIGHTS else .VIOLENT_CSI_WEIGHTS
  if (!(category %in% names(tbl))) return(0.0)
  v <- tbl[[category]]
  if (is.null(v)) 0.0 else as.numeric(v)
}


# ---------------------------------------------------------------------------
# Internal: coerce counts input to long-format data.frame
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.tps_csi_to_long <- function(x, key_col) {
  if (is.data.frame(x)) {
    need <- c(key_col, "category", "count")
    if (!all(need %in% names(x))) {
      stop(sprintf(
        "long-format data.frame must have columns %s",
        paste(need, collapse = ", ")
      ), call. = FALSE)
    }
    return(x[, need, drop = FALSE])
  }
  if (is.list(x)) {
    rows <- list()
    for (k in names(x)) {
      cats <- x[[k]]
      if (!is.list(cats) && !is.numeric(cats)) {
        next
      }
      for (c in names(cats)) {
        rows[[length(rows) + 1L]] <- data.frame(
          k = if (suppressWarnings(!is.na(as.integer(k)))) as.integer(k) else k,
          category = c,
          count = as.integer(cats[[c]]),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) == 0L) {
      return(data.frame(k = integer(0), category = character(0),
                        count = integer(0),
                        stringsAsFactors = FALSE)[,
                          c("k", "category", "count")] |>
              stats::setNames(c(key_col, "category", "count")))
    }
    df <- do.call(rbind, rows)
    names(df)[1] <- key_col
    return(df)
  }
  stop("counts input must be a data.frame or a nested list.", call. = FALSE)
}


# ---------------------------------------------------------------------------
# Per-year CSI
# ---------------------------------------------------------------------------

#' Toronto CSI per fiscal year from per-category counts
#'
#' Accepts either a long-format data.frame (columns \code{year},
#' \code{category}, \code{count}) or a nested list keyed
#' \code{[[year]][[category]] = count}.
#'
#' Returns a data.frame indexed by year with columns:
#' \itemize{
#'   \item \code{raw_weighted_sum} -- sum_c w_c * n_(c,year)
#'   \item \code{total_count} -- sum_c n_(c,year)
#'   \item \code{population} -- Toronto population that year
#'   \item \code{csi_per_capita} -- raw_weighted_sum / population *
#'         per_capita_unit
#'   \item \code{simple_count_rate} -- total_count / population *
#'         per_capita_unit
#' }
#' When \code{rebase_to_year} is supplied, an additional
#' \code{csi_index} column is added, anchored so that that year's value
#' equals \code{rebase_to_value}.
#'
#' @param counts_per_year Long data.frame or nested list (see above).
#' @param variant One of "total" or "violent".
#' @param weights Optional override vector of weights.
#' @param population Optional named integer vector (year -> pop);
#'   defaults to \code{MORIE_TPS_TORONTO_POPULATION_BY_YEAR()}.
#' @param per_capita_unit Rate denominator (default 100000).
#' @param rebase_to_year Optional anchor year for the index.
#' @param rebase_to_value Index value at the anchor year (default 100).
#' @return A data.frame with one row per year.
#' @export
morie_tps_csi_per_year <- function(counts_per_year,
                                     variant = c("total", "violent"),
                                     weights = NULL, population = NULL,
                                     per_capita_unit = 100000L,
                                     rebase_to_year = NULL,
                                     rebase_to_value = 100.0) {
  variant <- match.arg(variant)
  long <- .tps_csi_to_long(counts_per_year, key_col = "year")
  long$year <- suppressWarnings(as.integer(long$year))
  long$count <- as.numeric(long$count)
  long$weight <- vapply(long$category,
                         function(c) morie_tps_csi_weight(c,
                                                           variant = variant,
                                                           weights = weights),
                         numeric(1))
  long$weighted <- long$count * long$weight

  ag <- stats::aggregate(
    cbind(weighted, count) ~ year, data = long, FUN = sum, na.rm = TRUE
  )
  ag <- ag[order(ag$year), , drop = FALSE]
  names(ag)[names(ag) == "weighted"] <- "raw_weighted_sum"
  names(ag)[names(ag) == "count"] <- "total_count"

  pop_map <- if (is.null(population)) .TORONTO_POPULATION_BY_YEAR else population
  ag$population <- as.numeric(pop_map[as.character(ag$year)])
  ag$csi_per_capita <- ag$raw_weighted_sum / ag$population *
    as.numeric(per_capita_unit)
  ag$simple_count_rate <- ag$total_count / ag$population *
    as.numeric(per_capita_unit)

  if (!is.null(rebase_to_year)) {
    anchor_row <- which(ag$year == as.integer(rebase_to_year))
    if (length(anchor_row) == 0L) {
      stop(sprintf(
        "rebase_to_year = %s not in %s",
        rebase_to_year, paste(ag$year, collapse = ", ")
      ), call. = FALSE)
    }
    anchor <- as.numeric(ag$csi_per_capita[anchor_row])
    ag$csi_index <- ag$csi_per_capita / anchor * as.numeric(rebase_to_value)
  }
  rownames(ag) <- NULL
  ag
}


# ---------------------------------------------------------------------------
# Per-neighbourhood CSI
# ---------------------------------------------------------------------------

#' CSI per neighbourhood (HOOD_158)
#'
#' Mirrors \code{\link{morie_tps_csi_per_year}} but groups by
#' neighbourhood ID rather than fiscal year.  Population is not divided
#' in here because TPS open data does not ship a per-ward population
#' table; callers are expected to merge in the City of Toronto Open
#' Data NeighbourhoodCrimeRates per-ward population for per-capita
#' rates.  Returns the un-normalised weighted sum + total count.
#'
#' @param counts_per_hood Long data.frame (columns \code{HOOD_158},
#'   \code{category}, \code{count}) or nested list
#'   \code{[[hood]][[category]] = count}.
#' @param variant One of "total" or "violent".
#' @param weights Optional override vector of weights.
#' @return A data.frame with one row per neighbourhood.
#' @export
morie_tps_csi_per_neighbourhood <- function(counts_per_hood,
                                              variant = c("total", "violent"),
                                              weights = NULL) {
  variant <- match.arg(variant)
  long <- .tps_csi_to_long(counts_per_hood, key_col = "HOOD_158")
  long$count <- as.numeric(long$count)
  long$weight <- vapply(long$category,
                         function(c) morie_tps_csi_weight(c,
                                                           variant = variant,
                                                           weights = weights),
                         numeric(1))
  long$weighted <- long$count * long$weight
  ag <- stats::aggregate(
    cbind(weighted, count) ~ HOOD_158, data = long, FUN = sum, na.rm = TRUE
  )
  names(ag)[names(ag) == "weighted"] <- "raw_weighted_sum"
  names(ag)[names(ag) == "count"] <- "total_count"
  rownames(ag) <- NULL
  ag
}


# ---------------------------------------------------------------------------
# High-level orchestrator
# ---------------------------------------------------------------------------

#' Toronto CSI per-year + per-ward from a named list of TPS data.frames
#'
#' High-level orchestration: takes a named list \code{dfs} of TPS
#' open-data data.frames (one per CSI category) and returns both the
#' per-year and per-ward CSI as a single rich-result object.
#'
#' @param dfs Named list of TPS data.frames.  Keys outside of
#'   \code{MORIE_TPS_CSI_CATEGORIES()} are ignored.
#' @param year_col Year column name (default "OCC_YEAR").
#' @param hood_col Neighbourhood column name (default "HOOD_158").
#' @param variant One of "total" or "violent" (default "total").
#' @return A \code{morie_tps_result} named list carrying \code{by_year}
#'   and \code{by_hood} data.frames in \code{payload}.
#' @export
morie_tps_analyze_csi_from_dataframes <- function(dfs,
                                                     year_col = "OCC_YEAR",
                                                     hood_col = "HOOD_158",
                                                     variant = c("total",
                                                                 "violent")) {
  stopifnot(is.list(dfs))
  variant <- match.arg(variant)
  cats_known <- MORIE_TPS_CSI_CATEGORIES()

  py_counts <- list()
  pn_counts <- list()
  for (cat in names(dfs)) {
    if (!(cat %in% cats_known)) {
      next
    }
    df <- dfs[[cat]]
    if (!is.data.frame(df)) {
      next
    }
    if (year_col %in% names(df)) {
      y <- suppressWarnings(as.integer(df[[year_col]]))
      y <- y[is.finite(y)]
      if (length(y) > 0L) {
        tab <- table(y)
        for (yk in names(tab)) {
          py_counts[[yk]] <- py_counts[[yk]] %||% list()
          py_counts[[yk]][[cat]] <- as.integer(tab[[yk]])
        }
      }
    }
    if (hood_col %in% names(df)) {
      h_raw <- df[[hood_col]]
      h <- suppressWarnings(as.integer(h_raw))
      h <- h[is.finite(h)]
      if (length(h) > 0L) {
        tab <- table(h)
        for (hk in names(tab)) {
          pn_counts[[hk]] <- pn_counts[[hk]] %||% list()
          pn_counts[[hk]][[cat]] <- as.integer(tab[[hk]])
        }
      }
    }
  }

  warnings <- character(0)
  by_year <- if (length(py_counts) > 0L) {
    morie_tps_csi_per_year(py_counts, variant = variant)
  } else {
    warnings <- c(warnings,
                  sprintf("no usable %s column in any supplied data.frame.",
                          year_col))
    data.frame()
  }
  by_hood <- if (length(pn_counts) > 0L) {
    morie_tps_csi_per_neighbourhood(pn_counts, variant = variant)
  } else {
    warnings <- c(warnings,
                  sprintf("no usable %s column in any supplied data.frame.",
                          hood_col))
    data.frame()
  }

  included <- sort(intersect(names(dfs), cats_known))
  years_covered <- if (nrow(by_year) > 0L) {
    sort(as.integer(by_year$year))
  } else {
    integer(0)
  }
  wards_covered <- if (nrow(by_hood) > 0L) nrow(by_hood) else 0L
  total_weighted <- if (nrow(by_year) > 0L) {
    sum(by_year$raw_weighted_sum, na.rm = TRUE)
  } else {
    NA_real_
  }
  most_recent_csi <- if (nrow(by_year) > 0L) {
    as.numeric(by_year$csi_per_capita[nrow(by_year)])
  } else {
    NA_real_
  }

  summary_lines <- list(
    `CSI variant` = variant,
    `Categories included` = paste(included, collapse = ", "),
    `Years covered` = if (length(years_covered) > 0L) {
      paste(range(years_covered), collapse = "-")
    } else {
      "(none)"
    },
    `Wards covered` = as.integer(wards_covered),
    `Total weighted incidents (all years)` = round(total_weighted, 1),
    `CSI/100k most-recent year` = round(most_recent_csi, 2)
  )

  interpretation <- paste(
    "Crime Severity Index weights each incident by the average",
    "sentence times the incarceration rate of its underlying Criminal",
    "Code offence.  CSI rises with both volume and severity; a city",
    "where homicides drop but minor thefts rise will see a falling CSI",
    "even if the absolute crime count is flat.  Statistics Canada uses",
    "CSI to compare 33 Census Metropolitan Areas; Toronto's",
    "most-recent published CSI (2023) is approximately 60.4 (Total)",
    "and 71.8 (Violent), against a national mean of 80.5 -- Toronto",
    "sits below the national average on Total but above on Violent."
  )

  weights_used <- if (variant == "total") {
    .TOTAL_CSI_WEIGHTS
  } else {
    .VIOLENT_CSI_WEIGHTS
  }

  out <- list(
    title = "Toronto Crime Severity Index -- per-year + per-ward",
    call = sprintf("morie_tps_analyze_csi_from_dataframes(dfs, variant=%s)",
                   sQuote(variant)),
    summary_lines = summary_lines,
    tables = list(),
    warnings = warnings,
    interpretation = interpretation,
    payload = list(by_year = by_year, by_hood = by_hood,
                   variant = variant, weights = weights_used,
                   included = included)
  )
  class(out) <- c("morie_tps_result", "morie_rich_result", "list")
  out
}


# %||% fallback (also defined in tps_crime.R / arsau.R).
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
