# SPDX-License-Identifier: AGPL-3.0-or-later
#' Generic Multilevel Reconciliation Methodology (MRM) Use-of-Force callables
#'
#' Six jurisdiction-agnostic analyses for police Use-of-Force data,
#' mirroring the Python module \code{morie.mrm_uof}. Every function
#' accepts a \code{data.frame} (or \code{tibble}) and returns a named
#' \code{list} carrying both the numeric outputs and a multi-paragraph
#' plain-language \code{interpretation}, so the result can be printed
#' to a notebook without further post-processing.
#'
#' Functions
#' ---------
#'
#' \itemize{
#'   \item \code{\link{mrm_uof_force_concentration}}: Hill-MLE Pareto
#'     exponent + Gini coefficient + top-5 / top-10 share for incident
#'     counts aggregated by force / service.
#'   \item \code{\link{mrm_uof_weapon_diversity}}: weapon-by-force
#'     contingency: chi-square, Cramer's V, and the top-3 cells by
#'     standardised Pearson residual.
#'   \item \code{\link{mrm_uof_yoy_change}}: year-on-year percentage
#'     change with a manual largest-gap change-point fallback (the R
#'     side does not require \pkg{ruptures}).
#'   \item \code{\link{mrm_uof_region_locality}}: region-at-time vs.
#'     region-now contingency: diagonal share, chi-square, Cramer's V.
#'   \item \code{\link{mrm_uof_demographic_disparity}}: per-category
#'     outcome rates with Wilson 95\% intervals, risk-ratio versus a
#'     baseline group, optional non-parametric bootstrap percentile
#'     interval on the risk ratio.
#'   \item \code{\link{mrm_uof_data_quality_audit}}: per-column null
#'     and dtype audit, with optional schema-comparison against a
#'     supplied CKAN sidecar list or column-spec list.
#' }
#'
#' @name mrm_uof
NULL


# ---------------------------------------------------------------------------
# Internal helpers (NOT exported)
# ---------------------------------------------------------------------------

.uof_gini <- function(x) {
  x <- sort(as.numeric(x))
  n <- length(x)
  s <- sum(x)
  if (n == 0L || s == 0) {
    return(NA_real_)
  }
  (2 * sum(seq_len(n) * x) - (n + 1L) * s) / (n * s)
}

.uof_hill_alpha <- function(x, x_min = 1.0) {
  x <- as.numeric(x)
  x <- x[!is.na(x) & x >= x_min]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  denom <- sum(log(x / x_min))
  if (denom <= 0) {
    return(NA_real_)
  }
  1.0 + length(x) / denom
}

.uof_topk_share <- function(x, k) {
  x <- as.numeric(x)
  s <- sum(x, na.rm = TRUE)
  if (s == 0 || length(x) == 0L) {
    return(NA_real_)
  }
  k <- min(as.integer(k), length(x))
  sum(sort(x, decreasing = TRUE)[seq_len(k)]) / s
}

.uof_wilson_ci <- function(k, n, z = 1.959963984540054) {
  if (n == 0L) {
    return(c(NA_real_, NA_real_))
  }
  p <- k / n
  z2 <- z * z
  denom <- 1.0 + z2 / n
  centre <- (p + z2 / (2.0 * n)) / denom
  # Uncorrected Wilson -- the continuity-corrected form has corner
  # cases that need square-root-of-negative guards. The uncorrected
  # form is universally finite and is what most R wrappers
  # (binom::binom.wilson, Hmisc::binconf) emit by default.
  margin <- z * sqrt(p * (1 - p) / n + z2 / (4 * n * n)) / denom
  c(max(0.0, centre - margin), min(1.0, centre + margin))
}

.uof_cramers_v <- function(chi2, n, r, c) {
  k <- min(r - 1L, c - 1L)
  if (k <= 0L || n == 0L) {
    return(NA_real_)
  }
  sqrt(chi2 / (n * k))
}

.uof_fmt_pct <- function(p) {
  if (!is.finite(p)) {
    return("n/a")
  }
  sprintf("%.2f%%", 100 * p)
}

.uof_result <- function(title, call, summary_lines = list(),
                         warnings = character(0),
                         interpretation = "",
                         ...) {
  out <- list(
    title = title,
    call = call,
    summary_lines = summary_lines,
    warnings = warnings,
    interpretation = interpretation,
    ...
  )
  class(out) <- c("morie_mrm_uof_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# 1. force_concentration
# ---------------------------------------------------------------------------

#' Concentration of UoF incidents across forces / services
#'
#' Aggregates per-force incident counts and reports a Hill-MLE Pareto
#' tail exponent, the Gini coefficient, and the top-5 / top-10
#' concentration shares.
#'
#' @param df A \code{data.frame} or \code{tibble} with one row per
#'   incident (when \code{count_col} is \code{NULL}) or one row per
#'   force-period with a numeric \code{count_col}.
#' @param force_col Character. Name of the column identifying the
#'   force / service / agency.
#' @param count_col Character or \code{NULL}. If supplied, the
#'   per-row incident count to sum within each force; otherwise each
#'   row counts as one incident.
#' @return A named list with classes \code{morie_mrm_uof_result},
#'   \code{morie_rich_result}, \code{list}. Numeric outputs include
#'   \code{pareto_alpha_mle}, \code{gini}, \code{top5_share},
#'   \code{top10_share}, \code{n_forces}, \code{n_incidents}.
#' @examples
#' df <- data.frame(force = c(rep("A", 50), rep("B", 5)))
#' res <- mrm_uof_force_concentration(df, "force")
#' res$gini
#' @export
mrm_uof_force_concentration <- function(df, force_col, count_col = NULL) {
  stopifnot(is.data.frame(df), is.character(force_col), length(force_col) == 1L)

  if (!(force_col %in% names(df))) {
    return(.uof_result(
      "MRM-UOF Force Concentration",
      sprintf("mrm_uof_force_concentration(df=<%dr>, force_col=%s)",
              nrow(df), force_col),
      warnings = sprintf("force_col '%s' not in dataframe", force_col),
      interpretation = sprintf(
        "No analysis: column '%s' is absent from the supplied dataframe.",
        force_col
      ),
      n = 0L
    ))
  }

  if (is.null(count_col)) {
    counts <- table(df[[force_col]])
  } else {
    if (!(count_col %in% names(df))) {
      return(.uof_result(
        "MRM-UOF Force Concentration",
        sprintf("mrm_uof_force_concentration(df, force_col=%s, count_col=%s)",
                force_col, count_col),
        warnings = sprintf("count_col '%s' not in dataframe", count_col),
        interpretation = sprintf(
          "No analysis: count column '%s' is absent.", count_col
        ),
        n = 0L
      ))
    }
    counts <- tapply(df[[count_col]], df[[force_col]], sum, na.rm = TRUE)
  }
  counts <- sort(counts, decreasing = TRUE)
  x <- as.numeric(counts)
  n_forces <- length(x)
  n_incidents <- sum(x)

  warnings <- character(0)
  if (n_incidents == 0) {
    return(.uof_result(
      "MRM-UOF Force Concentration",
      sprintf("mrm_uof_force_concentration(df, force_col=%s)", force_col),
      warnings = "All counts are zero; concentration is undefined.",
      interpretation = "All per-force counts are zero, so no concentration statistics can be computed.",
      n = 0L,
      n_forces = n_forces,
      n_incidents = 0L
    ))
  }
  if (n_forces < 10L) {
    warnings <- c(warnings, sprintf(
      "Only %d force(s); concentration statistics with n<10 categories are descriptive at best.",
      n_forces
    ))
  }

  alpha <- .uof_hill_alpha(x)
  gini <- .uof_gini(x)
  top5 <- .uof_topk_share(x, 5L)
  top10 <- .uof_topk_share(x, 10L)

  tail_text <- if (is.na(alpha)) {
    "Pareto alpha could not be estimated."
  } else if (alpha < 2.0) {
    "An alpha below 2 indicates a very heavy upper tail: a small number of forces accounts for a disproportionate share of incidents."
  } else if (alpha < 3.0) {
    "An alpha between 2 and 3 indicates a heavy tail with finite mean but infinite variance under the power-law model."
  } else {
    "An alpha above 3 indicates a comparatively thin tail; incidents are spread relatively evenly across forces given a power-law fit."
  }

  gini_text <- if (is.na(gini)) {
    "Gini coefficient could not be computed."
  } else if (gini < 0.30) {
    "Gini below 0.30 indicates an approximately even distribution."
  } else if (gini < 0.60) {
    "Gini between 0.30 and 0.60 indicates moderate concentration."
  } else {
    "Gini at or above 0.60 indicates strong concentration: a few forces carry most of the volume."
  }

  interp <- sprintf(
    "Across %d force(s) and %d total incident(s), the top-5 forces hold %s of recorded volume and the top-10 hold %s. %s %s",
    n_forces, n_incidents, .uof_fmt_pct(top5), .uof_fmt_pct(top10), gini_text, tail_text
  )

  .uof_result(
    "MRM-UOF Force Concentration",
    sprintf("mrm_uof_force_concentration(df=<%dr>, force_col=%s)", nrow(df), force_col),
    summary_lines = list(
      `Forces (n)` = n_forces,
      `Incidents (n)` = n_incidents,
      `Pareto alpha (Hill MLE)` = alpha,
      `Gini coefficient` = gini,
      `Top-5 share` = top5,
      `Top-10 share` = top10
    ),
    warnings = warnings,
    interpretation = interp,
    n = n_forces,
    n_forces = n_forces,
    n_incidents = n_incidents,
    pareto_alpha_mle = alpha,
    gini = gini,
    top5_share = top5,
    top10_share = top10,
    counts = as.list(counts)
  )
}


# ---------------------------------------------------------------------------
# 2. weapon_diversity
# ---------------------------------------------------------------------------

#' Weapon-by-force contingency test
#'
#' Builds a weapon x force contingency table, runs a chi-square test
#' of independence, computes Cramer's V, and reports the top-3
#' (weapon, force) cells by standardised Pearson residual.
#'
#' @param df A data.frame.
#' @param weapon_col Categorical weapon column.
#' @param force_col Categorical force / service column.
#' @return A named list with \code{chi2}, \code{pvalue}, \code{df},
#'   \code{cramers_v}, \code{top_residuals} (list-of-lists), and an
#'   \code{interpretation} paragraph.
#' @export
mrm_uof_weapon_diversity <- function(df, weapon_col, force_col) {
  stopifnot(is.data.frame(df), is.character(weapon_col), is.character(force_col))

  for (col in c(weapon_col, force_col)) {
    if (!(col %in% names(df))) {
      return(.uof_result(
        "MRM-UOF Weapon Diversity",
        sprintf("mrm_uof_weapon_diversity(df, weapon_col=%s, force_col=%s)",
                weapon_col, force_col),
        warnings = sprintf("column '%s' missing", col),
        interpretation = sprintf("No analysis: required column '%s' is missing.", col),
        n = 0L
      ))
    }
  }

  tab <- table(df[[weapon_col]], df[[force_col]])
  n <- sum(tab)
  r <- nrow(tab)
  c <- ncol(tab)

  if (n == 0L || r < 2L || c < 2L) {
    return(.uof_result(
      "MRM-UOF Weapon Diversity",
      sprintf("mrm_uof_weapon_diversity(df, %s, %s)", weapon_col, force_col),
      warnings = "Contingency table is degenerate (n=0 or single row/column).",
      interpretation = "No analysis: contingency table has fewer than two rows or columns.",
      n = n, table_shape = c(r, c)
    ))
  }

  # Suppress the always-fired warning about chi-square approximation
  # -- we surface our own version below based on inspected expecteds.
  ct <- suppressWarnings(stats::chisq.test(tab))
  chi2 <- as.numeric(ct$statistic)
  pvalue <- as.numeric(ct$p.value)
  dof <- as.integer(ct$parameter)
  expected <- ct$expected
  v <- .uof_cramers_v(chi2, n, r, c)

  warnings <- character(0)
  low_expected <- any(expected < 5)
  n_low <- sum(expected < 5)
  if (low_expected) {
    warnings <- c(warnings, sprintf(
      "%d of %d expected cell(s) below 5; chi-square approximation may be unreliable.",
      n_low, length(expected)
    ))
  }

  std_resid <- (tab - expected) / sqrt(expected)
  abs_resid <- abs(std_resid)
  flat_order <- order(abs_resid, decreasing = TRUE)[seq_len(min(3, length(abs_resid)))]
  top_resid <- vector("list", length(flat_order))
  for (i in seq_along(flat_order)) {
    fi <- flat_order[i]
    ri <- ((fi - 1L) %% nrow(abs_resid)) + 1L
    ci <- ((fi - 1L) %/% nrow(abs_resid)) + 1L
    top_resid[[i]] <- list(
      weapon = rownames(tab)[ri],
      force = colnames(tab)[ci],
      observed = as.integer(tab[ri, ci]),
      expected = as.numeric(expected[ri, ci]),
      std_residual = as.numeric(std_resid[ri, ci])
    )
  }

  assoc_text <- if (is.na(v)) {
    "Association strength could not be computed."
  } else if (v < 0.10) {
    "Association is negligible (Cramer's V < 0.10)."
  } else if (v < 0.30) {
    "Association is weak (0.10 <= V < 0.30)."
  } else if (v < 0.50) {
    "Association is moderate (0.30 <= V < 0.50)."
  } else {
    "Association is strong (V >= 0.50)."
  }

  interp <- sprintf(
    "The chi-square test on a %d x %d table yields chi2=%.4f on %d df (p=%.4g). %s The strongest deviation from independence sits at (%s, %s) with a standardised residual of %+.2f.",
    r, c, chi2, dof, pvalue, assoc_text,
    top_resid[[1]]$weapon, top_resid[[1]]$force, top_resid[[1]]$std_residual
  )

  .uof_result(
    "MRM-UOF Weapon Diversity",
    sprintf("mrm_uof_weapon_diversity(df, %s, %s)", weapon_col, force_col),
    summary_lines = list(
      `N incidents` = n,
      `Table shape` = sprintf("%d weapons x %d forces", r, c),
      `Chi-square` = chi2,
      `df` = dof,
      `p-value` = pvalue,
      `Cramer's V` = v
    ),
    warnings = warnings,
    interpretation = interp,
    n = n,
    chi2 = chi2,
    pvalue = pvalue,
    df = dof,
    cramers_v = v,
    table_shape = c(r, c),
    low_expected_count = low_expected,
    top_residuals = top_resid
  )
}


# ---------------------------------------------------------------------------
# 3. yoy_change
# ---------------------------------------------------------------------------

#' Year-on-year change in incident counts
#'
#' Either supply \code{dfs_by_year} (named list mapping year string /
#' integer to a data.frame) or \code{df} + \code{year_col}.
#'
#' Change-point detection is the manual largest-absolute-difference
#' heuristic (the R port does not require \pkg{changepoint}).
#'
#' @param dfs_by_year Named list of \code{data.frame}s, names coerced
#'   to integer years.
#' @param df A data.frame to be grouped by \code{year_col}.
#' @param year_col Required when \code{df} is supplied.
#' @param count_col Optional column to sum within each year (rows
#'   counted otherwise).
#' @return Named list with \code{years}, \code{counts}, \code{yoy_pct},
#'   \code{change_point_year}, \code{mean_abs_yoy_pct}.
#' @export
mrm_uof_yoy_change <- function(dfs_by_year = NULL, df = NULL,
                                year_col = NULL, count_col = NULL) {
  warnings <- character(0)

  if (is.null(dfs_by_year) && is.null(df)) {
    return(.uof_result(
      "MRM-UOF Year-on-Year Change",
      "mrm_uof_yoy_change()",
      warnings = "Neither dfs_by_year nor df was supplied.",
      interpretation = "No analysis: no input data supplied.",
      n = 0L
    ))
  }

  if (!is.null(dfs_by_year)) {
    yrs <- sort(as.integer(names(dfs_by_year)))
    counts <- integer(length(yrs))
    for (i in seq_along(yrs)) {
      sub <- dfs_by_year[[as.character(yrs[i])]]
      if (is.null(sub)) {
        # name may have been numeric; try integer match
        sub <- dfs_by_year[[yrs[i]]]
      }
      if (is.null(count_col)) {
        counts[i] <- nrow(sub)
      } else if (count_col %in% names(sub)) {
        counts[i] <- sum(sub[[count_col]], na.rm = TRUE)
      } else {
        warnings <- c(warnings, sprintf("Year %d: count_col missing", yrs[i]))
        counts[i] <- 0L
      }
    }
  } else {
    if (is.null(year_col) || !(year_col %in% names(df))) {
      return(.uof_result(
        "MRM-UOF Year-on-Year Change",
        sprintf("mrm_uof_yoy_change(df, year_col=%s)", year_col),
        warnings = sprintf("year_col '%s' missing", year_col),
        interpretation = "No analysis: year column missing.",
        n = 0L
      ))
    }
    if (is.null(count_col)) {
      tab <- table(df[[year_col]])
    } else {
      if (!(count_col %in% names(df))) {
        return(.uof_result(
          "MRM-UOF Year-on-Year Change",
          sprintf("mrm_uof_yoy_change(df, year_col=%s, count_col=%s)", year_col, count_col),
          warnings = sprintf("count_col '%s' missing", count_col),
          interpretation = "No analysis: count column missing.",
          n = 0L
        ))
      }
      tab <- tapply(df[[count_col]], df[[year_col]], sum, na.rm = TRUE)
    }
    yrs <- sort(as.integer(names(tab)))
    counts <- as.integer(tab[as.character(yrs)])
  }

  n_years <- length(yrs)
  if (n_years == 0L) {
    return(.uof_result(
      "MRM-UOF Year-on-Year Change",
      "mrm_uof_yoy_change()",
      warnings = "No years present.",
      interpretation = "No analysis: no years were found.",
      n = 0L, years = integer(0), counts = integer(0)
    ))
  }

  yoy_pct <- rep(NA_real_, n_years)
  for (i in seq_len(n_years)[-1]) {
    prev <- counts[i - 1]
    if (!is.na(prev) && prev != 0) {
      yoy_pct[i] <- (counts[i] - prev) / prev * 100.0
    }
  }

  change_point_year <- NA_integer_
  change_point_method <- "none"
  if (n_years < 3L) {
    warnings <- c(warnings, sprintf("Only %d year(s); too few for change-point detection.", n_years))
  } else {
    diffs <- abs(diff(counts))
    if (length(diffs) > 0L) {
      idx <- which.max(diffs) + 1L
      change_point_year <- yrs[idx]
      change_point_method <- "largest-abs-diff (no changepoint package)"
    }
  }

  finite_yoy <- yoy_pct[is.finite(yoy_pct)]
  mean_abs_yoy <- if (length(finite_yoy) == 0L) NA_real_ else mean(abs(finite_yoy))

  cp_text <- if (is.na(change_point_year)) {
    "No change-point could be identified."
  } else {
    sprintf("A structural break was identified at year %d using %s.", change_point_year, change_point_method)
  }
  vol_text <- if (is.na(mean_abs_yoy)) {
    "Year-on-year volatility is undefined."
  } else {
    sprintf("Mean absolute year-on-year change across %d transition(s) is %.2f%%.", length(finite_yoy), mean_abs_yoy)
  }

  interp <- sprintf(
    "Series spans %d year(s) (%d-%d) with a total of %d incident(s). %s %s",
    n_years, yrs[1], yrs[n_years], sum(counts), vol_text, cp_text
  )

  .uof_result(
    "MRM-UOF Year-on-Year Change",
    sprintf("mrm_uof_yoy_change(n_years=%d)", n_years),
    summary_lines = list(
      `Years (n)` = n_years,
      `Total incidents` = sum(counts),
      `Mean |YoY| %` = mean_abs_yoy,
      `Change-point year` = if (is.na(change_point_year)) "none" else change_point_year,
      `Change-point method` = change_point_method
    ),
    warnings = warnings,
    interpretation = interp,
    n = n_years,
    years = yrs,
    counts = counts,
    yoy_pct = yoy_pct,
    change_point_year = change_point_year,
    change_point_method = change_point_method,
    mean_abs_yoy_pct = mean_abs_yoy
  )
}


# ---------------------------------------------------------------------------
# 4. region_locality
# ---------------------------------------------------------------------------

#' Region-at-time vs region-now locality contingency
#'
#' @param df A data.frame.
#' @param region_at_col Region at the time of the incident.
#' @param region_now_col Most-recent region.
#' @return Named list with \code{diagonal_share}, \code{chi2},
#'   \code{pvalue}, \code{df}, \code{cramers_v}.
#' @export
mrm_uof_region_locality <- function(df, region_at_col, region_now_col) {
  stopifnot(is.data.frame(df))
  warnings <- character(0)

  for (col in c(region_at_col, region_now_col)) {
    if (!(col %in% names(df))) {
      return(.uof_result(
        "MRM-UOF Region Locality",
        sprintf("mrm_uof_region_locality(df, %s, %s)", region_at_col, region_now_col),
        warnings = sprintf("column '%s' missing", col),
        interpretation = sprintf("No analysis: required column '%s' is absent.", col),
        n = 0L
      ))
    }
  }

  pair <- df[, c(region_at_col, region_now_col), drop = FALSE]
  n_pre <- nrow(pair)
  pair <- pair[stats::complete.cases(pair), , drop = FALSE]
  n_dropped <- n_pre - nrow(pair)
  if (n_dropped > 0L) {
    warnings <- c(warnings, sprintf("Dropped %d row(s) with NaN in region columns.", n_dropped))
  }
  if (nrow(pair) == 0L) {
    return(.uof_result(
      "MRM-UOF Region Locality",
      sprintf("mrm_uof_region_locality(df, %s, %s)", region_at_col, region_now_col),
      warnings = c(warnings, "Empty contingency after NaN drop."),
      interpretation = "No analysis: no rows remain after NaN drop.",
      n = 0L, n_dropped = n_dropped
    ))
  }

  # Union of categories so diagonal is well-defined.
  all_levels <- sort(unique(c(as.character(pair[[region_at_col]]),
                              as.character(pair[[region_now_col]]))))
  pair[[region_at_col]] <- factor(pair[[region_at_col]], levels = all_levels)
  pair[[region_now_col]] <- factor(pair[[region_now_col]], levels = all_levels)
  tab <- table(pair[[region_at_col]], pair[[region_now_col]])
  obs <- as.matrix(tab)
  n <- sum(obs)
  diag_share <- if (n > 0L) sum(diag(obs)) / n else NA_real_

  r <- nrow(obs)
  c <- ncol(obs)
  if (r >= 2L && c >= 2L) {
    ct <- suppressWarnings(stats::chisq.test(obs))
    chi2 <- as.numeric(ct$statistic)
    pvalue <- as.numeric(ct$p.value)
    dof <- as.integer(ct$parameter)
    v <- .uof_cramers_v(chi2, n, r, c)
    if (any(ct$expected < 5)) {
      warnings <- c(warnings, "Expected cell counts below 5; chi-square may be unreliable.")
    }
  } else {
    chi2 <- NA_real_
    pvalue <- NA_real_
    dof <- 0L
    v <- NA_real_
    warnings <- c(warnings, "Contingency table too small for chi-square test.")
  }

  if (n < 30L) {
    warnings <- c(warnings, sprintf("Sample size n=%d is small; descriptive only.", n))
  }

  loc_text <- if (is.na(diag_share)) {
    "Diagonal share could not be computed."
  } else if (diag_share >= 0.75) {
    sprintf("Diagonal share is %s, indicating high locality stability.", .uof_fmt_pct(diag_share))
  } else if (diag_share >= 0.50) {
    sprintf("Diagonal share is %s, indicating moderate locality stability.", .uof_fmt_pct(diag_share))
  } else {
    sprintf("Diagonal share is %s, indicating substantial cross-regional movement.", .uof_fmt_pct(diag_share))
  }

  interp <- sprintf(
    "%s Chi-square test on the %d x %d contingency table yields chi2=%.4f on %d df (p=%.4g); Cramer's V is %.4f.",
    loc_text, r, c, chi2, dof, pvalue, v
  )

  .uof_result(
    "MRM-UOF Region Locality",
    sprintf("mrm_uof_region_locality(df, %s, %s)", region_at_col, region_now_col),
    summary_lines = list(
      `N pairs` = n,
      `Categories` = sprintf("%d x %d", r, c),
      `Diagonal share` = diag_share,
      `Chi-square` = chi2,
      `df` = dof,
      `p-value` = pvalue,
      `Cramer's V` = v
    ),
    warnings = warnings,
    interpretation = interp,
    n = n,
    n_dropped = n_dropped,
    diagonal_share = diag_share,
    chi2 = chi2,
    pvalue = pvalue,
    df = dof,
    cramers_v = v,
    table_shape = c(r, c)
  )
}


# ---------------------------------------------------------------------------
# 5. demographic_disparity
# ---------------------------------------------------------------------------

#' Demographic disparity in outcome rates with risk-ratio CIs
#'
#' @param df A data.frame.
#' @param demo_col Categorical demographic column.
#' @param outcome_col Binary outcome column (0/1 or logical).
#' @param baseline Optional baseline category (default: largest-N group).
#' @param bootstrap_reps Bootstrap replications for the RR percentile
#'   CI. Set to 0 (default) to skip.
#' @return Named list with \code{baseline}, \code{baseline_rate},
#'   \code{per_category} (list of lists), \code{risk_ratios}.
#' @export
mrm_uof_demographic_disparity <- function(df, demo_col, outcome_col,
                                           baseline = NULL,
                                           bootstrap_reps = 0L) {
  stopifnot(is.data.frame(df))
  warnings <- character(0)

  for (col in c(demo_col, outcome_col)) {
    if (!(col %in% names(df))) {
      return(.uof_result(
        "MRM-UOF Demographic Disparity",
        sprintf("mrm_uof_demographic_disparity(df, %s, %s)", demo_col, outcome_col),
        warnings = sprintf("column '%s' missing", col),
        interpretation = sprintf("No analysis: required column '%s' is absent.", col),
        n = 0L
      ))
    }
  }

  sub <- df[, c(demo_col, outcome_col), drop = FALSE]
  sub <- sub[stats::complete.cases(sub), , drop = FALSE]
  if (nrow(sub) == 0L) {
    return(.uof_result(
      "MRM-UOF Demographic Disparity",
      sprintf("mrm_uof_demographic_disparity(df, %s, %s)", demo_col, outcome_col),
      warnings = "No non-null rows.",
      interpretation = "No analysis: no non-null rows after NaN drop.",
      n = 0L
    ))
  }

  # Coerce outcome to 0/1
  y <- sub[[outcome_col]]
  if (is.logical(y)) {
    y_int <- as.integer(y)
  } else if (is.numeric(y)) {
    y_int <- as.integer(y != 0)
  } else {
    y_str <- tolower(as.character(y))
    map_pos <- c("yes", "true", "1", "y", "t")
    map_neg <- c("no", "false", "0", "n", "f")
    y_int <- ifelse(y_str %in% map_pos, 1L,
                    ifelse(y_str %in% map_neg, 0L, NA_integer_))
    if (anyNA(y_int)) {
      warnings <- c(warnings, "Some outcome values were not yes/no/0/1; rows dropped.")
      keep <- !is.na(y_int)
      sub <- sub[keep, , drop = FALSE]
      y_int <- y_int[keep]
    }
  }
  sub$._outcome <- y_int

  # If outcome coercion dropped every row, bail out cleanly rather
  # than letting aggregate() error on an empty group.
  if (nrow(sub) == 0L) {
    return(.uof_result(
      "MRM-UOF Demographic Disparity",
      sprintf("mrm_uof_demographic_disparity(df, %s, %s)", demo_col, outcome_col),
      warnings = c(warnings,
                    "No interpretable outcome values found after coercion."),
      interpretation = "No analysis: no rows survived outcome coercion (none of the outcome values matched yes/no/0/1/true/false).",
      n = 0L
    ))
  }

  agg <- aggregate(._outcome ~ ., data = sub[, c(demo_col, "._outcome")],
                    FUN = function(v) c(n = length(v), k = sum(v)))
  per <- data.frame(
    category = as.character(agg[[demo_col]]),
    n = as.integer(agg$._outcome[, "n"]),
    k = as.integer(agg$._outcome[, "k"]),
    stringsAsFactors = FALSE
  )
  per$rate <- per$k / per$n
  per <- per[order(per$rate, decreasing = TRUE), , drop = FALSE]

  if (is.null(baseline)) {
    baseline <- per$category[which.max(per$n)]
  }
  if (!(baseline %in% per$category)) {
    return(.uof_result(
      "MRM-UOF Demographic Disparity",
      sprintf("mrm_uof_demographic_disparity(df, %s, %s, baseline=%s)",
              demo_col, outcome_col, baseline),
      warnings = sprintf("baseline '%s' not present", baseline),
      interpretation = sprintf("No analysis: baseline category '%s' is absent.", baseline),
      n = nrow(sub)
    ))
  }
  baseline_rate <- per$rate[per$category == baseline]
  if (baseline_rate < 0.01) {
    warnings <- c(warnings, sprintf(
      "Baseline rate %.4f is below 1%%; risk ratios are unstable.", baseline_rate
    ))
  }

  per_cat <- vector("list", nrow(per))
  for (i in seq_len(nrow(per))) {
    row <- per[i, ]
    wci <- .uof_wilson_ci(row$k, row$n)
    rr <- if (baseline_rate > 0) row$rate / baseline_rate else NA_real_
    rr_lo <- NA_real_
    rr_hi <- NA_real_
    if (row$n < 30L) {
      warnings <- c(warnings, sprintf("Group '%s' has n=%d < 30; wide CI.", row$category, row$n))
    }
    if (bootstrap_reps > 0 && row$category != baseline) {
      sub_cat <- sub$._outcome[sub[[demo_col]] == row$category]
      sub_base <- sub$._outcome[sub[[demo_col]] == baseline]
      if (length(sub_cat) > 0L && length(sub_base) > 0L) {
        set.seed(0)
        draws <- numeric(bootstrap_reps)
        for (b in seq_len(bootstrap_reps)) {
          bi <- sample(sub_cat, length(sub_cat), replace = TRUE)
          bj <- sample(sub_base, length(sub_base), replace = TRUE)
          rj <- mean(bj)
          draws[b] <- if (rj > 0) mean(bi) / rj else NA_real_
        }
        draws <- draws[is.finite(draws)]
        if (length(draws) >= 20L) {
          rr_lo <- quantile(draws, 0.025, names = FALSE)
          rr_hi <- quantile(draws, 0.975, names = FALSE)
        }
      }
    }
    per_cat[[i]] <- list(
      category = row$category, n = row$n, k = row$k,
      rate = row$rate, lo = wci[1], hi = wci[2],
      rr = rr, rr_lo = rr_lo, rr_hi = rr_hi,
      baseline = row$category == baseline
    )
  }

  non_base_rr <- vapply(per_cat, function(e) if (!e$baseline) e$rr else NA_real_, numeric(1))
  non_base_rr <- non_base_rr[is.finite(non_base_rr)]
  if (length(non_base_rr) > 0L) {
    max_rr <- max(non_base_rr)
    max_cat <- vapply(per_cat, function(e) e$category, character(1))[
      which(vapply(per_cat, function(e) !e$baseline && is.finite(e$rr) && e$rr == max_rr, logical(1)))[1]
    ]
    rr_text <- sprintf(
      "The largest disparity is for group '%s' with a risk ratio of %.3f relative to the baseline ('%s', rate=%.4f).",
      max_cat, max_rr, baseline, baseline_rate
    )
  } else {
    rr_text <- "No non-baseline risk ratios could be computed."
  }
  boot_text <- if (bootstrap_reps > 0L) {
    sprintf("Risk-ratio CIs from %d bootstrap replications (seed=0).", bootstrap_reps)
  } else {
    "Bootstrap not requested (bootstrap_reps=0); only Wilson intervals shown."
  }

  .uof_result(
    "MRM-UOF Demographic Disparity",
    sprintf("mrm_uof_demographic_disparity(df, %s, %s, bootstrap_reps=%d)",
            demo_col, outcome_col, bootstrap_reps),
    summary_lines = list(
      `N subjects` = nrow(sub),
      `Categories` = nrow(per),
      `Baseline` = baseline,
      `Baseline rate` = baseline_rate
    ),
    warnings = warnings,
    interpretation = paste(rr_text, boot_text, sep = " "),
    n = nrow(sub),
    baseline = baseline,
    baseline_rate = baseline_rate,
    per_category = per_cat,
    risk_ratios = setNames(
      vapply(per_cat, function(e) e$rr, numeric(1)),
      vapply(per_cat, function(e) e$category, character(1))
    ),
    bootstrap_reps = as.integer(bootstrap_reps),
    value = if (length(non_base_rr) > 0L) max(non_base_rr) else NA_real_
  )
}


# ---------------------------------------------------------------------------
# 6. data_quality_audit
# ---------------------------------------------------------------------------

#' Schema, null, and suspect-value audit
#'
#' @param df A data.frame.
#' @param sidecar Optional list with \code{fields} (list-of-list with
#'   \code{id} and \code{type}) and optionally \code{records}, in the
#'   CKAN \code{datastore_search} response shape.
#' @param expected_schema Optional list with a \code{columns} field
#'   carrying named entries with \code{name} and \code{dtype}.
#' @return Named list with \code{per_column}, \code{missing_columns},
#'   \code{extra_columns}, \code{dtype_mismatches}, \code{suspect_flags}.
#' @export
mrm_uof_data_quality_audit <- function(df, sidecar = NULL, expected_schema = NULL) {
  stopifnot(is.data.frame(df))
  warnings <- character(0)
  n_rows <- nrow(df)
  n_cols <- ncol(df)

  per_column <- vector("list", n_cols)
  for (i in seq_len(n_cols)) {
    col_name <- names(df)[i]
    s <- df[[i]]
    n_null <- sum(is.na(s))
    n_unique <- length(unique(s[!is.na(s)]))
    pct_null <- if (n_rows > 0L) n_null / n_rows else NA_real_
    pct_unique <- if (n_rows > 0L) n_unique / n_rows else NA_real_
    entry <- list(
      column = col_name,
      dtype = class(s)[1],
      n_null = n_null,
      pct_null = pct_null,
      n_unique = n_unique,
      pct_unique = pct_unique
    )
    if (is.numeric(s)) {
      nn <- s[!is.na(s)]
      entry$min <- if (length(nn) > 0L) min(nn) else NA_real_
      entry$max <- if (length(nn) > 0L) max(nn) else NA_real_
    } else {
      mt <- tryCatch(names(sort(table(s[!is.na(s)]), decreasing = TRUE))[1], error = function(e) NA)
      entry$mode <- mt
    }
    per_column[[i]] <- entry
  }

  expected_cols <- NULL
  if (!is.null(sidecar)) {
    if (is.list(sidecar) && !is.null(sidecar$fields)) {
      expected_cols <- lapply(sidecar$fields, function(f) {
        list(name = as.character(f$id), dtype = if (is.null(f$type)) NA_character_ else as.character(f$type))
      })
    } else {
      warnings <- c(warnings,
        "Sidecar provided but does not duck-type as CKAN ({fields, records}); ignored for schema comparison.")
    }
  }
  if (is.null(expected_cols) && !is.null(expected_schema)) {
    if (is.list(expected_schema) && !is.null(expected_schema$columns)) {
      expected_cols <- lapply(expected_schema$columns, function(c) {
        list(name = as.character(c$name), dtype = if (is.null(c$dtype)) NA_character_ else as.character(c$dtype))
      })
    } else {
      warnings <- c(warnings,
        "expected_schema did not duck-type (needs $columns of objects with $name); ignored.")
    }
  }

  missing_columns <- character(0)
  extra_columns <- character(0)
  dtype_mismatches <- list()
  if (!is.null(expected_cols)) {
    expected_names <- vapply(expected_cols, function(e) e$name, character(1))
    actual_names <- names(df)
    missing_columns <- setdiff(expected_names, actual_names)
    extra_columns <- setdiff(actual_names, expected_names)
    for (e in expected_cols) {
      if (e$name %in% actual_names && !is.na(e$dtype)) {
        actual_dt <- class(df[[e$name]])[1]
        if (!grepl(tolower(e$dtype), tolower(actual_dt), fixed = TRUE) &&
            !grepl(tolower(actual_dt), tolower(e$dtype), fixed = TRUE)) {
          dtype_mismatches[[length(dtype_mismatches) + 1L]] <- list(
            column = e$name, expected = e$dtype, actual = actual_dt
          )
        }
      }
    }
  }

  suspect_flags <- character(0)
  for (i in seq_along(per_column)) {
    e <- per_column[[i]]
    if (!is.na(e$pct_null) && e$pct_null > 0.50) {
      suspect_flags <- c(suspect_flags, sprintf("%s: %s null", e$column, .uof_fmt_pct(e$pct_null)))
    }
    if (is.numeric(df[[e$column]]) && !is.null(e$min) && !is.null(e$max) &&
        is.finite(e$min) && is.finite(e$max) && e$min == e$max && n_rows > 1L) {
      suspect_flags <- c(suspect_flags, sprintf("%s: constant value (%s)", e$column, format(e$min)))
    }
    if (!is.numeric(df[[e$column]]) && e$n_unique == n_rows && n_rows > 1L) {
      suspect_flags <- c(suspect_flags, sprintf("%s: every value unique \u2014 possible identifier", e$column))
    }
  }

  flag_paragraphs <- character(0)
  if (length(missing_columns) > 0L) {
    flag_paragraphs <- c(flag_paragraphs, sprintf(
      "%d expected column(s) missing.", length(missing_columns)))
  }
  if (length(extra_columns) > 0L) {
    flag_paragraphs <- c(flag_paragraphs, sprintf(
      "%d extra column(s) present that the schema does not declare.", length(extra_columns)))
  }
  if (length(dtype_mismatches) > 0L) {
    flag_paragraphs <- c(flag_paragraphs, sprintf(
      "%d dtype mismatch(es).", length(dtype_mismatches)))
  }
  if (length(suspect_flags) > 0L) {
    flag_paragraphs <- c(flag_paragraphs, sprintf(
      "%d suspect-value flag(s).", length(suspect_flags)))
  }
  if (length(flag_paragraphs) == 0L) {
    flag_paragraphs <- "No structural or content flags raised."
  }

  .uof_result(
    "MRM-UOF Data Quality Audit",
    sprintf("mrm_uof_data_quality_audit(df=<%dr x %dc>)", n_rows, n_cols),
    summary_lines = list(
      Rows = n_rows,
      Columns = n_cols,
      `Missing columns` = length(missing_columns),
      `Extra columns` = length(extra_columns),
      `Dtype mismatches` = length(dtype_mismatches),
      `Suspect flags` = length(suspect_flags)
    ),
    warnings = warnings,
    interpretation = paste(flag_paragraphs, collapse = " "),
    n = n_rows,
    n_rows = n_rows,
    n_cols = n_cols,
    per_column = per_column,
    missing_columns = missing_columns,
    extra_columns = extra_columns,
    dtype_mismatches = dtype_mismatches,
    suspect_flags = suspect_flags
  )
}


# ---------------------------------------------------------------------------
# Print method
# ---------------------------------------------------------------------------

#' @export
print.morie_mrm_uof_result <- function(x, ...) {
  cat(x$title, "\n", strrep("=", nchar(x$title)), "\n", sep = "")
  if (!is.null(x$call) && nzchar(x$call)) {
    cat("Call:", x$call, "\n\n", sep = " ")
  }
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\n", label_w, nms[i], format(v)))
    }
    cat("\n")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\n")
    cat("\n")
  }
  if (nzchar(x$interpretation)) {
    cat(x$interpretation, "\n")
  }
  invisible(x)
}
