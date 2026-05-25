# SPDX-License-Identifier: AGPL-3.0-or-later
#' Predictive-policing disparity audit (port of morie.fairness.predpol)
#'
#' Three callables that mirror the SciencesPo Predictive-Policing-Chicago
#' analysis, city-agnostically:
#'
#' \itemize{
#'   \item \code{morie_fairness_predpol_aggregate_areas}: per-record to
#'     per-area roll-up.
#'   \item \code{morie_fairness_predpol_calibration_audit}: Spearman
#'     correlation between predicted risk rank and realised outcome
#'     rank, plus per-group rank-gap analysis.
#'   \item \code{morie_fairness_predpol_score_disparity}: descriptive
#'     score-by-group summary with one-way ANOVA.
#' }
#'
#' @name morie_fairness_predpol
NULL


.predpol_result <- function(title, call, summary_lines = list(),
                             warnings = character(0),
                             interpretation = "", ...) {
  out <- list(
    title = title, call = call, summary_lines = summary_lines,
    warnings = warnings, interpretation = interpretation, ...
  )
  class(out) <- c("morie_fairness_result", "morie_rich_result", "list")
  out
}

.predpol_ordered_unique <- function(x) {
  x <- as.character(x)
  x[!duplicated(x)]
}

.predpol_mode <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}


# ---------------------------------------------------------------------------
# 1. aggregate_areas
# ---------------------------------------------------------------------------

#' Aggregate per-record predictive-policing data to per-area
#'
#' @param area Area identifier per record.
#' @param risk Predicted-risk score per record.
#' @param outcome Realised-outcome indicator/count per record.
#' @param group Optional protected attribute per record; the per-area
#'   majority becomes the area's label.
#' @param population Optional named numeric vector mapping area to
#'   population, or a per-record vector (taken as constant within an
#'   area). When supplied, outcome rate is per 10,000 inhabitants.
#' @return A list with \code{areas}, \code{mean_risk},
#'   \code{outcome_rate}, \code{group}, \code{n_records}.
#' @export
morie_fairness_predpol_aggregate_areas <- function(area, risk, outcome,
                                                    group = NULL,
                                                    population = NULL) {
  area <- as.character(area)
  risk <- as.numeric(risk)
  outcome <- as.numeric(outcome)
  if (!(length(area) == length(risk) && length(risk) == length(outcome))) {
    stop("area, risk and outcome must be the same length")
  }
  if (!is.null(group)) {
    group <- as.character(group)
    if (length(group) != length(area)) {
      stop("group must be the same length as area")
    }
  }
  per_pop_arr <- NULL
  if (!is.null(population) && !is.list(population) &&
      length(population) == length(area)) {
    per_pop_arr <- as.numeric(population)
  }

  areas <- sort(unique(area))
  mean_risk <- vapply(areas, function(a) mean(risk[area == a], na.rm = TRUE),
                      numeric(1))
  counts <- vapply(areas, function(a) sum(outcome[area == a], na.rm = TRUE),
                   numeric(1))
  n_records <- vapply(areas, function(a) sum(area == a), integer(1))

  if (is.null(population)) {
    outcome_rate <- vapply(areas,
                           function(a) mean(outcome[area == a], na.rm = TRUE),
                           numeric(1))
  } else {
    if (is.list(population) || (!is.null(names(population)) &&
                                length(population) != length(area))) {
      pop_lookup <- as.numeric(population)
      names(pop_lookup) <- names(population)
      pops <- as.numeric(pop_lookup[areas])
    } else if (!is.null(per_pop_arr)) {
      pops <- vapply(areas,
                     function(a) per_pop_arr[area == a][1L],
                     numeric(1))
    } else {
      pops <- rep(NA_real_, length(areas))
    }
    outcome_rate <- ifelse(is.finite(pops) & pops > 0,
                           counts / pops * 10000.0, NA_real_)
  }

  maj <- NULL
  if (!is.null(group)) {
    maj <- vapply(areas,
                  function(a) .predpol_mode(group[area == a]),
                  character(1))
  }

  list(
    areas = areas, mean_risk = mean_risk, outcome_rate = outcome_rate,
    group = maj, n_records = n_records
  )
}


# ---------------------------------------------------------------------------
# 2. calibration_audit
# ---------------------------------------------------------------------------

#' Predicted-vs-realised rank audit by demographic group
#'
#' @param areas Area identifiers (per area, not per record).
#' @param mean_risk Mean predicted-risk score per area.
#' @param outcome_rate Realised-outcome rate per area.
#' @param group Majority/dominant group per area.
#' @return \code{morie_fairness_result}; \code{$value} is the
#'   largest-magnitude per-group mean rank gap (positive = over-policed).
#' @export
morie_fairness_predpol_calibration_audit <- function(areas, mean_risk,
                                                      outcome_rate,
                                                      group) {
  areas <- as.character(areas)
  mean_risk <- as.numeric(mean_risk)
  outcome_rate <- as.numeric(outcome_rate)
  group <- as.character(group)
  n <- length(areas)
  if (!(n == length(mean_risk) && n == length(outcome_rate) &&
        n == length(group))) {
    stop("areas, mean_risk, outcome_rate and group must align")
  }
  if (n < 2L) stop("need at least two areas to compare rankings")

  warnings <- character(0)
  finite <- is.finite(mean_risk) & is.finite(outcome_rate)
  if (!all(finite)) {
    warnings <- c(warnings, sprintf(
      "%d area(s) had a non-finite risk or outcome value and were dropped.",
      sum(!finite)
    ))
    areas <- areas[finite]
    mean_risk <- mean_risk[finite]
    outcome_rate <- outcome_rate[finite]
    group <- group[finite]
    n <- length(areas)
    if (n < 2L) stop("fewer than two areas remain after dropping non-finite rows")
  }

  # Rank 1 = highest. R's rank() ranks ascending; negate to flip.
  risk_rank <- rank(-mean_risk, ties.method = "average")
  outcome_rank <- rank(-outcome_rate, ties.method = "average")
  rank_gap <- outcome_rank - risk_rank

  rho <- NA_real_
  pval <- NA_real_
  if (diff(range(mean_risk)) == 0 || diff(range(outcome_rate)) == 0) {
    warnings <- c(warnings,
      "Spearman calibration correlation is undefined \u2014 predicted risk or realised outcome is constant across all areas.")
  } else {
    sc <- suppressWarnings(stats::cor.test(mean_risk, outcome_rate,
                                           method = "spearman",
                                           exact = FALSE))
    rho <- as.numeric(sc$estimate)
    pval <- as.numeric(sc$p.value)
  }

  uniq <- .predpol_ordered_unique(group)
  per_group <- numeric(length(uniq))
  names(per_group) <- uniq
  group_n <- integer(length(uniq))
  names(group_n) <- uniq
  for (gv in uniq) {
    mask <- group == gv
    per_group[gv] <- mean(rank_gap[mask])
    group_n[gv] <- sum(mask)
  }
  worst_group <- names(per_group)[which.max(abs(per_group))]
  worst <- per_group[[worst_group]]

  area_rows <- vector("list", length(areas))
  for (i in seq_along(areas)) {
    area_rows[[i]] <- list(
      area = areas[i], group = group[i],
      mean_risk = round(mean_risk[i], 3),
      outcome_rate = round(outcome_rate[i], 3),
      risk_rank = round(risk_rank[i], 1),
      outcome_rank = round(outcome_rank[i], 1),
      gap = round(rank_gap[i], 1)
    )
  }

  if (!is.finite(rho)) {
    cal <- "Overall calibration could not be assessed \u2014 predicted risk or realised outcome is constant across all areas."
  } else if (rho >= 0.7) {
    cal <- sprintf("Overall the ranking is well calibrated (Spearman rho = %.2f): predicted risk broadly tracks realised outcomes.", rho)
  } else if (rho >= 0.3) {
    cal <- sprintf("Overall calibration is weak (Spearman rho = %.2f): predicted risk only loosely tracks realised outcomes.", rho)
  } else {
    cal <- sprintf("Overall the ranking is miscalibrated (Spearman rho = %.2f): predicted risk does not track realised outcomes.", rho)
  }

  if (abs(worst) <= 0.5) {
    disp <- "No group's areas are systematically mis-ranked; the rank gaps are small across groups."
  } else if (worst > 0) {
    disp <- sprintf("Group '%s' is over-predicted: its areas are ranked, on average, %.1f rank positions more dangerous than their realised outcomes warrant \u2014 the signature of disparate over-policing.",
                    worst_group, worst)
  } else {
    disp <- sprintf("Group '%s' is under-predicted: its areas are ranked, on average, %.1f rank positions less dangerous than their realised outcomes.",
                    worst_group, abs(worst))
  }

  .predpol_result(
    "Predictive-Policing Calibration Audit",
    sprintf("morie_fairness_predpol_calibration_audit(n_areas=%d)", n),
    summary_lines = list(
      `Areas audited` = n,
      `Spearman rho (risk vs outcome)` = rho,
      `Worst group rank gap` = worst,
      `Worst-affected group` = worst_group
    ),
    warnings = warnings,
    interpretation = paste(cal, disp, sep = " "),
    n = n,
    value = worst, spearman = rho, spearman_pvalue = pval,
    group_rank_gap = as.list(per_group),
    worst_group = worst_group,
    rank_gap = setNames(as.numeric(rank_gap), areas),
    per_area = area_rows
  )
}


# ---------------------------------------------------------------------------
# 3. score_disparity
# ---------------------------------------------------------------------------

#' Descriptive score-by-group disparity
#'
#' @param score Continuous risk score per individual.
#' @param group Protected attribute per individual.
#' @param reference Optional reference group label (default: lowest-mean).
#' @return \code{morie_fairness_result}; \code{$value} is the spread
#'   (max - min) of per-group mean scores.
#' @export
morie_fairness_predpol_score_disparity <- function(score, group,
                                                    reference = NULL) {
  score <- as.numeric(score)
  group <- as.character(group)
  if (length(score) != length(group)) {
    stop("score and group must be the same length")
  }
  if (length(.predpol_ordered_unique(group)) < 2L) {
    stop("need at least two groups to measure disparity")
  }

  warnings <- character(0)
  finite <- is.finite(score)
  if (!all(finite)) {
    warnings <- c(warnings, sprintf(
      "%d non-finite score value(s) dropped.", sum(!finite)
    ))
    score <- score[finite]
    group <- group[finite]
  }
  groups <- .predpol_ordered_unique(group)
  if (length(groups) < 2L) {
    stop("fewer than two groups remain after dropping NaNs")
  }

  stats_per <- vector("list", length(groups))
  names(stats_per) <- groups
  for (g in groups) {
    gv <- score[group == g]
    stats_per[[g]] <- list(
      n = length(gv),
      mean = if (length(gv)) mean(gv) else NA_real_,
      median = if (length(gv)) stats::median(gv) else NA_real_,
      sd = if (length(gv) > 1L) stats::sd(gv) else NA_real_
    )
  }

  samples <- lapply(groups, function(g) score[group == g])
  usable <- samples[vapply(samples, function(s) length(s) >= 2L, logical(1))]
  if (length(usable) >= 2L) {
    long_score <- unlist(usable)
    long_group <- factor(rep(seq_along(usable),
                             vapply(usable, length, integer(1))))
    av <- tryCatch(stats::oneway.test(long_score ~ long_group,
                                      var.equal = TRUE),
                   error = function(e) NULL)
    if (!is.null(av)) {
      fstat <- as.numeric(av$statistic)
      pval <- as.numeric(av$p.value)
    } else {
      fstat <- NA_real_
      pval <- NA_real_
    }
  } else {
    fstat <- NA_real_
    pval <- NA_real_
    warnings <- c(warnings, "ANOVA skipped: fewer than two groups with n >= 2.")
  }

  means <- vapply(groups, function(g) stats_per[[g]]$mean, numeric(1))
  names(means) <- groups
  if (is.null(reference)) {
    ref <- names(means)[which.min(means)]
  } else {
    ref <- as.character(reference)
    if (!(ref %in% names(means))) stop("reference group not found")
  }
  base_mean <- means[[ref]]
  gaps <- means - base_mean
  spread <- max(means) - min(means)
  significant <- isTRUE(is.finite(pval) && pval < 0.05)

  if (is.finite(pval)) {
    anova_line <- sprintf(
      "A one-way ANOVA finds the between-group difference %s (F = %.2f, p = %.4f). ",
      if (significant) "statistically significant" else "not significant",
      fstat, pval
    )
  } else {
    anova_line <- ""
  }
  interp <- sprintf(
    "Group mean risk scores span %.2f points (reference '%s'). %sNote: a score gap is not itself evidence of bias \u2014 it can reflect genuine base-rate differences. Pair this with morie_fairness_predpol_calibration_audit, which compares the score against realised outcomes.",
    spread, ref, anova_line
  )

  .predpol_result(
    "Predictive-Policing Score Disparity (descriptive)",
    sprintf("morie_fairness_predpol_score_disparity(n=%d, k_groups=%d)",
            length(score), length(groups)),
    summary_lines = list(
      `Group-mean spread` = spread,
      `ANOVA F` = fstat,
      `ANOVA p-value` = pval,
      `Reference group` = ref
    ),
    warnings = warnings,
    interpretation = interp,
    n = length(score),
    value = spread, spread = spread,
    group_means = as.list(means), gaps = as.list(gaps),
    anova_f = fstat, anova_pvalue = pval,
    significant = significant, reference = ref,
    per_group = stats_per
  )
}
