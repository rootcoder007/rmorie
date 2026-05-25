# SPDX-License-Identifier: AGPL-3.0-or-later
#' Group-disparity metrics for auditing classification systems
#'
#' R port of \code{morie.fairness.metrics}. Each callable is an
#' *audit* measure: given decisions a system made (and, where
#' available, the realised ground truth) plus a protected attribute,
#' it quantifies whether outcomes differ across groups. None of these
#' functions make predictions; they only measure disparity in
#' predictions that already exist.
#'
#' Functions
#' ---------
#' \itemize{
#'   \item \code{\link{morie_fairness_disparate_impact}}: the four-fifths
#'     rule.
#'   \item \code{\link{morie_fairness_demographic_parity}}:
#'     favourable-rate gap.
#'   \item \code{\link{morie_fairness_equalized_odds}}: TPR/FPR gaps
#'     (needs ground truth).
#'   \item \code{\link{morie_fairness_average_odds_difference}}: mean
#'     TPR+FPR gap.
#'   \item \code{\link{morie_fairness_gini}}: concentration of a score
#'     distribution.
#'   \item \code{\link{morie_fairness_bias_amplification}}: composite
#'     of parity gap and inequality.
#' }
#'
#' Prior art reimplemented independently (no code copied): the COMPAS
#' fairness audit in pbiecek's \emph{XAI Stories} and IBM's AI Fairness
#' 360 definitions; the predictive-policing disparity framing of the
#' SciencesPo \emph{Predictive-policing-Chicago} project (Lacherade,
#' Szabo, Krikava & Aeby, 2021) and Barman & Barman, arXiv:2603.18987.
#'
#' @name fairness_metrics
NULL


.MORIE_FAIRNESS_FOUR_FIFTHS <- 0.8  # EEOC four-fifths threshold


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.morie_fairness_as_1d <- function(x, name) {
  arr <- as.vector(x)
  if (length(arr) == 0L) {
    stop(sprintf("%s is empty", name), call. = FALSE)
  }
  arr
}

.morie_fairness_check_aligned <- function(...) {
  pairs <- list(...)  # list of c(name, length)
  n <- pairs[[1L]]$len
  for (p in pairs) {
    if (p$len != n) {
      stop(sprintf(
        "length mismatch: %s has %d rows, %s has %d",
        pairs[[1L]]$name, n, p$name, p$len
      ), call. = FALSE)
    }
  }
}

.morie_fairness_ordered_unique <- function(arr) {
  # Python's "first-seen" order; unique() in R is already first-seen.
  unique(arr)
}

.morie_fairness_favorable_rates <- function(outcome, group, favorable) {
  groups <- .morie_fairness_ordered_unique(group)
  rates <- vector("list", length(groups))
  names(rates) <- as.character(groups)
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    mask <- group == g
    n <- sum(mask)
    rate <- if (n > 0L) mean(outcome[mask] == favorable) else NA_real_
    rates[[i]] <- list(g = g, n = as.integer(n), rate = as.numeric(rate))
  }
  rates
}

.morie_fairness_resolve_privileged <- function(privileged, rates, warnings_env) {
  group_keys <- vapply(rates, function(r) as.character(r$g), character(1))
  if (!is.null(privileged)) {
    if (!(as.character(privileged) %in% group_keys)) {
      stop(sprintf(
        "privileged group '%s' not found; groups present: %s",
        privileged, paste(group_keys, collapse = ", ")
      ), call. = FALSE)
    }
    return(as.character(privileged))
  }
  rate_vals <- vapply(rates, function(r) r$rate, numeric(1))
  inferred <- group_keys[which.max(rate_vals)]
  warnings_env$w <- c(warnings_env$w, sprintf(
    "`privileged` not given; inferred as '%s' (the group with the highest favourable-outcome rate). Pass `privileged=` explicitly to audit against a specific reference group.",
    inferred
  ))
  inferred
}

.morie_fairness_rates_from_labels <- function(y_true, y_pred, group, favorable) {
  groups <- .morie_fairness_ordered_unique(group)
  out <- vector("list", length(groups))
  names(out) <- as.character(groups)
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    m <- group == g
    gt <- y_true[m]
    gp <- y_pred[m]
    pos <- gt == favorable
    neg <- !pos
    tpr <- if (any(pos)) mean(gp[pos] == favorable) else NA_real_
    fpr <- if (any(neg)) mean(gp[neg] == favorable) else NA_real_
    out[[i]] <- list(g = g, n = as.integer(sum(m)),
                     tpr = as.numeric(tpr), fpr = as.numeric(fpr))
  }
  out
}

.morie_fairness_gini_core <- function(x) {
  # Sorted-rank formula. Returns 0.0 for an all-zero or single-element
  # input (no inequality defined), matching the Python helper.
  x <- sort(as.numeric(x))
  n <- length(x)
  total <- sum(x)
  if (n < 2L || !is.finite(total) || total <= 0) {
    return(0.0)
  }
  idx <- seq_len(n)
  (2.0 * sum(idx * x)) / (n * total) - (n + 1.0) / n
}

.morie_fairness_result <- function(title, summary_lines = list(),
                                   tables = list(), sections = list(),
                                   warnings = character(0),
                                   interpretation = "",
                                   payload = list()) {
  out <- list(
    title = title,
    summary_lines = summary_lines,
    tables = tables,
    sections = sections,
    warnings = warnings,
    interpretation = interpretation,
    payload = payload,
    value = payload$value
  )
  class(out) <- c("morie_fairness_result", "morie_rich_result", "list")
  out
}


# ---------------------------------------------------------------------------
# 1. disparate_impact
# ---------------------------------------------------------------------------

#' Disparate Impact Ratio (EEOC four-fifths / 80% rule)
#'
#' For each group, the disparate-impact ratio is its favourable-
#' outcome rate divided by the privileged group's rate. A value below
#' 0.8 is the standard legal indicator of adverse impact.
#'
#' @param y_pred Vector of decisions / assignments per individual.
#' @param group Protected-attribute vector aligned with \code{y_pred}.
#' @param privileged Reference group. If \code{NULL}, the highest-rate
#'   group is inferred and a warning is emitted.
#' @param favorable Value of \code{y_pred} that counts as favourable
#'   (default \code{1}).
#' @return A \code{morie_fairness_result}; headline value is the
#'   worst (smallest) ratio across groups.
#' @examples
#' pred <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0)
#' race <- c("A","A","A","A","A","B","B","B","B","B")
#' morie_fairness_disparate_impact(pred, race, privileged = "A")$value
#' @export
morie_fairness_disparate_impact <- function(y_pred, group,
                                            privileged = NULL,
                                            favorable = 1) {
  yp <- .morie_fairness_as_1d(y_pred, "y_pred")
  grp <- .morie_fairness_as_1d(group, "group")
  .morie_fairness_check_aligned(
    list(name = "y_pred", len = length(yp)),
    list(name = "group",  len = length(grp))
  )

  rates <- .morie_fairness_favorable_rates(yp, grp, favorable)
  if (length(rates) < 2L) {
    stop("need at least two groups to measure disparity", call. = FALSE)
  }

  we <- new.env(parent = emptyenv())
  we$w <- character(0)
  priv <- .morie_fairness_resolve_privileged(privileged, rates, we)
  base <- rates[[priv]]$rate
  if (isTRUE(base == 0)) {
    we$w <- c(we$w, sprintf(
      "privileged group '%s' has a zero favourable-outcome rate; disparate-impact ratios are undefined (division by zero) and reported as NaN.",
      priv
    ))
  }

  table_rows <- list()
  ratios <- numeric(0)
  for (key in names(rates)) {
    r <- rates[[key]]
    if (key == priv) {
      ratio <- 1.0
    } else if (isTRUE(base == 0)) {
      ratio <- NaN
    } else {
      ratio <- r$rate / base
    }
    ratios[key] <- ratio
    table_rows[[length(table_rows) + 1L]] <- list(
      group = paste0(key, if (key == priv) " (ref)" else ""),
      n = r$n, fav_rate = round(r$rate, 4),
      di_ratio = if (key == priv) "-" else round(ratio, 4)
    )
  }

  non_ref <- ratios[names(ratios) != priv]
  finite <- non_ref[is.finite(non_ref)]
  worst <- if (length(finite) > 0L) min(finite) else NaN
  adverse <- isTRUE(is.finite(worst) && worst < .MORIE_FAIRNESS_FOUR_FIFTHS)

  interp <- if (!is.finite(worst)) {
    "Disparate-impact ratio could not be computed (privileged group has no favourable outcomes)."
  } else if (adverse) {
    sprintf(
      "Adverse impact detected: the worst disparate-impact ratio is %.3f, below the 0.80 four-fifths threshold. The system assigns favourable outcomes to at least one group at well under 80%% of the privileged group's rate.",
      worst
    )
  } else {
    sprintf(
      "No adverse impact under the four-fifths rule: the worst disparate-impact ratio is %.3f (>= 0.80). This does not by itself certify fairness - pair it with morie_fairness_equalized_odds when ground truth is available.",
      worst
    )
  }

  .morie_fairness_result(
    title = "Disparate Impact Ratio (four-fifths rule)",
    summary_lines = list(
      `Worst ratio` = worst,
      `Reference group` = priv,
      `Adverse impact (<0.80)` = adverse
    ),
    tables = list(list(
      title = "Per-group favourable-outcome rates:",
      headers = c("group", "n", "fav. rate", "DI ratio"),
      rows = table_rows
    )),
    warnings = we$w,
    interpretation = interp,
    payload = list(
      value = worst, ratios = as.list(ratios),
      rates = setNames(lapply(rates, function(r) r$rate), names(rates)),
      privileged = priv, adverse_impact = adverse,
      threshold = .MORIE_FAIRNESS_FOUR_FIFTHS
    )
  )
}


# ---------------------------------------------------------------------------
# 2. demographic_parity
# ---------------------------------------------------------------------------

#' Demographic Parity Gap (difference in favourable-outcome rates)
#'
#' The gap is \code{rate(group) - rate(privileged)}. Demographic
#' parity holds when every group receives favourable outcomes at the
#' same rate, i.e. all gaps are zero. Unlike the disparate-impact
#' ratio this additive form is well defined even when the privileged
#' rate is zero.
#'
#' @inheritParams morie_fairness_disparate_impact
#' @return A \code{morie_fairness_result}; headline value is the
#'   largest absolute gap across groups.
#' @examples
#' pred <- c(1, 1, 1, 1, 0, 0, 0, 1, 0, 0)
#' race <- c("A","A","A","A","A","B","B","B","B","B")
#' morie_fairness_demographic_parity(pred, race, privileged = "A")$value
#' @export
morie_fairness_demographic_parity <- function(y_pred, group,
                                              privileged = NULL,
                                              favorable = 1) {
  yp <- .morie_fairness_as_1d(y_pred, "y_pred")
  grp <- .morie_fairness_as_1d(group, "group")
  .morie_fairness_check_aligned(
    list(name = "y_pred", len = length(yp)),
    list(name = "group",  len = length(grp))
  )

  rates <- .morie_fairness_favorable_rates(yp, grp, favorable)
  if (length(rates) < 2L) {
    stop("need at least two groups to measure disparity", call. = FALSE)
  }
  we <- new.env(parent = emptyenv())
  we$w <- character(0)
  priv <- .morie_fairness_resolve_privileged(privileged, rates, we)
  base <- rates[[priv]]$rate

  table_rows <- list()
  gaps <- numeric(0)
  for (key in names(rates)) {
    r <- rates[[key]]
    gap <- r$rate - base
    gaps[key] <- gap
    table_rows[[length(table_rows) + 1L]] <- list(
      group = paste0(key, if (key == priv) " (ref)" else ""),
      n = r$n, fav_rate = round(r$rate, 4),
      parity_gap = if (key == priv) "-" else round(gap, 4)
    )
  }

  non_ref <- gaps[names(gaps) != priv]
  finite_nr <- non_ref[is.finite(non_ref)]
  worst <- if (length(finite_nr) > 0L) finite_nr[which.max(abs(finite_nr))]
           else if (length(non_ref) > 0L) NA_real_ else 0.0
  worst_val <- as.numeric(worst)

  interp <- paste0(
    sprintf("The largest favourable-rate gap is %+.3f (group rate minus the '%s' reference rate). ",
            worst_val, priv),
    if (abs(worst_val) >= 0.1)
      "A gap far from zero means the system grants favourable outcomes at materially different rates across groups."
    else
      "Gaps are small; favourable-outcome rates are close to parity, though this does not account for differences in ground-truth base rates."
  )

  .morie_fairness_result(
    title = "Demographic Parity Gap",
    summary_lines = list(
      `Largest |gap|` = worst_val,
      `Reference group` = priv
    ),
    tables = list(list(
      title = "Per-group favourable-outcome rates:",
      headers = c("group", "n", "fav. rate", "parity gap"),
      rows = table_rows
    )),
    warnings = we$w,
    interpretation = interp,
    payload = list(
      value = worst_val, gaps = as.list(gaps),
      rates = setNames(lapply(rates, function(r) r$rate), names(rates)),
      privileged = priv
    )
  )
}


# ---------------------------------------------------------------------------
# 3. equalized_odds
# ---------------------------------------------------------------------------

#' Equalized Odds (TPR / FPR gaps across groups)
#'
#' Equalized odds holds when both the true-positive rate and the
#' false-positive rate are equal across groups. Needs ground truth,
#' so it audits a system's errors rather than its decision rates - a
#' system can satisfy demographic parity yet make many more false
#' positives against one group.
#'
#' @param y_true Realised ground-truth outcome per individual.
#' @param y_pred The system's decision per individual.
#' @inheritParams morie_fairness_disparate_impact
#' @return A \code{morie_fairness_result}; headline value is the
#'   largest absolute TPR-or-FPR gap.
#' @examples
#' truth <- c(1,0,1,0,1,0,1,0)
#' pred  <- c(1,0,1,0,1,1,0,1)
#' race  <- c("A","A","A","A","B","B","B","B")
#' morie_fairness_equalized_odds(truth, pred, race, privileged="A")$value
#' @export
morie_fairness_equalized_odds <- function(y_true, y_pred, group,
                                          privileged = NULL,
                                          favorable = 1) {
  yt <- .morie_fairness_as_1d(y_true, "y_true")
  yp <- .morie_fairness_as_1d(y_pred, "y_pred")
  grp <- .morie_fairness_as_1d(group, "group")
  .morie_fairness_check_aligned(
    list(name = "y_true", len = length(yt)),
    list(name = "y_pred", len = length(yp)),
    list(name = "group",  len = length(grp))
  )

  per <- .morie_fairness_rates_from_labels(yt, yp, grp, favorable)
  if (length(per) < 2L) {
    stop("need at least two groups to measure disparity", call. = FALSE)
  }

  we <- new.env(parent = emptyenv())
  we$w <- character(0)
  rate_view <- lapply(per, function(d) list(g = d$g, n = d$n, rate = d$tpr))
  names(rate_view) <- names(per)
  priv <- .morie_fairness_resolve_privileged(privileged, rate_view, we)
  base_tpr <- per[[priv]]$tpr
  base_fpr <- per[[priv]]$fpr

  table_rows <- list()
  tpr_gaps <- numeric(0)
  fpr_gaps <- numeric(0)
  for (key in names(per)) {
    d <- per[[key]]
    tg <- d$tpr - base_tpr
    fg <- d$fpr - base_fpr
    tpr_gaps[key] <- tg
    fpr_gaps[key] <- fg
    if (is.na(d$tpr) || is.na(d$fpr)) {
      we$w <- c(we$w, sprintf(
        "group '%s' has no positive or no negative ground-truth cases; its TPR/FPR (and gaps) are partly undefined.",
        key
      ))
    }
    table_rows[[length(table_rows) + 1L]] <- list(
      group = paste0(key, if (key == priv) " (ref)" else ""),
      n = d$n,
      tpr = round(d$tpr, 4), fpr = round(d$fpr, 4),
      dtpr = if (key == priv) "-" else round(tg, 4),
      dfpr = if (key == priv) "-" else round(fg, 4)
    )
  }

  all_gaps <- c(tpr_gaps[names(tpr_gaps) != priv],
                fpr_gaps[names(fpr_gaps) != priv])
  finite <- all_gaps[is.finite(all_gaps)]
  worst <- if (length(finite) > 0L) finite[which.max(abs(finite))] else NaN
  worst_val <- as.numeric(worst)
  violation <- isTRUE(is.finite(worst_val) && abs(worst_val) >= 0.1)

  interp <- paste0(
    sprintf("The largest equalized-odds gap is %+.3f. ", worst_val),
    if (violation)
      "Error rates differ substantially across groups: the system is not equally accurate for everyone, which is a stronger fairness concern than an outcome-rate gap alone."
    else
      "TPR and FPR are close across groups; the system's error profile is roughly even."
  )

  .morie_fairness_result(
    title = "Equalized Odds (TPR / FPR gaps)",
    summary_lines = list(
      `Largest |gap|` = worst_val,
      `Reference group` = priv,
      `Violation (|gap|>=0.10)` = violation
    ),
    tables = list(list(
      title = "Per-group true/false positive rates:",
      headers = c("group", "n", "TPR", "FPR", "dTPR", "dFPR"),
      rows = table_rows
    )),
    warnings = we$w,
    interpretation = interp,
    payload = list(
      value = worst_val,
      tpr_gaps = as.list(tpr_gaps),
      fpr_gaps = as.list(fpr_gaps),
      rates = per, privileged = priv, violation = violation
    )
  )
}


# ---------------------------------------------------------------------------
# 4. average_odds_difference
# ---------------------------------------------------------------------------

#' Average Odds Difference (mean of TPR and FPR gaps)
#'
#' For each non-reference group, the average odds difference is
#' \code{0.5 * ((FPR_group - FPR_ref) + (TPR_group - TPR_ref))}.
#' Zero means parity of errors; values away from zero mean the
#' combined error profile favours one group over another. Used in
#' IBM AIF360 and in the COMPAS \emph{XAI Stories} audit.
#'
#' @inheritParams morie_fairness_equalized_odds
#' @return A \code{morie_fairness_result}; headline value is the
#'   largest absolute AOD across groups.
#' @export
morie_fairness_average_odds_difference <- function(y_true, y_pred, group,
                                                   privileged = NULL,
                                                   favorable = 1) {
  yt <- .morie_fairness_as_1d(y_true, "y_true")
  yp <- .morie_fairness_as_1d(y_pred, "y_pred")
  grp <- .morie_fairness_as_1d(group, "group")
  .morie_fairness_check_aligned(
    list(name = "y_true", len = length(yt)),
    list(name = "y_pred", len = length(yp)),
    list(name = "group",  len = length(grp))
  )

  per <- .morie_fairness_rates_from_labels(yt, yp, grp, favorable)
  if (length(per) < 2L) {
    stop("need at least two groups to measure disparity", call. = FALSE)
  }
  we <- new.env(parent = emptyenv())
  we$w <- character(0)
  rate_view <- lapply(per, function(d) list(g = d$g, n = d$n, rate = d$tpr))
  names(rate_view) <- names(per)
  priv <- .morie_fairness_resolve_privileged(privileged, rate_view, we)
  base_tpr <- per[[priv]]$tpr
  base_fpr <- per[[priv]]$fpr

  table_rows <- list()
  aod <- numeric(0)
  for (key in names(per)) {
    d <- per[[key]]
    val <- 0.5 * ((d$fpr - base_fpr) + (d$tpr - base_tpr))
    aod[key] <- val
    table_rows[[length(table_rows) + 1L]] <- list(
      group = paste0(key, if (key == priv) " (ref)" else ""),
      n = d$n, tpr = round(d$tpr, 4), fpr = round(d$fpr, 4),
      aod = if (key == priv) "-" else round(val, 4)
    )
  }
  non_ref <- aod[names(aod) != priv]
  finite <- non_ref[is.finite(non_ref)]
  worst <- if (length(finite) > 0L) finite[which.max(abs(finite))] else NaN
  worst_val <- as.numeric(worst)

  interp <- sprintf(
    "The largest average odds difference is %+.3f. Zero is parity; values away from zero mean the combined true-positive and false-positive error profile favours one group over another.",
    worst_val
  )

  .morie_fairness_result(
    title = "Average Odds Difference",
    summary_lines = list(
      `Largest |AOD|` = worst_val,
      `Reference group` = priv
    ),
    tables = list(list(
      title = "Per-group odds:",
      headers = c("group", "n", "TPR", "FPR", "AOD"),
      rows = table_rows
    )),
    warnings = we$w,
    interpretation = interp,
    payload = list(
      value = worst_val,
      average_odds_difference = as.list(aod),
      rates = per, privileged = priv
    )
  )
}


# ---------------------------------------------------------------------------
# 5. gini
# ---------------------------------------------------------------------------

#' Gini coefficient (concentration of a non-negative distribution)
#'
#' Ranges from 0 (perfect equality) to nearly 1 (one unit holds
#' everything). Applied to risk scores or stop counts, it measures
#' how unequally a predictive system concentrates its attention.
#' When \code{group} is supplied, per-group Gini values are also
#' reported.
#'
#' @param values Vector of non-negative quantities.
#' @param group Optional protected-attribute vector for a per-group
#'   breakdown.
#' @return A \code{morie_fairness_result}; headline value is the
#'   overall Gini.
#' @examples
#' morie_fairness_gini(c(5, 5, 5, 5))$value
#' morie_fairness_gini(c(0, 0, 0, 100))$value
#' @export
morie_fairness_gini <- function(values, group = NULL) {
  vals <- as.numeric(.morie_fairness_as_1d(values, "values"))
  warnings <- character(0)
  if (any(vals < 0, na.rm = TRUE)) {
    warnings <- c(warnings,
      "negative values present; the Gini coefficient assumes non-negative quantities and the result may be uninformative.")
  }

  overall <- .morie_fairness_gini_core(vals)

  sections <- list()
  per_group <- list()
  if (!is.null(group)) {
    grp <- .morie_fairness_as_1d(group, "group")
    .morie_fairness_check_aligned(
      list(name = "values", len = length(vals)),
      list(name = "group",  len = length(grp))
    )
    rows <- list()
    for (g in .morie_fairness_ordered_unique(grp)) {
      gv <- vals[grp == g]
      gini_g <- .morie_fairness_gini_core(gv)
      per_group[[as.character(g)]] <- gini_g
      rows[[length(rows) + 1L]] <- list(
        group = as.character(g), n = length(gv),
        mean = round(mean(gv), 4), gini = round(gini_g, 4)
      )
    }
    sections[[1L]] <- list(
      title = "Per-group concentration:",
      headers = c("group", "n", "mean", "Gini"),
      table = rows
    )
  }

  interp <- paste0(
    sprintf("Gini = %.3f. ", overall),
    if (overall >= 0.5)
      "The quantity is highly concentrated - a small share of units absorbs most of it."
    else
      "The quantity is relatively evenly spread."
  )

  .morie_fairness_result(
    title = "Gini Coefficient",
    summary_lines = list(Gini = overall, n = length(vals)),
    sections = sections,
    warnings = warnings,
    interpretation = interp,
    payload = list(value = overall, gini = overall,
                   per_group = per_group)
  )
}


# ---------------------------------------------------------------------------
# 6. bias_amplification
# ---------------------------------------------------------------------------

#' Bias Amplification Score (parity gap x Gini of group rates)
#'
#' \code{BAS = delta_parity * G}, where \code{delta_parity} is the
#' demographic-parity gap of the worst-affected group and \code{G}
#' is the Gini coefficient of the per-group favourable-outcome rates.
#' Large only when a directional disparity coincides with high
#' cross-group inequality.
#'
#' Reimplemented from Barman & Barman, arXiv:2603.18987.
#'
#' @inheritParams morie_fairness_disparate_impact
#' @return A \code{morie_fairness_result}; headline value is BAS.
#' @export
morie_fairness_bias_amplification <- function(y_pred, group,
                                              privileged = NULL,
                                              favorable = 1) {
  yp <- .morie_fairness_as_1d(y_pred, "y_pred")
  grp <- .morie_fairness_as_1d(group, "group")
  .morie_fairness_check_aligned(
    list(name = "y_pred", len = length(yp)),
    list(name = "group",  len = length(grp))
  )

  rates <- .morie_fairness_favorable_rates(yp, grp, favorable)
  if (length(rates) < 2L) {
    stop("need at least two groups to measure disparity", call. = FALSE)
  }
  we <- new.env(parent = emptyenv())
  we$w <- character(0)
  priv <- .morie_fairness_resolve_privileged(privileged, rates, we)
  base <- rates[[priv]]$rate

  gaps <- vapply(rates, function(r) r$rate - base, numeric(1))
  names(gaps) <- names(rates)
  non_ref <- gaps[names(gaps) != priv]
  delta_parity <- if (length(non_ref) > 0L) {
    as.numeric(non_ref[which.max(abs(non_ref))])
  } else 0.0

  rate_vec <- vapply(rates, function(r) r$rate, numeric(1))
  gini <- .morie_fairness_gini_core(rate_vec)
  bas <- as.numeric(delta_parity * gini)

  table_rows <- list()
  for (key in names(rates)) {
    r <- rates[[key]]
    table_rows[[length(table_rows) + 1L]] <- list(
      group = paste0(key, if (key == priv) " (ref)" else ""),
      n = r$n, fav_rate = round(r$rate, 4),
      parity_gap = if (key == priv) "-" else round(gaps[[key]], 4)
    )
  }

  interp <- paste0(
    sprintf("Bias Amplification Score = %+.4f (parity gap %+.3f x Gini %.3f). ",
            bas, delta_parity, gini),
    if (abs(bas) >= 0.05)
      "Both a directional disparity and substantial cross-group inequality are present - the system amplifies bias."
    else
      "At least one component is small, so little amplification is indicated."
  )

  .morie_fairness_result(
    title = "Bias Amplification Score",
    summary_lines = list(
      `Bias Amplification Score` = bas,
      `Demographic parity gap` = delta_parity,
      `Gini of group rates` = gini,
      `Reference group` = priv
    ),
    tables = list(list(
      title = "Per-group favourable-outcome rates:",
      headers = c("group", "n", "fav. rate", "parity gap"),
      rows = table_rows
    )),
    warnings = we$w,
    interpretation = interp,
    payload = list(
      value = bas, bias_amplification_score = bas,
      demographic_parity_gap = delta_parity, gini = gini,
      rates = setNames(lapply(rates, function(r) r$rate), names(rates)),
      privileged = priv
    )
  )
}


# ---------------------------------------------------------------------------
# print
# ---------------------------------------------------------------------------

#' @export
print.morie_fairness_result <- function(x, ...) {
  cat(x$title, "\
", strrep("=", nchar(x$title)), "\
", sep = "")
  if (length(x$summary_lines) > 0L) {
    nms <- names(x$summary_lines)
    label_w <- max(nchar(nms))
    for (i in seq_along(x$summary_lines)) {
      v <- x$summary_lines[[i]]
      if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
        v <- format(v, digits = 5)
      }
      cat(sprintf("  %-*s  %s\
", label_w, nms[i], format(v)))
    }
    cat("\
")
  }
  if (length(x$warnings) > 0L) {
    for (w in x$warnings) cat("Warning:", w, "\
")
    cat("\
")
  }
  if (nzchar(x$interpretation)) cat(x$interpretation, "\
")
  invisible(x)
}
