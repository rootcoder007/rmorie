# Callaway-Sant'Anna group-time average treatment effects.
# Source: Callaway, B. and Sant'Anna, P. H. C. (2021),
# Difference-in-differences with multiple time periods, Journal of
# Econometrics 225(2), 200-230 (arXiv 1803.09015): the building block
# ATT(g, t) of their Sec. 2-3, estimated against either never-treated
# or not-yet-treated units, and the aggregation schemes of their Sec.
# 4 (event-study/dynamic, cohort/group, and calendar-time).
#
# The point of the estimator is that no already-treated unit is ever
# used as a comparison, so none of the negative-weight comparisons
# that contaminate a two-way fixed-effects regression can enter.
# Cells with t < g are NOT effects; they are the parallel-trends
# check, and are reported separately as `pretrend`.
#
# Native implementation mirroring Python morie.fn.cssant exactly:
# same base period g - 1, same not-yet-treated rule g > max(t, g0),
# same influence functions and the same weighting in each aggregate.

# Balanced panel matrix from long-format columns.  Units and periods
# are taken in sorted order, matching numpy's unique().
#' Balanced panel matrix from long-format columns.  Units and periods
#'
#' are taken in sorted order, matching numpy\'s unique().
#'
#' @param y See Usage.
#' @param unit See Usage.
#' @param time See Usage.
#' @return A list with \code{Y}, \code{units}, \code{periods}.
#' @export
.mor_did_panel <- function(y, unit, time) {
  y <- as.numeric(y)
  u <- as.vector(unit); t <- as.vector(time)
  if (!(length(y) == length(u) && length(u) == length(t)))
    stop(sprintf(paste("y, unit and time must have the same length,",
                       "got %d, %d and %d."), length(y), length(u), length(t)))
  units <- sort(unique(u)); periods <- sort(unique(t))
  n <- length(units); T <- length(periods)
  if (n < 2L || T < 2L)
    stop(sprintf("need at least 2 units and 2 periods, got %d and %d.", n, T))
  ui <- match(u, units); ti <- match(t, periods)
  if (anyDuplicated(cbind(ui, ti)))
    stop("the panel has duplicate (unit, time) observations.")
  Y <- matrix(NA_real_, n, T)
  Y[cbind(ui, ti)] <- y
  if (anyNA(Y)) {
    miss <- sum(is.na(Y))
    stop(sprintf(paste("the panel is unbalanced: %d of %d unit-period cells",
                       "are absent. Every estimator here differences a unit",
                       "against its own earlier value, so a missing cell is",
                       "a missing comparison."), miss, n * T))
  }
  list(Y = Y, units = units, periods = periods)
}

# First treated period index (0-based) per unit; Inf if never treated.
#' First treated period index (0-based) per unit; Inf if never treated
#'
#' Part of the cssant_native implementation; see the file header for the
#' source it follows.
#'
#' @param D See Usage.
#' @param unit See Usage.
#' @param time See Usage.
#' @param units Defaults to \code{NULL}.
#' @param periods Defaults to \code{NULL}.
#' @return A list with \code{g}, \code{Dm}, \code{units}, \code{periods}.
#' @export
.mor_did_first <- function(D, unit, time, units = NULL, periods = NULL) {
  p <- .mor_did_panel(D, unit, time)
  Dm <- p$Y
  if (!is.null(units) && !identical(p$units, units))
    stop("the treatment panel has a different unit set.")
  if (!is.null(periods) && !identical(p$periods, periods))
    stop("the treatment panel has a different period set.")
  if (!all(Dm %in% c(0, 1))) stop("treatment must be binary 0/1.")
  if (ncol(Dm) > 1L) {
    off <- apply(Dm, 1, function(r) any(diff(r) < 0))
    if (any(off))
      stop(sprintf(paste("treatment must be absorbing; unit(s) %s switch back",
                         "to untreated. Staggered-DiD identification is",
                         "defined for adoption, not for switching in and out."),
                   paste(utils::head(p$units[off], 5), collapse = ", ")))
  }
  g <- rep(Inf, nrow(Dm))
  ever <- apply(Dm, 1, function(r) any(r > 0))
  if (any(ever))
    g[ever] <- apply(Dm[ever, , drop = FALSE], 1,
                     function(r) which(r > 0)[1] - 1)
  list(g = g, Dm = Dm, units = p$units, periods = p$periods)
}

#' Group-time average treatment effects ATT(g, t)
#'
#' The building block of Callaway and Sant'Anna (2021): for cohort
#' \code{g} and period \code{t}, the change in outcome from the base
#' period \code{g - 1} to \code{t} among cohort-\code{g} units, minus
#' the same change among a clean comparison group.
#'
#' @param Y Balanced outcome matrix, units by periods.
#' @param g First treated period index per unit (0-based), \code{Inf}
#'   for never treated.
#' @param control \code{"notyet"} (default) uses units not yet treated
#'   at \code{max(t, g)}; \code{"never"} uses never-treated units only.
#'   Both comparison groups the paper defines are available.
#' @return A named list of cells, each with \code{att},
#'   \code{n_treated}, \code{n_control}, \code{post}, \code{rel} and
#'   the influence function \code{infl}.
#' @references Callaway, B. and Sant'Anna, P. H. C. (2021).
#'   Difference-in-differences with multiple time periods. Journal of
#'   Econometrics, 225(2), 200-230.
#' @export
morie_grouptimeatt <- function(Y, g, control = "notyet") {
  n <- nrow(Y); T <- ncol(Y)
  out <- list()
  cohorts <- sort(unique(g[is.finite(g)]))
  for (gg in cohorts) {
    gi <- as.integer(gg)
    if (gi < 1L) next          # no pre-period exists for a period-0 adopter
    base <- gi                 # column index of period gi - 1 (0-based gi-1)
    treated <- g == gg
    for (t in seq_len(T) - 1L) {
      if (t == gi - 1L) next
      ctrl <- if (control == "never") !is.finite(g) else g > max(t, gg)
      if (sum(treated) == 0L || sum(ctrl) == 0L) next
      dY <- Y[, t + 1L] - Y[, base]
      mt <- mean(dY[treated]); mc <- mean(dY[ctrl])
      infl <- numeric(n)
      infl[treated] <- (dY[treated] - mt) / sum(treated) * n
      infl[ctrl] <- -(dY[ctrl] - mc) / sum(ctrl) * n
      out[[sprintf("%.17g|%.17g", gg, t)]] <- list(
        gg = gg, t = t, att = mt - mc,
        n_treated = sum(treated), n_control = sum(ctrl),
        post = t >= gg, rel = t - gg, infl = infl)
    }
  }
  out
}

#' Aggregate ATT(g, t) cells
#'
#' The overall, event-study, cohort and calendar-time aggregations of
#' Callaway and Sant'Anna (2021), Sec. 4.  Standard errors come from
#' the influence functions of the constituent cells, so they account
#' for the estimation of every cell that enters an aggregate.
#'
#' @param gt Cell list from \code{\link{morie_grouptimeatt}}.
#' @param g First treated period index per unit.
#' @param n_units Number of units.
#' @param weights_by \code{"cohort_size"} (default) weights post-
#'   treatment cells by cohort size; \code{"equal"} weights them
#'   equally.  Both routes are kept.
#' @return A list with \code{overall}, \code{overall_se}, \code{event},
#'   \code{cohort} and \code{calendar}, or an empty list if no cell is
#'   post-treatment.
#' @references Callaway, B. and Sant'Anna, P. H. C. (2021). Journal of
#'   Econometrics, 225(2), 200-230, Section 4.
#' @export
morie_aggregateatt <- function(gt, g, n_units, weights_by = "cohort_size") {
  keys <- names(gt)[vapply(gt, function(v) isTRUE(v$post), logical(1))]
  if (length(keys) == 0L) return(list())
  gof <- vapply(keys, function(k) gt[[k]]$gg, numeric(1))
  tof <- vapply(keys, function(k) gt[[k]]$t, numeric(1))
  relof <- vapply(keys, function(k) gt[[k]]$rel, numeric(1))
  sizes <- vapply(gof, function(gg) sum(g == gg), numeric(1))
  tot <- sum(sizes)
  w <- if (weights_by == "equal") rep(1 / length(keys), length(keys)) else
    sizes / tot
  names(w) <- keys
  combine <- function(kk, wts) {
    s <- sum(wts)
    if (s <= 0) return(c(NA_real_, NA_real_))
    est <- sum(wts / s * vapply(kk, function(k) gt[[k]]$att, numeric(1)))
    infl <- numeric(length(gt[[kk[1]]]$infl))
    for (m in seq_along(kk)) infl <- infl + wts[m] / s * gt[[kk[m]]]$infl
    c(est, sqrt(sum(infl^2) / n_units^2))
  }
  ov <- combine(keys, w)
  event <- list()
  for (rr in sort(unique(relof))) {
    kk <- keys[relof == rr]
    event[[sprintf("%.17g", rr)]] <- combine(kk, w[kk])
  }
  cohort <- list()
  for (gg in sort(unique(gof))) {
    kk <- keys[gof == gg]
    cohort[[sprintf("%.17g", gg)]] <- combine(kk, rep(1, length(kk)))
  }
  calendar <- list()
  for (tt in sort(unique(tof))) {
    kk <- keys[tof == tt]
    calendar[[sprintf("%.17g", tt)]] <- combine(kk, w[kk])
  }
  list(overall = ov[1], overall_se = ov[2], event = event,
       cohort = cohort, calendar = calendar)
}

#' Callaway-Sant'Anna staggered difference-in-differences
#'
#' Estimates every group-time average treatment effect ATT(g, t)
#' against a clean comparison group and aggregates them (Callaway and
#' Sant'Anna 2021).  Because an already-treated unit is never used as
#' a control, the negative-weight comparisons that contaminate a
#' two-way fixed-effects regression cannot arise.
#'
#' @param y Outcome, long format.
#' @param D Binary 0/1 absorbing treatment, long format.
#' @param unit,time Panel identifiers.
#' @param cohort Optional per-unit adoption period, constant within
#'   unit, as an alternative to deriving it from \code{D}.
#' @param control \code{"notyet"} (default) or \code{"never"}.
#' @return A list with \code{estimate} (overall ATT), \code{se},
#'   \code{ci}, \code{att_gt}, \code{n_by_cell}, \code{event},
#'   \code{cohort_att}, \code{calendar}, \code{pretrend},
#'   \code{pretrend_max_abs}, \code{pretrend_note},
#'   \code{control_group}, \code{cohorts}, \code{n_cells},
#'   \code{n_units}, \code{n_periods}, \code{clean_controls},
#'   \code{method}.
#' @references Callaway, B. and Sant'Anna, P. H. C. (2021).
#'   Difference-in-differences with multiple time periods. Journal of
#'   Econometrics, 225(2), 200-230.
#' @export
morie_cssant <- function(y, D, unit, time, cohort = NULL,
                         control = "notyet") {
  if (!(control %in% c("notyet", "never")))
    stop("control must be 'notyet' or 'never'.")
  p <- .mor_did_panel(y, unit, time)
  Y <- p$Y; units <- p$units; periods <- p$periods
  if (is.null(cohort)) {
    g <- .mor_did_first(D, unit, time, units, periods)$g
  } else {
    ch <- as.numeric(cohort)
    cm <- .mor_did_panel(ifelse(is.finite(ch), ch, -1), unit, time)$Y
    if (any(apply(cm, 1, max) != apply(cm, 1, min)))
      stop("cohort must be constant within a unit.")
    gv <- cm[, 1]
    g <- vapply(gv, function(v) {
      if (!is.finite(v) || v < 0) return(Inf)
      k <- match(v, periods)
      if (is.na(k)) Inf else k - 1
    }, numeric(1))
  }
  if (!any(is.finite(g))) stop("no unit is ever treated.")
  if (control == "never" && all(is.finite(g)))
    stop(paste("control='never' needs never-treated units and every unit is",
               "eventually treated; use control='notyet'."))
  gt <- morie_grouptimeatt(Y, g, control = control)
  if (length(gt) == 0L)
    stop(paste("no (g, t) cell has both treated and control units; check that",
               "some cohort adopts after period 0 with a clean comparison",
               "group."))
  agg <- morie_aggregateatt(gt, g, length(units))
  prek <- names(gt)[vapply(gt, function(v) !isTRUE(v$post), logical(1))]
  pre <- lapply(gt[prek], function(v) v$att)
  z <- 1.959963984540054
  est <- if (length(agg)) agg$overall else NA_real_
  se <- if (length(agg)) agg$overall_se else NA_real_
  list(estimate = est, se = se, ci = c(est - z * se, est + z * se),
       att_gt = lapply(gt, function(v) v$att),
       n_by_cell = lapply(gt, function(v) c(v$n_treated, v$n_control)),
       event = if (length(agg)) agg$event else list(),
       cohort_att = if (length(agg)) agg$cohort else list(),
       calendar = if (length(agg)) agg$calendar else list(),
       pretrend = pre,
       pretrend_max_abs = if (length(pre))
         max(abs(unlist(pre))) else 0,
       pretrend_note = paste("cells with t < g are not effects; they are the",
                             "parallel-trends check and are reported",
                             "separately"),
       control_group = control,
       cohorts = sort(unique(g[is.finite(g)])),
       n_cells = length(gt), n_units = length(units),
       n_periods = length(periods),
       clean_controls = paste("no already-treated unit is ever used as a",
                              "control, so no negative-weight comparison can",
                              "enter the aggregate"),
       method = "Callaway-Sant'Anna (2021) group-time ATT(g,t)")
}
