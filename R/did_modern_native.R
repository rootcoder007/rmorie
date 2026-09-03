# SPDX-License-Identifier: AGPL-3.0-or-later
#
# did_modern_native.R -- module 16: the three modern staggered-DiD
# estimators missing from the toolbox (Phase 29.2 items 3-5). All
# three ride the native TWFE machinery in did_native.R; no fixest /
# did2s / didimputation at runtime, cross-validated against them in
# tests when installed.

# Shared design helper: long panel -> cohort (first treated period,
# NA = never), event time, and the treated-post indicator.
#' Shared design helper: long panel -> cohort (first treated period,
#'
#' NA = never), event time, and the treated-post indicator.
#'
#' @param data A vector; indexed elementwise.
#' @param outcome Passed to \code{c}.
#' @param unit Passed to \code{c}.
#' @param time Passed to \code{c}.
#' @param treatment_time Passed to \code{c}.
#' @return A list with \code{y}, \code{unit}, \code{time}, \code{g}, \code{rel},
#' \code{treated_post}.
#' @export
.morie_did_modern_frame <- function(data, outcome, unit, time,
                                    treatment_time) {
  need <- c(outcome, unit, time, treatment_time)
  missing_cols <- setdiff(need, names(data))
  if (length(missing_cols)) {
    stop("Columns missing from data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  g <- data[[treatment_time]]
  g[!is.finite(g)] <- NA
  list(
    y = as.numeric(data[[outcome]]),
    unit = as.factor(data[[unit]]),
    time = as.numeric(data[[time]]),
    g = g,
    rel = ifelse(is.na(g), NA_real_, as.numeric(data[[time]]) - g),
    treated_post = as.numeric(!is.na(g) & data[[time]] >= g)
  )
}

#' Sun & Abraham (2021) interaction-weighted event study
#'
#' Saturated cohort-by-relative-time event study: each (cohort, rel
#' time) cell gets its own coefficient via the native TWFE demeaner
#' (never-treated units as the clean control group), then the
#' cell coefficients are aggregated to relative-time effects with
#' cohort-share weights -- the IW estimator that is immune to the
#' negative-weighting contamination of a pooled event study.
#'
#' @srrstats {G2.1} Inputs validated in the shared frame builder.
#' @param data Long panel data frame.
#' @param outcome,unit,time Column names.
#' @param treatment_time First treated period per unit (NA = never).
#' @param leads,lags Relative-time window to report. Defaults -4..4.
#' @param alpha CI tail.
#' @return data.frame: rel_time, estimate, std.error, conf.low,
#'   conf.high, n; attribute "cells" holds the (cohort, rel) cell
#'   coefficients.
#' @references Sun & Abraham (2021) J. Econometrics 225(2).
#' @examples
#' df <- expand.grid(id = 1:40, t = 1:8)
#' df$g <- ifelse(df$id <= 12, 4L, ifelse(df$id <= 24, 6L, NA))
#' df$y <- rnorm(nrow(df)) + ifelse(!is.na(df$g) & df$t >= df$g, 2, 0)
#' morie_did_sun_abraham(df, "y", "id", "t", "g")
#' @export
morie_did_sun_abraham <- function(data, outcome, unit, time,
                                  treatment_time, leads = 4L,
                                  lags = 4L, alpha = 0.05) {
  fr <- .morie_did_modern_frame(
    data, outcome, unit, time,
    treatment_time
  )
  if (all(is.na(fr$g))) stop("No treated units.", call. = FALSE)
  cohorts <- sort(unique(fr$g[!is.na(fr$g)]))
  # FULL saturation: every (cohort, rel time) cell that exists in the
  # data except the rel = -1 reference. Restricting to the reporting
  # window would leave treated observations outside it undummied,
  # contaminating the FE baseline (the classic SA pitfall).
  rels_all <- sort(unique(fr$rel[!is.na(fr$rel)]))
  rels_all <- setdiff(rels_all, -1)
  cols <- list()
  for (co in cohorts) {
    for (r in rels_all) {
      sel <- !is.na(fr$g) & fr$g == co & (fr$time - co) == r
      if (sum(sel) > 0L) {
        cols[[sprintf("c%g_r%+d", co, r)]] <- as.numeric(sel)
      }
    }
  }
  X <- do.call(cbind, cols)
  fit <- .morie_did_twfe_native(fr$y, X, fr$unit, as.factor(fr$time),
    cluster_ids = data[[unit]]
  )
  cells <- data.frame(
    cell = colnames(X),
    cohort = as.numeric(sub("^c([0-9.]+)_r.*$", "\\1", colnames(X))),
    rel = as.numeric(sub("^c[0-9.]+_r", "", colnames(X))),
    estimate = as.numeric(fit$beta),
    std.error = as.numeric(fit$se)
  )
  # Cohort-share weights within each relative time.
  n_by_cohort <- table(fr$g[!is.na(fr$g) & !duplicated(data[[unit]])])
  z <- stats::qnorm(1 - alpha / 2)
  rep_rels <- intersect(sort(unique(cells$rel)), seq(-leads, lags))
  rows <- lapply(rep_rels, function(r) {
    cc <- cells[cells$rel == r, , drop = FALSE]
    wts <- as.numeric(n_by_cohort[as.character(cc$cohort)])
    wts[is.na(wts)] <- 0
    if (sum(wts) == 0) wts <- rep(1, nrow(cc))
    wts <- wts / sum(wts)
    est <- sum(wts * cc$estimate)
    se <- sqrt(sum(wts^2 * cc$std.error^2))
    data.frame(
      rel_time = r, estimate = est, std.error = se,
      conf.low = est - z * se, conf.high = est + z * se,
      n = nrow(cc)
    )
  })
  out <- do.call(rbind, rows)
  attr(out, "cells") <- cells
  class(out) <- c("morie_event_study", class(out))
  out
}


# Solve additive unit/time FEs on (possibly unbalanced) untreated
# cells by alternating projections; returns per-level lookups.
#' Solve additive unit/time FEs on (possibly unbalanced) untreated
#'
#' cells by alternating projections; returns per-level lookups.
#'
#' @param y0 Numeric; combined arithmetically in the body.
#' @param u0 Passed to \code{nlevels}.
#' @param t0 Passed to \code{nlevels}.
#' @param iters A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.
#' @return A list with \code{a}, \code{g}.
#' @export
.morie_did_fe_solve <- function(y0, u0, t0, iters = 50L) {
  a <- stats::setNames(rep(0, nlevels(u0)), levels(u0))
  gm <- stats::setNames(rep(0, nlevels(t0)), levels(t0))
  for (i in seq_len(iters)) {
    a_new <- tapply(y0 - gm[t0], u0, mean)
    a_new[is.na(a_new)] <- 0
    gm_new <- tapply(y0 - a_new[u0], t0, mean)
    gm_new[is.na(gm_new)] <- 0
    if (max(abs(a_new - a), abs(gm_new - gm)) < 1e-10) {
      a <- a_new
      gm <- gm_new
      break
    }
    a <- a_new
    gm <- gm_new
  }
  list(a = a, g = gm)
}

#' Borusyak, Jaravel & Spiess (2024) imputation estimator
#'
#' Fits unit and time fixed effects on the UNTREATED observations
#' only (via the native alternating demeaner), imputes the untreated
#' potential outcome for every treated cell, and averages the
#' treatment-minus-imputation differences. Standard error by cluster
#' (unit) bootstrap of the whole impute-and-average pipeline --
#' conservative and assumption-light.
#'
#' @inheritParams morie_did_sun_abraham
#' @param n_bootstrap Cluster-bootstrap replications. Default 199.
#' @param seed RNG seed.
#' @return List of class \code{"morie_did"}: estimate, std.error,
#'   conf.int, p.value, method, n_units, n_periods, call.
#' @references Borusyak, Jaravel & Spiess (2024) REStud 91(6).
#' @examples
#' df <- expand.grid(id = 1:40, t = 1:8)
#' df$g <- ifelse(df$id <= 20, 5L, NA)
#' df$y <- rnorm(nrow(df)) + ifelse(!is.na(df$g) & df$t >= df$g, 2, 0)
#' morie_did_borusyak(df, "y", "id", "t", "g", n_bootstrap = 29L)
#' @export
morie_did_borusyak <- function(data, outcome, unit, time,
                               treatment_time, n_bootstrap = 199L,
                               seed = 42L, alpha = 0.05) {
  fr <- .morie_did_modern_frame(
    data, outcome, unit, time,
    treatment_time
  )
  est_once <- function(df_idx) {
    y <- fr$y[df_idx]
    u <- droplevels(fr$unit[df_idx])
    tt <- as.factor(fr$time[df_idx])
    d <- fr$treated_post[df_idx]
    untreated <- d == 0
    if (sum(untreated) < 4L || sum(d) == 0L) {
      return(NA_real_)
    }
    # FEs from untreated cells only (alternating projections).
    fe <- .morie_did_fe_solve(
      y[untreated], droplevels(u[untreated]),
      droplevels(tt[untreated])
    )
    y0_hat <- fe$a[as.character(u)] + fe$g[as.character(tt)]
    diffs <- (y - y0_hat)[d == 1]
    mean(diffs, na.rm = TRUE)
  }
  att <- est_once(seq_along(fr$y))
  set.seed(seed)
  units <- levels(fr$unit)
  boots <- vapply(seq_len(n_bootstrap), function(b) {
    su <- sample(units, replace = TRUE)
    idx <- unlist(lapply(su, function(uu) which(fr$unit == uu)))
    est_once(idx)
  }, numeric(1))
  se <- stats::sd(boots, na.rm = TRUE)
  z <- stats::qnorm(1 - alpha / 2)
  out <- list(
    estimate = att, std.error = se,
    conf.int = c(att - z * se, att + z * se),
    p.value = 2 * stats::pnorm(-abs(att / se)),
    att_gt = NULL,
    method = "Borusyak-Jaravel-Spiess imputation (cluster bootstrap SE)",
    n_units = length(units),
    n_periods = length(unique(fr$time)), call = match.call()
  )
  class(out) <- "morie_did"
  out
}

#' Gardner (2022) two-stage difference-in-differences
#'
#' Stage 1: estimate unit and time fixed effects from untreated
#' observations only and residualize the outcome everywhere. Stage 2:
#' regress the residualized outcome on the treated-post indicator.
#' Standard error by cluster (unit) bootstrap of both stages, which
#' accounts for the first-stage estimation error the naive OLS SE
#' misses.
#'
#' @inheritParams morie_did_borusyak
#' @return List of class \code{"morie_did"}.
#' @references Gardner (2022) "Two-stage differences in differences",
#'   working paper.
#' @examples
#' df <- expand.grid(id = 1:40, t = 1:8)
#' df$g <- ifelse(df$id <= 20, 5L, NA)
#' df$y <- rnorm(nrow(df)) + ifelse(!is.na(df$g) & df$t >= df$g, 2, 0)
#' morie_did_did2s(df, "y", "id", "t", "g", n_bootstrap = 29L)
#' @export
morie_did_did2s <- function(data, outcome, unit, time, treatment_time,
                            n_bootstrap = 199L, seed = 42L,
                            alpha = 0.05) {
  fr <- .morie_did_modern_frame(
    data, outcome, unit, time,
    treatment_time
  )
  est_once <- function(idx) {
    y <- fr$y[idx]
    u <- droplevels(fr$unit[idx])
    tt <- as.factor(fr$time[idx])
    d <- fr$treated_post[idx]
    untreated <- d == 0
    if (sum(untreated) < 4L || sum(d) == 0L) {
      return(NA_real_)
    }
    fe <- .morie_did_fe_solve(
      y[untreated], droplevels(u[untreated]),
      droplevels(tt[untreated])
    )
    y_tilde <- y - (fe$a[as.character(u)] + fe$g[as.character(tt)])
    # Stage 2: OLS of the residualized outcome on the indicator.
    sum(y_tilde * d, na.rm = TRUE) / sum(d[!is.na(y_tilde)])
  }
  att <- est_once(seq_along(fr$y))
  set.seed(seed)
  units <- levels(fr$unit)
  boots <- vapply(seq_len(n_bootstrap), function(b) {
    su <- sample(units, replace = TRUE)
    idx <- unlist(lapply(su, function(uu) which(fr$unit == uu)))
    est_once(idx)
  }, numeric(1))
  se <- stats::sd(boots, na.rm = TRUE)
  z <- stats::qnorm(1 - alpha / 2)
  out <- list(
    estimate = att, std.error = se,
    conf.int = c(att - z * se, att + z * se),
    p.value = 2 * stats::pnorm(-abs(att / se)),
    att_gt = NULL,
    method = "Gardner (2022) two-stage DiD (cluster bootstrap SE)",
    n_units = length(units),
    n_periods = length(unique(fr$time)), call = match.call()
  )
  class(out) <- "morie_did"
  out
}
