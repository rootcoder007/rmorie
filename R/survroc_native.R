# morie.fn -- function file (rootcoder007/morie)
# Time-dependent ROC for censored survival data.
#
# Why the ordinary ROC does not apply. A diagnostic marker for a
# survival outcome has no fixed case/control split: whether a subject
# is a case depends on the horizon you ask about. Heagerty, Lumley and
# Pepe define the *cumulative case / dynamic control* classification at
# time t --
#
#   se(c, t) = P(M > c | T <= t), sp(c, t) = P(M <= c | T > t)
#
# so every subject is a control until it fails and a case thereafter,
# and the ROC curve and its area are functions of t.
#
# Censoring is the whole difficulty. A subject censored before
# t has unknown case status, so the empirical proportions above
# are not computable. Dropping such subjects biases the result whenever
# censoring is related to the marker. The paper's estimator instead
# routes through Bayes' theorem and the Kaplan-Meier estimator:
#
#   se_hat(c,t) = (1 - S_{M>c}(t)) * P(M>c) / (1 - S(t))
#   sp_hat(c,t) = S_{M<=c}(t) * P(M<=c) / S(t)
#
# with S_{M>c} the Kaplan-Meier curve computed within the
# subset above the threshold. Nothing is discarded: a censored subject
# contributes to the risk sets of whichever subset it belongs to.
#
# The check that matters. With no censoring, the estimator must
# collapse to the plain empirical proportions, and the area under the
# curve must equal the Mann-Whitney statistic comparing markers of
# cases against controls. Both are exact identities and both are
# anchored -- an estimator that merely looks plausible under censoring
# but misses these is wrong.
#
# Two routes. `km` is the estimator above. `empirical` computes
# the proportions directly and is *only* valid without censoring; it
# is provided because it is the thing the KM route must reduce to, and
# it refuses to run on censored data rather than quietly discarding
# subjects.
#
# A caveat the estimator carries. The KM route can produce
# sensitivities outside [0, 1] when the marker-specific curves
# cross badly at small samples. That is a property of the estimator,
# not a bug; the value is reported along with a flag rather than
# clipped silently.
#
# References
# ----------
# Heagerty, P. J., Lumley, T. & Pepe, M. S. (2000) "Time-dependent ROC
# curves for censored survival data and a diagnostic marker",
# Biometrics 56(2), 337-344,
# doi:10.1111/j.0006-341X.2000.00337.x. Sec. 2 (the cumulative
# case / dynamic control definitions above), Sec. 2.1 (the
# Kaplan-Meier estimator of sensitivity and specificity reproduced
# here, and its reduction to the empirical estimator without
# censoring), and the time-dependent area under the curve.

ROUTES <- c("km", "empirical")

.survroc_clean <- function(times, events, marker = NULL) {
  T <- as.numeric(times)
  E <- as.integer(events)
  if (length(T) != length(E)) {
    stop(sprintf("survroc: %d times but %d event indicators",
                 length(T), length(E)))
  }
  if (length(T) == 0L) {
    stop("survroc: no subjects given")
  }
  if (any(T < 0)) {
    stop("survroc: a survival time cannot be negative")
  }
  if (any(!(E %in% c(0L, 1L)))) {
    stop("survroc: the event indicator must be 0 (censored) or 1 (event)")
  }
  if (is.null(marker)) {
    return(list(T = T, E = E, M = NULL))
  }
  M <- as.numeric(marker)
  if (length(M) != length(T)) {
    stop(sprintf("survroc: %d markers but %d subjects",
                 length(M), length(T)))
  }
  list(T = T, E = E, M = M)
}

morie_survroc_kaplan_meier <- function(times, events, at = NULL) {
  cln <- .survroc_clean(times, events)
  T <- cln$T
  E <- cln$E
  n <- length(T)
  ord <- order(T, -E)
  curve <- list(c(0.0, 1.0))
  s <- 1.0
  at_risk <- n
  i <- 1L
  while (i <= n) {
    t_cur <- T[ord[i]]
    d <- 0L
    k <- 0L
    while (i <= n && T[ord[i]] == t_cur) {
      d <- d + E[ord[i]]
      k <- k + 1L
      i <- i + 1L
    }
    if (d > 0L) {
      s <- s * (1.0 - d / as.numeric(at_risk))
      curve[[length(curve) + 1L]] <- c(t_cur, s)
    }
    at_risk <- at_risk - k
  }
  if (is.null(at)) {
    return(curve)
  }
  val <- 1.0
  at_f <- as.numeric(at)
  for (j in seq_along(curve)) {
    pair <- curve[[j]]
    if (pair[1] <= at_f) {
      val <- pair[2]
    } else {
      break
    }
  }
  val
}

.survroc_empirical <- function(T, E, M, c, t) {
  if (any(E == 0L & T < t)) {
    stop(sprintf("survroc: the empirical route needs complete "
                 "follow-up to time %g, but a subject is "
                 "censored before it", t))
  }
  case_idx <- which(T <= t & E == 1L)
  ctrl_idx <- which(T > t)
  if (length(case_idx) == 0L || length(ctrl_idx) == 0L) {
    stop(sprintf("survroc: at t = %g there are %d cases and "
                 "%d controls; both are needed",
                 t, length(case_idx), length(ctrl_idx)))
  }
  se <- sum(M[case_idx] > c) / as.numeric(length(case_idx))
  sp <- sum(M[ctrl_idx] <= c) / as.numeric(length(ctrl_idx))
  c(se = se, sp = sp)
}

.survroc_km_pair <- function(T, E, M, c, t) {
  n <- length(T)
  hi_idx <- which(M > c)
  lo_idx <- which(M <= c)
  S <- morie_survroc_kaplan_meier(T, E, t)
  if (S <= 0.0) {
    stop(sprintf("survroc: the overall survival estimate is "
                 "zero at t = %g, so specificity is not "
                 "defined there", t))
  }
  if (S >= 1.0) {
    stop(sprintf("survroc: no events by t = %g, so "
                 "sensitivity is not defined there", t))
  }
  p_hi <- length(hi_idx) / as.numeric(n)
  p_lo <- length(lo_idx) / as.numeric(n)
  s_hi <- if (length(hi_idx) > 0L) {
    morie_survroc_kaplan_meier(T[hi_idx], E[hi_idx], t)
  } else {
    1.0
  }
  s_lo <- if (length(lo_idx) > 0L) {
    morie_survroc_kaplan_meier(T[lo_idx], E[lo_idx], t)
  } else {
    1.0
  }
  se <- (1.0 - s_hi) * p_hi / (1.0 - S)
  sp <- s_lo * p_lo / S
  c(se = se, sp = sp)
}

.survroc_pair <- function(times, events, marker, c, t, route) {
  if (!(route %in% ROUTES)) {
    stop(sprintf("survroc: route must be one of %s, got '%s'",
                 paste(ROUTES, collapse = ", "), route))
  }
  cln <- .survroc_clean(times, events, marker)
  T <- cln$T
  E <- cln$E
  M <- cln$M
  tt <- as.numeric(t)
  if (tt <= 0.0) {
    stop("survroc: the horizon must be positive")
  }
  if (route == "empirical") {
    return(.survroc_empirical(T, E, M, as.numeric(c), tt))
  }
  .survroc_km_pair(T, E, M, as.numeric(c), tt)
}

morie_survroc_sensitivity <- function(times, events, marker, threshold, t,
                                      route = "km") {
  pr <- .survroc_pair(times, events, marker, threshold, t, route)
  unname(pr[1])
}

morie_survroc_specificity <- function(times, events, marker, threshold, t,
                                      route = "km") {
  pr <- .survroc_pair(times, events, marker, threshold, t, route)
  unname(pr[2])
}

morie_survroc_roc_at <- function(times, events, marker, t, route = "km") {
  cln <- .survroc_clean(times, events, marker)
  M <- cln$M
  vals <- sort(unique(M))
  rng <- range(vals)
  eps <- rng[2] - rng[1]
  if (eps == 0) eps <- 1.0
  cuts <- c(rng[1] - eps, vals, rng[2] + eps)
  pts <- vector("list", length(cuts))
  for (k in seq_along(cuts)) {
    cc <- cuts[k]
    pr <- .survroc_pair(times, events, marker, cc, t, route)
    se_v <- unname(pr[1])
    sp_v <- unname(pr[2])
    pts[[k]] <- list(threshold = cc, sensitivity = se_v,
                     specificity = sp_v, fpr = 1.0 - sp_v)
  }
  ord <- order(-sapply(pts, function(p) p$threshold))
  pts[ord]
}

morie_survroc_auc_at <- function(times, events, marker, t, route = "km") {
  pts <- morie_survroc_roc_at(times, events, marker, t, route)
  if (length(pts) < 2L) return(0.0)
  a <- 0.0
  for (k in seq_len(length(pts) - 1L)) {
    p <- pts[[k]]
    q <- pts[[k + 1L]]
    a <- a + (q$fpr - p$fpr) * (p$sensitivity + q$sensitivity) / 2.0
  }
  a
}

morie_survroc_time_dependent_roc <- function(times, events, marker, t,
                                             route = "km") {
  cln <- .survroc_clean(times, events, marker)
  T <- cln$T
  E <- cln$E
  pts <- morie_survroc_roc_at(times, events, marker, t, route)
  a <- 0.0
  if (length(pts) >= 2L) {
    for (k in seq_len(length(pts) - 1L)) {
      p <- pts[[k]]
      q <- pts[[k + 1L]]
      a <- a + (q$fpr - p$fpr) * (p$sensitivity + q$sensitivity) / 2.0
    }
  }
  oor <- pts[vapply(pts, function(p) {
    !(p$sensitivity >= -1e-9 && p$sensitivity <= 1 + 1e-9 &&
      p$specificity >= -1e-9 && p$specificity <= 1 + 1e-9)
  }, logical(1))]
  list(
    estimate = a,
    auc = a,
    roc = pts,
    horizon = as.numeric(t),
    route = route,
    n = length(T),
    n_events_by_t = sum(T <= t & E == 1L),
    n_at_risk_after_t = sum(T > t),
    n_censored_before_t = sum(T < t & E == 0L),
    survival_at_t = morie_survroc_kaplan_meier(T, E, t),
    out_of_range = oor,
    method = sprintf("Heagerty, Lumley & Pepe (2000) cumulative "
                     "case / dynamic control ROC, %s estimator", route)
  )
}

morie_survroc <- morie_survroc_time_dependent_roc
