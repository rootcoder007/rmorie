# The fifteen survival methods that de-externalization dropped.
#
# Commit 508cdd7 replaced R/survival.R with native Kaplan-Meier,
# Nelson-Aalen, log-rank, Cox and concordance, correctly removing the
# survival:: and cmprsk:: wrappers.  Five of the old entry points were
# renamed onto those natives; these fifteen had no replacement and were
# deleted, leaving NAMESPACE exporting names that did not exist -- which
# made the package fail to load and every CI run red from 2026-08-03.
#
# Restored here from the definitions, natively.  Only base and stats are
# used, as in survival.R; no survival::, no cmprsk::.
#
# R mirror of morie/src/morie/fn/survmore.py.

#' .ms_check
#'
#' Part of the survival_more implementation; see the file header for the
#' source it follows.
#'
#' @param time See Usage.
#' @param event See Usage.
#' @return A list with \code{t}, \code{e}.
#' @export
.ms_check <- function(time, event) {
  t <- as.numeric(time); e <- as.integer(event)
  if (length(t) != length(e))
    stop("time and event must have the same length")
  if (!length(t)) stop("need at least one observation")
  if (any(t < 0)) stop("follow-up times cannot be negative")
  if (!all(e %in% c(0L, 1L))) stop("event must be 0 (censored) or 1 (event)")
  list(t = t, e = e)
}

#' .ms_km
#'
#' Part of the survival_more implementation; see the file header for the
#' source it follows.
#'
#' @param time See Usage.
#' @param event See Usage.
#' @return A list with \code{time}, \code{surv}, \code{n_risk}, \code{n_event}, \code{greenwood}.
#' @export
.ms_km <- function(time, event) {
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  ut <- sort(unique(t[e == 1L]))
  S <- nr <- ne <- gw <- numeric(length(ut))
  surv <- 1; v <- 0
  for (i in seq_along(ut)) {
    u <- ut[i]
    n_i <- sum(t >= u); d_i <- sum(t == u & e == 1L)
    surv <- surv * (1 - d_i / n_i)
    if (n_i > d_i) v <- v + d_i / (n_i * (n_i - d_i))
    S[i] <- surv; nr[i] <- n_i; ne[i] <- d_i; gw[i] <- v
  }
  list(time = ut, surv = S, n_risk = nr, n_event = ne, greenwood = gw)
}

#' Breslow cumulative baseline hazard, what survival::basehaz returns
#'
#' Part of the survival_more implementation; see the file header for the
#' source it follows.
#'
#' @param time See Usage.
#' @param event See Usage.
#' @param X See Usage.
#' @param beta See Usage.
#' @return A list with \code{time}, \code{cumhaz}, \code{weight}.
#' @export
.ms_baseline <- function(time, event, X, beta) {
  # Breslow cumulative baseline hazard, what survival::basehaz returns
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  X <- as.matrix(X); p <- ncol(X)
  if (nrow(X) != length(t)) stop("X must have one row per observation")
  if (length(beta) != p) stop("beta must have one entry per column of X")
  w <- exp(as.numeric(X %*% beta))
  ut <- sort(unique(t[e == 1L]))
  H <- numeric(length(ut)); cum <- 0
  for (i in seq_along(ut)) {
    u <- ut[i]
    d_i <- sum(t == u & e == 1L)
    den <- .morie_fsum(w[t >= u])
    if (den <= 0) stop(sprintf("empty risk set at t = %g", u))
    cum <- cum + d_i / den
    H[i] <- cum
  }
  list(time = ut, cumhaz = H, weight = w)
}

#' .ms_h0_at
#'
#' Part of the survival_more implementation; see the file header for the
#' source it follows.
#'
#' @param ut See Usage.
#' @param H See Usage.
#' @param t See Usage.
#' @return A vector, from \code{vapply}.
#' @export
.ms_h0_at <- function(ut, H, t) {
  vapply(t, function(x) {
    k <- sum(ut <= x)
    if (k) H[k] else 0
  }, numeric(1))
}

#' Restricted mean survival time
#'
#' Area under the Kaplan-Meier curve up to \code{tau}, RMST(tau) = integral_0^tau S(t) dt, with the variance of Klein and Moeschberger (2003) eq.
#' (4.5.4).
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param tau restriction horizon; defaults to the largest observed event time.
#' @param alpha significance level for the interval.
#' @return list with the RMST, its standard error, the confidence interval and the horizon used.
#' @references Klein, J. P. and Moeschberger, M. L. (2003). Survival Analysis, 2nd ed., Springer, eq. (4.5.4).
#' @export
Rmst <- function(time, event, tau = NULL, alpha = 0.05) {
  # RMST(tau) = integral_0^tau S(t) dt, the area under the KM curve, with
  # the variance of Klein and Moeschberger (2003) eq. (4.5.4).  The
  # horizon is not optional in substance: RMST is estimable only out to
  # the largest observed time, and past the last EVENT the curve is a
  # flat plateau the data cannot pin down, so a tau that overruns is
  # reported rather than silently honoured.  RMST answers what a hazard
  # ratio does not -- how much longer on average over a fixed window --
  # and needs no proportional-hazards assumption.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  km <- .ms_km(t, e)
  ut <- km$time; S <- km$surv; nr <- km$n_risk; ne <- km$n_event
  tmax <- max(t)
  horizon <- if (is.null(tau)) tmax else as.numeric(tau)
  if (horizon <= 0) stop("tau must be positive")
  area <- 0; prev_t <- 0; prev_S <- 1
  for (i in seq_along(ut)) {
    if (ut[i] >= horizon) break
    area <- area + prev_S * (ut[i] - prev_t)
    prev_t <- ut[i]; prev_S <- S[i]
  }
  area <- area + prev_S * (horizon - prev_t)
  v <- 0
  for (i in seq_along(ut)) {
    if (ut[i] > horizon) break
    if (nr[i] <= ne[i]) next
    tail <- 0; pt <- ut[i]; ps <- S[i]
    if (i < length(ut)) for (j in (i + 1):length(ut)) {
      if (ut[j] >= horizon) break
      tail <- tail + ps * (ut[j] - pt)
      pt <- ut[j]; ps <- S[j]
    }
    tail <- tail + ps * (horizon - pt)
    v <- v + tail^2 * ne[i] / (nr[i] * (nr[i] - ne[i]))
  }
  se <- if (v > 0) sqrt(v) else 0
  zq <- stats::qnorm(1 - alpha / 2)
  list(rmst = area, se = se, variance = v, tau = horizon,
       lower = area - zq * se, upper = area + zq * se,
       max_time = tmax,
       max_event_time = if (length(ut)) ut[length(ut)] else 0,
       tau_beyond_data = horizon > tmax,
       tau_beyond_last_event = length(ut) > 0 && horizon > ut[length(ut)],
       n = length(t), n_events = sum(e),
       method = paste("Klein and Moeschberger (2003) eq. (4.5.4);",
                      "area under the Kaplan-Meier curve"))
}

#' Difference in restricted mean survival time between two groups
#'
#' RMST_1 - RMST_2 with standard error sqrt(V1 + V2), the two groups treated as independent.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param group two-level grouping vector.
#' @param tau restriction horizon; defaults to the largest event time common to both groups.
#' @param alpha significance level for the interval.
#' @return list with the difference, its standard error, the confidence interval and each group's RMST.
#' @references Klein, J. P. and Moeschberger, M. L. (2003). Survival Analysis, 2nd ed., Springer.
#' @export
Rmstdiff <- function(time, event, group, tau = NULL,
                                     alpha = 0.05) {
  # RMST_1 - RMST_2 with SE = sqrt(V1 + V2), the groups independent.
  # tau MUST be common to both arms and is capped at the smaller of the
  # two largest observed times: areas over different windows are
  # different quantities, and the shorter arm cannot support the longer
  # window.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  g <- group
  if (length(g) != length(t))
    stop("group must have one entry per observation")
  levels_ <- sort(unique(as.character(g)))
  if (length(levels_) != 2L)
    stop(sprintf("rmst_diff compares exactly two groups, got %d",
                 length(levels_)))
  parts <- lapply(levels_, function(lv) {
    idx <- which(as.character(g) == lv)
    if (!length(idx)) stop(sprintf("group '%s' is empty", lv))
    list(t = t[idx], e = e[idx])
  })
  cap <- min(vapply(parts, function(p) max(p$t), numeric(1)))
  horizon <- if (is.null(tau)) cap else min(as.numeric(tau), cap)
  capped <- !is.null(tau) && as.numeric(tau) > cap
  a <- Rmst(parts[[1]]$t, parts[[1]]$e, tau = horizon,
                           alpha = alpha)
  b <- Rmst(parts[[2]]$t, parts[[2]]$e, tau = horizon,
                           alpha = alpha)
  diff <- a$rmst - b$rmst
  se <- sqrt(a$variance + b$variance)
  zq <- stats::qnorm(1 - alpha / 2)
  stat <- if (se > 0) diff / se else 0
  list(difference = diff, se = se, z = stat,
       p_value = 2 * stats::pnorm(abs(stat), lower.tail = FALSE),
       lower = diff - zq * se, upper = diff + zq * se,
       tau = horizon, tau_capped = capped, levels = levels_,
       rmst = c(a$rmst, b$rmst),
       ratio = if (b$rmst != 0) a$rmst / b$rmst else NULL,
       method = "difference of restricted means over a COMMON horizon")
}

#' Martingale residuals from a fitted Cox model
#'
#' M_i = delta_i - H0(t_i) exp(x_i' beta): observed events minus expected.
#' They sum to zero at the fitted beta, which is returned so the fit can be checked.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param X covariate matrix, one row per subject.
#' @param beta fitted coefficient vector.
#' @return list with the residuals, their sum, and the cumulative baseline hazard.
#' @references Therneau, T. M., Grambsch, P. M. and Fleming, T. R. (1990). Martingale-based residuals for survival models. Biometrika 77(1), 147-160.
#' @export
Martingale <- function(time, event, X, beta) {
  # M_i = delta_i - H0(t_i) exp(x_i' beta): observed events minus
  # expected.  They sum to zero at the fitted beta, which is returned as
  # a check.  Used for functional form -- residuals from a NULL model
  # plotted against a candidate covariate show the transformation the
  # model wants.  Badly skewed (bounded above by 1, unbounded below), so
  # poor for outliers; the deviance residuals are the symmetrized version.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  bl <- .ms_baseline(t, e, X, beta)
  h <- .ms_h0_at(bl$time, bl$cumhaz, t)
  m <- e - h * bl$weight
  list(residuals = m, expected = h * bl$weight, sum = .morie_fsum(m),
       sums_to_zero = abs(.morie_fsum(m)) < 1e-6 * length(t),
       max = max(m), min = min(m), upper_bound = 1, n = length(t),
       skewed = TRUE,
       method = "M_i = delta_i - H0(t_i) exp(x_i' beta), Breslow baseline")
}

#' Deviance residuals from a fitted Cox model
#'
#' d_i = sign(M) sqrt(-2[M + delta log(delta - M)]), a symmetrizing transform of the martingale residuals: roughly normal when the model holds.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param X covariate matrix, one row per subject.
#' @param beta fitted coefficient vector.
#' @return list with the deviance residuals and the martingale residuals they derive from.
#' @references Therneau, T. M., Grambsch, P. M. and Fleming, T. R. (1990). Martingale-based residuals for survival models. Biometrika 77(1), 147-160.
#' @export
Devresid <- function(time, event, X, beta) {
  # d_i = sign(M) sqrt(-2[M + delta log(delta - M)]), a symmetrizing
  # transform of the martingale residuals: roughly normal when the model
  # fits, so a large |d_i| is an outlier in the usual sense.  The log
  # term is zero when delta = 0, which is the limit, not a special case
  # swept aside.  The sum of squares is NOT the model deviance for a Cox
  # fit -- the partial likelihood is not a full likelihood.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  m <- Martingale(t, e, X, beta)$residuals
  d <- numeric(length(t))
  for (i in seq_along(t)) {
    inner <- m[i]
    if (e[i]) {
      arg <- e[i] - m[i]
      if (arg <= 0)
        stop(sprintf(paste("delta - M is not positive at i = %d; the",
                           "deviance residual is undefined there"), i))
      inner <- m[i] + e[i] * log(arg)
    }
    val <- -2 * inner
    d[i] <- sign(if (m[i] >= 0) 1 else -1) * sqrt(max(val, 0))
  }
  list(residuals = d, martingale = m,
       sum_of_squares = .morie_fsum(d * d), max_abs = max(abs(d)),
       n = length(t), is_model_deviance = FALSE,
       method = "d_i = sign(M) sqrt(-2[M + delta log(delta - M)])")
}

#' Cox-Snell residuals from a fitted Cox model
#'
#' r_i = H0(t_i) exp(x_i' beta) = delta_i - M_i, the fitted cumulative hazard.
#' If the model is right these behave like a censored unit exponential sample.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param X covariate matrix, one row per subject.
#' @param beta fitted coefficient vector.
#' @return list with the Cox-Snell residuals and the Nelson-Aalen estimate computed from them.
#' @references Cox, D. R. and Snell, E. J. (1968). A general definition of residuals. JRSS-B 30(2), 248-275.
#' @export
Coxsnell <- function(time, event, X, beta) {
  # r_i = H0(t_i) exp(x_i' beta) = delta_i - M_i, the fitted cumulative
  # hazard.  If the model is right these behave like a censored unit
  # exponential sample, so the Nelson-Aalen hazard OF THE RESIDUALS
  # plotted against them should follow the 45-degree line.  That curve is
  # returned, since the residuals alone say nothing without it.  The
  # check is weak: the plot is fitted, not held out.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  bl <- .ms_baseline(t, e, X, beta)
  r <- .ms_h0_at(bl$time, bl$cumhaz, t) * bl$weight
  ord <- order(r)
  rt <- numeric(0); rH <- numeric(0); cum <- 0
  for (i in ord) {
    if (e[i] != 1L) next
    n_i <- sum(r >= r[i]); d_i <- sum(r == r[i] & e == 1L)
    cum <- cum + d_i / n_i
    rt <- c(rt, r[i]); rH <- c(rH, cum)
  }
  list(residuals = r, diagnostic_x = rt, diagnostic_h = rH,
       max_deviation = if (length(rt)) max(abs(rt - rH)) else 0,
       n = length(t),
       reference = paste("unit exponential; the Nelson-Aalen hazard of the",
                         "residuals should follow the 45-degree line"),
       in_sample_check = TRUE,
       method = "r_i = H0(t_i) exp(x_i' beta) = delta_i - M_i")
}

#' Schoenfeld residuals and the proportional-hazards test
#'
#' s_i = x_i minus the weighted risk-set mean at each event time.
#' Grambsch and Therneau's scaled version tests proportional hazards by regressing the residuals on time.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param X covariate matrix, one row per subject.
#' @param beta fitted coefficient vector.
#' @param vcov covariance matrix of beta, used to scale the residuals.
#' @param scaled whether to return the scaled residuals.
#' @return list with the residuals, the event times, and the proportional-hazards test statistic and p-value.
#' @references Grambsch, P. M. and Therneau, T. M. (1994). Proportional hazards tests and diagnostics based on weighted residuals. Biometrika 81(3), 515-526.
#' @export
Schoenfeld <- function(time, event, X, beta, vcov = NULL,
                                      scaled = TRUE) {
  # s_i = x_i - weighted risk-set mean at each event time.  Grambsch and
  # Therneau (1994) scale them, s* = beta + d V s; under proportional
  # hazards E[s*] = beta at every time, so a correlation between s* and
  # time is evidence against PH.  That test is returned.  One residual
  # per event time: with ties only the first event gets one, and
  # ties_dropped counts what that discarded.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  X <- as.matrix(X); p <- ncol(X)
  if (length(beta) != p) stop("beta must have one entry per column of X")
  w <- exp(as.numeric(X %*% beta))
  ut <- sort(unique(t[e == 1L]))
  times <- numeric(0); res <- NULL; dropped <- 0L
  for (u in ut) {
    rk <- t >= u; ev <- which(t == u & e == 1L)
    dropped <- dropped + length(ev) - 1L
    sw <- .morie_fsum(w[rk])
    xbar <- vapply(seq_len(p),
                   function(k) .morie_fsum(X[rk, k] * w[rk]) / sw,
                   numeric(1))
    times <- c(times, u)
    res <- rbind(res, X[ev[1], ] - xbar)
  }
  out <- list(time = times, residuals = res, n_events = length(times),
              ties_dropped = dropped, p = p,
              method = "s_i = x_i - weighted risk-set mean")
  if (scaled) {
    if (is.null(vcov))
      stop(paste("scaling needs the covariance of beta; pass vcov= from",
                 "the Cox fit, or scaled=FALSE"))
    V <- as.matrix(vcov)
    if (nrow(V) != p || ncol(V) != p) stop("vcov must be p x p")
    d <- length(times)
    sc <- t(apply(res, 1, function(r) beta + d * as.numeric(V %*% r)))
    if (p == 1L) sc <- matrix(sc, ncol = 1L)
    out$scaled <- sc
    stats_ <- lapply(seq_len(p), function(k) {
      y <- sc[, k]; n <- length(y)
      if (n < 3L) return(list(rho = NULL, z = NULL, p_value = NULL))
      st <- sum((times - mean(times))^2); sy <- sum((y - mean(y))^2)
      if (st <= 0 || sy <= 0) return(list(rho = 0, z = 0, p_value = 1))
      rho <- sum((times - mean(times)) * (y - mean(y))) / sqrt(st * sy)
      zz <- rho * sqrt(n - 1)
      list(rho = rho, z = zz,
           p_value = 2 * stats::pnorm(abs(zz), lower.tail = FALSE))
    })
    out$ph_test <- stats_
    out$ph_violated <- vapply(stats_, function(s)
      !is.null(s$p_value) && s$p_value < 0.05, logical(1))
    out$method <- paste("Grambsch and Therneau (1994) scaled Schoenfeld",
                        "residuals and the correlation-with-time PH test")
  }
  out
}

#' Hazard ratios with confidence intervals
#'
#' HR = exp(beta) with the interval formed on the LOG scale and exponentiated, so it is asymmetric about the hazard ratio and never crosses zero.
#'
#' @param beta coefficient vector on the log-hazard scale.
#' @param se standard errors of \code{beta}.
#' @param alpha significance level.
#' @param names optional names for the coefficients.
#' @return list with the hazard ratios, the interval bounds, and the z statistics and p-values.
#' @references Klein, J. P. and Moeschberger, M. L. (2003). Survival Analysis, 2nd ed., Springer.
#' @export
Hazratio <- function(beta, se, alpha = 0.05, names = NULL) {
  # HR = exp(beta), CI = exp(beta +/- z se).  The interval is formed on
  # the LOG scale and exponentiated, so it is asymmetric about the HR and
  # cannot cross zero.  Building it as HR +/- z se(HR) -- the common slip
  # -- gives an interval that can include negative hazard ratios and has
  # the wrong coverage.  Constant over time only if PH holds; check with
  # Schoenfeld before quoting one number.
  b <- as.numeric(beta); s <- as.numeric(se)
  if (length(b) != length(s)) stop("beta and se must have the same length")
  if (!length(b)) stop("need at least one coefficient")
  if (any(s < 0)) stop("standard errors cannot be negative")
  zq <- stats::qnorm(1 - alpha / 2)
  zs <- ifelse(s > 0, b / s, 0)
  list(hazard_ratio = exp(b), lower = exp(b - zq * s),
       upper = exp(b + zq * s), coef = b, se = s, z = zs,
       p_value = 2 * stats::pnorm(abs(zs), lower.tail = FALSE),
       names = names, alpha = alpha, interval_on_log_scale = TRUE,
       assumes_proportional_hazards = TRUE,
       method = paste("HR = exp(beta), interval exponentiated from the",
                      "log scale"))
}

#' Cumulative incidence function under competing risks
#'
#' Aalen-Johansen: CIF_k(t) = sum S(t_{i-1}) d_ki / n_i, where S is the ALL-CAUSE Kaplan-Meier.
#' That factor is the point: treating competing events as censored overstates the incidence.
#'
#' @param time observed follow-up times.
#' @param cause cause of failure, 0 for censored.
#' @param code the cause whose incidence is wanted.
#' @param alpha significance level for the interval.
#' @return list with the cumulative incidence, its standard error, the confidence interval and the event times.
#' @references Aalen, O. O. and Johansen, S. (1978). An empirical transition matrix for non-homogeneous Markov chains based on censored observations. Scandinavian Journal of Statistics 5(3), 141-150.
#' @export
Cif <- function(time, cause, code = 1, alpha = 0.05) {
  # Aalen-Johansen: CIF_k(t) = sum S(t_{i-1}) d_ki / n_i, with S the
  # ALL-CAUSE Kaplan-Meier.  The S(t_{i-1}) factor is the entire point:
  # censoring the competing events and running an ordinary KM gives
  # 1 - S_k, which OVERSTATES the incidence by assuming those who died of
  # something else would have remained at risk.  Both are returned so the
  # size of that bias is visible on the data at hand.
  t <- as.numeric(time); c_ <- as.integer(cause)
  if (length(t) != length(c_))
    stop("time and cause must have the same length")
  if (!length(t)) stop("need at least one observation")
  k <- as.integer(code)
  if (k == 0L) stop("cause 0 marks censoring; pick an event cause")
  if (!(k %in% c_)) stop(sprintf("cause %d does not occur in the data", k))
  ut <- sort(unique(t[c_ != 0L]))
  surv <- 1; cum <- 0; v <- 0
  F_ <- nr <- nk <- va <- numeric(length(ut))
  for (i in seq_along(ut)) {
    u <- ut[i]
    n_i <- sum(t >= u)
    d_all <- sum(t == u & c_ != 0L); d_k <- sum(t == u & c_ == k)
    cum <- cum + surv * d_k / n_i
    if (n_i > d_all) v <- v + surv^2 * d_k * (n_i - d_k) / n_i^3
    surv <- surv * (1 - d_all / n_i)
    F_[i] <- cum; nr[i] <- n_i; nk[i] <- d_k; va[i] <- v
  }
  naive <- 1 - .ms_km(t, as.integer(c_ == k))$surv
  zq <- stats::qnorm(1 - alpha / 2)
  se <- sqrt(pmax(va, 0))
  list(time = ut, cif = F_, se = se,
       lower = pmax(0, F_ - zq * se), upper = pmin(1, F_ + zq * se),
       n_risk = nr, n_event = nk, naive_one_minus_km = naive,
       naive_overstates_by = if (length(naive) && length(F_))
         naive[length(naive)] - F_[length(F_)] else 0,
       overall_survival_at_end = surv, cause = k, n = length(t),
       method = paste("Aalen-Johansen cumulative incidence; the naive",
                      "curve censors the competing events and overstates"))
}

#' Fine-Gray subdistribution hazard model
#'
#' A Cox model on the subdistribution hazard, in which subjects failing from a competing cause remain in the risk set with a decreasing weight, so the coefficients act directly on the cumulative incidence.
#'
#' @param time observed follow-up times.
#' @param cause cause of failure, 0 for censored.
#' @param X covariate matrix, one row per subject.
#' @param code the cause of interest.
#' @param max_iter maximum Newton-Raphson iterations.
#' @param tol convergence tolerance.
#' @return list with the coefficients, their standard errors, the log-likelihood and the iteration count.
#' @references Fine, J. P. and Gray, R. J. (1999). A proportional hazards model for the subdistribution of a competing risk. JASA 94(446), 496-509.
#' @export
Finegray <- function(time, cause, X, code = 1,
                                    max_iter = 50L, tol = 1e-9) {
  # Fine and Gray (1999, JASA 94:496-509).  A Cox model on the
  # SUBDISTRIBUTION risk set: a subject failing of a competing cause
  # stays at risk past its failure time with IPCW weight G(t)/G(T_i), G
  # the KM of the censoring distribution.  That is the whole difference
  # from a cause-specific Cox model, and the two answer different
  # questions -- a covariate can raise one and lower the other.  The
  # weights assume censoring independent of covariates.
  t <- as.numeric(time); c_ <- as.integer(cause)
  X <- as.matrix(X); p <- ncol(X); n <- length(t)
  if (length(c_) != n || nrow(X) != n)
    stop("time, cause and X must agree in length")
  k <- as.integer(code)
  g <- .ms_km(t, as.integer(c_ == 0L))
  G <- function(x) {
    j <- sum(g$time <= x)
    if (j) g$surv[j] else 1
  }
  ut <- sort(unique(t[c_ == k]))
  if (!length(ut)) stop(sprintf("cause %d does not occur in the data", k))
  beta <- numeric(p); Hm <- diag(p)
  for (it in seq_len(as.integer(max_iter))) {
    gvec <- numeric(p); Hm <- matrix(0, p, p)
    for (u in ut) {
      idx <- integer(0); wts <- numeric(0)
      for (i in seq_len(n)) {
        if (t[i] >= u) { idx <- c(idx, i); wts <- c(wts, 1) }
        else if (c_[i] != 0L && c_[i] != k) {
          gi <- G(t[i])
          if (gi > 0) { idx <- c(idx, i); wts <- c(wts, G(u) / gi) }
        }
      }
      keep <- wts > 0; idx <- idx[keep]; wts <- wts[keep]
      if (!length(idx)) next
      ev <- which(t == u & c_ == k)
      wt <- wts * exp(as.numeric(X[idx, , drop = FALSE] %*% beta))
      s0 <- .morie_fsum(wt)
      if (s0 <= 0) next
      s1 <- as.numeric(crossprod(X[idx, , drop = FALSE], wt))
      gvec <- gvec + colSums(X[ev, , drop = FALSE]) - length(ev) * s1 / s0
      s2 <- crossprod(X[idx, , drop = FALSE],
                      X[idx, , drop = FALSE] * wt)
      Hm <- Hm + length(ev) * (s2 / s0 - outer(s1, s1) / s0^2)
    }
    step <- solve(Hm, gvec)
    beta <- beta + step
    if (max(abs(step)) < tol) break
  }
  V <- solve(Hm)
  se <- sqrt(pmax(diag(V), 0))
  zs <- ifelse(se > 0, beta / se, 0)
  list(coef = beta, se = se, z = zs,
       p_value = 2 * stats::pnorm(abs(zs), lower.tail = FALSE),
       subdistribution_hazard_ratio = exp(beta), vcov = V, n = n,
       n_events = sum(c_ == k),
       n_competing = sum(c_ != 0L & c_ != k), cause = k,
       covariate_independent_censoring_assumed = TRUE,
       differs_from_cause_specific = TRUE,
       method = paste("Fine and Gray (1999) subdistribution hazard,",
                      "IPCW risk set"))
}

#' Kaplan-Meier estimator under left truncation
#'
#' The only change from the ordinary estimator is the risk set: a subject enters at \code{entry} rather than at time zero, so it is at risk only over (entry, time].
#'
#' @param entry entry (truncation) times.
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param alpha significance level for the interval.
#' @return list with the survival estimate, its standard error, the confidence interval and the risk-set sizes.
#' @references Klein, J. P. and Moeschberger, M. L. (2003). Survival Analysis, 2nd ed., Springer, Section 3.4.
#' @export
Ltkm <- function(entry, time, event,
                                             alpha = 0.05) {
  # The only change from the ordinary estimator is the risk set:
  # n_i = #{ j : entry_j < t_i <= time_j }.  Ignoring the truncation
  # counts subjects as at risk before enrolment and biases early survival
  # upward -- the immortal-time bias that makes prevalent-cohort studies
  # look protective.  Both curves are returned so the bias is visible.
  en <- as.numeric(entry)
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  if (length(en) != length(t))
    stop("entry and time must have the same length")
  if (any(en >= t))
    stop("every entry time must be strictly before its follow-up time")
  ut <- sort(unique(t[e == 1L]))
  S <- se <- nr <- ne <- numeric(length(ut))
  surv <- 1; v <- 0; empty <- numeric(0)
  for (i in seq_along(ut)) {
    u <- ut[i]
    n_i <- sum(en < u & u <= t); d_i <- sum(t == u & e == 1L)
    if (n_i == 0L) {
      empty <- c(empty, u)
      S[i] <- surv; se[i] <- surv * sqrt(v); nr[i] <- 0; ne[i] <- d_i
      next
    }
    surv <- surv * (1 - d_i / n_i)
    if (n_i > d_i) v <- v + d_i / (n_i * (n_i - d_i))
    S[i] <- surv; se[i] <- surv * sqrt(v); nr[i] <- n_i; ne[i] <- d_i
  }
  naive <- .ms_km(t, e)$surv
  zq <- stats::qnorm(1 - alpha / 2)
  list(time = ut, surv = S, se = se, n_risk = nr, n_event = ne,
       lower = pmax(0, S - zq * se), upper = pmin(1, S + zq * se),
       ignoring_truncation = naive,
       max_difference = if (length(S)) max(abs(S - naive)) else 0,
       empty_risk_sets = empty, n = length(t), n_events = sum(e),
       method = paste("Kaplan-Meier with the risk set restricted to",
                      "entry < t <= time"))
}

#' Landmark analysis of survival
#'
#' Subjects failing or censored before the landmark are dropped and the clock is reset there, which removes the immortal-time bias that arises from conditioning on a post-baseline event.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param landmark_time the landmark at which the clock is reset.
#' @param X optional covariate matrix.
#' @param group optional grouping vector.
#' @param alpha significance level for the interval.
#' @return list with the landmark survival estimate, the number retained and dropped, and the group comparison when supplied.
#' @references Anderson, J. R., Cain, K. C. and Gelber, R. D. (1983). Analysis of survival by tumor response. Journal of Clinical Oncology 1(11), 710-719.
#' @export
Landmark <- function(time, event, landmark_time, X = NULL,
                                    group = NULL, alpha = 0.05) {
  # Drop subjects who fail or are censored before the landmark, reset the
  # clock, analyse the survivors.  The standard remedy for immortal-time
  # bias when the exposure is only known after baseline: classifying
  # subjects by something that could not have happened unless they
  # survived makes the exposed look protected for no reason but that they
  # lived.  The cost is stated, not hidden: n_dropped is the data
  # discarded, and the estimate is conditional on reaching the landmark.
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  lm <- as.numeric(landmark_time)
  if (lm <= 0) stop("the landmark must be positive")
  keep <- which(t > lm)
  if (length(keep) < 2L)
    stop(sprintf(paste("the landmark leaves %d subjects; it is past the",
                       "bulk of the follow-up"), length(keep)))
  tt <- t[keep] - lm; ee <- e[keep]
  km <- .ms_km(tt, ee)
  out <- list(landmark = lm, n_original = length(t),
              n_retained = length(keep),
              n_dropped = length(t) - length(keep), kept_index = keep,
              time = tt, event = ee, km_time = km$time, km_surv = km$surv,
              conditional_on_surviving_to_landmark = TRUE,
              method = paste("landmark analysis; the clock is reset at the",
                             "landmark and earlier subjects are dropped"))
  if (!is.null(group)) {
    if (length(group) != length(t))
      stop("group must have one entry per observation")
    gk <- group[keep]
    out$group <- gk
    out$levels <- sort(unique(as.character(gk)))
    out$by_group <- lapply(out$levels, function(lv) {
      idx <- which(as.character(gk) == lv)
      if (length(idx) < 2L) return(NULL)
      c0 <- .ms_km(tt[idx], ee[idx])
      list(time = c0$time, surv = c0$surv)
    })
    names(out$by_group) <- out$levels
  }
  if (!is.null(X)) {
    X <- as.matrix(X)
    if (nrow(X) != length(t)) stop("X must have one row per observation")
    out$X <- X[keep, , drop = FALSE]
    out$p <- ncol(X)
  }
  out
}

#' Turnbull estimator for interval-censored data
#'
#' Self-consistency (EM) for the nonparametric MLE.
#' Mass sits only on the maximal intersections of the observed intervals, so the estimator is a step function that jumps nowhere else.
#'
#' @param left left endpoints of the censoring intervals.
#' @param right right endpoints; use \code{Inf} for right-censored observations.
#' @param max_iter maximum EM iterations.
#' @param tol convergence tolerance.
#' @return list with the estimated masses, the intersection intervals, the survival curve and the iteration count.
#' @references Turnbull, B. W. (1976). The empirical distribution function with arbitrarily grouped, censored and truncated data. JRSS-B 38(3), 290-295.
#' @export
Turnbull <- function(left, right, max_iter = 1000L,
                                    tol = 1e-10) {
  # Turnbull (1976, JRSS-B 38:290-295).  Mass sits only on the maximal
  # "Turnbull intervals" and is found by self-consistency, which is an EM
  # algorithm: monotone in likelihood, but only to a LOCAL maximum, and
  # the NPMLE is not unique INSIDE an interval -- it says the mass is in
  # there, not where.  The survival curve is therefore undefined across
  # each such gap, returned as ambiguous_intervals rather than
  # interpolated away.  Inf marks right censoring.
  L <- as.numeric(left)
  R <- as.numeric(right); R[is.na(R)] <- Inf
  if (length(L) != length(R))
    stop("left and right must have the same length")
  n <- length(L)
  if (!n) stop("need at least one interval")
  if (any(L > R)) stop("every left endpoint must not exceed its right")
  lefts <- sort(unique(L)); rights <- sort(unique(R[is.finite(R)]))
  inner <- list()
  for (q in lefts) {
    cand <- rights[rights >= q]
    if (!length(cand)) next
    pe <- min(cand)
    if (any(lefts > q & lefts < pe) || any(rights > q & rights < pe)) next
    inner[[length(inner) + 1L]] <- c(q, pe)
  }
  if (!length(inner))
    stop(paste("no Turnbull interval could be formed; every observation",
               "may be right-censored"))
  key <- vapply(inner, function(v) paste(v, collapse = "_"), character(1))
  inner <- inner[!duplicated(key)]
  inner <- inner[order(vapply(inner, function(v) v[1], numeric(1)))]
  m <- length(inner)
  alpha <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m))
    if (L[i] <= inner[[j]][1] && inner[[j]][2] <= R[i]) alpha[i, j] <- 1
  if (any(rowSums(alpha) == 0))
    stop(paste("an observation is compatible with no Turnbull interval;",
               "check the endpoints"))
  p <- rep(1 / m, m); it <- 0L; change <- Inf
  for (it in seq_len(as.integer(max_iter))) {
    den <- as.numeric(alpha %*% p)
    den[den <= 0] <- NA_real_
    new <- colSums(alpha * (p[col(alpha)] / den), na.rm = TRUE) / n
    change <- max(abs(new - p))
    p <- new
    if (change < tol) break
  }
  surv <- pmax(0, 1 - cumsum(p))
  d <- as.numeric(alpha %*% p)
  ll <- sum(log(d[d > 0]))
  list(intervals = inner, mass = p, surv = surv, loglik = ll,
       iterations = it, change = change, converged = change < tol,
       ambiguous_intervals = inner[p > 1e-8], n = n, n_intervals = m,
       npmle_not_unique_within_intervals = TRUE,
       method = "Turnbull (1976) self-consistency / EM")
}

.ms_dists <- c("exponential", "weibull", "lognormal", "loglogistic")

#' .ms_logsf_logpdf
#'
#' Part of the survival_more implementation; see the file header for the
#' source it follows.
#'
#' @param dist See Usage.
#' @param y See Usage.
#' @param mu See Usage.
#' @param logsig See Usage.
#' @return Nothing; this branch always raises.
#' @export
.ms_logsf_logpdf <- function(dist, y, mu, logsig) {
  sig <- exp(logsig); z <- (y - mu) / sig
  if (dist %in% c("weibull", "exponential")) {
    ez <- exp(pmin(z, 700))
    return(list(lS = -ez, lf = z - ez - logsig))
  }
  if (dist == "lognormal") {
    S <- pmax(stats::pnorm(z, lower.tail = FALSE), 1e-300)
    return(list(lS = log(S),
                lf = -0.5 * z^2 - 0.5 * log(2 * pi) - logsig))
  }
  if (dist == "loglogistic") {
    ez <- exp(pmin(z, 700))
    return(list(lS = -log1p(ez), lf = z - 2 * log1p(ez) - logsig))
  }
  stop(sprintf("unknown distribution '%s'; known: %s", dist,
               paste(.ms_dists, collapse = ", ")))
}

#' .ms_fit_lls
#'
#' Part of the survival_more implementation; see the file header for the
#' source it follows.
#'
#' @param dist See Usage.
#' @param time See Usage.
#' @param event See Usage.
#' @param X Defaults to \code{NULL}.
#' @return A list with \code{dist}, \code{coef}, \code{log_scale}, \code{scale}, \code{loglik}, \code{n_par}, \code{n}, \code{n_events}, \code{aic}, \code{bic}, \code{fixed_scale}, \code{convergence}.
#' @export
.ms_fit_lls <- function(dist, time, event, X = NULL) {
  z <- .ms_check(time, event); t <- z$t; e <- z$e
  if (any(t <= 0))
    stop("a log-location-scale model needs positive times")
  y <- log(t); n <- length(y)
  Xm <- if (is.null(X)) matrix(1, n, 1) else cbind(1, as.matrix(X))
  if (nrow(Xm) != n) stop("X must have one row per observation")
  p <- ncol(Xm)
  fixed <- dist == "exponential"
  nll <- function(theta) {
    beta <- theta[seq_len(p)]
    ls <- if (fixed) 0 else theta[p + 1L]
    mu <- as.numeric(Xm %*% beta)
    d <- .ms_logsf_logpdf(dist, y, mu, ls)
    v <- ifelse(e == 1L, d$lf, d$lS)
    if (any(!is.finite(v))) return(1e300)
    -sum(v)
  }
  x0 <- c(mean(y), rep(0, p - 1L))
  if (!fixed) x0 <- c(x0, 0.5 * log(max(stats::var(y), 1e-6)))
  # Nelder-Mead warns (rightly) that it is unreliable in one dimension;
  # the exponential with no covariates has a single parameter, so route
  # that case to a golden-section search instead of ignoring the warning.
  if (length(x0) == 1L) {
    lo <- x0 - 10; hi <- x0 + 10
    op <- stats::optimize(nll, interval = c(lo, hi), tol = 1e-12)
    fit <- list(par = op$minimum, convergence = 0L)
  } else {
    fit <- stats::optim(x0, nll, method = "Nelder-Mead",
                        control = list(maxit = 5000, reltol = 1e-12))
  }
  theta <- fit$par; ll <- -nll(theta); k <- length(theta)
  list(dist = dist, coef = theta[seq_len(p)],
       log_scale = if (fixed) 0 else theta[p + 1L],
       scale = if (fixed) 1 else exp(theta[p + 1L]),
       loglik = ll, n_par = k, n = n, n_events = sum(e),
       aic = 2 * k - 2 * ll, bic = k * log(n) - 2 * ll,
       fixed_scale = fixed, convergence = fit$convergence)
}

#' Parametric survival fit by maximum likelihood
#'
#' ML for a log-location-scale family with right censoring, maximising prod f(t)^delta S(t)^(1-delta).
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param dist distribution: one of weibull, exponential, lognormal or loglogistic.
#' @return list with the location and scale estimates, the log-likelihood, AIC and BIC.
#' @references Kalbfleisch, J. D. and Prentice, R. L. (2002). The Statistical Analysis of Failure Time Data, 2nd ed., Wiley.
#' @export
Parasurv <- function(time, event, dist = "weibull") {
  # ML for a log-location-scale family with right censoring:
  #   prod f(t)^delta S(t)^(1-delta).
  # Censored observations enter through S, not f -- dropping them or
  # treating them as events biases the fit in opposite directions.  The
  # exponential is the Weibull with scale fixed at 1, so its loglik is
  # never higher; the LR test on 1 df of whether the hazard is really
  # constant is returned for the Weibull fit.
  if (!(dist %in% .ms_dists))
    stop(sprintf("unknown distribution '%s'; known: %s", dist,
                 paste(.ms_dists, collapse = ", ")))
  fit <- .ms_fit_lls(dist, time, event)
  fit$intercept <- fit$coef[1]
  if (dist %in% c("weibull", "exponential")) {
    fit$weibull_shape <- 1 / fit$scale
    fit$weibull_scale <- exp(fit$coef[1])
  }
  if (dist == "weibull") {
    ex <- .ms_fit_lls("exponential", time, event)
    lr <- 2 * (fit$loglik - ex$loglik)
    fit$lr_vs_exponential <- lr
    fit$lr_p_value <- stats::pchisq(max(lr, 0), 1, lower.tail = FALSE)
    fit$constant_hazard_rejected <- fit$lr_p_value < 0.05
  }
  fit$method <- paste("maximum likelihood for a log-location-scale family",
                      "with right censoring")
  fit
}

#' Accelerated failure time regression
#'
#' log T = x'beta + sigma W.
#' Coefficients act MULTIPLICATIVELY ON TIME rather than on the hazard, so exp(beta) is a time ratio and not a hazard ratio.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param X covariate matrix, one row per subject.
#' @param dist distribution of the error term.
#' @param alpha significance level for the intervals.
#' @return list with the coefficients, standard errors, time ratios, the scale and the log-likelihood.
#' @references Kalbfleisch, J. D. and Prentice, R. L. (2002). The Statistical Analysis of Failure Time Data, 2nd ed., Wiley, Chapter 3.
#' @export
Aftfit <- function(time, event, X, dist = "weibull",
                               alpha = 0.05) {
  # log T = x'beta + sigma W.  Coefficients act MULTIPLICATIVELY ON TIME:
  # exp(beta_j) is the factor by which a unit of x_j stretches survival,
  # so positive means LONGER life -- the opposite convention to a Cox
  # model, where positive means higher hazard and shorter life.  Reading
  # an AFT coefficient as a log hazard ratio flips every effect.  For the
  # Weibull family alone AFT and PH describe the same model, related by
  # beta_PH = -beta_AFT/sigma; that conversion is returned only there.
  if (!(dist %in% .ms_dists))
    stop(sprintf("unknown distribution '%s'; known: %s", dist,
                 paste(.ms_dists, collapse = ", ")))
  fit <- .ms_fit_lls(dist, time, event, X)
  b <- fit$coef
  fit$intercept <- b[1]
  fit$beta <- b[-1]
  fit$time_ratio <- exp(b[-1])
  fit$positive_coef_means_longer_survival <- TRUE
  if (dist %in% c("weibull", "exponential")) {
    fit$ph_coef <- -b[-1] / fit$scale
    fit$hazard_ratio <- exp(-b[-1] / fit$scale)
    fit$ph_equivalent <- TRUE
  } else {
    fit$ph_equivalent <- FALSE
  }
  fit$method <- "accelerated failure time, log T = x'beta + sigma W"
  fit
}

#' Compare parametric survival families by information criterion
#'
#' Each family is fitted by maximum likelihood and ranked by AIC, with BIC reported alongside so a disagreement between the two is visible rather than hidden.
#'
#' @param time observed follow-up times.
#' @param event event indicator, 1 = event, 0 = censored.
#' @param X optional covariate matrix.
#' @param dists families to compare; defaults to all supported.
#' @return list with one row per family giving the log-likelihood, AIC and BIC, ordered by AIC.
#' @references Kalbfleisch, J. D. and Prentice, R. L. (2002). The Statistical Analysis of Failure Time Data, 2nd ed., Wiley.
#' @export
Paracompare <- function(time, event, X = NULL,
                                              dists = NULL) {
  # Each family fitted by ML and ranked by AIC, with BIC alongside.  The
  # families are NOT nested (except exponential inside Weibull), so a
  # likelihood ratio test does not apply between most pairs -- AIC is the
  # comparison that does, and the one nested case is reported separately.
  # A family that fails is reported with its error, not dropped, so the
  # ranking cannot silently be over a subset.  The best AIC is not
  # evidence the winner FITS; check the Cox-Snell residuals first.
  nm <- if (is.null(dists)) .ms_dists else dists
  fits <- list(); errs <- list()
  for (d in nm) {
    r <- tryCatch(
      if (is.null(X)) Parasurv(time, event, dist = d)
      else Aftfit(time, event, X, dist = d),
      error = function(e) e)
    if (inherits(r, "error")) errs[[d]] <- conditionMessage(r)
    else fits[[d]] <- r
  }
  if (!length(fits))
    stop(sprintf("no family could be fitted: %s",
                 paste(unlist(errs), collapse = "; ")))
  tab <- do.call(rbind, lapply(names(fits), function(d) data.frame(
    dist = d, loglik = fits[[d]]$loglik, aic = fits[[d]]$aic,
    bic = fits[[d]]$bic, n_par = fits[[d]]$n_par,
    stringsAsFactors = FALSE)))
  tab <- tab[order(tab$aic), , drop = FALSE]
  out <- list(table = tab, best_aic = tab$dist[1],
              best_bic = tab$dist[which.min(tab$bic)], fits = fits,
              failed = errs, families_not_nested = TRUE,
              aic_is_not_goodness_of_fit = TRUE,
              method = paste("AIC / BIC comparison of parametric survival",
                             "families"))
  if (!is.null(fits$weibull) && !is.null(fits$exponential)) {
    lr <- 2 * (fits$weibull$loglik - fits$exponential$loglik)
    out$lr_weibull_vs_exponential <- lr
    out$lr_p_value <- stats::pchisq(max(lr, 0), 1, lower.tail = FALSE)
  }
  out
}

# Pre-policy spellings.  The naming policy makes the CamelCase name
# canonical -- no underscores, the namespace is the prefix -- and keeps
# the older spelling as an alias so existing code keeps working.

#' @rdname Aftfit
#' @export
morie_survival_aft <- Aftfit

#' @rdname Cif
#' @export
morie_survival_cif <- Cif

#' @rdname Paracompare
#' @export
morie_survival_compare_parametric <- Paracompare

#' @rdname Coxsnell
#' @export
morie_survival_coxsnell <- Coxsnell

#' @rdname Devresid
#' @export
morie_survival_deviance <- Devresid

#' @rdname Finegray
#' @export
morie_survival_finegray <- Finegray

#' @rdname Hazratio
#' @export
morie_survival_hr <- Hazratio

#' @rdname Landmark
#' @export
morie_survival_landmark <- Landmark

#' @rdname Ltkm
#' @export
morie_survival_left_truncated_km <- Ltkm

#' @rdname Martingale
#' @export
morie_survival_martingale <- Martingale

#' @rdname Parasurv
#' @export
morie_survival_parametric <- Parasurv

#' @rdname Rmst
#' @export
morie_survival_rmst <- Rmst

#' @rdname Rmstdiff
#' @export
morie_survival_rmst_diff <- Rmstdiff

#' @rdname Schoenfeld
#' @export
morie_survival_schoenfeld <- Schoenfeld

#' @rdname Turnbull
#' @export
morie_survival_turnbull <- Turnbull
