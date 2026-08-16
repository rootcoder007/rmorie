# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Cox proportional-hazards shelf. R mirrors of the morie.fn modules
# breslot, efrnt, coxbsk, coxmgr, coxres, coxstr, coxdfb, dlbcox and
# dvres, over one shared Newton-Raphson core -- the same arrangement as
# _surv.py, and for the same reason: a tie correction implemented twice
# is a tie correction that disagrees with itself.
#
# Ported from the Python at full precision, so the two languages agree
# to machine precision on every function here (all are deterministic).

#' .morie_cox_prepare
#'
#' A step of the cox_native implementation. Called by \code{.morie_aft_common}, \code{morie_breslow_tie_correction}, \code{morie_cause_specific_hazard} and 8 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param time See Usage.
#' @param event See Usage.
#' @param X Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{t}, \code{e}, \code{X}.
#' @export
.morie_cox_prepare <- function(time, event, X = NULL) {
  t <- as.numeric(time)
  e <- as.numeric(event)
  if (length(t) != length(e)) {
    stop(sprintf("time has %d entries but event has %d", length(t), length(e)),
      call. = FALSE
    )
  }
  if (length(t) == 0L) stop("time must be non-empty", call. = FALSE)
  if (any(t < 0)) stop("time must be non-negative", call. = FALSE)
  if (!all(e == 0 | e == 1)) {
    stop("event must be 0 (censored) or 1 (event)", call. = FALSE)
  }
  if (is.null(X)) {
    return(list(t = t, e = e, X = NULL))
  }
  Xm <- as.matrix(X)
  storage.mode(Xm) <- "double"
  if (nrow(Xm) != length(t)) Xm <- t(Xm)
  if (nrow(Xm) != length(t)) {
    stop(sprintf("X has %d rows but time has %d", nrow(Xm), length(t)),
      call. = FALSE
    )
  }
  list(t = t, e = e, X = Xm)
}

# Score and information at a FIXED beta. Shared by the fitter and by
# the stratified model, which needs the pieces without taking a step.
#' Score and information at a FIXED beta. Shared by the fitter and by
#'
#' the stratified model, which needs the pieces without taking a step.
#'
#' @param ts A vector; indexed elementwise.
#' @param es See Usage.
#' @param Xs A matrix; indexed by row and column.
#' @param beta A matrix; passed to \code{\%*\%}.
#' @param offs Numeric; combined arithmetically in the body.
#' @param ties See Usage.
#' @return A list with \code{loglik}, \code{U}, \code{I}.
#' @export
.morie_cox_score <- function(ts, es, Xs, beta, offs, ties) {
  p <- ncol(Xs)
  eta <- pmax(pmin(as.vector(Xs %*% beta) + offs, 500), -500)
  w <- exp(eta)
  ll <- 0
  U <- numeric(p)
  I <- matrix(0, p, p)
  for (ut in unique(sort(ts[es == 1]))) {
    at_risk <- ts >= ut
    died <- at_risk & ts == ut & es == 1
    d <- sum(died)
    if (d == 0L) next
    wr <- w[at_risk]
    Xr <- Xs[at_risk, , drop = FALSE]
    wd <- w[died]
    Xd <- Xs[died, , drop = FALSE]
    S0r <- sum(wr)
    S1r <- as.vector(wr %*% Xr)
    S2r <- t(Xr * wr) %*% Xr
    S0d <- sum(wd)
    S1d <- as.vector(wd %*% Xd)
    S2d <- t(Xd * wd) %*% Xd
    ll <- ll + sum(eta[died])
    U <- U + colSums(Xd)
    if (identical(ties, "breslow") || d == 1L) {
      ll <- ll - d * log(S0r)
      mu <- S1r / S0r
      U <- U - d * mu
      I <- I + d * (S2r / S0r - outer(mu, mu))
    } else if (identical(ties, "efron")) {
      for (l in seq_len(d) - 1L) {
        f <- l / d
        S0 <- S0r - f * S0d
        S1 <- S1r - f * S1d
        S2 <- S2r - f * S2d
        ll <- ll - log(S0)
        mu <- S1 / S0
        U <- U - mu
        I <- I + S2 / S0 - outer(mu, mu)
      }
    } else {
      stop('ties must be "breslow" or "efron"', call. = FALSE)
    }
  }
  list(loglik = ll, U = U, I = I)
}

#' .morie_cox_fit
#'
#' A step of the cox_native implementation. Called by \code{morie_breslow_tie_correction}, \code{morie_cause_specific_hazard}, \code{morie_cox_breslow_step} and 4 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param e A vector; indexed elementwise.
#' @param X A matrix; indexed by row and column.
#' @param ties Passed to \code{.morie_cox_score}. Defaults to \code{"efron"}.
#' @param max_iter A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.
#' @param tol Defaults to \code{1e-09}.
#' @param offset Defaults to \code{NULL}.
#' @return A list with \code{beta}, \code{loglik}, \code{I}, \code{U}, \code{n_iter}, \code{converged}.
#' @export
.morie_cox_fit <- function(t, e, X, ties = "efron", max_iter = 50L,
                           tol = 1e-9, offset = NULL) {
  p <- ncol(X)
  beta <- numeric(p)
  off <- if (is.null(offset)) numeric(length(t)) else as.numeric(offset)
  ord <- order(t)
  ts <- t[ord]
  es <- e[ord]
  Xs <- X[ord, , drop = FALSE]
  offs <- off[ord]
  loglik <- -Inf
  converged <- FALSE
  it <- 0L
  for (it in seq_len(max_iter)) {
    sc <- .morie_cox_score(ts, es, Xs, beta, offs, ties)
    step <- tryCatch(solve(sc$I, sc$U),
      error = function(err) .morie_ginv(sc$I) %*% sc$U
    )
    step <- as.vector(step)
    beta <- beta + step
    loglik <- sc$loglik
    if (max(abs(step)) < tol) {
      converged <- TRUE
      break
    }
  }
  # The information returned is the one evaluated at the beta BEFORE the
  # final step, matching _surv.py exactly. Re-evaluating at the stepped
  # beta would be marginally more correct and would break parity, so the
  # two languages stay wrong together rather than disagreeing.
  list(
    beta = beta, loglik = loglik, I = sc$I, U = sc$U,
    n_iter = as.integer(it), converged = converged
  )
}

#' .morie_cox_baseline
#'
#' A step of the cox_native implementation. Called by \code{morie_cox_breslow_step}, \code{morie_cox_frailty}, \code{morie_cox_martingale_residuals} and 1 others in the module.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param e See Usage.
#' @param X A matrix; passed to \code{\%*\%}.
#' @param beta A matrix; passed to \code{\%*\%}.
#' @param offset Defaults to \code{NULL}.
#' @return A list with \code{times}, \code{hazard}, \code{cumhazard}.
#' @export
.morie_cox_baseline <- function(t, e, X, beta, offset = NULL) {
  off <- if (is.null(offset)) numeric(length(t)) else as.numeric(offset)
  w <- exp(pmax(pmin(as.vector(X %*% beta) + off, 500), -500))
  utimes <- unique(sort(t[e == 1]))
  dH <- numeric(length(utimes))
  for (i in seq_along(utimes)) {
    ut <- utimes[i]
    d <- sum(t == ut & e == 1)
    dH[i] <- d / max(sum(w[t >= ut]), 1e-300)
  }
  list(times = utimes, hazard = dH, cumhazard = cumsum(dH))
}

#' .morie_km_estimate
#'
#' A step of the cox_native implementation. Called by \code{morie_censoring_at_risk_weight}, \code{morie_competing_risks_fg}, \code{morie_cox_schoenfeld_residuals}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; indexed elementwise.
#' @param e See Usage.
#' @return A list with \code{times}, \code{survival}.
#' @export
.morie_km_estimate <- function(t, e) {
  utimes <- unique(sort(t[e == 1]))
  surv <- numeric(length(utimes))
  s <- 1
  for (i in seq_along(utimes)) {
    ut <- utimes[i]
    n_risk <- sum(t >= ut)
    d <- sum(t == ut & e == 1)
    s <- s * (1 - d / max(n_risk, 1))
    surv[i] <- s
  }
  list(times = utimes, survival = surv)
}

#' .morie_cox_result
#'
#' A step of the cox_native implementation. Called by \code{morie_breslow_tie_correction}, \code{morie_efron_tie_correction}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken and its elements indexed.
#' @param e Numeric; passed to \code{sum}.
#' @param X See Usage.
#' @param fit A list; the body reads \code{$beta}, \code{$converged}, \code{$I}, \code{$loglik}, \code{$n_iter} from it.
#' @param label Character; passed to \code{tolower}.
#' @param method See Usage.
#' @return A list with \code{beta}, \code{se}, \code{z}, \code{p_value}, \code{hazard_ratio}, \code{loglik}, \code{cov}, \code{information}, \code{n_ties}, \code{n_events}, \code{n}, \code{n_iter}, \code{converged}, \code{ties}, \code{time}, \code{event}, \code{X}, \code{method}.
#' @export
.morie_cox_result <- function(t, e, X, fit, label, method) {
  I <- fit$I
  cov <- tryCatch(solve(I), error = function(err) NULL)
  se <- if (is.null(cov)) {
    rep(NA_real_, length(fit$beta))
  } else {
    sqrt(pmax(diag(cov), 0))
  }
  z <- fit$beta / se
  ev <- t[e == 1]
  n_ties <- length(ev) - length(unique(ev))
  list(
    beta = fit$beta, se = se, z = z,
    p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    hazard_ratio = exp(fit$beta), loglik = fit$loglik, cov = cov,
    information = I, n_ties = as.integer(n_ties),
    n_events = as.integer(sum(e)), n = length(t),
    n_iter = fit$n_iter, converged = fit$converged,
    ties = tolower(label), time = t, event = e, X = X,
    method = method
  )
}


#' Cox model with the Breslow tie correction
#'
#' The Breslow approximation treats every tied event as facing the full
#' risk set, which over-counts it: with many ties the partial likelihood
#' is too flat and beta is attenuated toward zero. It is the cheap
#' option and the default in some software; \code{\link{morie_efron_tie_correction}}
#' costs almost nothing more and is closer to the exact discrete
#' likelihood.
#'
#' @param time follow-up times, non-negative.
#' @param event 1 for an event, 0 for right-censored.
#' @param X covariate matrix, one row per subject.
#' @param ... passed to the fitter (\code{max_iter}, \code{tol},
#'   \code{offset}).
#' @return list with \code{beta}, \code{se}, \code{z}, \code{p_value},
#'   \code{hazard_ratio}, \code{loglik}, \code{information}, and the
#'   data needed by the residual functions.
#' @references Breslow, N. (1974). Covariance analysis of censored
#'   survival data. \emph{Biometrics}, 30(1), 89-99.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(120), ncol = 2)
#' tt <- rexp(60, exp(X %*% c(0.7, -0.4)))
#' morie_breslow_tie_correction(tt, rep(1, 60), X)$hazard_ratio
#' @export
morie_breslow_tie_correction <- function(time, event, X, ...) {
  d <- .morie_cox_prepare(time, event, X)
  fit <- .morie_cox_fit(d$t, d$e, d$X, ties = "breslow", ...)
  .morie_cox_result(d$t, d$e, d$X, fit, "Breslow", "breslow_tie_correction")
}


#' Cox model with the Efron tie correction
#'
#' Efron's l/d weighting shrinks the risk set progressively across the
#' d tied events, which approximates the exact discrete likelihood at
#' essentially Breslow's cost. With no ties the two coincide exactly --
#' a useful test, and one the parity suite asserts.
#'
#' @inheritParams morie_breslow_tie_correction
#' @return as \code{\link{morie_breslow_tie_correction}}.
#' @references Efron, B. (1977). The efficiency of Cox's likelihood
#'   function for censored data. \emph{JASA}, 72(359), 557-565.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(120), ncol = 2)
#' tt <- round(rexp(60, exp(X %*% c(0.7, -0.4))), 1) # ties on purpose
#' morie_efron_tie_correction(tt, rep(1, 60), X)$beta
#' @export
morie_efron_tie_correction <- function(time, event, X, ...) {
  d <- .morie_cox_prepare(time, event, X)
  fit <- .morie_cox_fit(d$t, d$e, d$X, ties = "efron", ...)
  .morie_cox_result(d$t, d$e, d$X, fit, "Efron", "efron_tie_correction")
}


#' Breslow estimator of the baseline cumulative hazard
#'
#' The Cox partial likelihood throws the baseline away; this puts it
#' back, as a step function jumping at each event time by
#' \eqn{d_j / \sum_{i \in R_j} \exp(x_i'\beta)}.
#'
#' "Baseline" means x = 0, which for uncentred covariates is a subject
#' who does not exist -- centre the covariates or the curve is not
#' interpretable. The curve is also undefined past the last event time;
#' it is drawn flat there by convention, not by evidence.
#'
#' @inheritParams morie_breslow_tie_correction
#' @param beta coefficient vector. Fitted internally when NULL.
#' @param ties tie correction used for the internal fit.
#' @return list with \code{times}, \code{hazard}, \code{cumhazard},
#'   \code{survival}.
#' @references Breslow, N. (1972). Discussion of Professor Cox's paper.
#'   \emph{JRSS-B}, 34(2), 216-217.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), ncol = 1)
#' tt <- rexp(60, exp(X * 0.5))
#' tail(morie_cox_breslow_step(tt, rep(1, 60), X)$cumhazard, 1)
#' @export
morie_cox_breslow_step <- function(time, event, X, beta = NULL,
                                   ties = "efron") {
  d <- .morie_cox_prepare(time, event, X)
  if (is.null(beta)) beta <- .morie_cox_fit(d$t, d$e, d$X, ties = ties)$beta
  beta <- as.numeric(beta)
  if (length(beta) != ncol(d$X)) {
    stop(sprintf(
      "beta has %d entries but X has %d columns",
      length(beta), ncol(d$X)
    ), call. = FALSE)
  }
  bh <- .morie_cox_baseline(d$t, d$e, d$X, beta)
  list(
    times = bh$times, hazard = bh$hazard, cumhazard = bh$cumhazard,
    survival = exp(-bh$cumhazard), beta = beta, n = length(d$t),
    tail_caveat = paste(
      "the baseline is undefined beyond the last event",
      "time; the curve is flat there by convention,",
      "not by evidence"
    ),
    method = "cox_breslow_step"
  )
}


#' Cox martingale residuals
#'
#' \eqn{M_i = \delta_i - \exp(x_i'\beta)\hat H_0(t_i)}: observed events
#' minus expected. Bounded above by 1 and unbounded below, hence
#' strongly left-skewed -- do not read them as if they were normal.
#' Their use is functional form: plot them against a covariate not in
#' the model, and curvature tells you the scale it belongs on.
#'
#' @param fit a fit from \code{\link{morie_efron_tie_correction}} or
#'   \code{\link{morie_breslow_tie_correction}}.
#' @return list with \code{residuals}, \code{expected}, \code{event},
#'   \code{cumhazard}, \code{times}.
#' @references Therneau, T. M., Grambsch, P. M. and Fleming, T. R.
#'   (1990). Martingale-based residuals for survival models.
#'   \emph{Biometrika}, 77(1), 147-160.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), ncol = 1)
#' tt <- rexp(60, exp(X * 0.5))
#' f <- morie_efron_tie_correction(tt, rep(1, 60), X)
#' round(mean(morie_cox_martingale_residuals(f)$residuals), 8)
#' @export
morie_cox_martingale_residuals <- function(fit) {
  for (k in c("time", "event", "X", "beta")) {
    if (is.null(fit[[k]])) {
      stop(sprintf(paste(
        "fit is missing '%s'; pass a result from",
        "morie_efron_tie_correction or",
        "morie_breslow_tie_correction"
      ), k), call. = FALSE)
    }
  }
  t <- as.numeric(fit$time)
  e <- as.numeric(fit$event)
  X <- as.matrix(fit$X)
  beta <- as.numeric(fit$beta)
  bh <- .morie_cox_baseline(t, e, X, beta)
  w <- exp(pmax(pmin(as.vector(X %*% beta), 500), -500))
  # findInterval gives the count of event times <= t, i.e. numpy's
  # searchsorted(side = "right"); 0 means "before the first event".
  idx <- findInterval(t, bh$times)
  H_at <- ifelse(idx >= 1L, bh$cumhazard[pmax(idx, 1L)], 0)
  expected <- w * H_at
  resid <- e - expected
  list(
    residuals = resid, expected = expected, event = e,
    mean = mean(resid), cumhazard = bh$cumhazard, times = bh$times,
    method = "cox_martingale_residuals"
  )
}


#' Cox deviance residuals
#'
#' The martingale residuals symmetrised:
#' \eqn{d_i = \mathrm{sign}(M_i)\sqrt{-2[M_i + \delta_i\log(\delta_i - M_i)]}},
#' which is roughly standard normal under a correct model.
#'
#' Symmetry degrades under heavy censoring -- a mass of small negative
#' residuals is what censoring looks like, not lack of fit.
#'
#' @param fit a Cox fit, as for \code{\link{morie_cox_martingale_residuals}}.
#' @return list with \code{residuals}, \code{martingale},
#'   \code{n_extreme} (|d| > 2.5), \code{mean}, \code{sd}.
#' @references Therneau, T. M. and Grambsch, P. M. (2000).
#'   \emph{Modeling Survival Data}, Sec. 4.5. Springer.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), ncol = 1)
#' tt <- rexp(60, exp(X * 0.5))
#' f <- morie_efron_tie_correction(tt, rep(1, 60), X)
#' round(sd(morie_deviance_residual_cox(f)$residuals), 3)
#' @export
morie_deviance_residual_cox <- function(fit) {
  m <- morie_cox_martingale_residuals(fit)
  M <- m$residuals
  e <- m$event
  inner <- -2 * (M + ifelse(e > 0, e * log(pmax(e - M, 1e-300)), 0))
  d <- sign(M) * sqrt(pmax(inner, 0))
  list(
    residuals = d, martingale = M,
    n_extreme = as.integer(sum(abs(d) > 2.5)),
    mean = mean(d), sd = if (length(d) > 1L) stats::sd(d) else NA_real_,
    censoring_caveat = paste(
      "symmetry degrades under heavy censoring; a",
      "mass of small negative residuals is expected",
      "there, not lack of fit"
    ),
    method = "deviance_residual_cox"
  )
}


#' Schoenfeld residuals and the proportional-hazards test
#'
#' One residual per EVENT (not per subject): the covariate value of the
#' subject who failed, minus the risk-weighted mean over the risk set.
#' Under proportional hazards they have no trend in time, so the
#' correlation with transformed time is a direct test of PH.
#'
#' A significant trend means the hazard ratio moves over follow-up. The
#' response is to stratify on the offending covariate or model it with a
#' time interaction -- not to drop it, which changes the question rather
#' than answering it.
#'
#' @param fit a Cox fit, as for \code{\link{morie_cox_martingale_residuals}}.
#' @param transform \code{"km"} (1 - Kaplan-Meier, the default),
#'   \code{"rank"} or \code{"identity"}.
#' @return list with \code{residuals} (events x p), \code{times},
#'   \code{correlation} and \code{p_value} per covariate.
#' @references Grambsch, P. M. and Therneau, T. M. (1994). Proportional
#'   hazards tests and diagnostics based on weighted residuals.
#'   \emph{Biometrika}, 81(3), 515-526.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(80), ncol = 2)
#' tt <- rexp(40, exp(X %*% c(0.6, 0)))
#' f <- morie_efron_tie_correction(tt, rep(1, 40), X)
#' round(morie_cox_schoenfeld_residuals(f)$p_value, 3)
#' @export
morie_cox_schoenfeld_residuals <- function(fit, transform = c(
                                             "km", "rank",
                                             "identity"
                                           )) {
  transform <- match.arg(transform)
  t <- as.numeric(fit$time)
  e <- as.numeric(fit$event)
  X <- as.matrix(fit$X)
  beta <- as.numeric(fit$beta)
  w <- exp(pmax(pmin(as.vector(X %*% beta), 500), -500))
  ev_idx <- which(e == 1)
  ev_idx <- ev_idx[order(t[ev_idx])]
  p <- ncol(X)
  res <- matrix(0, length(ev_idx), p)
  for (r_i in seq_along(ev_idx)) {
    i <- ev_idx[r_i]
    at_risk <- t >= t[i]
    wr <- w[at_risk]
    res[r_i, ] <- X[i, ] - as.vector(wr %*% X[at_risk, , drop = FALSE]) / sum(wr)
  }
  times <- t[ev_idx]
  g <- switch(transform,
    km = {
      km <- .morie_km_estimate(t, e)
      idx <- pmin(pmax(findInterval(times, km$times), 1L), length(km$survival))
      1 - km$survival[idx]
    },
    rank = as.numeric(order(order(times)) - 1L),
    identity = times
  )
  corr <- rep(NA_real_, p)
  pval <- rep(NA_real_, p)
  if (nrow(res) > 2L && diff(range(g)) > 0) {
    for (j in seq_len(p)) {
      if (sqrt(mean((res[, j] - mean(res[, j]))^2)) == 0) next
      cc <- stats::cor(g, res[, j])
      corr[j] <- cc
      zst <- cc * sqrt(max(nrow(res) - 2L, 1L) / max(1 - cc^2, 1e-12))
      pval[j] <- 2 * stats::pnorm(abs(zst), lower.tail = FALSE)
    }
  }
  list(
    residuals = res, times = times, correlation = corr, p_value = pval,
    transform = transform, method = "cox_schoenfeld_residuals"
  )
}


#' Stratified Cox model
#'
#' A separate, unspecified baseline hazard per stratum with one shared
#' coefficient vector: the strata contribute their own partial
#' likelihoods, which are summed.
#'
#' The stratifier gets no coefficient and no hazard ratio -- that is the
#' price of not assuming proportional hazards for it. Stratify on the
#' nuisance variable, never on the exposure you are trying to estimate.
#' Strata with no events contribute nothing at all and are reported.
#'
#' @inheritParams morie_breslow_tie_correction
#' @param stratum stratum label per subject.
#' @param ties \code{"efron"} or \code{"breslow"}.
#' @param max_iter,tol Newton-Raphson controls.
#' @return list with \code{beta}, \code{se}, \code{z}, \code{p_value},
#'   \code{hazard_ratio}, \code{loglik}, \code{strata},
#'   \code{events_per_stratum}, \code{empty_strata}.
#' @references Kalbfleisch, J. D. and Prentice, R. L. (2002). \emph{The
#'   Statistical Analysis of Failure Time Data}, 2nd ed., Sec. 4.4.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(80), ncol = 1)
#' s <- rep(1:2, each = 40)
#' tt <- rexp(80, exp(X * 0.5) * s)
#' morie_cox_stratified(tt, rep(1, 80), X, s)$beta
#' @export
morie_cox_stratified <- function(time, event, X, stratum, ties = "efron",
                                 max_iter = 50L, tol = 1e-9) {
  d <- .morie_cox_prepare(time, event, X)
  st <- as.vector(stratum)
  if (length(st) != length(d$t)) {
    stop(sprintf(
      "stratum has %d entries but time has %d",
      length(st), length(d$t)
    ), call. = FALSE)
  }
  levels_ <- unique(sort(st))
  p <- ncol(d$X)
  beta <- numeric(p)
  empty <- levels_[vapply(
    levels_, function(lv) sum(d$e[st == lv]) == 0,
    logical(1)
  )]
  converged <- FALSE
  it <- 0L
  ll_total <- 0
  I_total <- matrix(0, p, p)
  for (it in seq_len(max_iter)) {
    U <- numeric(p)
    I_total <- matrix(0, p, p)
    ll_total <- 0
    for (lv in levels_) {
      m <- st == lv
      if (sum(d$e[m]) == 0) next
      ts <- d$t[m]
      ord <- order(ts)
      sc <- .morie_cox_score(
        ts[ord], d$e[m][ord],
        d$X[m, , drop = FALSE][ord, , drop = FALSE],
        beta, numeric(sum(m)), ties
      )
      U <- U + sc$U
      I_total <- I_total + sc$I
      ll_total <- ll_total + sc$loglik
    }
    step <- as.vector(tryCatch(solve(I_total, U),
      error = function(err) .morie_ginv(I_total) %*% U
    ))
    beta <- beta + step
    if (max(abs(step)) < tol) {
      converged <- TRUE
      break
    }
  }
  se <- tryCatch(sqrt(pmax(diag(solve(I_total)), 0)),
    error = function(err) rep(NA_real_, p)
  )
  z <- beta / se
  list(
    beta = beta, se = se, z = z,
    p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    hazard_ratio = exp(beta), loglik = ll_total, information = I_total,
    strata = levels_,
    events_per_stratum = vapply(
      levels_,
      function(lv) as.integer(sum(d$e[st == lv])),
      integer(1)
    ),
    empty_strata = empty, n = length(d$t), n_iter = as.integer(it),
    converged = converged,
    stratifier_caveat = paste(
      "a stratification variable has no coefficient",
      "and no hazard ratio; stratify on the",
      "nuisance, never on the exposure"
    ),
    method = "cox_stratified"
  )
}


#' Cox DFBETA influence diagnostics
#'
#' The approximate change in each coefficient from deleting each
#' subject, \eqn{L_i I^{-1}} where \eqn{L_i} is the score residual.
#' \code{dfbetas} rescales by the coefficient's own standard error, so a
#' value near 1 means that one subject moves beta by a full standard
#' error.
#'
#' Influence is not outlyingness: a subject with an extreme covariate in
#' a well-populated region may have none, and an ordinary-looking
#' subject at the end of follow-up may have a great deal. Read
#' \code{dfbetas}, not \code{dfbeta}.
#'
#' @param fit a Cox fit carrying \code{information} and \code{se}.
#' @return list with \code{dfbeta}, \code{dfbetas},
#'   \code{score_residuals}, \code{max_influence},
#'   \code{most_influential} (0-based, to match the Python module).
#' @references Cain, K. C. and Lange, N. T. (1984). Approximate case
#'   influence for the proportional hazards regression model with
#'   censored data. \emph{Biometrics}, 40(2), 493-499.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), ncol = 1)
#' tt <- rexp(60, exp(X * 0.5))
#' f <- morie_efron_tie_correction(tt, rep(1, 60), X)
#' morie_cox_dfbeta_influence(f)$most_influential
#' @export
morie_cox_dfbeta_influence <- function(fit) {
  t <- as.numeric(fit$time)
  e <- as.numeric(fit$event)
  X <- as.matrix(fit$X)
  beta <- as.numeric(fit$beta)
  I <- as.matrix(fit$information)
  se <- as.numeric(fit$se)
  n <- nrow(X)
  p <- ncol(X)
  w <- exp(pmax(pmin(as.vector(X %*% beta), 500), -500))
  L <- matrix(0, n, p)
  for (ut in unique(sort(t[e == 1]))) {
    at_risk <- t >= ut
    died <- at_risk & t == ut & e == 1
    wr <- w[at_risk]
    S0 <- sum(wr)
    Xr <- X[at_risk, , drop = FALSE]
    mu <- as.vector(wr %*% Xr) / S0
    d <- sum(died)
    L[died, ] <- L[died, , drop = FALSE] +
      sweep(X[died, , drop = FALSE], 2L, mu, "-")
    L[at_risk, ] <- L[at_risk, , drop = FALSE] -
      d * (sweep(Xr, 2L, mu, "-") * wr) / S0
  }
  Iinv <- tryCatch(solve(I), error = function(err) .morie_ginv(I))
  dfbeta <- L %*% Iinv
  dfbetas <- sweep(dfbeta, 2L, ifelse(se > 0, se, NA_real_), "/")
  worst <- which.max(apply(abs(dfbetas), 1L, max))
  list(
    dfbeta = dfbeta, dfbetas = dfbetas, score_residuals = L,
    max_influence = max(abs(dfbetas), na.rm = TRUE),
    most_influential = as.integer(worst - 1L),
    method = "cox_dfbeta_influence"
  )
}


#' Fit a Cox model and return its DFBETA diagnostics
#'
#' Convenience over \code{\link{morie_cox_dfbeta_influence}} for when
#' there is no fit in hand yet. The arithmetic is identical.
#'
#' @inheritParams morie_breslow_tie_correction
#' @param ties \code{"efron"} or \code{"breslow"}.
#' @return list with \code{dfbeta}, \code{dfbetas},
#'   \code{score_residuals}, \code{beta}, \code{se}.
#' @references Cain, K. C. and Lange, N. T. (1984). Approximate case
#'   influence for the proportional hazards regression model with
#'   censored data. \emph{Biometrics}, 40(2), 493-499.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), ncol = 1)
#' tt <- rexp(60, exp(X * 0.5))
#' dim(morie_dfbeta_cox(tt, rep(1, 60), X)$dfbeta)
#' @export
morie_dfbeta_cox <- function(time, event, X, ties = c("efron", "breslow")) {
  ties <- match.arg(ties)
  fit <- if (identical(ties, "efron")) {
    morie_efron_tie_correction(time, event, X)
  } else {
    morie_breslow_tie_correction(time, event, X)
  }
  inf <- morie_cox_dfbeta_influence(fit)
  list(
    dfbeta = inf$dfbeta, dfbetas = inf$dfbetas,
    score_residuals = inf$score_residuals, beta = fit$beta, se = fit$se,
    max_influence = inf$max_influence,
    most_influential = inf$most_influential, method = "dfbeta_cox"
  )
}
