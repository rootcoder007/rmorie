# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Survival shelf remainder. R mirrors of the morie.fn modules depcen,
# chrwgt, gamfr, ggmaft and coxtmv, over the Cox core in cox_native.R
# and the frailty fitter in competing_risks_native.R.

#' Dependent-censoring diagnostic
#'
#' Fits a Cox model with CENSORING as the outcome. Covariates that
#' predict censoring are covariates whose censoring is informative, and
#' the fitted coefficients say which ones and how strongly.
#'
#' What no diagnostic can do is detect censoring that depends on the
#' unobserved event time itself -- the quantity being censored is by
#' definition not there to correlate with. A null result here is
#' therefore not evidence of independent censoring; it only rules out
#' the dependence that runs through the measured covariates.
#'
#' @param time follow-up times.
#' @param event 1 for an event, 0 for censored.
#' @param X covariate matrix.
#' @param ties tie correction.
#' @return list with \code{beta_censoring}, \code{se}, \code{p_value},
#'   \code{beta_event}, \code{dependent}, \code{n_flagged}.
#' @references Robins, J. M. and Finkelstein, D. M. (2000). Correcting
#'   for noncompliance and dependent censoring. \emph{Biometrics},
#'   56(3), 779-788.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(120), ncol = 2)
#' tt <- rexp(60); ev <- rbinom(60, 1, 0.6)
#' morie_dependent_censoring_hazard(tt, ev, X)$n_flagged
#' @export
morie_dependent_censoring_hazard <- function(time, event, X, ties = "efron") {
  d <- .morie_cox_prepare(time, event, X)
  fit_ev <- .morie_cox_fit(d$t, d$e, d$X, ties = ties)
  cen <- 1 - d$e
  if (sum(cen) == 0) {
    stop("there are no censored observations to model", call. = FALSE)
  }
  fit_c <- .morie_cox_fit(d$t, cen, d$X, ties = ties)
  se <- tryCatch(sqrt(pmax(diag(solve(fit_c$I)), 0)),
                 error = function(e) rep(NA_real_, length(fit_c$beta)))
  z <- fit_c$beta / se
  p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
  flagged <- as.integer(sum(p < 0.05, na.rm = TRUE))
  list(beta_censoring = fit_c$beta, se = se, z = z, p_value = p,
       beta_event = fit_ev$beta, dependent = flagged > 0L,
       n_flagged = flagged, n_censored = as.integer(sum(cen)),
       converged = fit_c$converged,
       warnings = paste("censoring that depends on the unobserved event time",
                        "is undetectable by any diagnostic; a null result here",
                        "does not establish independent censoring"),
       method = "dependent_censoring_hazard")
}


#' Inverse-probability-of-censoring weights
#'
#' \eqn{w_i = 1/\hat G(t_i)} for the uncensored, where G is the
#' Kaplan-Meier estimate of the CENSORING distribution. Each surviving
#' subject stands in for the ones like them who were censored, which is
#' what restores the original population.
#'
#' The weights explode as the censoring survivor approaches zero at long
#' follow-up, so a handful of late survivors can end up carrying the
#' estimate. The fix is to truncate the time axis at a point where G is
#' still comfortably positive, not to weight through administrative
#' censoring and hope.
#'
#' @param time follow-up times.
#' @param censor 1 if censored, 0 if the event was observed.
#' @param at optional single time at which to evaluate G, instead of
#'   each subject's own time.
#' @param stabilize multiply by the mean of G.
#' @return list with \code{weights}, \code{G}, \code{ess},
#'   \code{max_weight_share}.
#' @references Robins, J. M. and Rotnitzky, A. (1992). Recovery of
#'   information and adjustment for dependent censoring using surrogate
#'   markers. In \emph{AIDS Epidemiology}, 297-331. Birkhauser.
#' @examples
#' set.seed(1)
#' tt <- rexp(100); cc <- rbinom(100, 1, 0.3)
#' round(morie_censoring_at_risk_weight(tt, cc)$ess, 1)
#' @export
morie_censoring_at_risk_weight <- function(time, censor, at = NULL,
                                           stabilize = TRUE) {
  t <- as.numeric(time)
  c_ <- as.numeric(censor)
  if (length(t) != length(c_)) {
    stop(sprintf("time has %d entries but censor has %d", length(t),
                 length(c_)), call. = FALSE)
  }
  if (!all(c_ == 0 | c_ == 1)) stop("censor must be 0/1", call. = FALSE)
  km <- .morie_km_estimate(t, c_)
  ct <- km$times
  csurv <- km$survival
  Gfun <- function(u) {
    u <- as.numeric(u)
    if (length(ct) == 0L) return(rep(1, length(u)))
    pos <- findInterval(u, ct)
    ifelse(pos >= 1L, csurv[pmin(pmax(pos, 1L), length(csurv))], 1)
  }
  eval_t <- if (is.null(at)) t else rep(as.numeric(at), length(t))
  g <- pmax(Gfun(eval_t), 1e-8)
  w <- ifelse(c_ == 0, 1 / g, 0)
  if (stabilize) w <- w * mean(Gfun(eval_t))
  tot <- sum(w)
  share <- if (tot > 0) max(w) / tot else NA_real_
  ess <- tot^2 / max(sum(w^2), 1e-300)
  list(weights = w, G = g, max_weight_share = share, ess = ess,
       n_censored = as.integer(sum(c_)), n = length(t),
       warnings = c(paste("weights explode as the censoring survivor",
                          "approaches zero at long follow-up; truncate the",
                          "time axis rather than weighting through",
                          "administrative censoring"),
                    if (is.finite(share) && share > 0.1) {
                      sprintf("one subject carries %.1f%% of the weight",
                              100 * share)
                    }),
       method = "censoring_at_risk_weight")
}


#' Gamma-frailty Cox model
#'
#' \code{\link{morie_cox_frailty}} under its statistical name. The gamma
#' is conjugate to the Poisson likelihood the Cox model implies, which
#' is why the frailty update is closed-form rather than an inner
#' optimisation.
#'
#' The consequence worth remembering is that the MARGINAL hazard ratio
#' -- what you get from a model that ignores the clustering -- is
#' attenuated toward 1 relative to the conditional one estimated here.
#' The two are answers to different questions, not an estimate and a
#' better estimate.
#'
#' @inheritParams morie_cox_frailty
#' @param ... passed to \code{\link{morie_cox_frailty}}.
#' @return list as \code{\link{morie_cox_frailty}} plus
#'   \code{marginal_attenuation}.
#' @references Clayton, D. G. (1978). A model for association in
#'   bivariate life tables. \emph{Biometrika}, 65(1), 141-151.
#' @examples
#' set.seed(4)
#' X <- matrix(rnorm(120), ncol = 1)
#' cl <- rep(1:20, each = 6)
#' tt <- rexp(120, exp(X * 0.5))
#' round(morie_gamma_frailty_cox(tt, rep(1, 120), X, cl)$theta, 3)
#' @export
morie_gamma_frailty_cox <- function(time, event, X, cluster, ...) {
  r <- morie_cox_frailty(time, event, X, cluster, ...)
  r$marginal_attenuation <- TRUE
  r$method <- "gamma_frailty_cox"
  r
}


#' Generalized gamma AFT model
#'
#' The three-parameter family that NESTS the Weibull (q = 1), the
#' log-normal (q = 0) and the exponential, so choosing among them is a
#' likelihood-ratio test rather than a judgement call.
#'
#' The catch is that q is weakly identified: it is estimated almost
#' entirely from tail behaviour, and with few events the test has very
#' little power. A family selected this way on a small dataset is a
#' coin-flip dressed as a test, which is why both LR statistics and both
#' p-values are returned rather than just the winner.
#'
#' @param time follow-up times, strictly positive.
#' @param event 1 for an event, 0 for censored.
#' @param X covariate matrix.
#' @param max_iter,tol optimiser controls.
#' @return list with \code{beta}, \code{sigma}, \code{q}, \code{loglik},
#'   \code{lr_vs_weibull}, \code{p_vs_weibull}, \code{lr_vs_lognormal},
#'   \code{p_vs_lognormal}, \code{preferred}.
#' @references Prentice, R. L. (1974). A log gamma model and its maximum
#'   likelihood estimation. \emph{Biometrika}, 61(3), 539-544.
#' @examples
#' set.seed(5)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- exp(1 + X %*% c(0.5, -0.3) + 0.6 * rnorm(100))
#' morie_generalized_gamma_aft(tt, rep(1, 100), X)$preferred
#' @export
morie_generalized_gamma_aft <- function(time, event, X, max_iter = 500L,
                                        tol = 1e-6) {
  d <- .morie_cox_prepare(time, event, X)
  if (any(d$t <= 0)) {
    stop("AFT models need strictly positive times", call. = FALSE)
  }
  A <- cbind(1, d$X)
  p <- ncol(A)
  logt <- log(d$t)
  e <- d$e
  nll <- function(theta) {
    b <- theta[seq_len(p)]
    ls <- theta[p + 1L]
    q <- theta[p + 2L]
    sigma <- exp(pmax(pmin(ls, 20), -20))
    z <- (logt - as.vector(A %*% b)) / sigma
    if (abs(q) < 1e-6) {
      ld <- stats::dnorm(z, log = TRUE)
      lsv <- stats::pnorm(z, lower.tail = FALSE, log.p = TRUE)
    } else {
      qi <- 1 / q^2
      w <- q * z
      u <- qi * exp(pmax(pmin(w, 500), -500))
      ld <- log(abs(q)) - lgamma(qi) + qi * log(qi) + qi * w - u
      sf <- if (q > 0) {
        stats::pgamma(u, shape = qi, lower.tail = FALSE)
      } else {
        stats::pgamma(u, shape = qi, lower.tail = TRUE)
      }
      lsv <- log(pmax(sf, 1e-300))
    }
    val <- -sum(ifelse(e > 0, ld - log(sigma), lsv))
    if (is.finite(val)) val else 1e12
  }
  start <- c(as.vector(qr.solve(A, logt)), 0, 1)
  res <- stats::optim(start, nll, method = "Nelder-Mead",
                      control = list(maxit = max_iter * 20, reltol = tol))
  beta <- res$par[seq_len(p)]
  log_sigma <- res$par[p + 1L]
  q <- res$par[p + 2L]
  ll <- -res$value
  restricted <- function(q_fixed) {
    r <- stats::optim(c(beta, log_sigma),
                      function(th) nll(c(th, q_fixed)),
                      method = "Nelder-Mead",
                      control = list(maxit = max_iter * 20, reltol = tol))
    -r$value
  }
  ll_w <- restricted(1)
  ll_ln <- restricted(0)
  lr_w <- max(2 * (ll - ll_w), 0)
  lr_ln <- max(2 * (ll - ll_ln), 0)
  p_w <- stats::pchisq(lr_w, 1, lower.tail = FALSE)
  p_ln <- stats::pchisq(lr_ln, 1, lower.tail = FALSE)
  preferred <- if (p_w < 0.05 && p_ln < 0.05) {
    "generalized gamma"
  } else if (lr_w <= lr_ln) {
    "weibull"
  } else {
    "lognormal"
  }
  list(beta = beta, sigma = exp(log_sigma), log_sigma = log_sigma, q = q,
       loglik = ll, lr_vs_weibull = lr_w, p_vs_weibull = p_w,
       lr_vs_lognormal = lr_ln, p_vs_lognormal = p_ln, preferred = preferred,
       aic = 2 * (p + 2) - 2 * ll, n = length(d$t),
       converged = identical(res$convergence, 0L),
       warnings = c(paste("q is weakly identified and is estimated from tail",
                          "behaviour; with few events a family choice made",
                          "this way is not reliable"),
                    if (!identical(res$convergence, 0L)) {
                      "the optimiser did not converge"
                    }),
       method = "generalized_gamma_aft")
}


#' Cox model with time-varying COEFFICIENTS
#'
#' Splits follow-up at event-time quantiles and fits a separate
#' coefficient in each interval, with a likelihood-ratio test against
#' the constant-coefficient fit.
#'
#' This is the time-varying COEFFICIENT, not the time-varying covariate
#' -- the covariate is fixed and its effect is allowed to change. It is
#' the direct fix when \code{\link{morie_cox_schoenfeld_residuals}} shows
#' proportional hazards failing.
#'
#' Late intervals routinely hold very few events, and a hazard ratio
#' estimated from three of them is not informative however clean it
#' looks. Read \code{events_per_interval} first.
#'
#' @inheritParams morie_dependent_censoring_hazard
#' @param n_intervals number of follow-up intervals.
#' @return list with \code{beta} (intervals x p), \code{se}, \code{z},
#'   \code{p_value}, \code{cutpoints}, \code{events_per_interval},
#'   \code{constant_beta}, \code{lr_vs_constant}, \code{p_vs_constant}.
#' @references Therneau, T. M. and Grambsch, P. M. (2000). \emph{Modeling
#'   Survival Data}, Sec. 6.5. Springer.
#' @examples
#' set.seed(6)
#' X <- matrix(rnorm(200), ncol = 1)
#' tt <- rexp(200, exp(X * 0.6))
#' morie_cox_time_varying(tt, rep(1, 200), X)$events_per_interval
#' @export
morie_cox_time_varying <- function(time, event, X, n_intervals = 3,
                                   ties = "efron") {
  d <- .morie_cox_prepare(time, event, X)
  n_intervals <- as.integer(n_intervals)
  if (n_intervals < 1L) {
    stop("n_intervals must be at least 1", call. = FALSE)
  }
  p <- ncol(d$X)
  ev_times <- sort(d$t[d$e == 1])
  if (length(ev_times) < n_intervals) {
    stop(sprintf("only %d events, too few for %d intervals",
                 length(ev_times), n_intervals), call. = FALSE)
  }
  qs <- seq(0, 1, length.out = n_intervals + 1L)
  qs <- qs[-c(1L, length(qs))]
  cuts <- if (length(qs)) {
    unique(stats::quantile(ev_times, qs, names = FALSE, type = 7L))
  } else {
    numeric(0)
  }
  edges <- c(0, cuts, Inf)
  betas <- matrix(0, n_intervals, p)
  ses <- matrix(0, n_intervals, p)
  counts <- integer(n_intervals)
  ll_tv <- 0
  for (j in seq_len(n_intervals)) {
    lo <- edges[j]
    hi <- edges[j + 1L]
    # First interval closed at zero: a strict `t > lo` drops a subject
    # observed at exactly t = 0 from every interval silently.
    keep <- if (j == 1L) d$t >= lo else d$t > lo
    tj <- pmin(d$t[keep], hi) - lo
    ej <- ifelse(d$t[keep] <= hi, d$e[keep], 0)
    counts[j] <- as.integer(sum(ej))
    if (counts[j] == 0L) {
      ses[j, ] <- NA_real_
      next
    }
    f <- .morie_cox_fit(tj, ej, d$X[keep, , drop = FALSE], ties = ties)
    betas[j, ] <- f$beta
    ll_tv <- ll_tv + f$loglik
    ses[j, ] <- tryCatch(sqrt(pmax(diag(solve(f$I)), 0)),
                         error = function(e) rep(NA_real_, p))
  }
  fc <- .morie_cox_fit(d$t, d$e, d$X, ties = ties)
  lr <- max(2 * (ll_tv - fc$loglik), 0)
  df <- max(p * (n_intervals - 1L), 1L)
  z <- betas / ses
  list(beta = betas, se = ses, z = z,
       p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
       hazard_ratio = exp(betas), cutpoints = cuts,
       events_per_interval = counts, constant_beta = fc$beta,
       loglik = ll_tv, loglik_constant = fc$loglik, lr_vs_constant = lr,
       p_vs_constant = stats::pchisq(lr, df, lower.tail = FALSE),
       n_intervals = n_intervals, n = length(d$t),
       warnings = paste("late intervals often hold few events; read",
                        "events_per_interval before trusting a late",
                        "coefficient"),
       method = "cox_time_varying")
}
