# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Accelerated-failure-time shelf. R mirrors of the morie.fn modules
# aftwbl, aftllg, aftgma and aftres over one shared fitter, as _aft.py
# does.
#
# These are the only functions in the survival port whose parity is not
# exact to machine precision: R's optim and scipy's minimize walk
# different paths to the same optimum, so the fitted parameters agree to
# about 1e-6 rather than 1e-16. The standard errors DO agree closely,
# because both languages take them from a central-difference Hessian of
# the same objective rather than from the optimiser's own internal
# approximation.

#' .morie_aft_log_dens_surv
#'
#' A step of the aft_native implementation. Called by \code{.morie_aft_fit}, \code{morie_aft_residuals}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param z Numeric; combined arithmetically in the body.
#' @param family See Usage.
#' @return Nothing; this branch always raises.
#' @export
.morie_aft_log_dens_surv <- function(z, family) {
  if (identical(family, "weibull")) {
    zc <- pmax(pmin(z, 500), -500)
    return(list(logdens = z - exp(zc), logsurv = -exp(zc)))
  }
  if (identical(family, "loglogistic")) {
    zz <- pmax(pmin(z, 500), -500)
    lae <- ifelse(zz > 0, zz + log1p(exp(-zz)), log1p(exp(zz)))
    return(list(logdens = zz - 2 * lae, logsurv = -lae))
  }
  if (identical(family, "lognormal")) {
    return(list(
      logdens = stats::dnorm(z, log = TRUE),
      logsurv = stats::pnorm(z, lower.tail = FALSE, log.p = TRUE)
    ))
  }
  stop(sprintf(paste(
    'family must be "weibull", "loglogistic" or',
    '"lognormal", got "%s"'
  ), family), call. = FALSE)
}

# Inverse of the central-difference Hessian. The optimiser's own
# inverse-Hessian is a secant approximation accumulated along whatever
# path it walked, so it is not reproducible across solvers; this is.
#' Inverse of the central-difference Hessian. The optimiser\'s own
#'
#' inverse-Hessian is a secant approximation accumulated along whatever
#' path it walked, so it is not reproducible across solvers; this is.
#'
#' @param fn See Usage.
#' @param theta A vector; its length is taken.
#' @param rel Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.
#' @return The value of \code{tryCatch}.
#' @export
.morie_numeric_cov <- function(fn, theta, rel = 1e-5) {
  k <- length(theta)
  h <- rel * pmax(abs(theta), 1)
  H <- matrix(0, k, k)
  for (i in seq_len(k)) {
    for (j in i:k) {
      tp <- theta
      tp[i] <- tp[i] + h[i]
      tp[j] <- tp[j] + h[j]
      tm <- theta
      tm[i] <- tm[i] - h[i]
      tm[j] <- tm[j] - h[j]
      tpm <- theta
      tpm[i] <- tpm[i] + h[i]
      tpm[j] <- tpm[j] - h[j]
      tmp <- theta
      tmp[i] <- tmp[i] - h[i]
      tmp[j] <- tmp[j] + h[j]
      H[i, j] <- H[j, i] <- (fn(tp) - fn(tpm) - fn(tmp) + fn(tm)) /
        (4 * h[i] * h[j])
    }
  }
  tryCatch(solve(H), error = function(err) NULL)
}

#' .morie_aft_fit
#'
#' A step of the aft_native implementation. Called by \code{.morie_aft_common}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken.
#' @param e See Usage.
#' @param X A matrix; passed to \code{as.matrix}.
#' @param family Passed to \code{.morie_aft_log_dens_surv}. Defaults to \code{"weibull"}.
#' @param max_iter Carried through into a list the body builds. Defaults to \code{500L}.
#' @param tol Defaults to \code{1e-06}.
#' @param add_intercept A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{beta}, \code{log_sigma}, \code{loglik}, \code{cov}, \code{n_iter}, \code{converged}.
#' @export
.morie_aft_fit <- function(t, e, X, family = "weibull", max_iter = 500L,
                           tol = 1e-6, add_intercept = TRUE) {
  n <- length(t)
  A <- if (add_intercept) cbind(1, X) else as.matrix(X)
  p <- ncol(A)
  logt <- log(pmax(t, 1e-300))
  nll <- function(theta) {
    b <- theta[seq_len(p)]
    sigma <- exp(pmax(pmin(theta[p + 1L], 20), -20))
    z <- (logt - as.vector(A %*% b)) / sigma
    ds <- .morie_aft_log_dens_surv(z, family)
    -sum(ifelse(e > 0, ds$logdens - log(sigma), ds$logsurv))
  }
  start <- c(as.vector(qr.solve(A, logt)), 0)
  res <- stats::optim(start, nll,
    method = "BFGS",
    control = list(maxit = max_iter, reltol = 1e-14)
  )
  # A second pass from the first answer: BFGS stops on a relative
  # criterion, and restarting resets the approximation, which buys the
  # last few digits that the cross-language comparison needs.
  res <- stats::optim(res$par, nll,
    method = "BFGS",
    control = list(maxit = max_iter, reltol = 1e-14)
  )
  theta <- res$par
  list(
    beta = theta[seq_len(p)], log_sigma = theta[p + 1L],
    loglik = -res$value, cov = .morie_numeric_cov(nll, theta),
    n_iter = as.integer(res$counts[[1L]]),
    converged = identical(res$convergence, 0L)
  )
}

#' .morie_aft_result
#'
#' A step of the aft_native implementation. Called by \code{.morie_aft_common}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param t A vector; its length is taken.
#' @param e Numeric; passed to \code{sum}.
#' @param X Carried through into a list the body builds.
#' @param fit A list; the body reads \code{$beta}, \code{$converged}, \code{$cov}, \code{$log_sigma}, \code{$loglik}, \code{$n_iter} from it.
#' @param family Carried through into a list the body builds.
#' @param title See Usage.
#' @param method Carried through into a list the body builds.
#' @return A list with \code{beta}, \code{se}, \code{time_ratio}, \code{sigma}, \code{log_sigma}, \code{loglik}, \code{aic}, \code{family}, \code{n}, \code{n_events}, \code{n_iter}, \code{converged}, \code{cov}, \code{time}, \code{event}, \code{X}, \code{method}.
#' @export
.morie_aft_result <- function(t, e, X, fit, family, title, method) {
  p <- length(fit$beta)
  se <- if (is.null(fit$cov)) {
    rep(NA_real_, p)
  } else {
    sqrt(pmax(diag(fit$cov)[seq_len(p)], 0))
  }
  list(
    beta = fit$beta, se = se, time_ratio = exp(fit$beta),
    sigma = exp(fit$log_sigma), log_sigma = fit$log_sigma,
    loglik = fit$loglik, aic = 2 * (p + 1) - 2 * fit$loglik,
    family = family, n = length(t), n_events = as.integer(sum(e)),
    n_iter = fit$n_iter, converged = fit$converged, cov = fit$cov,
    time = t, event = e, X = X, method = method
  )
}

#' .morie_aft_common
#'
#' A step of the aft_native implementation. Called by \code{morie_aft_generalized_gamma}, \code{morie_aft_log_logistic}, \code{morie_aft_weibull}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param time Passed to \code{.morie_cox_prepare}.
#' @param event Passed to \code{.morie_cox_prepare}.
#' @param X Passed to \code{.morie_cox_prepare}.
#' @param family Passed to \code{.morie_aft_fit}.
#' @param title Passed to \code{.morie_aft_result}.
#' @param method Passed to \code{.morie_aft_result}.
#' @param ... Passed through.
#' @return The value of \code{.morie_aft_result}.
#' @export
.morie_aft_common <- function(time, event, X, family, title, method, ...) {
  d <- .morie_cox_prepare(time, event, X)
  if (any(d$t <= 0)) {
    stop("AFT models need strictly positive times", call. = FALSE)
  }
  fit <- .morie_aft_fit(d$t, d$e, d$X, family = family, ...)
  .morie_aft_result(d$t, d$e, d$X, fit, family, title, method)
}


#' Weibull accelerated-failure-time model
#'
#' Regression on log time, \eqn{\log T = x'\beta + \sigma W} with W
#' standard extreme-value. The Weibull is the one family that is both
#' AFT and proportional hazards, which is why it is the usual starting
#' point.
#'
#' The direction of the coefficient is the opposite of Cox's and is the
#' standard trap: \code{exp(beta)} is a TIME RATIO, so a positive
#' coefficient means LONGER survival. In a Cox model a positive
#' coefficient means a higher hazard, i.e. shorter survival.
#'
#' @param time follow-up times, strictly positive.
#' @param event 1 for an event, 0 for right-censored.
#' @param X covariate matrix, one row per subject. An intercept is added.
#' @param ... passed to the fitter (\code{max_iter}, \code{tol},
#'   \code{add_intercept}).
#' @return list with \code{beta}, \code{se}, \code{time_ratio},
#'   \code{sigma}, \code{loglik}, \code{aic}, and the pieces
#'   \code{\link{morie_aft_residuals}} needs.
#' @references Kalbfleisch, J. D. and Prentice, R. L. (2002). \emph{The
#'   Statistical Analysis of Failure Time Data}, 2nd ed., Ch. 2-3.
#' @examples
#' set.seed(2)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- exp(0.5 + X %*% c(0.8, -0.4) - 0.7 * log(rexp(100)))
#' round(morie_aft_weibull(tt, rep(1, 100), X)$time_ratio, 3)
#' @export
morie_aft_weibull <- function(time, event, X, ...) {
  .morie_aft_common(
    time, event, X, "weibull", "Weibull AFT model",
    "aft_weibull", ...
  )
}


#' Log-logistic accelerated-failure-time model
#'
#' \eqn{\log T = x'\beta + \sigma W} with W standard logistic. The
#' hazard rises then falls, which is what recommends it when the risk
#' peaks early and then recedes -- a shape no proportional-hazards
#' Weibull can produce.
#'
#' As for every AFT model, \code{exp(beta)} is a time ratio: positive
#' means longer survival.
#'
#' @inheritParams morie_aft_weibull
#' @return as \code{\link{morie_aft_weibull}}.
#' @references Bennett, S. (1983). Log-logistic regression models for
#'   survival data. \emph{Applied Statistics}, 32(2), 165-171.
#' @examples
#' set.seed(2)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- exp(0.5 + X %*% c(0.8, -0.4) + 0.6 * rlogis(100))
#' round(morie_aft_log_logistic(tt, rep(1, 100), X)$sigma, 3)
#' @export
morie_aft_log_logistic <- function(time, event, X, ...) {
  .morie_aft_common(
    time, event, X, "loglogistic", "Log-logistic AFT model",
    "aft_log_logistic", ...
  )
}


#' Log-normal accelerated-failure-time model
#'
#' \eqn{\log T = x'\beta + \sigma W} with W standard normal -- the
#' log-normal member of the generalized-gamma family, which is what this
#' module provides. Like the log-logistic it gives a hazard that rises
#' and then falls; unlike it, the survival function has no closed form
#' and is evaluated through the normal.
#'
#' @inheritParams morie_aft_weibull
#' @return as \code{\link{morie_aft_weibull}}.
#' @references Lawless, J. F. (2003). \emph{Statistical Models and
#'   Methods for Lifetime Data}, 2nd ed., Sec. 5.3.
#' @examples
#' set.seed(2)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- exp(0.5 + X %*% c(0.8, -0.4) + 0.6 * rnorm(100))
#' round(morie_aft_generalized_gamma(tt, rep(1, 100), X)$beta, 3)
#' @export
morie_aft_generalized_gamma <- function(time, event, X, ...) {
  .morie_aft_common(
    time, event, X, "lognormal", "Log-normal AFT model",
    "aft_generalized_gamma", ...
  )
}


#' Residuals from an AFT fit
#'
#' Standardised residuals \eqn{z = (\log t - x'\beta)/\sigma},
#' Cox-Snell residuals \eqn{-\log S(z)}, and the martingale and
#' deviance forms built from them.
#'
#' The Cox-Snell residuals are the reason to bother: under ANY correctly
#' specified parametric family they are a censored sample from the unit
#' exponential, so one plot of their Nelson-Aalen cumulative hazard
#' against themselves checks Weibull, log-logistic and log-normal with
#' the same picture. Departure from the 45-degree line is misfit.
#'
#' @param fit a fit from one of the AFT fitters.
#' @return list with \code{standardized}, \code{cox_snell},
#'   \code{martingale}, \code{deviance}, \code{event}, \code{family}.
#' @references Cox, D. R. and Snell, E. J. (1968). A general definition
#'   of residuals. \emph{JRSS-B}, 30(2), 248-265.
#' @examples
#' set.seed(2)
#' X <- matrix(rnorm(200), ncol = 2)
#' tt <- exp(0.5 + X %*% c(0.8, -0.4) - 0.7 * log(rexp(100)))
#' f <- morie_aft_weibull(tt, rep(1, 100), X)
#' round(mean(morie_aft_residuals(f)$cox_snell), 2)
#' @export
morie_aft_residuals <- function(fit) {
  for (k in c("time", "event", "X", "beta", "log_sigma", "family")) {
    if (is.null(fit[[k]])) {
      stop(sprintf(paste(
        "fit is missing '%s'; pass a result from one of",
        "the AFT fitters"
      ), k), call. = FALSE)
    }
  }
  t <- as.numeric(fit$time)
  e <- as.numeric(fit$event)
  X <- as.matrix(fit$X)
  beta <- as.numeric(fit$beta)
  sigma <- exp(fit$log_sigma)
  fam <- fit$family
  A <- if (length(beta) == ncol(X) + 1L) cbind(1, X) else X
  z <- (log(pmax(t, 1e-300)) - as.vector(A %*% beta)) / sigma
  ds <- .morie_aft_log_dens_surv(z, fam)
  cs <- -ds$logsurv
  mart <- e - cs
  inner <- -2 * (mart + ifelse(e > 0, e * log(pmax(e - mart, 1e-300)), 0))
  dev <- sign(mart) * sqrt(pmax(inner, 0))
  list(
    standardized = z, cox_snell = cs, martingale = mart, deviance = dev,
    event = e, family = fam, method = "aft_residuals"
  )
}
