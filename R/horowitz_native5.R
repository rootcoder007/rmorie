# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Horowitz shelf mirror, part 5: Lewbel's special-regressor estimator
# for heteroskedastic binary response. Mirrors morie.fn.hrzlew.
#
# Collision scan: horowitz_native5.R and morie_lewbel_binary were free
# in both R trees; .hrz_silverman and .hrz_gauss_kernel are reused
# from R/aaa_helpers_horowitz.R so both languages share one bandwidth
# rule and one kernel.
#
# Spec: Horowitz, Sec. 4.5 DESCRIBES this estimator but prints no
# formula, so the formula comes from the primary source instead --
# Dong, Y. and Lewbel, A., Simple Estimators for Binary Choice Models
# With Endogenous Regressors, Corollary 1 and Estimator 1.

#' Lewbel's special-regressor estimator for binary response
#'
#' For \eqn{Y = 1\{V + X'\beta + \varepsilon > 0\}} with V the
#' SPECIAL REGRESSOR -- continuously distributed with large support,
#' entering additively, coefficient known to be 1 -- write
#' \eqn{V = S'b + U} with \eqn{U} independent of \eqn{(S,
#' \varepsilon)} and construct
#'
#' \deqn{T = [Y - 1\{V \ge 0\}] / f(U).}
#'
#' Then \eqn{T = X'\beta + \tilde\varepsilon} with
#' \eqn{E(Z\tilde\varepsilon) = 0}, so beta follows from a LINEAR
#' regression of T on X: least squares when X is exogenous, two-stage
#' least squares on instruments when it is not. A discrete-choice
#' problem collapses to a linear one.
#'
#' Why the chapter reaches for this: maximum score and smoothed
#' maximum score are consistent under weak assumptions but do not
#' identify \eqn{P(Y = 1|X = x)} and converge more slowly than
#' \eqn{n^{-1/2}}. This estimator buys back both, at the price of
#' needing a special regressor, and still allows heteroskedasticity
#' of unknown form.
#'
#' The dividing density is the fragility: \eqn{\hat f(U)} sits in a
#' DENOMINATOR, so tail observations carry enormous weight -- the
#' source itself notes outliers may need discarding. The smallest
#' fitted density and the largest weight are returned rather than
#' left implicit. Mirrors \code{morie.fn.hrzlew}.
#'
#' @param x numeric matrix of regressors excluding V; a constant is
#'   added when absent.
#' @param y binary 0/1 response.
#' @param z the special regressor V.
#' @param bandwidth bandwidth for the density of U; Silverman's rule
#'   when NULL. Ignored when \code{density = "normal"}.
#' @param instruments instruments for endogenous columns of x; least
#'   squares is used when NULL.
#' @param density "nonparametric" or "normal" (Estimator 1, step 2).
#' @return list: beta, se, coefficient_on_V, min_density, max_weight,
#'   root_n_consistent, heteroskedasticity_allowed,
#'   identifies_choice_probabilities, bandwidth, endogenous, n, d,
#'   method.
#' @references Horowitz, Sec. 4.5; Dong and Lewbel, Corollary 1 and
#'   Estimator 1. The indicator is \eqn{1\{V \ge 0\}} -- at least one
#'   secondary description states \eqn{1\{V < 0\}}, which changes the
#'   estimand.
#' @examples
#' n <- 500
#' x <- cbind(1, rnorm(n))
#' v <- rnorm(n) * 6
#' y <- as.numeric(v + x %*% c(0, 1) + rnorm(n) > 0)
#' morie_lewbel_binary(x, y, v)$beta
#' @export
morie_lewbel_binary <- function(x, y, z, bandwidth = NULL, instruments = NULL,
                                density = "nonparametric") {
  yv <- as.numeric(y)
  X <- if (is.matrix(x)) x else matrix(as.numeric(x), ncol = 1L)
  v <- as.numeric(z)
  if (nrow(X) != length(yv)) X <- t(X)
  if (nrow(X) != length(yv)) {
    stop("x must have one row per entry of y.", call. = FALSE)
  }
  if (length(v) != length(yv)) {
    stop(sprintf("z must have one entry per row of x, got %d for %d.",
                 length(v), length(yv)), call. = FALSE)
  }
  if (!all(yv %in% c(0, 1))) stop("y must be binary 0/1.", call. = FALSE)
  if (!density %in% c("nonparametric", "normal")) {
    stop("density must be 'nonparametric' or 'normal'.", call. = FALSE)
  }
  n <- length(yv)
  if (n < 10L) stop(sprintf("need at least 10 observations, got %d.", n),
                    call. = FALSE)

  has_const <- any(apply(X, 2L, function(cc) isTRUE(all.equal(cc, rep(cc[1L], n)))))
  Xd <- if (has_const) X else cbind(1, X)
  d <- ncol(Xd)

  zi <- NULL
  if (!is.null(instruments)) {
    zi <- if (is.matrix(instruments)) instruments else
      matrix(as.numeric(instruments), ncol = 1L)
    if (nrow(zi) != n) zi <- t(zi)
    if (nrow(zi) != n) {
      stop("instruments must have one row per observation.", call. = FALSE)
    }
    zc <- any(apply(zi, 2L, function(cc) isTRUE(all.equal(cc, rep(cc[1L], n)))))
    if (!zc) zi <- cbind(1, zi)
    if (ncol(zi) < d) {
      stop(sprintf("need at least %d instruments for %d regressors, got %d.",
                   d, d, ncol(zi)), call. = FALSE)
    }
  }

  # Step 1: demean V, take residuals of V on S (everything but V)
  vc <- v - mean(v)
  s_mat <- if (is.null(zi)) Xd else cbind(Xd, zi[, -1L, drop = FALSE])
  cf <- qr.coef(qr(s_mat), vc)
  cf[is.na(cf)] <- 0
  u <- as.numeric(vc - s_mat %*% cf)

  # Step 2: f(U)
  if (density == "normal") {
    sd_u <- sqrt(mean(u^2))
    if (sd_u <= 0) {
      stop("the special regressor is fully explained by S; U has zero variance and f(U) is undefined.",
           call. = FALSE)
    }
    fhat <- stats::dnorm(u / sd_u) / sd_u
    hh <- NULL
  } else {
    hh <- if (is.null(bandwidth)) .hrz_silverman(u) else as.numeric(bandwidth)
    if (hh <= 0) stop(sprintf("bandwidth must be positive, got %g.", hh),
                      call. = FALSE)
    fhat <- rowSums(.hrz_gauss_kernel(outer(u, u, "-") / hh)) / (n * hh)
  }
  if (any(fhat <= 0)) {
    stop("the fitted density of U vanishes at some observation; T would be undefined there.",
         call. = FALSE)
  }

  # Step 3: T = [Y - I(V >= 0)] / f(U)
  tt <- (yv - as.numeric(v >= 0)) / fhat

  # Step 4: linear regression of T on X, 2SLS when instruments given
  a_mat <- if (is.null(zi)) Xd else {
    pz <- qr.coef(qr(zi), Xd)
    pz[is.na(pz)] <- 0
    zi %*% pz
  }
  beta <- qr.coef(qr(a_mat), tt)
  beta[is.na(beta)] <- 0
  beta <- unname(as.numeric(beta))
  resid <- as.numeric(tt - Xd %*% beta)
  dof <- max(n - d, 1L)
  xtx <- crossprod(a_mat)
  xtx_inv <- tryCatch(solve(xtx), error = function(e)
    stop("the regressor cross-product is singular; beta is not identified.",
         call. = FALSE))
  se <- unname(sqrt(diag(xtx_inv) * sum(resid^2) / dof))

  list(beta = beta, se = se, coefficient_on_V = 1,
       min_density = min(fhat), max_weight = max(1 / fhat),
       root_n_consistent = TRUE, heteroskedasticity_allowed = TRUE,
       identifies_choice_probabilities = TRUE,
       bandwidth = hh, endogenous = !is.null(zi), n = n, d = d,
       method = "Lewbel special regressor: T = [Y - I(V >= 0)] / f(U), then a linear regression")
}
