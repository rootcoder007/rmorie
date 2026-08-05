# SPDX-License-Identifier: AGPL-3.0-or-later
#' Variance-stabilised DR-DiD: trim the DR weights, report the ESS
#'
#' The doubly robust moment of Sant'Anna and Zhao (2020) is a weighted
#' average whose control weights contain \code{pi/(1 - pi)}, so a single
#' control unit with a propensity near one can dominate.  Kish's
#' effective sample size \code{ESS = (sum |w|)^2 / sum w^2} equals
#' \code{n} for equal weights and falls towards 1 as one weight
#' dominates.  Stabilisation caps the control weights at their
#' \code{q}-quantile and renormalises.  \code{q = 1} leaves every weight
#' untouched, so the estimate then equals the raw estimate exactly.
#'
#' @param y Outcome change, one entry per unit.
#' @param D Binary treatment indicator.
#' @param X Optional baseline covariates.
#' @param q Quantile at which the control weights are capped, in (0, 1].
#' @return List with \code{estimate}, \code{tau_raw}, \code{ess_raw},
#'   \code{ess_stab}, \code{ess_ratio}, \code{cap}, \code{max_weight},
#'   \code{n}.
#' @references Sant'Anna, P. H. C. and Zhao, J. (2020). Journal of
#'   Econometrics 219(1), 101-122, equation (2.6).  Kish, L. (1965).
#'   Survey Sampling, Wiley, section 11.7.  Crump, R. K., Hotz, V. J.,
#'   Imbens, G. W. and Mitnik, O. A. (2009). Biometrika 96(1), 187-199.
#' @export
Drvst <- function(y, D, X = NULL, q = 0.95) {
  yv <- .s03vec(y); dv <- .s03vec(D); n <- length(yv)
  if (n == 0L) stop("Drvst: empty input, y has no observations")
  if (length(dv) != n) stop("Drvst: y and D must have the same length")
  if (!(q > 0 && q <= 1)) stop("Drvst: q must lie in (0, 1]")
  s <- sum(dv)
  if (s <= 0 || s >= n)
    stop("Drvst: D must contain both treated and control units")
  fit <- .s03drdid(yv, dv, X)
  ess <- function(v) {
    a <- sum(abs(v)); b <- sum(v * v)
    if (b > 0) a * a / b else 0
  }
  w <- fit$w1 - fit$w0
  ess0 <- ess(w)
  raw0 <- fit$w0[dv < 0.5]
  cap <- if (length(raw0)) .s03quantile7(sort(raw0), q) else 0
  w0 <- ifelse(dv >= 0.5 | fit$w0 <= cap, fit$w0, cap)
  tot <- sum(w0)
  if (tot > 0) w0 <- w0 / tot
  ws <- fit$w1 - w0
  ess1 <- ess(ws)
  tau <- sum(ws * (yv - fit$mu0))
  .t1_result(estimate = tau, tau_raw = fit$tau, ess_raw = ess0,
             ess_stab = ess1,
             ess_ratio = if (ess0 > 0) ess1 / ess0 else NaN,
             cap = cap, max_weight = max(abs(ws)), n = n,
             method = "Variance-stabilized DR-DiD")
}
