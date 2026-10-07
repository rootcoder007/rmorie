# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Research P1 (continued): Le Cam's two-point lower bound
# (research/lean/P1LeCam.lean; Le Cam 1973; Yu 1997; Tsybakov 2009 ch. 2).
#
#   Research.P1LeCam.sum_min     sum_i min(p_i, q_i) = 1 - TV(p, q)
#   Research.P1LeCam.tv_nonneg / tv_le_one
#   Research.P1LeCam.two_point   E_p|T - theta_p| + E_q|T - theta_q| >= |theta_p - theta_q| (1 - TV)
#   Research.P1LeCam.minimax     max of the two risks >= |theta_p - theta_q| (1 - TV) / 2

#' Le Cam's two-point lower bound on a finite sample space
#'
#' Two laws \eqn{p, q} on the same finite space with parameter values
#' \eqn{\theta_p, \theta_q}. For every estimator \eqn{T},
#' \eqn{E_p|T-\theta_p| + E_q|T-\theta_q| \ge |\theta_p-\theta_q|(1 - TV)}
#' (\code{Research.P1LeCam.two_point}), so the worse of the two risks is at
#' least \eqn{|\theta_p-\theta_q|(1-TV)/2} (\code{minimax}), using
#' \eqn{\sum\min(p,q) = 1 - TV} (\code{sum_min}). For the dark figure: two
#' recording mechanisms whose recorded-crime laws are close in total
#' variation but whose true rates differ by \eqn{\delta} leave every
#' estimator a worst-case error of at least \eqn{\delta(1-TV)/2}.
#' @param p,q Probability vectors on the same finite space.
#' @param theta_p,theta_q The parameter under each law.
#' @param estimator Optional vector of the estimator's value at each point.
#' @return A list with \code{tv}, \code{sum_min}, \code{delta}, \code{bound}
#'   (\eqn{\delta(1-TV)/2}), and when \code{estimator} is given
#'   \code{risk_p}, \code{risk_q}, \code{minimax_risk} and \code{satisfied};
#'   plus \code{theorems}.
#' @examples
#' # recorded counts 0..4 under two reporting rates for the same true rate 2 vs 3 offences
#' p <- dbinom(0:4, 4, 0.5); q <- dbinom(0:4, 4, 0.6)
#' b <- morie_two_point_bound(p, q, theta_p = 2, theta_q = 3, estimator = 0:4 * 1.25)
#' c(tv = b$tv, bound = b$bound, minimax_risk = b$minimax_risk, satisfied = b$satisfied)
#' @export
morie_two_point_bound <- function(p, q, theta_p, theta_q, estimator = NULL) {
  if (!is.numeric(p) || !is.numeric(q) || length(p) != length(q) || anyNA(p) || anyNA(q)) stop("p and q must be numeric of equal length without NA", call. = FALSE)
  if (any(p < 0) || any(q < 0) || abs(sum(p) - 1) > 1e-8 || abs(sum(q) - 1) > 1e-8) stop("p and q must be probability vectors", call. = FALSE)
  if (length(theta_p) != 1L || length(theta_q) != 1L || is.na(theta_p) || is.na(theta_q)) stop("theta_p and theta_q must be single numbers", call. = FALSE)
  tv <- sum(abs(p - q)) / 2
  delta <- abs(theta_p - theta_q)
  out <- list(tv = tv, sum_min = sum(pmin(p, q)), delta = delta, bound = delta * (1 - tv) / 2)
  if (!is.null(estimator)) {
    if (length(estimator) != length(p) || anyNA(estimator)) stop("estimator must give one value per point of the space", call. = FALSE)
    rp <- sum(p * abs(estimator - theta_p))
    rq <- sum(q * abs(estimator - theta_q))
    out$risk_p <- rp
    out$risk_q <- rq
    out$minimax_risk <- max(rp, rq)
    out$satisfied <- max(rp, rq) >= out$bound - 1e-12
  }
  out$theorems <- c("Research.P1LeCam.sum_min", "Research.P1LeCam.tv_le_one", "Research.P1LeCam.two_point", "Research.P1LeCam.minimax")
  out
}
