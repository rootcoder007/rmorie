# SPDX-License-Identifier: AGPL-3.0-or-later
#' The four-way decomposition expressed as shares of the total
#'
#' The absolute components answer how much; the proportions answer how
#' much of what happened. They only make sense when the components point
#' the same way -- with mixed signs a share can exceed one or go negative
#' -- so sign agreement is checked and reported.
#'
#' Formula: \code{TE = CDE + INTref + INTmed + PIE}, reported as each
#' component over \code{E[TE]}.
#'
#' @param Y Outcome.
#' @param X Exposure.
#' @param M Mediator.
#' @param Cc Optional covariates; read at their means.
#' @param a,astar,m Exposure contrast and controlled mediator level.
#' @return List with \code{estimate}, \code{p_cde}, \code{p_intref},
#'   \code{p_intmed}, \code{p_pie}, \code{p_mediated},
#'   \code{p_interaction}, \code{te}, \code{same_sign}, \code{n}.
#' @references VanderWeele, T. J. (2014). Epidemiology 25:749-761.
#' @export
VandIE <- function(Y, X, M, Cc = NULL, a = 1, astar = 0, m = 0) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  d <- .s4_fourway(mm$theta, mm$beta, mm$cbar, a, astar, m)
  parts <- c(d$cde, d$intref, d$intmed, d$pie)
  same <- as.numeric(all((parts >= 0) == (parts[1] >= 0)))
  sh <- function(v) if (d$te != 0) v / d$te else NaN
  .t1_result(estimate = sh(d$intref + d$intmed + d$pie), p_cde = sh(d$cde),
             p_intref = sh(d$intref), p_intmed = sh(d$intmed),
             p_pie = sh(d$pie), p_mediated = sh(d$intmed + d$pie),
             p_interaction = sh(d$intref + d$intmed), te = d$te,
             same_sign = same, n = length(as.numeric(Y)),
             method = "VanderWeele four-way decomposition, proportions")
}
