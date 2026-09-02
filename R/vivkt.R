# SPDX-License-Identifier: AGPL-3.0-or-later
#' Show how the classical two-way split sits inside the four-way one
#'
#' The natural direct and indirect decomposition is not a rival of the
#' four-way split; it is the four-way split with two pairs already
#' summed. \code{CDE + INTref} is the pure direct effect and
#' \code{PIE + INTmed} the total indirect effect, so the older
#' decomposition is recovered exactly and the interaction it was hiding
#' becomes visible.
#'
#' Formula: \code{PDE = CDE + INTref}, \code{TIE = PIE + INTmed},
#' \code{TE = PDE + TIE}.
#'
#' @param X Exposure.
#' @param M Mediator.
#' @param Y Outcome.
#' @param Cc Optional covariates; read at their means.
#' @param a,astar,m Exposure contrast and controlled mediator level.
#' @return List with \code{estimate}, \code{pde}, \code{tie}, \code{cde},
#'   \code{intref}, \code{intmed}, \code{pie}, \code{check}, \code{n}.
#' @references VanderWeele, T. J. (2014). Epidemiology 25:749-761;
#'   Robins & Greenland (1992) Epidemiology 3:143-155; Pearl (2001) UAI
#'   17:411-420.
#' @export
#' @examples
#' Vivkt(X = 5L, M = 5L, Y = c(1, 2, 3, 4, 5, 6, 7, 8))
Vivkt <- function(X, M, Y, Cc = NULL, a = 1, astar = 0, m = 0) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  d <- .s4_fourway(mm$theta, mm$beta, mm$cbar, a, astar, m)
  pde <- d$cde + d$intref
  tie <- d$pie + d$intmed
  .t1_result(estimate = d$te, pde = pde, tie = tie, cde = d$cde,
             intref = d$intref, intmed = d$intmed, pie = d$pie,
             check = d$te - (pde + tie), n = length(as.numeric(Y)),
             method = "Two-way and four-way decompositions together")
}
