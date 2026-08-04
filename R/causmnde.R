# SPDX-License-Identifier: AGPL-3.0-or-later
#' Natural direct and indirect effects, then split each again
#'
#' \code{TE = NDE + NIE} is where mediation analysis usually stops. Each
#' half still contains an interaction term, and separating them says
#' whether the direct path carries an interaction with the mediator and
#' whether the indirect path does. The pure indirect effect is the piece
#' that would survive even if exposure and mediator never interacted.
#'
#' Formula: \code{NDE = CDE + INTref}, \code{NIE = PIE + INTmed},
#' \code{TE = NDE + NIE}.
#'
#' @param X Exposure.
#' @param M Mediator.
#' @param Y Outcome.
#' @param Cc Optional covariates; read at their means.
#' @param a,astar,m Exposure contrast and controlled mediator level.
#' @return List with \code{NDE}, \code{NIE}, \code{PDE}, \code{PIE},
#'   \code{estimate}, \code{cde}, \code{intref}, \code{intmed}, \code{n}.
#' @references VanderWeele, T. J. (2015). Explanation in Causal
#'   Inference, Oxford University Press, chapter 14; VanderWeele (2014)
#'   Epidemiology 25:749-761.
#' @export
Causmnde <- function(X, M, Y, Cc = NULL, a = 1, astar = 0, m = 0) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  d <- .s4_fourway(mm$theta, mm$beta, mm$cbar, a, astar, m)
  nde <- d$cde + d$intref
  nie <- d$pie + d$intmed
  .t1_result(NDE = nde, NIE = nie, PDE = nde, PIE = d$pie, estimate = d$te,
             cde = d$cde, intref = d$intref, intmed = d$intmed,
             n = length(as.numeric(Y)),
             method = "Natural effects with pure and interaction parts")
}
