# SPDX-License-Identifier: AGPL-3.0-or-later
#' Split a total effect into mediation, interaction, both, neither
#'
#' The four-way decomposition shows that mediation analysis and
#' interaction analysis are two halves of one accounting identity: a
#' total effect divides into a part due to neither, a part due to
#' interaction alone, a part due to mediation alone, and a part that
#' needs both at once. The fourth piece is the one neither literature
#' could name on its own.
#'
#' Formula: \code{TE = CDE + INTref + INTmed + PIE} from
#' \code{Y = th0 + th1 a + th2 m + th3 a m + th4'c} and
#' \code{M = b0 + b1 a + b2'c}.
#'
#' @param X Exposure.
#' @param M Mediator.
#' @param Y Outcome.
#' @param Cc Optional covariates; read at their means.
#' @param a,astar Exposure contrast levels.
#' @param m Level the mediator is controlled at for the CDE.
#' @return List with \code{estimate}, \code{cde}, \code{intref},
#'   \code{intmed}, \code{pie}, \code{pai}, \code{pe}, \code{theta},
#'   \code{beta}, \code{n}.
#' @references VanderWeele, T. J. (2014). A unification of mediation and
#'   interaction: a 4-way decomposition. Epidemiology 25:749-761.
#' @export
Intmd4 <- function(X, M, Y, Cc = NULL, a = 1, astar = 0, m = 0) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  d <- .s4_fourway(mm$theta, mm$beta, mm$cbar, a, astar, m)
  .t1_result(estimate = d$te, cde = d$cde, intref = d$intref,
             intmed = d$intmed, pie = d$pie, pai = d$intref + d$intmed,
             pe = d$intref + d$intmed + d$pie, theta = mm$theta,
             beta = mm$beta, n = length(as.numeric(Y)),
             method = "VanderWeele four-way decomposition")
}
