# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effects defined by shifting the mediator distribution, not the value
#'
#' Natural effects need each person counterfactual mediator value under
#' the exposure they did not get, which is not identified when something
#' the exposure caused also confounds the mediator-outcome link.
#' Interventional effects draw the mediator from its population
#' distribution under the other exposure level instead. Weaker
#' assumptions, and a question a policy could implement -- at the cost of
#' no longer decomposing an individual effect.
#'
#' Formula: \code{IDE = (th1 + th3(b0 + b1 a* + b2'c))(a - a*)},
#' \code{IIE = (th2 + th3 a) b1 (a - a*)}, summing to the total effect.
#'
#' @param Y Outcome.
#' @param X Exposure.
#' @param M Mediator.
#' @param Cc Optional covariates; read at their means.
#' @param a,astar Exposure contrast levels.
#' @return List with \code{estimate}, \code{ide}, \code{iie},
#'   \code{check}, \code{theta}, \code{beta}, \code{n}.
#' @references VanderWeele, T. J., Vansteelandt, S. & Robins, J. M.
#'   (2014). Epidemiology 25:300-306; VanderWeele (2014) Epidemiology
#'   25:749-761.
#' @export
Intvse <- function(Y, X, M, Cc = NULL, a = 1, astar = 0) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  d <- a - astar
  bc <- mm$beta[1] + mm$beta[2] * astar
  if (length(mm$cbar)) bc <- bc + sum(mm$beta[2 + seq_along(mm$cbar)] * mm$cbar)
  ide <- (mm$theta[2] + mm$theta[4] * bc) * d
  iie <- (mm$theta[3] + mm$theta[4] * a) * mm$beta[2] * d
  fw <- .s4_fourway(mm$theta, mm$beta, mm$cbar, a, astar, 0)
  .t1_result(estimate = fw$te, ide = ide, iie = iie,
             check = fw$te - (ide + iie), theta = mm$theta, beta = mm$beta,
             n = length(as.numeric(Y)),
             method = "Interventional direct and indirect effects")
}
