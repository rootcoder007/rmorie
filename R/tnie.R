# SPDX-License-Identifier: AGPL-3.0-or-later
#' Total natural indirect effect
#'
#' \deqn{TNIE = E\[Y(a, M(a)) - Y(a, M(a^*))\] = (\theta_2\beta_1 + \theta_3\beta_1 a)(a
#' - a^*).}{TNIE = E\[Y(a, M(a)) - Y(a, M(a*))\] = (theta2 beta1 + theta3 beta1 a)(a -
#' a*).}
#'
#' Robins, J. M. and Greenland, S. (1992), \emph{Epidemiology} 3(2), 143-155,
#' doi:10.1097/00001648-199203000-00013, is the shelf citation; it is closed
#' access with no open copy in any repository (Unpaywall reports is_oa false).
#' This is the NIE printed as equation (0.3) of Valeri, L. and VanderWeele,
#' T. J. (2013), \emph{Psychological Methods} 18(2), 137-150,
#' doi:10.1037/a0031034, open access at PMC3659198: the indirect effect with
#' the exposure held at a in the outcome model.  Together with the pure natural
#' direct effect it adds to the total effect, and the same total is reached the
#' other way round by the total natural direct effect plus the pure natural
#' indirect effect: PNDE + TNIE = TNDE + PNIE.  That identity is exact and is
#' what this module is checked against.
#'
#' See Ctde for the two fitted models and for the identification assumptions
#' this arithmetic does not check.
#'
#' @param X,M,Y Exposure, mediator, outcome.
#' @param C Optional covariates.
#' @param a,astar Exposure contrast.
#' @return list: estimate (TNIE), pnde, tnde, pnie, te, mediated_interaction,
#'   beta, theta, a, astar, n, method.
#' @keywords internal
#' @examples
#' A <- c(0, 0, 0, 0, 1, 1, 1, 1); e <- c(1, -1, 1, -1, 1, -1, 1, -1)
#' M <- 0.4 + 1.5 * A + e; Y <- 1 + 2 * A + 3 * M + 0.5 * A * M
#' Tnie(A, M, Y)$estimate
#' @export
Tnie <- function(X, M, Y, C = NULL, a = 1, astar = 0) {
  f <- .med_fit(X, M, Y, C, "total_natural_indirect_effect")
  eff <- .med_effects(f$beta, f$theta, f$cbar, as.numeric(a), as.numeric(astar))
  c(eff, list(estimate = eff$tnie, a = as.numeric(a), astar = as.numeric(astar),
              n = f$n, method = "Total natural indirect effect"))
}
