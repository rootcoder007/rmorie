# SPDX-License-Identifier: AGPL-3.0-or-later
#' The part of the effect that needs mediation and interaction at once
#'
#' This is the component neither literature could express alone. It is
#' non-zero only when the exposure both moves the mediator and interacts
#' with it in the outcome, so a single zero in either place kills it --
#' a sharp diagnostic rather than a residual catch-all.
#'
#' Formula: \code{INTmed = th3 b1 (a - a*)^2}.
#'
#' @param Y Outcome.
#' @param X Exposure.
#' @param M Mediator.
#' @param Cc Optional covariates; read at their means.
#' @param a,astar Exposure contrast levels.
#' @return List with \code{estimate}, \code{interaction},
#'   \code{mediator_shift}, \code{theta}, \code{beta}, \code{n}.
#' @references VanderWeele, T. J. (2014). Epidemiology 25:749-761,
#'   table 1.
#' @export
Medint <- function(Y, X, M, Cc = NULL, a = 1, astar = 0) {
  mm <- .s4_medmodels(Y, X, M, Cc)
  d <- a - astar
  .t1_result(estimate = mm$theta[4] * mm$beta[2] * d * d,
             interaction = mm$theta[4], mediator_shift = mm$beta[2] * d,
             theta = mm$theta, beta = mm$beta, n = length(as.numeric(Y)),
             method = "Mediated interaction INTmed")
}
