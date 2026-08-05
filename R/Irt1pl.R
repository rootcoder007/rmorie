# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rasch one-parameter logistic model
#'
#' The single item parameter is what gives the model its defining
#' property: the raw score is a sufficient statistic for ability.
#'
#' Formula: P(X = 1) = exp(theta - b) / (1 + exp(theta - b)).
#'
#' @param y Observed 0/1 responses.
#' @param theta Person abilities, same length as y.
#' @param b Item difficulty.
#' @return List with \code{estimate} (mean success probability),
#'   \code{p}, \code{information}, \code{loglik}, \code{b}, \code{n},
#'   \code{method}.
#' @references Rasch (1960), Probabilistic Models for Some Intelligence
#'   and Attainment Tests, Danmarks Paedagogiske Institut.
#' @export
Irt1pl <- function(y, theta, b) {
  ys <- as.integer(.s03vec(y))
  th <- .s03vec(theta)
  if (length(ys) == 0L) stop("rasch_one_parameter: y is empty")
  if (length(th) != length(ys)) stop("rasch_one_parameter: y and theta have different lengths")
  if (any(!(ys %in% c(0L, 1L)))) stop("rasch_one_parameter: responses must be 0 or 1")
  bv <- as.numeric(b)
  p <- vapply(th - bv, .s03sigmoid, 0)
  ll <- sum(ifelse(ys == 1L, log(p), log(1 - p)))
  .t1_result(estimate = mean(p), p = p, information = p * (1 - p),
             loglik = ll, b = bv, n = length(ys),
             method = "P = exp(theta - b)/(1 + exp(theta - b)), Rasch (1960)")
}
