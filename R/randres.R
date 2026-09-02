# SPDX-License-Identifier: AGPL-3.0-or-later
#' Recover a population rate from deliberately noised answers
#'
#' The respondent answers one of two questions chosen by a private
#' randomiser, so no individual answer reveals anything -- which is what
#' makes people answer honestly. The rate is still identified because the
#' noise mechanism is known. The estimator blows up as p nears one half,
#' where the answer carries no information at all.
#'
#' Formula: \code{pi = (lambda - (1 - p))/(2p - 1)},
#' \code{Var = lambda(1 - lambda)/(n (2p - 1)^2)}.
#'
#' @param y Observed yes/no answers.
#' @param truth True statuses when known; reported only.
#' @param p Probability the randomiser selected the sensitive question.
#' @return List with \code{estimate}, \code{se}, \code{lambda},
#'   \code{truth_rate}, \code{n}.
#' @references Warner, S. L. (1965). JASA 60:63-69, equations (1), (3).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Randres(V)
Randres <- function(y, truth = NULL, p = 0.7) {
  v <- as.numeric(unlist(y))
  n <- length(v)
  lam <- sum(v) / n
  p <- as.numeric(p)
  d <- 2 * p - 1
  pi_ <- if (d != 0) (lam - (1 - p)) / d else NaN
  var <- if (d != 0) lam * (1 - lam) / (n * d * d) else NaN
  tr <- if (is.null(truth)) NaN else sum(as.numeric(truth)) / n
  .t1_result(estimate = pi_, se = if (!is.na(var) && var >= 0) sqrt(var) else NaN,
             lambda = lam, truth_rate = tr, n = n,
             method = "Warner randomized response estimator")
}
