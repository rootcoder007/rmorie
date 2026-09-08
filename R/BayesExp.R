# SPDX-License-Identifier: AGPL-3.0-or-later

#' Bayes theorem in explicit form
#'
#' P(A|Z) = P(Z|A)P(A) / [P(Z|A)P(A) + P(Z|~A)P(~A)].  The defaults are
#' the book's false-positive worked example: 2 percent prevalence, a 95
#' percent sensitive test, a 10 percent false-positive rate.
#'
#' @param p_a prior P(A), in [0, 1].
#' @param p_z_given_a likelihood P(Z | A), in [0, 1].
#' @param p_z_given_not_a likelihood P(Z | not A), in [0, 1].
#' @return list(posterior).
#' @references Morin, D. J. (2016). Probability: For the Enthusiastic
#'   Beginner. Createspace. Eqs. (2.52), (2.58)-(2.62).
#' @examples
#' BayesExp()$posterior
#' @export
BayesExp <- function(p_a = 0.02, p_z_given_a = 0.95, p_z_given_not_a = 0.1) {
  for (p in c(p_a, p_z_given_a, p_z_given_not_a)) {
    if (length(p) != 1L || is.na(p) || p < 0 || p > 1) {
      stop("probabilities must be single values in [0, 1].", call. = FALSE)
    }
  }
  num <- p_z_given_a * p_a
  den <- num + p_z_given_not_a * (1 - p_a)
  if (den == 0) {
    stop("P(Z) = 0: event Z impossible under both branches.", call. = FALSE)
  }
  list(posterior = num / den)
}
