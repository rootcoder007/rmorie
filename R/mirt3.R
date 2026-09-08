# SPDX-License-Identifier: AGPL-3.0-or-later
#' Three-dimensional compensatory MIRT, no guessing parameter
#'
#' This module is an ALIAS. The response function is implemented once, in
#' \code{Mirt2}; the dimension is set by the length of \code{a}, not by
#' the module name, so the only thing this entry point adds is the M2PL
#' restriction \code{c = 0} and a check that exactly three slopes were
#' supplied.
#'
#' \code{P(x = 1 | theta) = 1 / (1 + exp[-D (a1 t1 + a2 t2 + a3 t3 + d)])}.
#'
#' @param y Binary 0/1 responses, length n.
#' @param theta Ability matrix, n by 3.
#' @param a Item slopes, length 3.
#' @param d Item intercept.
#' @param D Metric constant.
#' @return As \code{Mirt2}: estimate, loglik, p, pbar, deviance, n, m.
#' @references Chalmers (2012), Journal of Statistical Software 48(6),
#'   1-29, \doi{10.18637/jss.v048.i06}, eq. (1) p.3, with gamma = 0.
#' @export
Mirt3 <- function(y, theta, a, d, D = 1) {
  if (length(.t1_vec(a)) != 3L) {
    stop(paste("Mirt3 is the three-dimensional case; a must have exactly",
               "3 slopes (use Mirt2 for other dimensions)"))
  }
  Mirt2(y, theta, a, d, 0, D)
}
