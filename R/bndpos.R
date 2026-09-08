# SPDX-License-Identifier: AGPL-3.0-or-later
#' Positive-only treatment bound (increasing monotone treatment response)
#'
#' Assuming treatment never hurts, \code{y(1) >= y(0)} for every unit, pins
#' the lower bound at exactly zero and leaves only the upper bound to be
#' estimated. Unlike the worst-case bound, the arm bounds use every
#' \code{(y, D)} pair rather than only the pairs in the arm of interest, so
#' they stay informative even when one arm is nearly empty.
#'
#' Formula: \code{upper = [E(y | D = 1) P(D = 1) + y_max P(D = 0)] -
#' [E(y | D = 0) P(D = 0) + y_min P(D = 1)]}, \code{lower = 0}.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @param y_max Upper end of the logically possible support; at least
#'   \code{max(y)}.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{p_treated}, \code{n}.
#' @references Manski, C. F. (1997). Monotone treatment response.
#'   Econometrica 65(6), 1311-1334. \doi{10.2307/2171738}. Binary-treatment
#'   form as equation (2.13) and the paragraph beneath it in Molinari, F.
#'   (2021), Handbook of Econometrics 7A (arXiv:2004.11751 pp. 18-19).
#' @export
#' @examples
#' set.seed(1)
#' Bndpos(y = rnorm(40), D = rbinom(40, 1, 0.5), y_max = 3)
Bndpos <- function(y, D, y_max) {
  z <- .bnd_yd(y, D, "Bndpos")
  ymax <- as.numeric(y_max)[1]
  y0 <- min(z$y)
  y1 <- max(z$y)
  if (ymax < y1) stop("Bndpos: y_max is below max(y)")
  cm <- .bnd_cellmeans(z$y, z$d)
  hi1 <- cm$m1 * cm$p1 + ymax * cm$p0
  lo0 <- cm$m0 * cm$p0 + y0 * cm$p1
  lo <- 0
  hi <- hi1 - lo0
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), p_treated = cm$p1,
             n = length(z$y), method = "Positive-only treatment bound")
}
