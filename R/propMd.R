# SPDX-License-Identifier: AGPL-3.0-or-later
#' Share of the total effect that runs through the mediator
#'
#' The measure reads as a proportion only when the two pieces point the
#' same way. With opposite signs the denominator is a difference of
#' magnitudes and the ratio can exceed one or go negative -- information,
#' not an error, so it is returned with a flag rather than clamped.
#'
#' Formula: \code{PM = NIE / (NIE + NDE)}.
#'
#' @param NIE Natural indirect effect.
#' @param NDE Natural direct effect.
#' @return List with \code{estimate}, \code{te}, \code{same_sign}.
#' @references VanderWeele, T. J. (2013). Epidemiology 24:175-176;
#'   VanderWeele (2015), Explanation in Causal Inference, section 2.7.
#' @export
#' @examples
#' PropMd(NIE = 5L, NDE = 5L)
PropMd <- function(NIE, NDE) {
  nie <- as.numeric(NIE); nde <- as.numeric(NDE)
  te <- nie + nde
  same <- as.numeric((nie >= 0) == (nde >= 0))
  .t1_result(estimate = if (te != 0) nie / te else NaN, te = te,
             same_sign = same, method = "Proportion mediated, NIE / TE")
}
