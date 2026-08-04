# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fraction of explained variance attributable to the mediator
#'
#' A variance share is not a causal quantity and says nothing about
#' direction: it would report the same number if the arrow between
#' mediator and outcome ran the other way. It is a descriptive complement
#' to a coefficient-based proportion mediated.
#'
#' Formula: \code{R2_med = (R2_full - R2_partial) / R2_full}.
#'
#' @param r2_full R-squared including the mediator.
#' @param r2_partial R-squared without it.
#' @return List with \code{estimate}, \code{delta_r2}, \code{r2_full}, \code{r2_partial}.
#' @references de Heus, P. (2012). Behavior Research Methods 44:213-221.
#' @export
Vrmed <- function(r2_full, r2_partial) {
  rf <- as.numeric(r2_full); rp <- as.numeric(r2_partial)
  .t1_result(estimate = if (rf != 0) (rf - rp) / rf else NaN,
             delta_r2 = rf - rp, r2_full = rf, r2_partial = rp,
             method = "Variance-based mediation share")
}
