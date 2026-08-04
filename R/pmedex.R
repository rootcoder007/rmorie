# SPDX-License-Identifier: AGPL-3.0-or-later
#' Indirect effect divided by an independently supplied total
#'
#' Not the same as the proportion mediated even when it prints the same
#' number: here the total is whatever the caller estimated, which need
#' not equal \code{NIE + NDE} when the two came from different models.
#' The implied direct effect is returned so the gap is visible.
#'
#' Formula: \code{PTE = NIE / TE}.
#'
#' @param nie Natural indirect effect.
#' @param te Total effect, estimated separately.
#' @return List with \code{estimate}, \code{implied_nde}, \code{te}.
#' @references VanderWeele, T. J. (2013). Epidemiology 24:175-176.
#' @export
Pmedex <- function(nie, te) {
  nie <- as.numeric(nie); te <- as.numeric(te)
  .t1_result(estimate = if (te != 0) nie / te else NaN,
             implied_nde = te - nie, te = te,
             method = "Proportion of the total effect explained")
}
