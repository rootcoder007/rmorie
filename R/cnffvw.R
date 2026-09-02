# SPDX-License-Identifier: AGPL-3.0-or-later

#' Cinelli-Hazlett sensitivity under a hypothesised confounder
#'
#' Formula: adjusted estimate under hypothesised confounder R2_Y * R2_D
#'
#' \code{cnffvw} and \code{chzlt} document the SAME method.  Rather than
#' carry a second implementation -- which would agree with the first at
#' 1e-9 forever while doubling the surface -- this function forwards to
#' \code{\link{Chzlt}} with the argument names of its own stub.
#'
#' @param y Outcome.
#' @param D Treatment.
#' @param X Observed covariates, or NULL.
#' @param R2_Y Hypothesised partial R2 with the outcome.
#' @param R2_D Hypothesised partial R2 with the treatment.
#' @param q Fraction of the estimate the confounder would explain.
#' @return Whatever \code{\link{Chzlt}} returns.
#' @references Cinelli & Hazlett (2020), Making Sense of Sensitivity,
#'   JRSS B 82(1):39-67.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Cnffvw(V, V)
Cnffvw <- function(y, D, X = NULL, R2_Y = 0, R2_D = 0, q = 1) {
  Chzlt(y, D, X, R2_Y, R2_D, q)
}
