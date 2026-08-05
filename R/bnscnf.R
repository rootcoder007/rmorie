# SPDX-License-Identifier: AGPL-3.0-or-later
#' Confidence set for a partially identified parameter (alias)
#'
#' "Confidence set for partial ID" and "inference for partially identified
#' parameters" name the same test inversion: the set of parameter values a
#' moment-inequality test fails to reject. This name forwards to
#' \code{Bndinf} rather than repeating the construction.
#'
#' @param theta_grid Candidate parameter values to test.
#' @param moments Interval data, an (n, 2) matrix.
#' @param alpha Miss probability, default 0.05.
#' @return The payload of \code{Bndinf}.
#' @references Imbens, G. W. and Manski, C. F. (2004). Confidence intervals
#'   for partially identified parameters. Econometrica 72(6), 1845-1857 --
#'   the stub's attribution; the set reported is the criterion level set of
#'   Chernozhukov, Hong and Tamer (2007) as given in equation (4.10) of
#'   Molinari, F. (2021), Handbook of Econometrics 7A (arXiv:2004.11751
#'   p. 97).
#' @export
Bnscnf <- function(theta_grid, moments, alpha = 0.05) {
  r <- Bndinf(theta_grid, moments, alpha)
  r$method <- "Confidence set for partial ID"
  r
}
