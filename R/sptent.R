# SPDX-License-Identifier: AGPL-3.0-or-later
#' Tent semivariogram model
#'
#' The tent correlation R1(h) = 1 - h/alpha for h <= alpha, zero beyond,
#' is the member of the spherical family valid in R^1; gamma(h) = c0 +
#' sigma0^2 h/alpha up to the true range alpha. It is gstat's "Lin" model
#' with a range, and a valid covariance only in one dimension.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param nugget Nugget effect c0. A discontinuity AT the origin, so
#'   gamma(0) is 0 even when nugget > 0.
#' @param sill Partial sill sigma0^2. Total sill is nugget + sill.
#' @param range True range alpha, where the correlation reaches zero.
#' @return Named list: gamma, nugget, sill, range, model.
#' @references Schabenberger & Gotway (2005), Sec 4.3.3, p. 146.
#' @examples
#' sptent(h = c(0, 0.5, 1, 2), nugget = 0.1, sill = 1, range = 1)
#' @export
sptent <- function(h, nugget = 0, sill = 1, range = 1) {
  g <- .sp_semivariogram(h, nugget, sill, range, "tent")
  list(gamma = g, nugget = as.numeric(nugget), sill = as.numeric(sill),
       range = as.numeric(range), model = "tent")
}
