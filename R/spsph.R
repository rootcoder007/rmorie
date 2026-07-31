# SPDX-License-Identifier: AGPL-3.0-or-later
#' Spherical semivariogram model.
#'
#' gamma(h) = c0 + sigma0^2 (3h/(2 alpha) - (h/alpha)^3 / 2) for 0 < h <= alpha; the sill is reached exactly at alpha (a TRUE range).
#'
#' `range` is the PRACTICAL range in the book's parameterisation: the lag
#' at which correlation has fallen to exp(-3) = 0.049787.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param nugget Nugget effect c0. A discontinuity AT the origin, so
#'   gamma(0) is 0 even when nugget > 0.
#' @param sill Partial sill sigma0^2. Total sill is nugget + sill.
#' @param range Practical range alpha, positive.
#' @return Named list: gamma, nugget, sill, range, model.
#' @references Schabenberger & Gotway (2005), Sec 4.3.3, eqs (4.13), (4.15), pp. 146-147.
#' @examples
#' spsph(h = c(0, 0.5, 1, 2), nugget = 0.1, sill = 1, range = 1)
#' @export
spsph <- function(h, nugget = 0, sill = 1, range = 1) {
  g <- .sp_semivariogram(h, nugget, sill, range, "spherical")
  list(gamma = g, nugget = as.numeric(nugget), sill = as.numeric(sill),
       range = as.numeric(range), model = "spherical")
}
