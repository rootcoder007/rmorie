# SPDX-License-Identifier: AGPL-3.0-or-later
#' Circular semivariogram model
#'
#' The circular correlation R2(h) = (2/pi) (acos(h/alpha) - (h/alpha)
#' sqrt(1 - h^2/alpha^2)) for h <= alpha, zero beyond, is the member of the
#' spherical family valid in R^2; gamma(h) = c0 + sigma0^2 (1 - R2(h)). It
#' is gstat's "Cir" model.
#'
#' @param h Numeric vector of non-negative lag distances.
#' @param nugget Nugget effect c0. A discontinuity AT the origin, so
#'   gamma(0) is 0 even when nugget > 0.
#' @param sill Partial sill sigma0^2. Total sill is nugget + sill.
#' @param range True range alpha, where the correlation reaches zero.
#' @return Named list: gamma, nugget, sill, range, model.
#' @references Schabenberger & Gotway (2005), Sec 4.3.3, p. 146.
#' @examples
#' spcirc(h = c(0, 0.5, 1, 2), nugget = 0.1, sill = 1, range = 1)
#' @export
spcirc <- function(h, nugget = 0, sill = 1, range = 1) {
  g <- .sp_semivariogram(h, nugget, sill, range, "circular")
  list(gamma = g, nugget = as.numeric(nugget), sill = as.numeric(sill),
       range = as.numeric(range), model = "circular")
}
