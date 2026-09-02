# SPDX-License-Identifier: AGPL-3.0-or-later
#' Gaussian semivariogram model
#'
#' gamma(h) = c0 + sigma0^2 (1 - exp(-3h^2/alpha^2)) for h > 0.
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
#' @references Schabenberger & Gotway (2005), Sec 4.3.2, eq (4.10), p. 143.
#' @examples
#' spgaus(h = c(0, 0.5, 1, 2), nugget = 0.1, sill = 1, range = 1)
#' @export
spgaus <- function(h, nugget = 0, sill = 1, range = 1) {
  g <- .sp_semivariogram(h, nugget, sill, range, "gaussian")
  list(gamma = g, nugget = as.numeric(nugget), sill = as.numeric(sill),
       range = as.numeric(range), model = "gaussian")
}
