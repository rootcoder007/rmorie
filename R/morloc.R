# SPDX-License-Identifier: AGPL-3.0-or-later
#' Local Moran's I per location (re-export).
#'
#' Formula: see localmoran
#'
#' @param x Values at the n locations.
#' @param W Spatial weights.
#' @param mlvar Divide m2 by n rather than n-1.

#' @return List with the payload of :func:`morie.fn.lismor.localmoran`.
#' @references Anselin (1995), Local Indicators of Spatial Association -- LISA, Geographical Analysis 27(2):93-115, formula (12) p.99. The article is paywalled; the formula and the divide-by-n variance convention were taken from spdep::localmoran, the reference implementation, which cites that equation explicitly.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Lisamoran(V, V)
Lisamoran <- function(x, W, mlvar = TRUE) {
  Localmoran(x, W, mlvar = mlvar)
}
