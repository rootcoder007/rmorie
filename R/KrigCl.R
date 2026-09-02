# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ordinary kriging (alias of Krig)
#'
#' The same estimator as \code{\link{Krig}} and \code{\link{Krigsm}};
#' delegates rather than carrying a second copy of the solver.
#'
#' @inheritParams Krig
#' @return As \code{\link{Krig}}.
#' @references Matheron (1963), Principles of geostatistics, Economic
#'   Geology 58(8):1246-1266, \doi{10.2113/gsecongeo.58.8.1246}.
#' @export
#' @examples
#' KrigCl(known_coords = c(1, 2, 3, 4, 5, 6, 7, 8), known_values = c(1, 2, 3, 4, 5, 6, 7, 8), predict_coords = c(1, 2, 3, 4, 5, 6, 7, 8))
KrigCl <- function(known_coords, known_values, predict_coords,
                   nugget = 0, sill = 1, range_ = 1) {
  Krig(known_coords, known_values, predict_coords, nugget, sill, range_)
}
