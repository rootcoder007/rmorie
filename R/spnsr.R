# SPDX-License-Identifier: AGPL-3.0-or-later
#' Effects of nugget, sill and range on a kriging prediction.
#'
#' Evaluated on a fixed one-dimensional layout so the effects are
#' exhibited rather than described. The NUGGET drives the weights toward
#' equality and pulls the prediction toward the mean; with a pure nugget
#' the prediction IS the mean. The SILL scales the kriging variance but
#' leaves the weights, and therefore the prediction, unchanged. The RANGE
#' controls how far influence extends.
#'
#' @param nugget Nugget effect, non-negative.
#' @param sill Partial sill, non-negative.
#' @param range Range, positive.
#' @param target_dist Distance from the prediction location to the
#'   nearest datum; defaults to 0.5.
#' @param model Correlogram family.
#' @return Named list: prediction, variance, weights, weight_spread, mean,
#'   nugget, sill, range, model.
#' @references Schabenberger & Gotway (2005), Sec 5.2.3, p. 228.
#' @examples
#' spnsr(nugget = 0.1, sill = 1, range = 1)
#' @export
spnsr <- function(nugget = 0, sill = 1, range = 1, target_dist = NULL,
                  model = "exponential") {
  if (nugget < 0 || sill < 0) stop("`nugget` and `sill` must be >= 0")
  if (range <= 0) stop("`range` must be > 0")
  d <- if (is.null(target_dist)) 0.5 else as.numeric(target_dist)
  coords <- matrix(c(0, 1, 2, 3, 4), ncol = 1)
  z <- c(1, 3, 2, 5, 4)
  cm <- list(nugget = nugget, sill = sill, range = range, model = model)
  r <- .sp_simple_kriging(coords, z, matrix(d, 1, 1), cm)
  w <- as.numeric(r$weights[, 1])
  list(prediction = r$prediction[1], variance = r$variance[1], weights = w,
       weight_spread = max(w) - min(w), mean = mean(z),
       nugget = nugget, sill = sill, range = range, model = model)
}
