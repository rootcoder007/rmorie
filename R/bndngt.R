# SPDX-License-Identifier: AGPL-3.0-or-later
#' Negative-only treatment bound (decreasing monotone treatment response)
#'
#' The mirror image of \code{Bndpos}. Assuming treatment never helps,
#' \code{y(1) <= y(0)} for every unit, pins the upper bound at exactly zero;
#' only the lower bound is estimated, and it uses every \code{(y, D)} pair.
#'
#' Formula: \code{lower = [E(y | D = 1) P(D = 1) + y_min P(D = 0)] -
#' [E(y | D = 0) P(D = 0) + y_max P(D = 1)]}, \code{upper = 0}, with
#' \code{y_max = max(y)}.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @param y_min Lower end of the logically possible support; at most
#'   \code{min(y)}.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{p_treated}, \code{n}.
#' @references Manski, C. F. (1997). Monotone treatment response.
#'   Econometrica 65(6), 1311-1334. \doi{10.2307/2171738}. Equation (2.13)
#'   of Molinari, F. (2021), Handbook of Econometrics 7A
#'   (arXiv:2004.11751 pp. 18-19), read with the treatment order reversed.
#' @export
Bndngt <- function(y, D, y_min) {
  z <- .bnd_yd(y, D, "Bndngt")
  ymin <- as.numeric(y_min)[1]
  y0 <- min(z$y)
  y1 <- max(z$y)
  if (ymin > y0) stop("Bndngt: y_min is above min(y)")
  cm <- .bnd_cellmeans(z$y, z$d)
  lo1 <- cm$m1 * cm$p1 + ymin * cm$p0
  hi0 <- cm$m0 * cm$p0 + y1 * cm$p1
  lo <- lo1 - hi0
  hi <- 0
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), p_treated = cm$p1,
             n = length(z$y), method = "Negative-only treatment bound")
}
