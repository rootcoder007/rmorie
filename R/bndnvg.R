# SPDX-License-Identifier: AGPL-3.0-or-later
#' Naive gross bound on the average treatment effect
#'
#' The treated mean is taken at face value as \code{E[y(1)]} and nothing at
#' all is assumed about \code{E[y(0)]} beyond the observed support, so the
#' counterfactual mean is placed at each end of that support in turn. The
#' width of the interval is exactly the range of \code{y}, whatever the data
#' look like, which is the point: the bound shows how little the data alone
#' deliver.
#'
#' Formula: \code{[E(y | D = 1) - y_1, E(y | D = 1) - y_0]} with
#' \code{y_0 = min y} and \code{y_1 = max y}.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{p_treated}, \code{n}.
#' @references Manski, C. F. (1990). Nonparametric bounds on treatment
#'   effects. American Economic Review Papers and Proceedings 80(2),
#'   319-323. Restated as equation (2.11) of Molinari, F. (2021),
#'   Microeconometrics with partial identification, Handbook of Econometrics
#'   7A, 355-486 (arXiv:2004.11751 p. 17).
#' @export
#' @examples
#' set.seed(1)
#' r <- Bndnvg(y = rnorm(10), D = rbinom(10, 1, 0.5)); TRUE
Bndnvg <- function(y, D) {
  z <- .bnd_yd(y, D, "Bndnvg")
  cm <- .bnd_cellmeans(z$y, z$d)
  if (cm$p1 <= 0) stop("Bndnvg: no treated unit")
  y0 <- min(z$y)
  y1 <- max(z$y)
  lo <- cm$m1 - y1
  hi <- cm$m1 - y0
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), p_treated = cm$p1,
             n = length(z$y),
             method = "Naive gross treatment-effect bound")
}
