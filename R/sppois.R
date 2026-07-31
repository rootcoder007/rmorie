# SPDX-License-Identifier: AGPL-3.0-or-later
#' Homogeneous Poisson process (HPP), the CSR reference model.
#'
#' Two defining properties (Sec 3.2.2): N(A) ~ Poisson(lambda nu(A)) for
#' any region A, and counts in DISJOINT regions are independent.
#' Conditional on N(A) = n the events are independently uniform over A,
#' which is exactly the binomial process of Sec 3.2.1 and is how an HPP
#' is simulated.
#'
#' @param lam Intensity, events per unit area; must be positive.
#' @param region c(xmin, ymin, xmax, ymax) or a matrix of vertices; the
#'   unit square when NULL.
#' @param seed Optional seed for the realisation.
#' @return Named list: points, n, lambda, area, expected_n, var_n
#'   (equal to expected_n, as the Poisson requires), region.
#' @references Schabenberger & Gotway (2005), Sec 3.2.2.
#' @examples
#' sppois(lam = 2, region = c(0, 0, 10, 10), seed = 1)$expected_n
#' @export
sppois <- function(lam = 1, region = NULL, seed = NULL) {
  if (lam <= 0) stop("`lam` must be > 0")
  reg <- if (is.null(region)) c(0, 0, 1, 1) else .sp_region(region)
  area <- (reg[3] - reg[1]) * (reg[4] - reg[2])
  if (!is.null(seed)) set.seed(seed)
  n <- stats::rpois(1, lam * area)
  pts <- cbind(stats::runif(n, reg[1], reg[3]), stats::runif(n, reg[2], reg[4]))
  list(points = pts, n = as.integer(n), lambda = as.numeric(lam), area = area,
       expected_n = as.numeric(lam) * area, var_n = as.numeric(lam) * area,
       region = reg)
}
