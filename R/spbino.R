# SPDX-License-Identifier: AGPL-3.0-or-later
#' Binomial point process: a FIXED number of independent uniform points.
#'
#' The count is not random -- that is the whole difference from the
#' Poisson process. For any sub-region B, N(B) ~ Binomial(n, p) with
#' p = nu(B) / nu(A), so E\[N(B)\] = np and Var\[N(B)\] = np(1 - p). The
#' variance is SMALLER than the mean, whereas a Poisson process has them
#' equal; conditioning on the total removes that extra variability.
#' Conditioning an HPP on N(A) = n gives exactly this process
#' (Sec 3.2.3, process equivalence).
#'
#' @param n Number of events, fixed; must be non-negative.
#' @param region c(xmin, ymin, xmax, ymax) or vertices; unit square when NULL.
#' @param seed Optional seed for the realisation.
#' @return Named list: points, n, area, intensity, binomial_mean_half,
#'   binomial_var_half, counts_in_fraction (a function of p), region.
#' @references Schabenberger & Gotway (2005), Sec 3.2.1.
#' @examples
#' spbino(n = 200, region = c(0, 0, 10, 10), seed = 1)$binomial_var_half
#' @export
spbino <- function(n = 100, region = NULL, seed = NULL) {
  n <- as.integer(n)
  if (n < 0) stop("`n` must be >= 0")
  reg <- if (is.null(region)) c(0, 0, 1, 1) else .sp_region(region)
  area <- (reg[3] - reg[1]) * (reg[4] - reg[2])
  if (!is.null(seed)) set.seed(seed)
  pts <- cbind(stats::runif(n, reg[1], reg[3]), stats::runif(n, reg[2], reg[4]))
  counts_in_fraction <- function(p) c(mean = n * p, var = n * p * (1 - p))
  h <- counts_in_fraction(0.5)
  list(points = pts, n = n, area = area,
       intensity = if (area > 0) n / area else NA_real_,
       binomial_mean_half = unname(h[1]), binomial_var_half = unname(h[2]),
       counts_in_fraction = counts_in_fraction, region = reg)
}
