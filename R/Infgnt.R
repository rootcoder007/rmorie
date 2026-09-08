# SPDX-License-Identifier: AGPL-3.0-or-later
#' Fisher information metric of a parametric family
#'
#' The Fisher information is the Riemannian metric on the parameter
#' manifold.  The expectation is taken by summing over a discrete
#' support or by the midpoint rule on a continuous grid, and the score
#' is a central difference of the log density.  For N(theta, sigma^2)
#' the metric is 1/sigma^2 and for Bernoulli(theta) it is
#' 1/(theta(1 - theta)); both closed forms are what the tests check.
#'
#' Formula: g_ij(theta) = E\[d_i log p d_j log p\].
#'
#' @param log_p Function log_p(x, theta).
#' @param theta Parameter value.
#' @param support Sample points, or midpoints of a continuous grid.
#' @param discrete TRUE sums over the support, FALSE applies the
#'   midpoint rule.
#' @param h Central-difference step for the score.
#' @return List with \code{estimate}, \code{metric}, \code{total_mass},
#'   \code{n}, \code{method}.
#' @references Amari (1985), Differential-Geometrical Methods in
#'   Statistics, Lecture Notes in Statistics 28, Springer.
#'   \doi{10.1007/978-1-4612-5056-2}
#' @export
Infgnt <- function(log_p, theta, support, discrete = TRUE, h = 1e-5) {
  th <- .s03vec(theta)
  d <- length(th)
  if (d == 0L) stop("information_geometry: theta is empty")
  if (!is.function(log_p)) stop("information_geometry: log_p must be callable")
  xs <- .s03vec(support)
  if (length(xs) < 2L) stop("information_geometry: support needs at least two points")
  hv <- as.numeric(h)
  if (hv <= 0) stop("information_geometry: h must be positive")
  dx <- if (isTRUE(discrete)) 1 else (xs[2] - xs[1])
  G <- matrix(0, d, d)
  total <- 0
  for (x in xs) {
    w <- exp(as.numeric(log_p(x, th))) * dx
    total <- total + w
    sc <- numeric(d)
    for (j in seq_len(d)) {
      tp <- th
      tm <- th
      tp[j] <- tp[j] + hv
      tm[j] <- tm[j] - hv
      sc[j] <- (as.numeric(log_p(x, tp)) - as.numeric(log_p(x, tm))) / (2 * hv)
    }
    G <- G + w * (sc %o% sc)
  }
  .t1_result(estimate = G[1, 1], metric = G, total_mass = total, n = length(xs),
             method = "g_ij = E[d_i log p d_j log p] by quadrature over the support, Amari (1985)")
}
