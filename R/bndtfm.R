# SPDX-License-Identifier: AGPL-3.0-or-later
#' Worst-case bounds for a monotonically transformed outcome
#'
#' Mean bounds are not invariant under a monotone transform: the
#' counterfactual mass is placed at the transformed support, which moves.
#' Quantile bounds are invariant, because both ends are quantiles of the
#' observed distribution and a monotone map commutes with the type-1
#' quantile. Both facts are reported: the mean bound on \code{t(y)} and the
#' equivariance gap of the median bound, exactly zero for increasing
#' \code{t}.
#'
#' Formula: worst-case bound of Molinari (2021) equation (2.11) applied to
#' \code{t(y)}; gap \code{= |t(r_y) - r_t| + |t(s_y) - s_t|} at the median.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @param X Discrete stratum label, one per unit; the mean bound is
#'   computed within stratum and averaged.
#' @param transform \code{t(y_i)} already evaluated, one value per unit,
#'   non-decreasing in \code{y}. Passing values rather than a function keeps
#'   the two language arms evaluating the same map.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{gap}, \code{n_strata}, \code{n}.
#' @references Chernozhukov, V., Lee, S. and Rosen, A. M. (2013).
#'   Intersection bounds: estimation and inference. Econometrica 81(2),
#'   667-737. \doi{10.3982/ECTA8718}, the stub's attribution. The
#'   invariance evaluated here is the quantile bound of Manski (2003)
#'   Section 1.3 as printed on pp. 12-13 of Molinari, F. (2021), Handbook
#'   of Econometrics 7A (arXiv:2004.11751); the mean bound is equation
#'   (2.11) of the same.
#' @export
#' @examples
#' set.seed(1)
#' y <- rnorm(40)
#' Bndtfm(y = y, D = rbinom(40, 1, 0.5), X = rbinom(40, 1, 0.5),
#'        transform = exp(y))
Bndtfm <- function(y, D, X, transform) {
  z <- .bnd_yd(y, D, "Bndtfm")
  xv <- unlist(X)
  tv <- as.numeric(unlist(transform))
  n <- length(z$y)
  if (length(xv) != n) stop("Bndtfm: X must have one value per unit")
  if (length(tv) != n)
    stop("Bndtfm: transform must have one value per unit")
  ord <- order(z$y)
  if (any(diff(tv[ord]) < 0))
    stop("Bndtfm: transform is not monotone in y")
  t0 <- min(tv)
  t1 <- max(tv)
  grp <- unique(xv)
  lo <- 0
  hi <- 0
  for (g in grp) {
    sel <- xv == g
    b <- .bnd_wc_ate(tv[sel], z$d[sel], t0, t1)
    w <- sum(sel) / n
    lo <- lo + w * b[1]
    hi <- hi + w * b[2]
  }
  obs_y <- z$y[z$d == 1]
  obs_t <- tv[z$d == 1]
  gap <- 0
  if (length(obs_y) > 0L) {
    p1 <- length(obs_y) / n
    for (lev in c(1 - 0.5 / p1, 0.5 / p1)) {
      if (lev > 0 && lev <= 1) {
        qy <- .bnd_q1(obs_y, lev)
        gap <- gap + abs(tv[match(qy, z$y)] - .bnd_q1(obs_t, lev))
      }
    }
  }
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), gap = gap,
             n_strata = length(grp), n = n,
             method = "Bound under outcome transformation")
}
