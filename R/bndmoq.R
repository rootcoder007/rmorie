# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sharp quantile bounds for a selectively observed outcome
#'
#' Quantiles behave better than means under selection. The mean bound is
#' informative only when the outcome support is bounded on the relevant
#' side; the quantile bound is informative whenever the observed fraction
#' exceeds \code{1 - alpha} (lower) or \code{alpha} (upper), regardless of
#' the range of \code{y}. Because both ends are quantiles of the observed
#' distribution, the bound is equivariant under any increasing transform.
#'
#' Formula, with \code{q(p)} the type-1 quantile among the observed and
#' \code{p_1 = P(D = 1)}: \code{lower = q(1 - (1 - alpha) / p_1)} if
#' \code{p_1 > 1 - alpha} and \code{y_0} otherwise;
#' \code{upper = q(alpha / p_1)} if \code{p_1 >= alpha} and \code{y_1}
#' otherwise.
#'
#' @param y Outcome; entries with \code{D = 0} are unobserved.
#' @param D Observation indicator, coded 0/1.
#' @param X Discrete stratum label, one per unit; used to report the widest
#'   within-stratum interval alongside the pooled one.
#' @param quantile Quantile level \code{alpha} in (0, 1).
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{max_width}, \code{n_strata}, \code{p_observed},
#'   \code{n}.
#' @section Note: the stub this replaced attributed the construction to
#'   Chernozhukov and Hansen (2005), which is the instrumental-variable
#'   quantile regression model, a different object. The bound evaluated
#'   here is Manski's and is cited as such.
#' @references Manski, C. F. (2003). Partial Identification of Probability
#'   Distributions. Springer, Section 1.3. The two expressions are
#'   \code{r(alpha, x)} and \code{s(alpha, x)} on pp. 12-13 of Molinari, F.
#'   (2021), Handbook of Econometrics 7A (arXiv:2004.11751).
#' @export
Bndmoq <- function(y, D, X, quantile) {
  z <- .bnd_yd(y, D, "Bndmoq")
  xv <- unlist(X)
  n <- length(z$y)
  if (length(xv) != n) stop("Bndmoq: X must have one value per unit")
  a <- as.numeric(quantile)[1]
  if (!(a > 0 && a < 1)) stop("Bndmoq: quantile must lie in (0, 1)")
  y0 <- min(z$y)
  y1 <- max(z$y)
  band <- function(ys, ds) {
    m <- length(ys)
    obs <- ys[ds == 1]
    p1 <- length(obs) / m
    if (length(obs) == 0L) return(c(y0, y1, p1))
    lo <- if (p1 > 1 - a) .bnd_q1(obs, 1 - (1 - a) / p1) else y0
    hi <- if (p1 >= a) .bnd_q1(obs, a / p1) else y1
    c(lo, hi, p1)
  }
  b <- band(z$y, z$d)
  grp <- unique(xv)
  mw <- 0
  for (g in grp) {
    sel <- xv == g
    bg <- band(z$y[sel], z$d[sel])
    if (bg[2] - bg[1] > mw) mw <- bg[2] - bg[1]
  }
  .t1_result(lower = b[1], upper = b[2], width = b[2] - b[1],
             estimate = 0.5 * (b[1] + b[2]), max_width = mw,
             n_strata = length(grp), p_observed = b[3], n = n,
             method = "Quantile-equivariant bound")
}
