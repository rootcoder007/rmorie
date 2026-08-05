# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sample-selection bound for a selectively observed outcome
#'
#' Heckman's selection model buys point identification with an exclusion
#' restriction and joint normality. Dropping both leaves the identified set
#' of Manski's worst-case bound: the observed part of the mean is known and
#' the unobserved part can sit anywhere in the support. Stratifying on a
#' discrete \code{X} and averaging the within-stratum intervals sharpens the
#' bound whenever selection rates differ by stratum, and reproduces the
#' pooled bound when they do not.
#'
#' Formula, per stratum: \code{[E(y | D = 1, x) P(D = 1 | x) +
#' y_0 P(D = 0 | x), E(y | D = 1, x) P(D = 1 | x) + y_1 P(D = 0 | x)]},
#' averaged with weights \code{P(x)}.
#'
#' @param y Outcome; entries with \code{D = 0} are unobserved and their
#'   recorded value is never used.
#' @param D Selection indicator, 1 when \code{y} is observed.
#' @param X Discrete stratum label, one per unit.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{p_observed}, \code{n_strata}, \code{n}.
#' @references Heckman, J. J. (1979). Sample selection bias as a
#'   specification error. Econometrica 47(1), 153-161.
#'   \doi{10.2307/1912352}. Manski, C. F. (2003). Partial Identification of
#'   Probability Distributions. Springer. Worst-case region as Theorem
#'   SIR-2.1, equation (2.2), of Molinari, F. (2021), Handbook of
#'   Econometrics 7A (arXiv:2004.11751 p. 12).
#' @export
Bnssel <- function(y, D, X) {
  z <- .bnd_yd(y, D, "Bnssel")
  xv <- unlist(X)
  n <- length(z$y)
  if (length(xv) != n) stop("Bnssel: X must have one value per unit")
  obs <- z$y[z$d == 1]
  if (length(obs) == 0L) stop("Bnssel: no observed outcome")
  y0 <- min(obs)
  y1 <- max(obs)
  grp <- unique(xv)
  lo <- 0
  hi <- 0
  for (g in grp) {
    sel <- xv == g
    ng <- sum(sel)
    dg <- z$d[sel]
    yg <- z$y[sel]
    n1 <- sum(dg == 1)
    p1 <- n1 / ng
    m1 <- if (n1 > 0L) sum(yg[dg == 1]) / n1 else 0
    a <- .bnd_wc_arm(m1, p1, y0, y1)
    lo <- lo + (ng / n) * a[1]
    hi <- hi + (ng / n) * a[2]
  }
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi),
             p_observed = length(obs) / n, n_strata = length(grp), n = n,
             method = "Sample-selection bound")
}
