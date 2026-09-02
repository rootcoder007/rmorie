# SPDX-License-Identifier: AGPL-3.0-or-later
#' Intersection bounds on the ATE under an exclusion restriction
#'
#' If the excluded variable \code{X} shifts who gets treated but not the
#' counterfactual means, then each value of \code{X} yields its own
#' worst-case interval for the same \code{E[y(t)]}, and the parameter must
#' lie in all of them at once. Intersecting is what makes the assumption
#' bite; it is also what makes it refutable, since an empty intersection
#' contradicts mean independence.
#'
#' Formula: \code{E[y(t)] in [max_x lower(x), min_x upper(x)]}, then the ATE
#' interval formed from the two intersected arms.
#'
#' @param y Observed outcome.
#' @param D Binary treatment indicator, coded 0/1.
#' @param X Discrete excluded variable, one value per unit.
#' @return List with \code{lower}, \code{upper}, \code{width},
#'   \code{estimate}, \code{n_cells}, \code{refuted}, \code{n}.
#' @references Manski, C. F. (1995). Identification Problems in the Social
#'   Sciences. Harvard University Press. Intersection-bound form as equation
#'   (2.15) of Molinari, F. (2021), Handbook of Econometrics 7A
#'   (arXiv:2004.11751 p. 19).
#' @export
#' @examples
#' set.seed(1)
#' r <- Bnstvr(y = rnorm(10), D = rbinom(10, 1, 0.5), X = rnorm(10)); TRUE
Bnstvr <- function(y, D, X) {
  z <- .bnd_yd(y, D, "Bnstvr")
  xv <- unlist(X)
  n <- length(z$y)
  if (length(xv) != n) stop("Bnstvr: X must have one value per unit")
  y0 <- min(z$y)
  y1 <- max(z$y)
  grp <- unique(xv)
  lo1 <- -Inf; hi1 <- Inf; lo0 <- -Inf; hi0 <- Inf
  for (g in grp) {
    sel <- xv == g
    cm <- .bnd_cellmeans(z$y[sel], z$d[sel])
    a1 <- .bnd_wc_arm(cm$m1, cm$p1, y0, y1)
    a0 <- .bnd_wc_arm(cm$m0, cm$p0, y0, y1)
    if (a1[1] > lo1) lo1 <- a1[1]
    if (a1[2] < hi1) hi1 <- a1[2]
    if (a0[1] > lo0) lo0 <- a0[1]
    if (a0[2] < hi0) hi0 <- a0[2]
  }
  refuted <- if (lo1 > hi1 || lo0 > hi0) 1 else 0
  lo <- lo1 - hi0
  hi <- hi1 - lo0
  .t1_result(lower = lo, upper = hi, width = hi - lo,
             estimate = 0.5 * (lo + hi), n_cells = length(grp),
             refuted = refuted, n = n,
             method = "Bound with treatment-variation assumption")
}
