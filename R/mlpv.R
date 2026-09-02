# SPDX-License-Identifier: AGPL-3.0-or-later
#' Pseudo-R^2 at level 1: residual variance removed by the predictors
#'
#' \code{PR = (sigma2_e(null) - sigma2_e(full)) / sigma2_e(null)}.
#'
#' The null model is the one-way random-effects ANOVA -- cluster
#' intercepts and nothing else -- and the full model adds the level-1
#' predictors \code{X} on top of those same intercepts. Both residual
#' variances are computed on the within-cluster (fixed-effects)
#' transformation, which sweeps the cluster means out of \code{y} and
#' \code{X}; that is what makes them comparable, since both are then
#' variances of the same deviations about the same intercepts.
#'
#' Degrees of freedom are \code{n - J} for the null model and
#' \code{n - J - p} for the full one, \code{J} being the number of
#' clusters. Using them, rather than dividing both by \code{n}, is what
#' stops a predictor from appearing to explain variance purely by being
#' counted.
#'
#' \code{PR} is NOT bounded below by zero. A negative value is a real and
#' informative outcome -- the added predictors cost more degrees of
#' freedom than they repaid -- and is reported as it comes out rather
#' than clamped.
#'
#' @param y Response, length n.
#' @param X Level-1 predictors without an intercept column, n by p.
#' @param cluster Cluster identifier per observation.
#' @return List with estimate (PR), pr, sigma2_null, sigma2_full,
#'   df_null, df_full, n_clusters, p, n.
#' @references Raudenbush and Bryk (2002), Hierarchical Linear Models,
#'   2nd ed., Sage, ch. 4, "proportion reduction in variance" at level 1.
#'   The book was not in the local corpus and could not be obtained; the
#'   quantity is implemented exactly as the ratio printed above, its
#'   standard published form, stated in full so it can be checked.
#' @export
#' @examples
#' Mlpv(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), cluster = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)))
Mlpv <- function(y, X, cluster) {
  v <- .t1_vec(y)
  n <- length(v)
  if (n == 0L) stop("y is empty")
  g <- as.character(unlist(cluster))
  if (length(g) != n) stop("y and cluster must have the same length")
  Xm <- as.matrix(X)
  if (nrow(Xm) != n) Xm <- t(Xm)
  if (nrow(Xm) != n) stop("X must have one row per observation")
  p <- ncol(Xm)
  ids <- unique(g)
  J <- length(ids)
  if (n - J - p <= 0) stop("not enough observations: n - J - p must be positive")
  sweep1 <- function(col) col - ave(col, g)
  yw <- sweep1(v)
  s2_null <- sum(yw^2) / (n - J)
  if (s2_null <= 0) {
    stop("the null model has no within-cluster variance; the ratio is undefined")
  }
  Xw <- apply(Xm, 2, sweep1)
  Xw <- matrix(Xw, nrow = n)
  resid <- .t1_lstsq(Xw, yw)$resid
  s2_full <- sum(resid^2) / (n - J - p)
  pr <- (s2_null - s2_full) / s2_null
  .t1_result(estimate = pr, pr = pr, sigma2_null = s2_null,
             sigma2_full = s2_full, df_null = n - J, df_full = n - J - p,
             n_clusters = J, p = p, n = n,
             method = "Proportional reduction in level-1 variance")
}
