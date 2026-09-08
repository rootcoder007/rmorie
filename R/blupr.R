# SPDX-License-Identifier: AGPL-3.0-or-later
#' BLUP of a random intercept
#'
#' uhat_j = (s2u / (s2u + s2e/n_j)) * mean_j(y - X beta).
#'
#' @param y Response.
#' @param group Group label per observation.
#' @param s2u Between-group variance, non-negative.
#' @param s2e Within-group variance, strictly positive.
#' @param X Fixed-effect design, or NULL.
#' @param beta Fixed-effect coefficients, required when X is given.
#'
#' @return List with u, shrink, nj, groupmean, levels, vpc, J, n.
#' @references Henderson (1975), Biometrics 31(2), 423-447; Robinson
#'   (1991), Statistical Science 6(1), 15-32.  Standard published form;
#'   neither article is in the local corpus and neither was read.
#' @export
#' @examples
#' set.seed(1)
#' Blupint(y = rnorm(30), group = rep(1:5, each = 6), s2u = 1, s2e = 1)
Blupint <- function(y, group, s2u, s2e, X = NULL, beta = NULL) {
  y <- .t1_vec(y)
  n <- length(y)
  g <- as.character(group)
  if (length(g) != n) stop("group must have one label per observation")
  s2u <- as.numeric(s2u)
  s2e <- as.numeric(s2e)
  if (s2u < 0) stop("s2u must be non-negative")
  if (s2e <= 0) stop("s2e must be strictly positive")
  if (is.null(X)) {
    r <- y
  } else {
    Xm <- .t1_mat(X)
    if (nrow(Xm) != n) stop("X must have one row per observation")
    b <- .t1_vec(beta)
    if (length(b) != ncol(Xm)) stop("beta must have one entry per column of X")
    r <- y - as.numeric(Xm %*% b)
  }
  labs <- unique(g)
  nj <- integer(0)
  gm <- numeric(0)
  u <- numeric(0)
  sh <- numeric(0)
  for (L in labs) {
    idx <- which(g == L)
    m <- length(idx)
    mean <- sum(r[idx]) / m
    k <- s2u / (s2u + s2e / m)
    nj <- c(nj, m)
    gm <- c(gm, mean)
    sh <- c(sh, k)
    u <- c(u, k * mean)
  }
  .t1_result(u = u, shrink = sh, nj = nj, groupmean = gm, levels = labs,
             vpc = s2u / (s2u + s2e), J = length(labs), n = n,
             method = "BLUP of a random intercept (Henderson 1975; Robinson 1991)")
}
