# SPDX-License-Identifier: AGPL-3.0-or-later
#' BLUP of random intercept and slope
#'
#' vhat_j = D Z_j' (Z_j D Z_j' + s2e I)^\{-1\} (y_j - X_j beta).
#'
#' @param y Response.
#' @param group Group label per observation.
#' @param Z Random-effect design, n x q.
#' @param D Covariance of the random coefficients, q x q.
#' @param s2e Residual variance, strictly positive.
#' @param X Fixed-effect design, or NULL.
#' @param beta Fixed-effect coefficients, required when X is given.
#'
#' @return List with v, levels, nj, fitted, J, q, n.
#' @references Henderson (1975), Biometrics 31(2), 423-447; Robinson
#'   (1991), Statistical Science 6(1), 15-32.  Standard published form;
#'   neither article is in the local corpus and neither was read.
#' @export
#' @examples
#' set.seed(1)
#' Blupslope(y = rnorm(30), group = rep(1:5, each = 6), Z = cbind(1, rnorm(30)),
#'           D = diag(2), s2e = 1)
Blupslope <- function(y, group, Z, D, s2e, X = NULL, beta = NULL) {
  y <- .t1_vec(y)
  n <- length(y)
  g <- as.character(group)
  if (length(g) != n) stop("group must have one label per observation")
  Zm <- .t1_mat(Z)
  if (nrow(Zm) != n) stop("Z must have one row per observation")
  q <- ncol(Zm)
  Dm <- .t1_mat(D)
  if (nrow(Dm) != q || ncol(Dm) != q) stop("D must be q by q")
  s2e <- as.numeric(s2e)
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
  V <- list()
  nj <- integer(0)
  fit <- numeric(n)
  for (L in labs) {
    idx <- which(g == L)
    m <- length(idx)
    nj <- c(nj, m)
    Zj <- Zm[idx, , drop = FALSE]
    M <- Zj %*% Dm %*% t(Zj) + s2e * diag(m)
    dimnames(M) <- NULL
    w <- as.numeric(solve(M, r[idx]))
    vj <- as.numeric(Dm %*% t(Zj) %*% w)
    V[[length(V) + 1L]] <- vj
    fit[idx] <- as.numeric(Zj %*% vj)
  }
  .t1_result(v = V, levels = labs, nj = nj, fitted = fit,
             J = length(labs), q = q, n = n,
             method = "BLUP of random coefficients (Henderson 1975; Robinson 1991)")
}
