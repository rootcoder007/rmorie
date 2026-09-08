# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mahalanobis depth of each row of x
#'
#' Formula: \eqn{MD(z) = 1/(1 + (z-\mu)' \Sigma^{-1} (z-\mu))}.  Depth
#' is 1 exactly at \eqn{\mu} and decreases monotonically outwards, so a
#' depth ordering is a centre-outward ordering; the map is affine
#' invariant because \eqn{\Sigma^{-1}} absorbs any non-singular linear
#' transform.  Defaults are the sample mean and the sample covariance
#' with divisor n-1, which makes the depth non-robust: one far outlier
#' inflates \eqn{\Sigma} and flattens every depth.
#'
#' @param x n by p matrix of points, one per row.
#' @param mu Centre; column means if NULL.
#' @param Sigma Scatter matrix; sample covariance if NULL.
#' @return List with \code{depth}, \code{estimate} (maximum depth),
#'   \code{deepest} (0-based row index), \code{d2}, \code{n}, \code{p},
#'   \code{method}.
#' @references Liu (1990), Annals of Statistics 18:405-414, introduces data depth as a
#' centre-outward ordering; the Mahalanobis form 1/(1+d^2) is that of Liu and Singh
#' (1993), JASA 88:252-260, using Mahalanobis (1936).  The Project Euclid PDF for Liu
#' (1990) could not be retrieved from this host (the fetch returned a 1.2 kB error page),
#' so this is the standard published form, anchored in the harness on depth(mu) = 1
#' exactly and on affine invariance -- neither of which depends on this code.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Mahaldep(V)
Mahaldep <- function(x, mu = NULL, Sigma = NULL) {
  X <- as.matrix(x)
  n <- nrow(X)
  p <- ncol(X)
  if (is.null(mu)) mu <- colMeans(X) else mu <- .t4_vec(mu)
  if (length(mu) != p) stop("mu must have one entry per column of x")
  if (is.null(Sigma)) {
    if (n < 2L) stop("need at least 2 rows to estimate Sigma")
    C <- sweep(X, 2, mu)
    S <- crossprod(C) / (n - 1)
  } else S <- as.matrix(Sigma)
  if (nrow(S) != p || ncol(S) != p) stop("Sigma must be p x p")
  Sinv <- solve(S)
  C <- sweep(X, 2, mu)
  d2 <- rowSums((C %*% Sinv) * C)
  dep <- 1 / (1 + d2)
  best <- which.max(dep)
  .t4_result(depth = dep, estimate = dep[best], deepest = as.integer(best - 1L),
             d2 = d2, n = as.integer(n), p = as.integer(p),
             method = "Mahalanobis depth")
}
