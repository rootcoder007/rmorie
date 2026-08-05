# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sigmoid basis transformation of a design matrix (ESL eq. 2.31)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Section 2.6.1, book p. 30 (PDF p. 49):
#' h_k(x) = 1 / (1 + exp(-x' beta_k)) (2.31), the sigmoid transformation
#' common to neural network models.  With the transformed features the
#' parameters theta of the expansion (2.30) are estimated by minimising
#' RSS(theta) = sum_i (y_i - f_theta(x_i))^2 (2.32), which is done when y is
#' supplied.  (2.31) carries no extra bias term and none is added.
#'
#' @param X N-by-p design.
#' @param B p-by-K matrix whose columns are the beta_k of (2.31).
#' @param y optional N-vector; fits theta by least squares when given.
#' @return list: estimate, h, theta, fitted, rss, n, p, K, method.
#' @examples
#' Sigbasis(cbind(c(0, 1)), cbind(c(1)))$h
#' @export
Sigbasis <- function(X, B, y = NULL) {
  Xm <- .s03mat(X)
  Bm <- .s03mat(B)
  n <- nrow(Xm)
  if (n == 0L) stop("sigbasis: X is empty")
  p <- ncol(Xm)
  if (nrow(Bm) != p) stop("sigbasis: B must have one row per column of X")
  K <- ncol(Bm)
  if (K == 0L) stop("sigbasis: B has no columns")
  H <- matrix(0, n, K)
  for (i in seq_len(n)) for (cc in seq_len(K)) {
    H[i, cc] <- .s03sigmoid(sum(Xm[i, ] * Bm[, cc]))
  }
  theta <- NULL
  fitted <- NULL
  rss <- NaN
  if (!is.null(y)) {
    yv <- .s03vec(y)
    if (length(yv) != n) stop("sigbasis: X and y must have the same number of rows")
    if (n < K) stop("sigbasis: fewer observations than basis functions")
    theta <- .s03lstsq(H, yv, 0)
    fitted <- as.numeric(.s03matvec(H, theta))
    rss <- sum((yv - fitted)^2)
  }
  list(estimate = H[1, 1], h = H, theta = theta, fitted = fitted, rss = rss,
       n = n, p = p, K = K,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eqs. (2.30)-(2.32)")
}
