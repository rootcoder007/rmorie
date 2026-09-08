# SPDX-License-Identifier: AGPL-3.0-or-later
#' Single-layer feed-forward network as an adaptive basis expansion (ESL 2.45)
#'
#' Hastie, Tibshirani and Friedman (2009), The Elements of Statistical
#' Learning, 2nd ed., Springer, Section 2.8.3, book p. 36 (PDF p. 55):
#' f_theta(x) = sum_m beta_m sigma(alpha_m' x + b_m) (2.45), where
#' sigma(x) = 1/(1 + e^-x) is the activation function.
#'
#' With alpha and b fixed this is a linear basis expansion, so given y the
#' output weights beta follow in closed form by least squares on the hidden
#' activations; alpha and b are inputs rather than being invented.
#'
#' @param X N-by-p design.
#' @param alpha p-by-M matrix whose columns are the directions alpha_m.
#' @param b M-vector of bias terms.
#' @param beta optional M-vector of output weights.
#' @param y optional N-vector; refits beta by least squares.
#' @return list: estimate, fitted, hidden, beta, rss, n, p, M, method.
#' @examples
#' Nnet1lay(cbind(c(0, 0)), cbind(c(1)), 0, beta = 2)$fitted
#' @export
Nnet1lay <- function(X, alpha, b, beta = NULL, y = NULL) {
  Xm <- .s03mat(X)
  Am <- .s03mat(alpha)
  bv <- .s03vec(b)
  n <- nrow(Xm)
  if (n == 0L) stop("nnet1lay: X is empty")
  p <- ncol(Xm)
  if (nrow(Am) != p) stop("nnet1lay: alpha must have one row per column of X")
  M <- ncol(Am)
  if (M == 0L) stop("nnet1lay: alpha has no columns")
  if (length(bv) != M) stop("nnet1lay: b must have one entry per hidden unit")
  Z <- matrix(0, n, M)
  for (i in seq_len(n)) for (m in seq_len(M)) {
    Z[i, m] <- .s03sigmoid(sum(Xm[i, ] * Am[, m]) + bv[m])
  }
  if (!is.null(y)) {
    yv <- .s03vec(y)
    if (length(yv) != n) stop("nnet1lay: X and y must have the same number of rows")
    if (n < M) stop("nnet1lay: fewer observations than hidden units")
    bw <- .s03lstsq(Z, yv, 0)
  } else if (!is.null(beta)) {
    bw <- .s03vec(beta)
    if (length(bw) != M) stop("nnet1lay: beta must have one entry per hidden unit")
  } else {
    stop("nnet1lay: supply beta, or y to fit it")
  }
  fitted <- as.numeric(.s03matvec(Z, bw))
  rss <- NaN
  if (!is.null(y)) rss <- sum((.s03vec(y) - fitted)^2)
  list(estimate = fitted[1], fitted = fitted, hidden = Z, beta = bw, rss = rss,
       n = n, p = p, M = M,
       method = "Hastie-Tibshirani-Friedman (2009) ESL eq. (2.45)")
}
