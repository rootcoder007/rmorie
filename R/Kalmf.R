# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kalman filter for the linear Gaussian state space model
#'
#' The recursion alternates a prediction and an update, and the
#' prediction error decomposition gives the exact log-likelihood as a
#' by-product.  With Q = 0, F = I, H = I and P0 = R the filtered state
#' is exactly the running mean of the prior mean and the observations so
#' far, which is the closed form the tests check.
#'
#' Formula: x_pred = F x; P_pred = F P F' + Q; S = H P_pred H' + R;
#'   K = P_pred H' S^\{-1\}; x = x_pred + K (y - H x_pred);
#'   P = (I - K H) P_pred.
#'
#' @param y Observation matrix, one row per time point.
#' @param F State transition matrix.
#' @param H Observation matrix.
#' @param Q State noise covariance.
#' @param R Observation noise covariance.
#' @param x0 Initial state mean; zero by default.
#' @param P0 Initial state covariance; the identity by default.
#' @return List with \code{estimate}, \code{state}, \code{cov},
#'   \code{predicted}, \code{predicted_cov}, \code{loglik}, \code{n},
#'   \code{method}.
#' @references Kalman (1960), A new approach to linear filtering and
#'   prediction problems, Transactions of the ASME, Journal of Basic
#'   Engineering 82(1):35-45. \doi{10.1115/1.3662552}
#' @export
#' @examples
#' set.seed(1)
#' y <- matrix(cumsum(rnorm(20)) + rnorm(20), 20, 1)
#' Kalmf(y, F = matrix(1), H = matrix(1), Q = matrix(0.1), R = matrix(1))
Kalmf <- function(y, F, H, Q, R, x0 = NULL, P0 = NULL) {
  Y <- .s03mat(y)
  n <- nrow(Y)
  m <- ncol(Y)
  if (n == 0L) stop("kalman_filter: y is empty")
  Fm <- .s03mat(F)
  Hm <- .s03mat(H)
  Qm <- .s03mat(Q)
  Rm <- .s03mat(R)
  d <- nrow(Fm)
  if (ncol(Fm) != d) stop("kalman_filter: F must be square")
  if (nrow(Hm) != m || ncol(Hm) != d) stop("kalman_filter: H must be m x d")
  if (nrow(Qm) != d || nrow(Rm) != m) stop("kalman_filter: Q must be d x d and R m x m")
  x <- if (is.null(x0)) rep(0, d) else .s03vec(x0)
  P <- if (is.null(P0)) diag(1, d) else .s03mat(P0)
  if (length(x) != d || nrow(P) != d) stop("kalman_filter: x0 and P0 must match the state dimension")
  xs <- list()
  Ps <- list()
  xp <- list()
  Pp <- list()
  ll <- 0
  for (t in seq_len(n)) {
    xpred <- as.numeric(Fm %*% x)
    Ppred <- Fm %*% P %*% t(Fm) + Qm
    HP <- Hm %*% Ppred
    S <- HP %*% t(Hm) + Rm
    v <- Y[t, ] - as.numeric(Hm %*% xpred)
    K <- matrix(0, d, m)
    for (j in seq_len(d)) K[j, ] <- .s03cholsolve(S, HP[, j])
    x <- xpred + as.numeric(K %*% v)
    P <- Ppred - (K %*% Hm) %*% Ppred
    sv <- .s03cholsolve(S, v)
    L <- .s03chol(S)
    ll <- ll - 0.5 * (m * log(2 * pi) + 2 * sum(log(diag(L))) + sum(v * sv))
    xs[[t]] <- x
    Ps[[t]] <- P
    xp[[t]] <- xpred
    Pp[[t]] <- Ppred
  }
  .t1_result(estimate = xs[[n]][1], state = xs, cov = Ps, predicted = xp,
             predicted_cov = Pp, loglik = ll, n = n,
             method = "predict/update recursion of Kalman (1960) with the prediction error decomposition")
}
