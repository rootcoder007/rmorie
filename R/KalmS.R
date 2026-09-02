# SPDX-License-Identifier: AGPL-3.0-or-later
#' Rauch-Tung-Striebel fixed-interval smoother
#'
#' A forward Kalman pass followed by the backward recursion.  With
#' Q = 0 and F = I the state is constant, so every smoothed state equals
#' the final filtered state -- an exact identity the tests use.
#'
#' Formula: C = P_t F' P_\{t+1|t\}^\{-1\};
#'   x_\{t|n\} = x_\{t|t\} + C (x_\{t+1|n\} - x_\{t+1|t\});
#'   P_\{t|n\} = P_\{t|t\} + C (P_\{t+1|n\} - P_\{t+1|t\}) C'.
#'
#' @param y Observation matrix, one row per time point.
#' @param F State transition matrix.
#' @param H Observation matrix.
#' @param Q State noise covariance.
#' @param R Observation noise covariance.
#' @param x0 Initial state mean; zero by default.
#' @param P0 Initial state covariance; the identity by default.
#' @param ridge Ridge added to the predicted covariance before solving.
#' @return List with \code{estimate}, \code{smoothed},
#'   \code{smoothed_cov}, \code{filtered}, \code{loglik}, \code{n},
#'   \code{method}.
#' @references Rauch, Tung and Striebel (1965), Maximum likelihood
#'   estimates of linear dynamic systems, AIAA Journal 3(8):1445-1450.
#'   \doi{10.2514/3.3166}
#' @export
#' @examples
#' set.seed(1)
#' y <- matrix(cumsum(rnorm(20)) + rnorm(20), 20, 1)
#' KalmS(y, F = matrix(1), H = matrix(1), Q = matrix(0.1), R = matrix(1))
KalmS <- function(y, F, H, Q, R, x0 = NULL, P0 = NULL, ridge = 1e-12) {
  Y <- .s03mat(y)
  n <- nrow(Y); m <- ncol(Y)
  if (n == 0L) stop("kalman_smoother: y is empty")
  Fm <- .s03mat(F); Hm <- .s03mat(H); Qm <- .s03mat(Q); Rm <- .s03mat(R)
  d <- nrow(Fm)
  if (nrow(Hm) != m || ncol(Hm) != d) stop("kalman_smoother: H must be m x d")
  x <- if (is.null(x0)) rep(0, d) else .s03vec(x0)
  P <- if (is.null(P0)) diag(1, d) else .s03mat(P0)
  xs <- list(); Ps <- list(); xp <- list(); Pp <- list(); ll <- 0
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
    xs[[t]] <- x; Ps[[t]] <- P; xp[[t]] <- xpred; Pp[[t]] <- Ppred
  }
  xsm <- xs; Psm <- Ps
  if (n > 1L) for (t in seq(n - 1L, 1L)) {
    A <- Pp[[t + 1L]] + diag(as.numeric(ridge), d)
    PF <- Ps[[t]] %*% t(Fm)
    C <- matrix(0, d, d)
    for (i in seq_len(d)) C[i, ] <- .s03cholsolve(A, PF[i, ])
    xsm[[t]] <- xs[[t]] + as.numeric(C %*% (xsm[[t + 1L]] - xp[[t + 1L]]))
    Psm[[t]] <- Ps[[t]] + C %*% (Psm[[t + 1L]] - Pp[[t + 1L]]) %*% t(C)
  }
  .t1_result(estimate = xsm[[1]][1], smoothed = xsm, smoothed_cov = Psm,
             filtered = xs, loglik = ll, n = n,
             method = "forward Kalman pass plus the RTS backward recursion, Rauch, Tung & Striebel (1965)")
}
