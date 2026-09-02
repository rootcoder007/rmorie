# SPDX-License-Identifier: AGPL-3.0-or-later
#' RTS backward recursion applied to an existing forward pass
#'
#' The backward half only: the filtered means and covariances are
#' supplied, so the smoother costs one sweep and no re-filtering.  The
#' last smoothed state always equals the last filtered state, which is
#' the identity that anchors the recursion.
#'
#' Formula: C = P_t F' P_\{t+1|t\}^\{-1\};
#'   x_\{t|n\} = x_\{t|t\} + C (x_\{t+1|n\} - x_\{t+1|t\}).
#'
#' @param y Observation matrix; only its length is used.
#' @param model Named list with entry F.
#' @param filtered Named list with state, cov, predicted, predicted_cov.
#' @param ridge Ridge added to the predicted covariance before solving.
#' @return List with \code{estimate}, \code{smoothed},
#'   \code{smoothed_cov}, \code{n}, \code{method}.
#' @references Rauch, Tung and Striebel (1965), AIAA Journal
#'   3(8):1445-1450. \doi{10.2514/3.3166}
#' @export
#' @examples
#' set.seed(1)
#' y <- matrix(cumsum(rnorm(20)) + rnorm(20), 20, 1)
#' model <- list(F = matrix(1), H = matrix(1), Q = matrix(0.1), R = matrix(1),
#'               x0 = 0, P0 = matrix(1))
#' filtered <- Klmflt(y, model)
#' Klmsmh(y, model, filtered)
Klmsmh <- function(y, model, filtered, ridge = 1e-12) {
  Y <- .s03mat(y)
  n <- nrow(Y)
  if (n == 0L) stop("kalman_smoother: y is empty")
  if (is.null(model$F)) stop("kalman_smoother: model is missing entry F")
  Fm <- .s03mat(model$F)
  d <- nrow(Fm)
  need <- function(k) {
    if (is.null(filtered[[k]])) stop(paste("kalman_smoother: model is missing entry", k))
    filtered[[k]]
  }
  xs <- lapply(need("state"), .s03vec)
  Ps <- lapply(need("cov"), .s03mat)
  xp <- lapply(need("predicted"), .s03vec)
  Pp <- lapply(need("predicted_cov"), .s03mat)
  if (length(xs) != n || length(Ps) != n || length(xp) != n || length(Pp) != n)
    stop("kalman_smoother: filtered quantities do not have n entries")
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
             n = n,
             method = "backward RTS pass over supplied filtered quantities, Rauch, Tung & Striebel (1965)")
}
