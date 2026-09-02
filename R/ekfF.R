# SPDX-License-Identifier: AGPL-3.0-or-later

#' Extended Kalman filter
#'
#' Formula: linearize f, h via Jacobians at the current state, then run
#' the linear Kalman recursions on the linearisation.
#'
#' \preformatted{
#'   predict   x- = f(x),        P- = F P F' + Q
#'   gain      S  = H P- H' + R, K  = P- H' / S
#'   update    x  = x- + K (y - h(x-)),  P = P- - K S K'
#' }
#'
#' Scalar observations, d-dimensional state.  \code{f} and \code{h} are
#' the (possibly nonlinear) transition and observation maps; \code{F} and
#' \code{H} are their Jacobians, supplied either as functions of the
#' state or as constant matrices, in which case the filter degenerates to
#' the plain linear Kalman filter.
#'
#' @param y Observation sequence (scalars).
#' @param f State transition x_t = f(x_\{t-1\}).
#' @param h Observation map y_t = h(x_t); returns a scalar.
#' @param F d x d Jacobian df/dx at the state.
#' @param H Length-d Jacobian dh/dx at the state.
#' @param Q d x d state noise covariance.
#' @param R Observation noise variance (> 0).
#' @param x0 Initial state mean (default zeros).
#' @param P0 Initial state covariance (default identity).
#' @return List with \code{estimate}, \code{state}, \code{cov},
#'   \code{loglik}, \code{n}, \code{method}.
#' @references Schmidt (1966), in Leondes (ed.), Advances in Control
#'   Systems 3:293-340; Jazwinski (1970), Stochastic Processes and
#'   Filtering Theory, Academic Press.
#' @export
#' @examples
#' set.seed(1)
#' y <- matrix(cumsum(rnorm(20)), 20, 1)
#' EkfF(y, f = function(x) x, h = function(x) x, F = matrix(1), H = matrix(1),
#'      Q = matrix(0.1), R = matrix(1))
EkfF <- function(y, f, h, F, H, Q, R, x0 = NULL, P0 = NULL) {
  .ekf_mat <- function(A) {
    if (is.matrix(A)) return(matrix(as.numeric(A), nrow(A), ncol(A)))
    A <- as.numeric(A)
    matrix(A, nrow = length(A), ncol = 1L)
  }
  .ekf_apply <- function(g, x) if (is.function(g)) g(x) else g
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) stop("empty input: y has no observations")
  Q <- .ekf_mat(Q)
  d <- nrow(Q)
  if (ncol(Q) != d) stop("Q must be square")
  R <- as.numeric(R)
  if (R <= 0) stop("R must be positive")
  x <- if (is.null(x0)) numeric(d) else as.numeric(x0)
  if (length(x) != d) stop("x0 length must match Q")
  P <- if (is.null(P0)) diag(1, d) else .ekf_mat(P0)
  if (nrow(P) != d || ncol(P) != d) stop("P0 must be d x d")
  loglik <- 0
  for (t in seq_len(n)) {
    xp <- as.numeric(.ekf_apply(f, x))
    Fk <- .ekf_mat(.ekf_apply(F, x))
    FP <- matrix(0, d, d)
    for (i in seq_len(d)) for (j in seq_len(d))
      FP[i, j] <- sum(Fk[i, ] * P[, j])
    Pp <- matrix(0, d, d)
    for (i in seq_len(d)) for (j in seq_len(d))
      Pp[i, j] <- sum(FP[i, ] * Fk[j, ]) + Q[i, j]
    hx <- as.numeric(.ekf_apply(h, xp))
    Hk <- as.numeric(.ekf_apply(H, xp))
    if (length(Hk) != d) stop("H must return a length-d row")
    PH <- numeric(d)
    for (i in seq_len(d)) PH[i] <- sum(Pp[i, ] * Hk)
    S <- sum(Hk * PH) + R
    K <- PH / S
    v <- y[t] - hx
    x <- xp + K * v
    for (i in seq_len(d)) for (j in seq_len(d))
      Pp[i, j] <- Pp[i, j] - K[i] * S * K[j]
    P <- Pp
    loglik <- loglik - 0.5 * (log(2 * pi * S) + v * v / S)
  }
  .t1_result(estimate = x[1], state = x, cov = as.numeric(t(P)),
             loglik = loglik, n = n, method = "Extended Kalman filter")
}
