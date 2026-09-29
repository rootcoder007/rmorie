.ssk_m <- function(A) if (is.matrix(A)) A else if (length(A) == 1) matrix(A, 1, 1) else as.matrix(A)

#' Linear Gaussian state-space model: Kalman filter and smoother
#'
#' R arm of \code{morie.fn.stsmod}: \eqn{y_t = Z a_t + e_t}, \eqn{e_t \sim
#' N(0, H)}; \eqn{a_{t+1} = T a_t + R n_t}, \eqn{n_t \sim N(0, Q)},
#' \eqn{a_1 \sim N(a1, P1)} (default 0 and \eqn{10^6 I}). Kalman filter with
#' the prediction-error decomposition of the log-likelihood, filtered states
#' and the Durbin-Koopman backward smoother.
#'
#' @param y Observations (vector, or n x p matrix).
#' @param Z,T,H,Q,R System matrices (scalars allowed).
#' @param a1,P1 Initial state mean and covariance.
#' @return List with \code{loglik}, \code{filtered}, \code{smoothed},
#'   \code{predicted}, \code{innovations}, \code{F}.
#' @references Durbin, J. and Koopman, S. J. (2012). Time Series Analysis by
#'   State Space Methods, 2nd ed. Oxford University Press.
#' @examples
#' Stsmod(c(1.1, 0.9, 1.4, 1.2, 1.6), 1, 1, 0.5, 0.2, 1, a1 = 0, P1 = 10)$loglik
#' @export
Stsmod <- function(y, Z, T, H, Q, R, a1 = NULL, P1 = NULL) {
  Y <- if (is.matrix(y)) y else matrix(y, ncol = 1)
  Zm <- .ssk_m(Z)
  Tm <- .ssk_m(T)
  Hm <- .ssk_m(H)
  Qm <- .ssk_m(Q)
  Rm <- .ssk_m(R)
  m <- nrow(Tm)
  p <- ncol(Y)
  a <- if (is.null(a1)) numeric(m) else as.numeric(a1)
  P <- if (is.null(P1)) diag(1e6, m) else .ssk_m(P1)
  RQR <- Rm %*% Qm %*% t(Rm)
  ll <- 0
  keep <- vector("list", nrow(Y))
  for (k in seq_len(nrow(Y))) {
    v <- Y[k, ] - as.vector(Zm %*% a)
    PZt <- P %*% t(Zm)
    F <- Zm %*% PZt + Hm
    Fi <- solve(F)
    K <- Tm %*% PZt %*% Fi
    Fv <- as.vector(Fi %*% v)
    ll <- ll - 0.5 * (p * log(2 * pi) + as.numeric(determinant(F)$modulus) + sum(v * Fv))
    L <- Tm - K %*% Zm
    keep[[k]] <- list(a = a, P = P, v = v, Fi = Fi, L = L, filt = a + as.vector(PZt %*% Fv), F = F)
    a <- as.vector(Tm %*% a + K %*% v)
    P <- Tm %*% P %*% t(L) + RQR
  }
  r <- numeric(m)
  smooth <- vector("list", nrow(Y))
  for (k in rev(seq_len(nrow(Y)))) {
    kk <- keep[[k]]
    r <- as.vector(t(Zm) %*% kk$Fi %*% kk$v + t(kk$L) %*% r)
    smooth[[k]] <- kk$a + as.vector(kk$P %*% r)
  }
  list(loglik = ll, filtered = lapply(keep, `[[`, "filt"), smoothed = smooth, predicted = lapply(keep, `[[`, "a"),
       innovations = if (p == 1) vapply(keep, function(z) z$v[1], 0) else lapply(keep, `[[`, "v"),
       F = if (p == 1) vapply(keep, function(z) z$F[1, 1], 0) else lapply(keep, `[[`, "F"), estimate = ll)
}
