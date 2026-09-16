# SPDX-License-Identifier: AGPL-3.0-or-later
#' Besag-York-Mollie disease-mapping log-posterior kernel
#'
#' y_i ~ Poisson(E_i exp(x_i'beta + u_i + v_i)) with an ICAR prior
#' p(u) propto exp(-(tau_u/2) sum_\{i~j\}(u_i - u_j)^2) and v_i ~ N(0,
#' 1/tau_v).  The ICAR normalising rank is n - 1.
#'
#' @param y Observed counts per area.
#' @param E Expected counts, strictly positive.
#' @param A Neighbourhood adjacency, n x n.
#' @param u Spatially structured effects.
#' @param v Unstructured effects.
#' @param taus,tauv Precisions of the two components.
#' @param X Covariates, one row per area, or NULL.
#' @param beta Covariate coefficients, or NULL.
#'
#' @return List with logpost, loglik, logpu, logpv, rr, fitted, usum,
#'   npair, n.
#' @references Besag, York and Mollie (1991), Ann. Inst. Statist. Math.
#'   43(1), 1-20; Besag (1974), JRSS B 36(2), 192-236.  Standard
#'   published form; neither article is in the local corpus and neither
#'   was read.
#' @export
Bymfit <- function(y, E, A, u, v, taus = 1, tauv = 1, X = NULL,
                   beta = NULL) {
  y <- .t1_vec(y)
  E <- .t1_vec(E)
  u <- .t1_vec(u)
  v <- .t1_vec(v)
  n <- length(y)
  if (length(E) != n || length(u) != n || length(v) != n)
    stop("y, E, u and v must have the same length")
  if (any(E <= 0)) stop("expected counts must be strictly positive")
  if (any(y < 0)) stop("counts must be non-negative")
  Am <- .t1_mat(A)
  if (nrow(Am) != n || ncol(Am) != n) stop("A must be n by n")
  ts <- as.numeric(taus)
  tv <- as.numeric(tauv)
  if (ts <= 0 || tv <= 0) stop("precisions must be strictly positive")
  if (is.null(X)) {
    eta0 <- rep(0, n)
  } else {
    Xm <- .t1_mat(X)
    if (nrow(Xm) != n) stop("X must have one row per area")
    b <- .t1_vec(beta)
    if (length(b) != ncol(Xm)) stop("beta must have one entry per column of X")
    eta0 <- as.numeric(Xm %*% b)
  }
  rr <- exp(eta0 + u + v)
  mu <- E * rr
  ll <- sum(y * log(mu) - mu - lgamma(y + 1))
  q <- 0
  npair <- 0L
  if (n > 1L) for (i in seq_len(n - 1L)) for (j in (i + 1L):n) {
    if (Am[i, j] != 0 || Am[j, i] != 0) {
      q <- q + (u[i] - u[j])^2
      npair <- npair + 1L
    }
  }
  lpu <- 0.5 * (n - 1) * log(ts) - 0.5 * ts * q
  lpv <- 0.5 * n * log(tv) - 0.5 * tv * sum(v^2) - 0.5 * n * log(2 * pi)
  .t1_result(logpost = ll + lpu + lpv, loglik = ll, logpu = lpu,
             logpv = lpv, rr = rr, fitted = mu, usum = sum(u),
             npair = npair, n = n,
             method = "BYM convolution log-posterior kernel (Besag-York-Mollie 1991)")
}
