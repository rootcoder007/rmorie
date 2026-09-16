# SPDX-License-Identifier: AGPL-3.0-or-later
#' Log-domain Sinkhorn for entropic optimal transport
#'
#' Cuturi (2013), Sinkhorn distances, NIPS 26, 2292-2300
#' (arXiv:1306.0895), for the entropic problem; Schmitzer (2019),
#' Stabilized sparse scaling algorithms for entropy regularized transport
#' problems, SIAM J. Sci. Comput. 41(3), A1443-A1481 (arXiv:1610.06519 --
#' FETCHED), for the log-domain stabilisation.  The iteration is f_i <-
#' eps log a_i - eps logsumexp_j((g_j - C_ij)/eps), g_j <- eps log b_j -
#' eps logsumexp_i((f_i - C_ij)/eps), T_ij = exp((f_i + g_j - C_ij)/eps):
#' Sinkhorn's scaling with u = exp(f/eps), v = exp(g/eps) substituted out.
#' u and v underflow at small eps while f and g do not, so the two forms
#' are mathematically identical and numerically are not.
#'
#' @param a,b the two marginals.
#' @param C the cost matrix.
#' @param epsilon the entropic regularisation.
#' @param max_iter iteration cap.
#' @param tol marginal tolerance.
#' @param f0,g0 optional warm-start potentials.
#' @return list: T, cost, f, g, estimate, err, n_iter, method.
#' @keywords internal
#' @examples
#' Sinkhlog(c(0.5, 0.5), c(0.5, 0.5), matrix(c(0, 1, 1, 0), 2, 2), 0.5)$cost
#' @export
Sinkhlog <- function(a, b, C, epsilon = 0.1, max_iter = 200, tol = 1e-13,
                     f0 = NULL, g0 = NULL) {
  av <- .s03vec(a)
  bv <- .s03vec(b)
  Cm <- .s03mat(C)
  n <- length(av)
  m <- length(bv)
  e <- as.numeric(epsilon)
  f <- if (!is.null(f0)) .s03vec(f0) else numeric(n)
  g <- if (!is.null(g0)) .s03vec(g0) else numeric(m)
  la <- ifelse(av > 0, log(pmax(av, 1e-300)), -1e300)
  lb <- ifelse(bv > 0, log(pmax(bv, 1e-300)), -1e300)
  it <- 0L
  err <- NaN
  for (itr in seq_len(as.integer(max_iter))) {
    it <- itr
    for (i in seq_len(n)) {
      f[i] <- e * la[i] - e * .s03logsumexp((g - Cm[i, ]) / e)
    }
    for (j in seq_len(m)) {
      f_minus <- numeric(n)
      for (i in seq_len(n)) f_minus[i] <- (f[i] - Cm[i, j]) / e
      g[j] <- e * lb[j] - e * .s03logsumexp(f_minus)
    }
    err <- 0
    for (i in seq_len(n)) {
      s <- 0
      for (j in seq_len(m)) s <- s + exp((f[i] + g[j] - Cm[i, j]) / e)
      err <- err + abs(s - av[i])
    }
    if (err < tol) break
  }
  T <- matrix(0, n, m)
  for (i in seq_len(n)) for (j in seq_len(m)) T[i, j] <- exp((f[i] + g[j] - Cm[i, j]) / e)
  cost <- 0
  for (i in seq_len(n)) for (j in seq_len(m)) cost <- cost + T[i, j] * Cm[i, j]
  list(T = T, cost = cost, f = f, g = g, estimate = cost, err = err,
       n_iter = it,
       method = "Log-domain Sinkhorn for entropic OT (Cuturi 2013; Schmitzer 2019)")
}
