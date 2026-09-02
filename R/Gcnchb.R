# SPDX-License-Identifier: AGPL-3.0-or-later
#' ChebNet spectral filter
#'
#' The Chebyshev recurrence is what makes the filter K-localised and
#' costs only K sparse products.  lambda_max comes from the Jacobi
#' eigenvalues of the supplied symmetric Laplacian; with no
#' coefficients passed the deterministic choice theta_k = 1/(k+1) is
#' used.
#'
#' Formula: sum_{k=0}^{K-1} theta_k T_k(Lt) X, Lt = 2 L / lambda_max - I,
#'   T_k = 2 Lt T_{k-1} - T_{k-2}, T_0 = I, T_1 = Lt.
#'
#' @param L Symmetric graph Laplacian.
#' @param X Node feature matrix, one row per node.
#' @param K Filter order (number of Chebyshev terms).
#' @param theta Optional length-K coefficient vector.
#' @return List with \code{estimate}, \code{H}, \code{lambda_max},
#'   \code{K}, \code{n}, \code{method}.
#' @references Defferrard, Bresson and Vandergheynst (2016),
#'   Convolutional neural networks on graphs with fast localized
#'   spectral filtering, NIPS 29, eqs. (4)-(5). arXiv:1606.09375
#' @export
#' @examples
#' Gcnchb(L = 5L, X = 5L)
Gcnchb <- function(L, X, K = 3, theta = NULL) {
  M <- .s03mat(L)
  n <- nrow(M)
  if (n == 0L) stop("chebnet: Laplacian is empty")
  if (ncol(M) != n) stop("chebnet: Laplacian must be square")
  H <- .s03mat(X)
  if (nrow(H) != n) stop("chebnet: X must have one row per node")
  order <- as.integer(K)
  if (order < 1L) stop("chebnet: K must be at least 1")
  th <- if (is.null(theta)) 1 / (seq_len(order)) else .s03vec(theta)
  if (length(th) != order) stop("chebnet: theta must have K entries")
  ei <- .s03jacobi(M)
  lmax <- max(ei$values)
  if (lmax <= 0) stop("chebnet: Laplacian has no positive eigenvalue")
  Lt <- 2 * M / lmax - diag(1, n)
  Tprev <- H
  out <- th[1] * Tprev
  if (order > 1L) {
    Tcur <- Lt %*% H
    out <- out + th[2] * Tcur
    if (order > 2L) for (kk in 3:order) {
      Tn <- 2 * (Lt %*% Tcur) - Tprev
      out <- out + th[kk] * Tn
      Tprev <- Tcur
      Tcur <- Tn
    }
  }
  .t1_result(estimate = mean(as.numeric(out)), H = out, lambda_max = lmax,
             K = order, n = n,
             method = "sum_k theta_k T_k(2L/lmax - I) X, Defferrard et al. (2016) eqs. (4)-(5)")
}
