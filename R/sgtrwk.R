# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-walk kernel of a graph as a resolvent
#'
#' \code{sum_k lam^k A^k} counts walks of every length with a geometric
#' discount and equals \code{(I - lam A)^{-1}} exactly when the series
#' converges, i.e. when \code{lam} is below the reciprocal spectral
#' radius. Computing the resolvent instead of truncating the series is
#' not an optimisation: a truncation at K terms is a different kernel.
#'
#' Formula: \code{K = sum_{k>=0} lam^k A^k = (I - lam A)^{-1}}.
#'
#' @param A Adjacency matrix.
#' @param lam Discount, positive and small enough for convergence.
#' @return List with \code{K}, \code{estimate} (sum of all entries),
#'   \code{trace}, \code{n}.
#' @references Gaertner, T., Flach, P. & Wrobel, S. (2003). On graph
#'   kernels: hardness results and efficient alternatives. Learning
#'   Theory and Kernel Machines, LNCS 2777, pages 129-143.
#'   \doi{10.1007/978-3-540-45167-9_11}.
#' @export
Sgtrwk <- function(A, lam = 0.1) {
  M <- as.matrix(A)
  n <- nrow(M)
  if (n == 0L) stop("Sgtrwk: adjacency matrix is empty")
  if (ncol(M) != n) stop("Sgtrwk: adjacency matrix must be square")
  lam <- as.numeric(lam)
  if (lam <= 0) stop("Sgtrwk: lam must be positive")
  K <- solve(diag(1, n) - lam * M)
  .t1_result(K = K, estimate = sum(K), trace = sum(diag(K)), n = n,
             method = "Random-walk kernel (I - lam A)^{-1}")
}
