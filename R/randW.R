# SPDX-License-Identifier: AGPL-3.0-or-later
#' Random-walk graph kernel between two graphs
#'
#' The walks are counted on the direct product graph, whose adjacency is
#' the Kronecker product \code{A1 (x) A2}: a walk there is a pair of
#' walks, one in each graph, of equal length. Convergence needs
#' \code{lam} below \code{1 / (rho(A1) rho(A2))}, a much tighter
#' constraint than the single-graph case.
#'
#' Formula: \code{k(G1, G2) = sum_ij [(I - lam (A1 (x) A2))^{-1}]_ij}.
#'
#' @param G1 Square adjacency matrix of the first graph.
#' @param G2 Square adjacency matrix of the second graph.
#' @param lam Discount factor, positive.
#' @return List with \code{estimate}, \code{trace}, \code{n1}, \code{n2}.
#' @references Gaertner, T., Flach, P. & Wrobel, S. (2003). On graph
#'   kernels: hardness results and efficient alternatives. Learning
#'   Theory and Kernel Machines, LNCS 2777, pages 129-143.
#'   \doi{10.1007/978-3-540-45167-9_11}.
#' @export
#' @examples
#' RandW(G1 = 5L, G2 = 5L)
RandW <- function(G1, G2, lam = 0.05) {
  A <- as.matrix(G1); B <- as.matrix(G2)
  n1 <- nrow(A); n2 <- nrow(B)
  if (n1 == 0L || n2 == 0L) stop("RandW: both graphs must be non-empty")
  if (ncol(A) != n1) stop("RandW: G1 must be square")
  if (ncol(B) != n2) stop("RandW: G2 must be square")
  lam <- as.numeric(lam)
  if (lam <= 0) stop("RandW: lam must be positive")
  N <- n1 * n2
  W <- kronecker(A, B)
  R <- solve(diag(1, N) - lam * W)
  .t1_result(estimate = sum(R), trace = sum(diag(R)), n1 = n1, n2 = n2,
             method = "Direct-product random-walk graph kernel")
}
