# SPDX-License-Identifier: AGPL-3.0-or-later
#' Graph convolution with the renormalisation trick
#'
#' Adding the self-loop before normalising keeps the eigenvalues of the
#' propagation operator inside [-1, 1], which is what stops repeated
#' application from exploding.
#'
#' Formula: H' = relu(Dt^{-1/2} (A + I) Dt^{-1/2} X W),
#'   Dt = diag(rowSums(A + I)).
#'
#' @param A Square adjacency matrix.
#' @param X Node feature matrix, one row per node.
#' @param W Weight matrix, one row per input feature.
#' @return List with \code{estimate}, \code{H}, \code{preactivation},
#'   \code{n}, \code{method}.
#' @references Kipf and Welling (2017), ICLR 2017, eqs. (2) and (8).
#'   arXiv:1609.02907
#' @export
#' @examples
#' A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' X <- matrix(rnorm(6), 3, 2)
#' W <- matrix(rnorm(4), 2, 2)
#' GcnL(A, X, W)
GcnL <- function(A, X, W) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("gcn: adjacency matrix is empty")
  if (ncol(M) != n) stop("gcn: adjacency matrix must be square")
  H <- .s03mat(X)
  if (nrow(H) != n) stop("gcn: X must have one row per node")
  Wm <- .s03mat(W)
  if (nrow(Wm) != ncol(H)) stop("gcn: W must have one row per input feature")
  At <- M + diag(1, n)
  d <- rowSums(At)
  s <- ifelse(d <= 0, 0, d^(-0.5))
  An <- (s %o% s) * At
  Z <- (An %*% H) %*% Wm
  Hout <- matrix(vapply(as.numeric(Z), .s03relu, 0), nrow(Z), ncol(Z))
  .t1_result(estimate = mean(as.numeric(Hout)), H = Hout,
             preactivation = Z, n = n,
             method = "H' = relu(Dt^{-1/2}(A+I)Dt^{-1/2} X W), Kipf & Welling (2017) eq. (8)")
}
