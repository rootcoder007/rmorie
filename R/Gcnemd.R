# SPDX-License-Identifier: AGPL-3.0-or-later
#' Symmetrically normalised graph convolution layer
#'
#' Kipf and Welling's layer BEFORE the renormalisation trick: the
#' adjacency carries no self-loop, so an isolated node receives nothing.
#' Isolated nodes have zero degree and their normalising factor is taken
#' to be zero rather than infinite.
#'
#' Formula: H' = relu(D^{-1/2} A D^{-1/2} X W), D = diag(rowSums(A)).
#'
#' @param G Square adjacency matrix.
#' @param X Node feature matrix, one row per node.
#' @param W Weight matrix, one row per input feature.
#' @return List with \code{estimate} (mean output activation),
#'   \code{H}, \code{preactivation}, \code{n}, \code{method}.
#' @references Kipf and Welling (2017), Semi-supervised classification
#'   with graph convolutional networks, ICLR 2017, eq. (2).
#'   arXiv:1609.02907
#' @export
#' @examples
#' G <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' X <- matrix(rnorm(6), 3, 2)
#' W <- matrix(rnorm(4), 2, 2)
#' Gcnemd(G, X, W)
Gcnemd <- function(G, X, W) {
  M <- .s03mat(G)
  n <- nrow(M)
  if (n == 0L) stop("gcn: graph is empty")
  if (ncol(M) != n) stop("gcn: adjacency matrix must be square")
  H <- .s03mat(X)
  if (nrow(H) != n) stop("gcn: X must have one row per node")
  Wm <- .s03mat(W)
  if (nrow(Wm) != ncol(H)) stop("gcn: W must have one row per input feature")
  d <- rowSums(M)
  s <- ifelse(d <= 0, 0, d^(-0.5))
  An <- (s %o% s) * M
  Z <- (An %*% H) %*% Wm
  Hout <- matrix(vapply(as.numeric(Z), .s03relu, 0), nrow(Z), ncol(Z))
  .t1_result(estimate = mean(as.numeric(Hout)), H = Hout,
             preactivation = Z, n = n,
             method = "H' = relu(D^{-1/2} A D^{-1/2} X W), Kipf & Welling (2017) eq. (2)")
}
