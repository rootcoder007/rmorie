# SPDX-License-Identifier: AGPL-3.0-or-later
#' GCNII: initial residual plus identity mapping
#'
#' The first bracket of the GCNII layer is the initial residual
#' connection, the second the identity mapping.  With the weight matrix
#' taken as the identity -- the "identity mapping" limit of the paper's
#' own analysis, and the only deterministic choice available since no
#' weight is passed -- the second bracket collapses to I for every beta.
#'
#' Formula: H <- relu((1 - alpha) Ah H + alpha H0), repeated K times,
#'   with Ah = Dt^{-1/2}(A + I)Dt^{-1/2}.
#'
#' @param A Square adjacency matrix.
#' @param H0 Initial node representation, one row per node.
#' @param alpha Initial-residual weight in [0, 1].
#' @param beta Identity-mapping weight, retained for the interface.
#' @param K Number of layers.
#' @return List with \code{estimate}, \code{H}, \code{alpha},
#'   \code{beta}, \code{K}, \code{n}, \code{method}.
#' @references Chen, Wei, Huang, Ding and Li (2020), Simple and deep
#'   graph convolutional networks, ICML 2020, PMLR 119:1725-1735, eq.
#'   (3). arXiv:2007.02133
#' @export
#' @examples
#' A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' H0 <- matrix(rnorm(6), 3, 2)
#' GcnII(A, H0)
GcnII <- function(A, H0, alpha = 0.1, beta = 0.5, K = 4) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("gcnii: adjacency matrix is empty")
  if (ncol(M) != n) stop("gcnii: adjacency matrix must be square")
  H <- .s03mat(H0)
  if (nrow(H) != n) stop("gcnii: H0 must have one row per node")
  a <- as.numeric(alpha)
  if (a < 0 || a > 1) stop("gcnii: alpha must lie in [0, 1]")
  if (as.numeric(beta) < 0) stop("gcnii: beta must be non-negative")
  layers <- as.integer(K)
  if (layers < 1L) stop("gcnii: K must be at least 1")
  At <- M + diag(1, n)
  d <- rowSums(At)
  s <- ifelse(d <= 0, 0, d^(-0.5))
  Ah <- (s %o% s) * At
  Hl <- H
  for (i in seq_len(layers)) {
    P <- Ah %*% Hl
    Z <- (1 - a) * P + a * H
    Hl <- matrix(vapply(as.numeric(Z), .s03relu, 0), nrow(Z), ncol(Z))
  }
  .t1_result(estimate = mean(as.numeric(Hl)), H = Hl, alpha = a,
             beta = as.numeric(beta), K = layers, n = n,
             method = "H = relu((1-a) Ah H + a H0) with W = I, Chen et al. (2020) eq. (3)")
}
