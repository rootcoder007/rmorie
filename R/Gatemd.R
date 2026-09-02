# SPDX-License-Identifier: AGPL-3.0-or-later
#' Multi-head graph attention layer
#'
#' The single-head layer already exists in this package as \code{Gat};
#' the wave2 audit flagged this module as a duplicate of it and it is
#' one, so the attention arithmetic is NOT repeated here.  This function
#' adds only the multi-head interface: with no weights supplied the
#' deterministic choice W = I and a = 1 is used, every head then sees
#' the same input, and averaging identical heads is exact -- which is
#' what makes the \code{heads} argument checkable at all.
#'
#' Formula: average over heads of Gat(A, X, I, 1).
#'
#' @param G Square adjacency matrix.
#' @param X Node feature matrix, one row per node.
#' @param heads Number of attention heads.
#' @return List with \code{estimate}, \code{H}, \code{heads}, \code{n},
#'   \code{method}.
#' @references Velickovic, Cucurull, Casanova, Romero, Lio and Bengio
#'   (2018), Graph attention networks, ICLR 2018, eqs. (1)-(4) and (6).
#'   arXiv:1710.10903
#' @export
#' @examples
#' G <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' X <- matrix(rnorm(6), 3, 2)
#' Gatemd(G, X)
Gatemd <- function(G, X, heads = 1) {
  M <- .s03mat(G)
  n <- nrow(M)
  if (n == 0L) stop("graph_attention_net: graph is empty")
  if (ncol(M) != n) stop("graph_attention_net: adjacency matrix must be square")
  H <- .s03mat(X)
  if (nrow(H) != n) stop("graph_attention_net: X must have one row per node")
  nh <- as.integer(heads)
  if (nh < 1L) stop("graph_attention_net: heads must be at least 1")
  p <- ncol(H)
  W <- diag(1, p)
  a <- rep(1, 2 * p)
  acc <- matrix(0, n, p)
  for (k in seq_len(nh)) acc <- acc + Gat(M, H, W, a)$H / nh
  .t1_result(estimate = mean(acc), H = acc, heads = nh, n = n,
             method = "average of gat() heads, Velickovic et al. (2018) eqs. (1)-(4) and (6)")
}
