# SPDX-License-Identifier: AGPL-3.0-or-later
#' Graph attention layer
#'
#' The GAT attention coefficient is a LeakyReLU of a linear score on the
#' concatenated endpoint features, softmaxed over the neighbourhood of
#' the receiving node (which includes the node itself).  With no
#' parameters supplied the deterministic choice W = I and a = 1 is used,
#' so the score reduces to LeakyReLU(sum(h_i) + sum(h_j)) with the
#' paper's negative slope of 0.2.
#'
#' Formula: alpha_ij = softmax_j LeakyReLU(sum(h_i) + sum(h_j));
#'   h_i' = sigmoid(sum_j alpha_ij h_j).
#'
#' @param G Square adjacency matrix.
#' @param X Node feature matrix, one row per node.
#' @param heads Number of attention heads; identical heads are averaged.
#' @return List with \code{estimate}, \code{H}, \code{alpha_first},
#'   \code{heads}, \code{n}, \code{method}.
#' @references Velickovic, Cucurull, Casanova, Romero, Lio and Bengio
#'   (2018), Graph attention networks, ICLR 2018, eqs. (1)-(4).
#'   arXiv:1710.10903
#' @export
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
  lrelu <- function(z) ifelse(z > 0, z, 0.2 * z)
  rs <- rowSums(H)
  out <- matrix(0, n, p)
  alpha1 <- NULL
  for (i in seq_len(n)) {
    nb <- which(seq_len(n) == i | M[i, ] != 0)
    e <- lrelu(rs[i] + rs[nb])
    w <- exp(e - max(e))
    w <- w / sum(w)
    if (i == 1L) alpha1 <- w
    for (a in seq_along(nb)) out[i, ] <- out[i, ] + w[a] * H[nb[a], ]
  }
  out <- matrix(vapply(as.numeric(out), .s03sigmoid, 0), n, p)
  .t1_result(estimate = mean(as.numeric(out)), H = out, alpha_first = alpha1,
             heads = nh, n = n,
             method = "GAT eqs. (1)-(4) with W = I, a = 1, LeakyReLU slope 0.2")
}
