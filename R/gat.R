# SPDX-License-Identifier: AGPL-3.0-or-later
#' Let each node decide how much to weight each neighbour
#'
#' Fixed aggregation treats every neighbour alike, which is wrong when
#' some edges matter more. Attention makes the weight a learned function
#' of the two endpoints, computed per edge from node features alone, so
#' the layer never needs the whole graph and transfers to unseen graphs.
#'
#' Formula: \code{e_ij = LeakyReLU(a'\[W h_i || W h_j\])},
#' \code{alpha_ij = softmax_j(e_ij)},
#' \code{h_i' = sum_j alpha_ij W h_j}.
#'
#' @param A Adjacency; self-loops are added.
#' @param X Node features.
#' @param W Shared linear map.
#' @param a Attention vector of length 2 f_out.
#' @param alpha_leaky LeakyReLU negative slope.
#' @return List with \code{H}, \code{alpha}, \code{estimate}, \code{n}, \code{f_out}.
#' @references Velickovic et al. (2018). Graph attention networks. ICLR
#'   2018, equations (1)-(4).
#' @export
#' @examples
#' Gat(A = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), W = 5L, a = c(1, 2,
#' 3, 4, 5, 6, 7, 8))
Gat <- function(A, X, W, a, alpha_leaky = 0.2) {
  Am <- as.matrix(A)
  Xm <- as.matrix(X)
  Wm <- as.matrix(W)
  av <- as.numeric(a)
  n <- nrow(Am)
  Wh <- Xm %*% Wm
  fo <- ncol(Wh)
  alpha <- matrix(0, n, n)
  for (i in seq_len(n)) {
    nb <- which(Am[i, ] != 0 | seq_len(n) == i)
    e <- numeric(length(nb))
    for (t in seq_along(nb)) {
      s <- sum(av[seq_len(fo)] * Wh[i, ]) + sum(av[fo + seq_len(fo)] * Wh[nb[t], ])
      e[t] <- if (s > 0) s else alpha_leaky * s
    }
    alpha[i, nb] <- .s4_softmax(e)
  }
  H <- alpha %*% Wh
  .t1_result(H = H, alpha = alpha, estimate = sum(H) / (n * fo), n = n,
             f_out = fo, method = "Graph attention layer")
}
