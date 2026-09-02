# SPDX-License-Identifier: AGPL-3.0-or-later
#' Relational graph convolution (R-GCN)
#'
#' Each relation gets its own weight matrix and its own normalisation
#' constant, the paper's default being the per-relation neighbour count.
#' A relation with no neighbours for a node contributes nothing rather
#' than dividing by zero.
#'
#' Formula: h_i' = relu(sum_r sum_\{j in N_i^r\} W_r h_j / |N_i^r| + W_0 h_i).
#'
#' @param A_r List of per-relation square adjacency matrices.
#' @param X Node feature matrix, one row per node.
#' @param W_r List of per-relation weight matrices.
#' @param W0 Optional self-loop weight matrix; identity by default.
#' @return List with \code{estimate}, \code{H}, \code{n},
#'   \code{relations}, \code{method}.
#' @references Schlichtkrull, Kipf, Bloem, van den Berg, Titov and
#'   Welling (2018), Modeling relational data with graph convolutional
#'   networks, ESWC 2018, eq. (2). arXiv:1703.06103
#' @export
#' @examples
#' Kgnn(A_r = c(1, 2, 3, 4, 5, 6, 7, 8), X = 5L, W_r = c(1, 2, 3, 4, 5, 6, 7, 8))
Kgnn <- function(A_r, X, W_r, W0 = NULL) {
  if (length(A_r) == 0L) stop("r_gcn: no relations supplied")
  if (length(A_r) != length(W_r)) stop("r_gcn: A_r and W_r have different lengths")
  H <- .s03mat(X)
  n <- nrow(H)
  if (n == 0L) stop("r_gcn: X is empty")
  p <- ncol(H)
  Ms <- list()
  Ws <- list()
  for (r in seq_along(A_r)) {
    M <- .s03mat(A_r[[r]])
    if (nrow(M) != n) stop(sprintf("r_gcn: relation %d has the wrong node count", r - 1L))
    if (ncol(M) != n) stop(sprintf("r_gcn: relation %d is not square", r - 1L))
    Wm <- .s03mat(W_r[[r]])
    if (nrow(Wm) != p) stop(sprintf("r_gcn: W_r[%d] must have one row per input feature", r - 1L))
    Ms[[r]] <- M
    Ws[[r]] <- Wm
  }
  q <- ncol(Ws[[1]])
  W0m <- if (is.null(W0)) diag(1, p, q) else .s03mat(W0)
  out <- matrix(0, n, q)
  for (r in seq_along(Ms)) {
    M <- Ms[[r]]
    agg <- matrix(0, n, p)
    for (i in seq_len(n)) {
      nb <- which(M[i, ] != 0)
      if (length(nb) == 0L) next
      cc <- length(nb)
      for (j in nb) agg[i, ] <- agg[i, ] + H[j, ] / cc
    }
    out <- out + agg %*% Ws[[r]]
  }
  Z <- out + H %*% W0m
  Hout <- matrix(vapply(as.numeric(Z), .s03relu, 0), n, q)
  .t1_result(estimate = mean(as.numeric(Hout)), H = Hout, n = n,
             relations = length(Ms),
             method = "h' = relu(sum_r sum_j W_r h_j / |N_i^r| + W_0 h_i), Schlichtkrull et al. (2018) eq. (2)")
}
