# SPDX-License-Identifier: AGPL-3.0-or-later
#' GATv2 attention layer
#'
#' GATv2 moves the nonlinearity inside: the score is a linear read-out
#' OF a LeakyReLU, not a LeakyReLU of a linear read-out.  That single
#' reordering is what turns static attention into dynamic attention.
#' With W = I and a = 1 the score becomes
#' sum_c LeakyReLU(h_ic) + sum_c LeakyReLU(h_jc), which differs from
#' GAT's LeakyReLU(sum h_i + sum h_j) whenever a feature vector mixes
#' signs.
#'
#' Formula: e_ij = sum_c LeakyReLU(h_ic) + sum_c LeakyReLU(h_jc);
#'   alpha = softmax over the neighbourhood; h_i' = sigmoid(sum alpha h_j).
#'
#' @param A Square adjacency matrix.
#' @param X Node feature matrix, one row per node.
#' @return List with \code{estimate}, \code{H}, \code{alpha_first},
#'   \code{n}, \code{method}.
#' @references Brody, Alon and Yahav (2022), How attentive are graph
#'   attention networks?, ICLR 2022, eq. (7). arXiv:2105.14491
#' @export
#' @examples
#' A <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3, byrow = TRUE)
#' X <- matrix(rnorm(6), 3, 2)
#' GatV2(A, X)
GatV2 <- function(A, X) {
  M <- .s03mat(A)
  n <- nrow(M)
  if (n == 0L) stop("gat_v2: adjacency matrix is empty")
  if (ncol(M) != n) stop("gat_v2: adjacency matrix must be square")
  H <- .s03mat(X)
  if (nrow(H) != n) stop("gat_v2: X must have one row per node")
  p <- ncol(H)
  lrelu <- function(z) ifelse(z > 0, z, 0.2 * z)
  g <- rowSums(matrix(lrelu(as.numeric(H)), n, p))
  out <- matrix(0, n, p)
  alpha1 <- NULL
  for (i in seq_len(n)) {
    nb <- which(seq_len(n) == i | M[i, ] != 0)
    e <- g[i] + g[nb]
    w <- exp(e - max(e))
    w <- w / sum(w)
    if (i == 1L) alpha1 <- w
    for (a in seq_along(nb)) out[i, ] <- out[i, ] + w[a] * H[nb[a], ]
  }
  out <- matrix(vapply(as.numeric(out), .s03sigmoid, 0), n, p)
  .t1_result(estimate = mean(as.numeric(out)), H = out, alpha_first = alpha1,
             n = n,
             method = "GATv2 eq. (7): a' LeakyReLU(W[h_i || h_j]) with W = I, a = 1")
}
