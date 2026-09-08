# SPDX-License-Identifier: AGPL-3.0-or-later
#' Vertex strengths of a weighted graph
#'
#' Formula: s_i = sum_j a_ij w_ij
#'
#' @param W Weighted adjacency matrix; a zero entry means no edge.

#' @param W See Usage.
#' @return List with ``strength``, ``degree``, ``ratio``, ``total``, ``n``.
#' @references Barrat, Barthelemy, Pastor-Satorras and Vespignani (2004), The
#' architecture of complex weighted networks, PNAS 101:3747-3752, arXiv:cond-mat/0311416,
#' equation (2). Verified against the paper.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Vstrength(V)
Vstrength <- function(W) {
  W <- as.matrix(W)
  diag(W) <- 0
  n <- nrow(W)
  s <- rowSums(W)
  k <- rowSums(W != 0)
  .t1_result(strength = s, degree = k,
             ratio = ifelse(k > 0, s / k, NA_real_), total = sum(s), n = n,
             method = "Weighted-graph vertex strengths")
}
