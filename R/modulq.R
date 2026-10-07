# SPDX-License-Identifier: AGPL-3.0-or-later
#' Newman-Girvan modularity Q (alias of Sgtmodq)
#'
#' This module is an ALIAS. Modularity is implemented once, in
#' \code{Sgtmodq}; this entry point delegates.
#'
#' \code{Q = (1 / 2m) sum_ij (A_ij - k_i k_j / 2m) delta(c_i, c_j)} with
#' \code{2m = sum_ij A_ij}. The configuration-model null term is why a
#' single all-node community scores exactly zero however dense the graph.
#'
#' @param G Symmetric adjacency or weight matrix.
#' @param communities Integer community label per node.
#' @return List with Q, estimate, n_communities, n.
#' @references Newman and Girvan (2004), Physical Review E 69, 026113,
#'   \doi{10.1103/PhysRevE.69.026113}.
#' @examples
#' A <- matrix(0, 6, 6); A[1:3, 1:3] <- 1; A[4:6, 4:6] <- 1; A[3, 4] <- A[4, 3] <- 1; diag(A) <- 0
#' Modulq(A, communities = c(1, 1, 1, 2, 2, 2))$Q
#' @export
Modulq <- function(G, communities) Sgtmodq(G, communities)
