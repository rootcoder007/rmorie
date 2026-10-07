# SPDX-License-Identifier: AGPL-3.0-or-later
#' Modularity Q of a community partition (alias of Sgtmodq)
#'
#' This module is an ALIAS. Modularity is implemented once, in
#' \code{Sgtmodq}; this entry point delegates. It is the same quantity as
#' \code{Modulq}; the two module names differ only in which paper they
#' cite.
#'
#' \code{Q = (1 / 2m) sum_ij [A_ij - k_i k_j / 2m] delta(c_i, c_j)}.
#'
#' @param A Symmetric adjacency or weight matrix.
#' @param communities Integer community label per node.
#' @return List with Q, estimate, n_communities, n.
#' @references Newman (2006), PNAS 103(23), 8577-8582,
#'   \doi{10.1073/pnas.0601602103}.
#' @examples
#' A <- matrix(0, 6, 6); A[1:3, 1:3] <- 1; A[4:6, 4:6] <- 1; A[3, 4] <- A[4, 3] <- 1; diag(A) <- 0
#' Modlar(A, communities = c(1, 1, 1, 2, 2, 2))$Q
#' @export
Modlar <- function(A, communities) Sgtmodq(A, communities)
