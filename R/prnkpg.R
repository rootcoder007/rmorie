# SPDX-License-Identifier: AGPL-3.0-or-later
#' PageRank -- alias of \code{\link{Pgrank}}
#'
#' PageRank is already implemented three-way in \code{Pgrank}, so this is
#' a name, not a second implementation: a duplicate would be one more
#' place for the dangling-node mass to be handled differently.
#'
#' Formula: \code{x = (1 - d)/n + d A^T D^-1 x}.
#'
#' @param G Adjacency matrix; \code{G[i, j]} non-zero is a link i to j.
#' @param damping Damping factor.
#' @param n_iter Power iterations.
#' @return The value of \code{\link{Pgrank}}.
#' @references Page, L., Brin, S., Motwani, R. & Winograd, T. (1999).
#'   Stanford InfoLab technical report 1999-66.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Prnkpg(M)
Prnkpg <- function(G, damping = 0.85, n_iter = 100) {
  Pgrank(G, d = damping, n_iter = n_iter)
}
