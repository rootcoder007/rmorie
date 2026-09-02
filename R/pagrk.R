# SPDX-License-Identifier: AGPL-3.0-or-later
#' PageRank
#'
#' DUPLICATE: PageRank is already implemented as \code{Pgrank}; per
#' ledger/wave2/DUPMAP.tsv this is an alias, not a third power
#' iteration.
#'
#' Formula: \code{x = (1 - alpha)/n + alpha A^T D^-1 x}, with the mass
#' of dangling nodes spread uniformly so the vector sums to one.
#'
#' @param A Adjacency; \code{A\[i, j\]} non-zero means a link from i to j.
#' @param alpha Damping factor.
#' @param n_iter Power iterations.
#' @return List with \code{pr}, \code{estimate}, \code{top}, \code{n}.
#' @references Page, L., Brin, S., Motwani, R. & Winograd, T. (1999).
#'   The PageRank citation ranking: bringing order to the web. Stanford
#'   InfoLab Technical Report 1999-66.
#' @export
#' @examples
#' M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' Pagrk(M)
Pagrk <- function(A, alpha = 0.85, n_iter = 100) {
  Pgrank(A, d = alpha, n_iter = n_iter)
}
