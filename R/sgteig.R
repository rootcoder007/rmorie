# SPDX-License-Identifier: AGPL-3.0-or-later
#' Eigenvector centrality from the leading eigenvector (re-export)
#'
#' Formula: see eigcent
#'
#' @param A Symmetric non-negative adjacency matrix.

#' @param A See Usage.
#' @return List with the payload of :func:`morie.fn.eigcen.eigcent`.
#' @references Bonacich (1972), Factoring and weighting approaches to status scores and
#' clique identification, Journal of Mathematical Sociology 2:113-120. Paywalled; the
#' measure is the principal eigenvector of the adjacency matrix, as it is universally
#' described in the centrality literature (e.g. Bonacich 2000, Social Networks
#' 22:357-365, which restates his own definition).
#' @export
#' @examples
#' A <- matrix(c(4, 1, 0.5, 1, 3, 0.8, 0.5, 0.8, 2), nrow = 3)
#' res <- Sgteigcent(A = A)
#' res
Sgteigcent <- function(A) {
  Eigcent(A)
}
