# SPDX-License-Identifier: AGPL-3.0-or-later
#' Eigenvector centrality.
#'
#' Formula: A x = lambda_max x; centrality is the principal eigenvector
#'
#' @param A Symmetric non-negative adjacency matrix.

#' @return List with ``centrality`` (max-scaled), ``unit``, ``eigenvalue``, ``n``.
#' @references Bonacich (1972), Factoring and weighting approaches to status scores and clique identification, Journal of Mathematical Sociology 2:113-120. Paywalled; the measure is the principal eigenvector of the adjacency matrix, as it is universally described in the centrality literature (e.g. Bonacich 2000, Social Networks 22:357-365, which restates his own definition).
#' @export
#' @examples
#' Eigcent(matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), 3, 3))
Eigcent <- function(A) {
  A <- as.matrix(A); n <- nrow(A)
  e <- .t1_eigsym(A)
  v <- e$vectors[, 1]
  if (sum(v) < 0) v <- -v
  mx <- max(abs(v))
  .t1_result(centrality = if (mx > 0) v / mx else v, unit = v,
             eigenvalue = e$values[1], n = n,
             method = "Eigenvector centrality (principal eigenvector)")
}
