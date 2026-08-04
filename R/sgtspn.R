# SPDX-License-Identifier: AGPL-3.0-or-later
#' Number of spanning trees by the matrix-tree theorem
#'
#' Every cofactor of the graph Laplacian L = D - A equals the number of
#' spanning trees, so delete one row and the matching column and take the
#' determinant.  The complete graph K_n returns Cayley's n^(n-2).  Source
#' consulted: Kirchhoff (1847), Annalen der Physik 148(12), 497-508.
#'
#' @param A symmetric adjacency (or weight) matrix.
#' @param drop 0-based index of the deleted row/column.
#' @return list: estimate, cofactor, laplacian, degrees, n, method.
#' @keywords internal
#' @examples
#' sgtspn(matrix(1, 4, 4) - diag(4))$estimate
#' @export
sgtspn <- function(A, drop = 0L) {
  m <- as.matrix(A); dimnames(m) <- NULL
  n <- nrow(m)
  deg <- rowSums(m)
  lap <- diag(deg, nrow = n) - m
  keep <- setdiff(seq_len(n), as.integer(drop) + 1L)
  val <- if (n > 1L) det(lap[keep, keep, drop = FALSE]) else 1
  list(estimate = round(val), cofactor = val, laplacian = lap, degrees = deg,
       n = n, method = "Spanning-tree count by the matrix-tree theorem (Kirchhoff 1847)")
}

# CANONICAL TEST
# stopifnot(sgtspn(matrix(1,4,4)-diag(4))$estimate == 16)

#' @rdname sgtspn
#' @keywords internal
#' @export
morie_sgtspn <- sgtspn
