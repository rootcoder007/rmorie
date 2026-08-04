# SPDX-License-Identifier: AGPL-3.0-or-later
#' Degree matrix and volume of a graph.
#'
#' Formula: D = diag(d_1, ..., d_n),  d_v = sum_u A_uv;  vol(G) = sum_v d_v
#'
#' @param Adj Symmetric adjacency matrix with a zero diagonal; entries may be 0/1 or non-negative edge weights.
#'
#' @return List with ``degrees``, ``D``, ``volume``, ``n``.
#' @references Chung, F. (1997), Spectral Graph Theory, CBMS Regional Conference Series in Mathematics 92, American Mathematical Society, is this shelf's primary book and is NOT in the reference library, so it could not be read.  The conventions below were taken instead from the author's own survey, Chung, F., Four proofs for the Cheeger inequality and graph partition algorithms, Proceedings of ICCM 2007 Vol. II pp. 1-4, Sect. 2 (Preliminaries), which restates them in her notation; that paper was FETCHED and is archived in the reference library with a row in EXTERNAL_SOURCES.md.  D is the diagonal degree matrix; vol(S) = sum over v in S of d_v, so vol(G) is the sum of all degrees, which is twice the number of edges for an unweighted graph.
#' @export
Degmat <- function(Adj) {
  A <- .t1_mat(Adj); n <- nrow(A)
  if (n == 0L || ncol(A) != n) stop("Adj must be a non-empty square matrix")
  if (any(A < 0)) stop("edge weights must be non-negative")
  if (any(A != t(A))) stop("Adj must be symmetric")
  d <- rowSums(A)

  .t1_result(degrees = d, D = diag(d, n), volume = sum(d), n = n,
             method = "Degree matrix and graph volume")
}
