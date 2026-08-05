# SPDX-License-Identifier: AGPL-3.0-or-later
#' Partition a graph into well-connected communities
#'
#' Louvain can leave a community internally disconnected -- a node moved
#' away can sever the group it left behind, and nothing in the algorithm
#' ever checks. Leiden inserts a refinement phase that guarantees every
#' returned community is connected, which is why its partitions survive
#' being looked at.
#'
#' An alias. The optimisation is \code{\link{Leidenclus}};
#' \code{ledger/wave2/DUPMAP.tsv} records \code{leid} as a duplicate of
#' \code{scleid} and it is the same algorithm on the same quality
#' function, so only the calling convention differs here.
#'
#' Formula: local move, refinement, then aggregation, repeated -- Traag,
#' Waltman and van Eck (2019).
#'
#' @param y Ignored; accepted for interface compatibility.
#' @param A Weighted adjacency matrix, n by n.
#' @param resolution Resolution parameter of the quality function.
#' @param quality Quality function, passed through.
#' @param max_iter Passes.
#' @return Whatever \code{\link{Leidenclus}} returns, unchanged.
#' @references Traag, V. A., Waltman, L. and van Eck, N. J. (2019).
#'   Scientific Reports 9:5233. \doi{10.1038/s41598-019-41695-z}.
#' @seealso \code{\link{Leidenclus}}
#' @export
Leid <- function(y, A, resolution = 1, quality = "modularity",
                 max_iter = 20) {
  Leidenclus(A, resolution, quality, max_iter)
}
