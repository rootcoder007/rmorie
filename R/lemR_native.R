# SPDX-License-Identifier: AGPL-3.0-or-later
#' Leiden refined community detection
#'
#' Alias of \code{\link{Leidenclus}}, the Leiden algorithm of Traag, Waltman
#' and van Eck (2019, Algorithm A.2): fast local moving, refinement of each
#' community into well-connected sub-communities, and aggregation, repeated.
#'
#' @param A Symmetric non-negative weighted adjacency matrix.
#' @param resolution Resolution parameter gamma.
#' @param quality "modularity" or "cpm".
#' @param max_iter Leiden iterations.
#' @param seed Philox seed.
#' @return List with labels (0-based, matching the Python arm),
#'   estimate, quality, n_communities, connected, passes, n, method.
#' @references Traag, V. A., Waltman, L. and van Eck, N. J. (2019).
#'   From Louvain to Leiden: guaranteeing well-connected communities.
#'   Scientific Reports, 9, 5233, arXiv:1810.08473.
#' @examples
#' blocks <- matrix(0, 6, 6)
#' blocks[1:3, 1:3] <- 1; blocks[4:6, 4:6] <- 1; diag(blocks) <- 0
#' blocks[3, 4] <- blocks[4, 3] <- 1
#' LemR(blocks)$labels
#' @export
LemR <- function(A, resolution = 1, quality = "modularity", max_iter = 20L, seed = 0) {
  .morie_arg(A, "m")
  Leidenclus(A, resolution = resolution, quality = quality, max_iter = max_iter, seed = seed)
}

#' @rdname LemR
#' @export
leiden_grph <- LemR
