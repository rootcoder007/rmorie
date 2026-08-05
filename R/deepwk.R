# SPDX-License-Identifier: AGPL-3.0-or-later

#' DeepWalk node embeddings (alias of Deepw)
#'
#' \code{deepwk} and \code{deepw} are the SAME method: truncated uniform
#' random walks fed to skip-gram, Perozzi, Al-Rfou & Skiena (2014).  Two
#' module names for one method is exactly the duplicate this campaign is
#' trying not to create, so this function forwards to \code{Deepw}
#' rather than repeating it -- a second copy would agree with the first
#' at 1e-9 forever and tell nobody anything.
#'
#' @param G An n x n adjacency matrix.
#' @param walk_len Length of each walk.
#' @param dim Embedding dimension.
#' @param n_walks Walks started from each node.
#' @param window Skip-gram context window.
#' @param epochs Passes over the corpus.
#' @param lr SGD step size.
#' @param neg Negative samples per positive pair.
#' @param seed Seed of the deterministic stream.
#' @return Whatever \code{\link{Deepw}} returns.
#' @references Perozzi, Al-Rfou & Skiena (2014), DeepWalk, KDD
#'   2014:701-710.
#' @export
Deepwk <- function(G, walk_len = 10, dim = 8, n_walks = 4, window = 3,
                   epochs = 1, lr = 0.05, neg = 2, seed = 42) {
  Deepw(G, walk_len, dim, n_walks, window, epochs, lr, neg, seed)
}
