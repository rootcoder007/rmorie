# SPDX-License-Identifier: AGPL-3.0-or-later

#' .seq_hamming
#'
#' A step of the cluseq implementation. Called by \code{Cluseq}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param a A vector; its length is taken.
#' @param b A vector; its length is taken.
#' @return A numeric value.
#' @export
.seq_hamming <- function(a, b) {
  if (length(a) != length(b))
    stop("sequences must be the same length to compare")
  sum(a != b)
}

#' SNP-distance sequence clustering
#'
#' Formula: SNP-distance + linkage threshold
#'
#' Pairwise SNP distances are computed on the aligned sequences and
#' isolates are joined into a cluster whenever ANY pair is within the
#' threshold -- single linkage, which is what makes the result a
#' transitive closure rather than a set of cliques.  A threshold of zero
#' leaves every distinct sequence on its own; a threshold at least as
#' large as the diameter collapses everything into one cluster.
#'
#' @param sequences Aligned sequences of equal length.
#' @param snp_threshold Maximum SNP distance for a link.
#' @return List with \code{estimate}, \code{z}, \code{counts},
#'   \code{n_clusters}, \code{distances}, \code{max_distance},
#'   \code{n}, \code{method}.
#' @references Croucher et al. (2015), Nucleic Acids Research
#'   43(3):e15.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Cluseq(V)
Cluseq <- function(sequences, snp_threshold = 5) {
  seqs <- lapply(sequences, function(s)
    if (is.character(s) && length(s) == 1L) strsplit(s, "")[[1]] else s)
  n <- length(seqs)
  if (n == 0L) stop("empty input: no sequences supplied")
  thr <- as.integer(snp_threshold)
  if (thr < 0L) stop("snp_threshold must be non-negative")
  D <- matrix(0L, n, n)
  if (n > 1L) for (i in seq_len(n - 1L)) for (j in seq(i + 1L, n)) {
    d <- .seq_hamming(seqs[[i]], seqs[[j]])
    D[i, j] <- d
    D[j, i] <- d
  }
  parent <- seq_len(n)
  find <- function(a) {
    while (parent[a] != a) {
      parent[a] <<- parent[parent[a]]
      a <- parent[a]
    }
    a
  }
  if (n > 1L) for (i in seq_len(n - 1L)) for (j in seq(i + 1L, n))
    if (D[i, j] <= thr) {
      ra <- find(i)
      rb <- find(j)
      if (ra != rb) parent[max(ra, rb)] <- min(ra, rb)
    }
  roots <- c()
  z <- integer(n)
  for (i in seq_len(n)) {
    r <- find(i)
    if (!(r %in% roots)) roots <- c(roots, r)
    z[i] <- which(roots == r)[1]
  }
  K <- length(roots)
  counts <- vapply(seq_len(K), function(c) sum(z == c), 0L)
  mx <- if (n > 1L) max(D) else 0L
  .t1_result(estimate = K, z = z - 1L, counts = counts, n_clusters = K,
             distances = D, max_distance = as.numeric(mx), n = n,
             method = "SNP-distance single-linkage sequence clustering")
}
