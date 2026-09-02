# SPDX-License-Identifier: AGPL-3.0-or-later
#' Atom-pair fingerprint.
#'
#' Each unordered pair of atoms contributes the triple (type_i, type_j,
#' bond distance), folded by
#' h = ((ta*1000003 + tb)*1000033 + dist) mod nbits with (ta, tb) sorted.
#'
#' @param adjacency Bond adjacency; non-zero means bonded.
#' @param atomtype Integer atom type per atom.
#' @param nbits Width of the folded fingerprint.
#' @param maxdist Longest topological distance kept.
#'
#' @return List with bits, count, nset, npairs, distance, a, nbits.
#' @references Carhart, Smith and Venkataraghavan (1985), J. Chem. Inf.
#'   Comput. Sci. 25(2), 64-73.  Standard published form; the article is
#'   paywalled and was not read.  The folding hash is this
#'   implementation's own choice, stated rather than attributed.
#' @export
#' @examples
#' Atompairfp(adjacency = 5L, atomtype = 5L)
Atompairfp <- function(adjacency, atomtype, nbits = 2048, maxdist = 30) {
  A <- .t1_mat(adjacency); a <- nrow(A)
  if (ncol(A) != a) stop("adjacency must be square")
  t <- as.integer(.t1_vec(atomtype))
  if (length(t) != a) stop("atomtype must have one entry per atom")
  nbits <- as.integer(nbits)
  if (nbits < 1L) stop("nbits must be positive")
  D <- matrix(-1L, a, a)
  for (s in seq_len(a)) {
    dist <- rep(-1L, a); dist[s] <- 0L; frontier <- s
    while (length(frontier) > 0L) {
      nxt <- integer(0)
      for (u in frontier) {
        v <- which(A[u, ] != 0 & dist == -1L)
        if (length(v) > 0L) { dist[v] <- dist[u] + 1L; nxt <- c(nxt, v) }
      }
      frontier <- nxt
    }
    D[s, ] <- dist
  }
  bits <- integer(nbits); cnt <- integer(nbits)
  npairs <- 0L; dists <- integer(0)
  if (a > 1L) for (i in seq_len(a - 1L)) for (j in (i + 1L):a) {
    dd <- D[i, j]
    if (dd == -1L || dd > as.integer(maxdist)) next
    ta <- min(t[i], t[j]); tb <- max(t[i], t[j])
    h <- ((ta * 1000003 + tb) * 1000033 + dd) %% nbits
    bits[h + 1L] <- 1L
    cnt[h + 1L] <- cnt[h + 1L] + 1L
    npairs <- npairs + 1L
    dists <- c(dists, dd)
  }
  .t1_result(bits = bits, count = cnt, nset = sum(bits), npairs = npairs,
             distance = dists, a = a, nbits = nbits,
             method = "Atom-pair fingerprint (Carhart et al. 1985)")
}
