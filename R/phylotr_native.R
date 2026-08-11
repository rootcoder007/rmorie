# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Neighbor-joining tree construction, original Saitou-Nei (1987)
# algorithm (Phylotr). Bit-identical mirror of
# src/morie/fn/phylotr.py. Anchored on the paper's printed worked
# example (Table 1 matrix; S12 = 36.67, S0 = 39.28, joins and branch
# lengths of Figure 3).

#' Neighbor-joining phylogenetic tree (Saitou-Nei 1987)
#'
#' Starting from a star tree, every pair of current OTUs is scored
#' by the total branch length \eqn{S_{ij}} of the tree joining them
#' (eq. 4 of the paper); the smallest \eqn{S_{ij}} pair is joined.
#' Branch lengths follow eqs. (6a)-(6b),
#' \eqn{L_i = (D_{ij} + D_{iZ} - D_{jZ})/2} with \eqn{D_{iZ}} the
#' average distance to the remaining OTUs, and the combined OTU
#' takes averaged distances \eqn{D_{(ij)k} = (D_{ik} + D_{jk})/2}
#' (eq. 5). The cycle repeats until three OTUs remain. Ties in
#' \eqn{S_{ij}} break to the first pair in row-major order, pinned
#' identically in the Python arm. This is the 1987 algorithm as
#' printed (averaged updates), not the later Studier-Keppler
#' variant.
#'
#' @param distance Symmetric distance matrix.
#' @param labels Optional OTU names (default "1".."n").
#' @return List with \code{joins} (per cycle: labels a, b, new, La,
#'   Lb, S), \code{s0}, \code{final_labels}, \code{final_lengths},
#'   \code{n}, \code{method}.
#' @references Saitou, N. and Nei, M. (1987), The neighbor-joining
#'   method: a new method for reconstructing phylogenetic trees,
#'   Molecular Biology and Evolution 4(4), 406-425. Equations (1),
#'   (4), (5), (6a), (6b), pp. 408-409; worked example Table 1,
#'   Table 2 and Figure 3, pp. 410-411. Local source:
#'   library/pdf/fetched-wave3/Saitou-Nei-1987-NeighborJoining-MBE.pdf.
#' @export
Phylotr <- function(distance, labels = NULL) {
  D <- as.matrix(distance)
  storage.mode(D) <- "double"
  n <- nrow(D)
  if (ncol(D) != n) stop("distance must be square", call. = FALSE)
  if (n < 4L) stop("need at least 4 OTUs", call. = FALSE)
  labs <- if (is.null(labels)) as.character(seq_len(n)) else
    as.character(labels)
  if (length(labs) != n) {
    stop("labels length must match matrix size", call. = FALSE)
  }
  sij <- function(D, m, i, j) {
    ks <- setdiff(seq_len(m), c(i, j))
    t1 <- sum(D[i, ks] + D[j, ks])
    t3 <- 0
    for (k in ks) for (l in ks) if (l > k) t3 <- t3 + D[k, l]
    t1 / (2 * (m - 2)) + D[i, j] / 2 + t3 / (m - 2)
  }
  tot <- sum(D[upper.tri(D)])
  s0 <- tot / (n - 1)
  joins <- list()
  m <- n
  while (m > 3L) {
    best <- Inf; bi <- 0L; bj <- 0L
    for (i in seq_len(m)) {
      for (j in seq_len(m)) {
        if (j > i) {
          s <- sij(D, m, i, j)
          if (s < best) { best <- s; bi <- i; bj <- j }
        }
      }
    }
    ks <- setdiff(seq_len(m), c(bi, bj))
    diz <- sum(D[bi, ks]) / (m - 2)
    djz <- sum(D[bj, ks]) / (m - 2)
    li <- (D[bi, bj] + diz - djz) / 2
    lj <- (D[bi, bj] + djz - diz) / 2
    new_lab <- paste0("(", labs[bi], "-", labs[bj], ")")
    joins[[length(joins) + 1L]] <- list(
      a = labs[bi], b = labs[bj], new = new_lab,
      La = li, Lb = lj, S = best)
    Dn <- matrix(0, m - 1, m - 1)
    Dn[seq_along(ks), seq_along(ks)] <- D[ks, ks]
    v <- (D[bi, ks] + D[bj, ks]) / 2
    Dn[seq_along(ks), m - 1] <- v
    Dn[m - 1, seq_along(ks)] <- v
    D <- Dn
    labs <- c(labs[ks], new_lab)
    m <- m - 1L
  }
  la <- (D[1, 2] + D[1, 3] - D[2, 3]) / 2
  lb <- (D[1, 2] + D[2, 3] - D[1, 3]) / 2
  lc <- (D[1, 3] + D[2, 3] - D[1, 2]) / 2
  list(joins = joins, s0 = s0, final_labels = labs,
       final_lengths = c(la, lb, lc), n = n,
       method = "neighbor joining, original 1987 algorithm (Saitou-Nei)")
}
