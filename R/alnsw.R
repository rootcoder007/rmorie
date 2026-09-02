# SPDX-License-Identifier: AGPL-3.0-or-later
#' Smith-Waterman local sequence alignment
#'
#' Smith and Waterman (1981), "Identification of common molecular
#' subsequences", Journal of Molecular Biology 147(1), 195-197,
#' doi:10.1016/0022-2836(81)90087-5 (citation verified against Crossref).
#'
#' Two changes to the global recurrence make it local, and both are essential:
#' H(i, 0) = H(0, j) = 0 and
#' H(i, j) = max(0, H(i-1, j-1) + s(a_i, b_j), H(i-1, j) - g, H(i, j-1) - g),
#' and the traceback starts at the largest cell anywhere in the matrix and
#' stops at the first zero rather than starting at the corner.  The zero floor
#' is what lets a bad prefix be abandoned instead of dragged along; without it
#' the matrix is Needleman-Wunsch with a zeroed border and the answer is a
#' different alignment.  Consequently the score can never be negative -- the
#' empty alignment always scores zero -- which is checked as an anchor.
#'
#' Ties: the maximum cell is taken in row-major order, earliest first, and
#' traceback prefers diagonal, then up, then left, so both language arms report
#' the same alignment.
#'
#' @param seq1,seq2 the sequences, as single strings or character vectors.
#' @param sub_matrix optional square substitution scores over the sorted symbol
#'   union.
#' @param gap linear gap penalty; non-negative.
#' @return list: score, estimate, aligned1, aligned2, length, start1, end1,
#'   start2, end2, n_match, n_mismatch, n_gap, n, m, method.
#' @keywords internal
#' @examples
#' Alnsw("GGTTGACTA", "TGTTACGG", gap = 2)$score
#' @export
Alnsw <- function(seq1, seq2, sub_matrix = NULL, gap = 1) {
  a <- .aln_symbols(seq1)
  b <- .aln_symbols(seq2)
  n <- length(a)
  m <- length(b)
  if (n == 0L || m == 0L) stop("smith_waterman: neither sequence may be empty")
  g <- as.numeric(gap)
  if (g < 0) stop("smith_waterman: gap must be non-negative")
  alpha <- sort(unique(c(a, b)))
  sf <- .aln_score(sub_matrix, alpha, "smith_waterman")
  H <- matrix(0, nrow = n + 1L, ncol = m + 1L)
  bi <- 0L
  bj <- 0L
  best <- 0
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      d <- H[i, j] + sf(a[i], b[j])
      u <- H[i, j + 1L] - g
      l <- H[i + 1L, j] - g
      v <- 0
      if (d > v) v <- d
      if (u > v) v <- u
      if (l > v) v <- l
      H[i + 1L, j + 1L] <- v
      if (v > best) { best <- v
      bi <- i
      bj <- j }
    }
  }
  o1 <- character(0)
  o2 <- character(0)
  i <- bi
  j <- bj
  while (i > 0L && j > 0L && H[i + 1L, j + 1L] > 0) {
    if (H[i + 1L, j + 1L] == H[i, j] + sf(a[i], b[j])) {
      o1 <- c(a[i], o1)
      o2 <- c(b[j], o2)
      i <- i - 1L
      j <- j - 1L
    } else if (H[i + 1L, j + 1L] == H[i, j + 1L] - g) {
      o1 <- c(a[i], o1)
      o2 <- c("-", o2)
      i <- i - 1L
    } else {
      o1 <- c("-", o1)
      o2 <- c(b[j], o2)
      j <- j - 1L
    }
  }
  nm <- 0L
  nx <- 0L
  ng <- 0L
  for (p in seq_along(o1)) {
    if (o1[p] == "-" || o2[p] == "-") ng <- ng + 1L
    else if (o1[p] == o2[p]) nm <- nm + 1L
    else nx <- nx + 1L
  }
  list(score = best, estimate = best, aligned1 = paste(o1, collapse = ""),
       aligned2 = paste(o2, collapse = ""), length = length(o1),
       start1 = i + 1L, end1 = bi, start2 = j + 1L, end2 = bj,
       n_match = nm, n_mismatch = nx, n_gap = ng, n = n, m = m,
       method = "Smith and Waterman (1981) local DP, zero floor, traceback from the maximum")
}
