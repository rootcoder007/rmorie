# SPDX-License-Identifier: AGPL-3.0-or-later
#' Needleman-Wunsch global sequence alignment
#'
#' Needleman and Wunsch (1970), "A general method applicable to the search for
#' similarities in the amino acid sequence of two proteins", Journal of
#' Molecular Biology 48(3), 443-453, doi:10.1016/0022-2836(70)90057-4
#' (citation verified against Crossref).
#'
#' The dynamic program fills an (n+1) x (m+1) matrix whose entry F(i, j) is the
#' best score of an alignment of the first i symbols of seq1 against the first
#' j of seq2: F(0, 0) = 0, F(i, 0) = -i g, F(0, j) = -j g, and
#' F(i, j) = max(F(i-1, j-1) + s(a_i, b_j), F(i-1, j) - g, F(i, j-1) - g).
#'
#' Global is the operative word: the first row and column carry the full gap
#' penalty, so every symbol of both sequences must be placed.  Zeroing that
#' border is the single edit that turns this into a semi-global (overlap)
#' alignment and returns a different, usually larger, number for the same
#' inputs, which is why the border initialisation is asserted directly in the
#' tests and not only through the final score.
#'
#' Traceback prefers, on ties, the diagonal, then the up move, then the left
#' move.  The preference is arbitrary but fixed, so the two language arms
#' return the same alignment and not merely the same score.
#'
#' sub_matrix, when given, is indexed by the sorted union of the symbols
#' occurring in the two sequences; when omitted, matches score +1 and
#' mismatches -1.
#'
#' @param seq1,seq2 the sequences, as single strings or character vectors;
#'   neither may be empty.
#' @param sub_matrix optional square substitution scores over the sorted symbol
#'   union.
#' @param gap linear gap penalty subtracted per gapped position; non-negative.
#' @return list: score, estimate, aligned1, aligned2, length, n_match,
#'   n_mismatch, n_gap, n, m, method.
#' @keywords internal
#' @examples
#' Alnnw("GATTACA", "GCATGCU")$score
#' @export
Alnnw <- function(seq1, seq2, sub_matrix = NULL, gap = 1) {
  a <- .aln_symbols(seq1); b <- .aln_symbols(seq2)
  n <- length(a); m <- length(b)
  if (n == 0L || m == 0L) stop("needleman_wunsch: neither sequence may be empty")
  g <- as.numeric(gap)
  if (g < 0) stop("needleman_wunsch: gap must be non-negative")
  alpha <- sort(unique(c(a, b)))
  sf <- .aln_score(sub_matrix, alpha, "needleman_wunsch")
  F <- matrix(0, nrow = n + 1L, ncol = m + 1L)
  for (i in seq_len(n)) F[i + 1L, 1L] <- -g * i
  for (j in seq_len(m)) F[1L, j + 1L] <- -g * j
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      d <- F[i, j] + sf(a[i], b[j])
      u <- F[i, j + 1L] - g
      l <- F[i + 1L, j] - g
      best <- d
      if (u > best) best <- u
      if (l > best) best <- l
      F[i + 1L, j + 1L] <- best
    }
  }
  o1 <- character(0); o2 <- character(0)
  i <- n; j <- m
  while (i > 0L || j > 0L) {
    if (i > 0L && j > 0L && F[i + 1L, j + 1L] == F[i, j] + sf(a[i], b[j])) {
      o1 <- c(a[i], o1); o2 <- c(b[j], o2); i <- i - 1L; j <- j - 1L
    } else if (i > 0L && F[i + 1L, j + 1L] == F[i, j + 1L] - g) {
      o1 <- c(a[i], o1); o2 <- c("-", o2); i <- i - 1L
    } else {
      o1 <- c("-", o1); o2 <- c(b[j], o2); j <- j - 1L
    }
  }
  nm <- 0L; nx <- 0L; ng <- 0L
  for (p in seq_along(o1)) {
    if (o1[p] == "-" || o2[p] == "-") ng <- ng + 1L
    else if (o1[p] == o2[p]) nm <- nm + 1L
    else nx <- nx + 1L
  }
  list(score = F[n + 1L, m + 1L], estimate = F[n + 1L, m + 1L],
       aligned1 = paste(o1, collapse = ""), aligned2 = paste(o2, collapse = ""),
       length = length(o1), n_match = nm, n_mismatch = nx, n_gap = ng, n = n, m = m,
       method = "Needleman and Wunsch (1970) global DP with linear gap penalty")
}

#' @noRd
.aln_symbols <- function(s) {
  if (is.character(s) && length(s) == 1L) strsplit(s, "", fixed = TRUE)[[1]] else as.character(s)
}

#' @noRd
.aln_score <- function(sub_matrix, alpha, who) {
  if (is.null(sub_matrix)) return(function(x, y) if (x == y) 1 else -1)
  M <- as.matrix(sub_matrix); storage.mode(M) <- "double"
  if (nrow(M) != length(alpha) || ncol(M) != length(alpha)) {
    stop(sprintf("%s: sub_matrix must be square over the symbol alphabet", who))
  }
  function(x, y) M[match(x, alpha), match(y, alpha)]
}
