# SPDX-License-Identifier: AGPL-3.0-or-later
#' Levenshtein distance between two sequences.
#'
#' Formula: the Wagner-Fischer dynamic program, \eqn{D[0,0]=0},
#' \eqn{D[i,0] = i c_{del}}, \eqn{D[0,j] = j c_{ins}},
#' \eqn{D[i,j] = \min(D[i-1,j]+c_{del},\, D[i,j-1]+c_{ins},\,
#' D[i-1,j-1] + c_{sub}[s_1[i] \ne s_2[j]])}, answer at \eqn{D[m,n]}.
#' Only the previous row is kept, so memory is O(n); the recurrence is
#' unchanged.  Inputs are compared element by element, so token vectors
#' work as well as strings.  Unit costs give Levenshtein's metric;
#' unequal costs break symmetry, deliberately.
#'
#' @param s1,s2 Sequences to compare (character strings or vectors).
#' @param insert,delete,substitute Edit costs.
#' @return The edit distance.
#' @references Levenshtein (1966), Soviet Physics Doklady 10:707-710 (the metric); Wagner and Fischer (1974), JACM 21:168-173 (the dynamic program).  Neither was fetchable -- Doklady is not online and JACM is paywalled -- so this is the standard published recurrence, anchored in the harness against R's own utils::adist.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Editdist(V, V)
Editdist <- function(s1, s2, insert = 1, delete = 1, substitute = 1) {
  a <- if (is.character(s1) && length(s1) == 1L) strsplit(s1, "", fixed = TRUE)[[1]] else s1
  b <- if (is.character(s2) && length(s2) == 1L) strsplit(s2, "", fixed = TRUE)[[1]] else s2
  m <- length(a); n <- length(b)
  if (m == 0L) return(n * insert)
  if (n == 0L) return(m * delete)
  prev <- (0:n) * insert
  for (i in seq_len(m)) {
    cur <- numeric(n + 1L); cur[1] <- i * delete
    for (j in seq_len(n)) {
      cost <- if (identical(a[i], b[j])) 0 else substitute
      cur[j + 1L] <- min(prev[j + 1L] + delete, cur[j] + insert, prev[j] + cost)
    }
    prev <- cur
  }
  prev[n + 1L]
}
