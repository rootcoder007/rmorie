# SPDX-License-Identifier: AGPL-3.0-or-later
#' Count the distinct phrases a sequence needs to build itself
#'
#' An algorithmic rather than statistical notion of randomness: a
#' sequence is complex when it cannot be assembled cheaply from its own
#' past. A periodic signal parses into a handful of phrases however long
#' it runs; noise needs a new phrase almost every step. No distribution
#' and no stationarity required.
#'
#' Formula: \code{C(s)} is the number of phrases in the LZ76 exhaustive
#' parse; normalised, \code{C(s) log_a(n) / n}.
#'
#' @param y Sequence; values compared for equality.
#' @return List with \code{estimate}, \code{normalized}, \code{alphabet}, \code{n}.
#' @references Lempel, A. & Ziv, J. (1976). IEEE Trans Inform Theory
#'   22:75-81.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Lzcomp(V)
Lzcomp <- function(y) {
  s <- as.character(as.numeric(unlist(y)))
  n <- length(s)
  i <- 0L
  k <- 1L
  l <- 1L
  c_ <- 1L
  kmax <- 1L
  repeat {
    if (s[i + k] == s[l + k]) {
      k <- k + 1L
      if (l + k > n) { c_ <- c_ + 1L
      break }
    } else {
      if (k > kmax) kmax <- k
      i <- i + 1L
      if (i == l) {
        c_ <- c_ + 1L
        l <- l + kmax
        if (l + 1L > n) break
        i <- 0L
        k <- 1L
        kmax <- 1L
      } else k <- 1L
    }
  }
  a <- length(unique(s))
  norm <- if (a > 1L && n > 1L) c_ * (log(n) / log(a)) / n else NaN
  .t1_result(estimate = as.numeric(c_), normalized = norm, alphabet = a, n = n,
             method = "Lempel-Ziv complexity, LZ76 parse")
}
