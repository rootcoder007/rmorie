# SPDX-License-Identifier: AGPL-3.0-or-later
#' String similarity that rewards a shared prefix
#'
#' The Winkler adjustment exists because people mistype the ends of names
#' far more often than the beginnings, so a shared prefix is evidence.
#' The boost is capped at four characters to keep the score bounded.
#'
#' Formula: \code{jaro = (m/|s1| + m/|s2| + (m - t)/m)/3},
#' \code{jw = jaro + l p (1 - jaro)}.
#'
#' @param s1,s2 Strings to compare.
#' @param p Prefix scaling factor.
#' @param max_prefix Longest prefix that earns a boost.
#' @return List with \code{estimate}, \code{jaro}, \code{matches},
#'   \code{transpositions}, \code{prefix}.
#' @references Winkler, W. E. (1990). Proc Surv Res Meth Sect ASA
#'   354-359; Jaro, M. A. (1989) JASA 84:414-420.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Jarow(V, V)
Jarow <- function(s1, s2, p = 0.1, max_prefix = 4) {
  a <- strsplit(as.character(s1), "")[[1]]
  b <- strsplit(as.character(s2), "")[[1]]
  la <- length(a)
  lb <- length(b)
  none <- .t1_result(estimate = 0, jaro = 0, matches = 0, transpositions = 0,
                     prefix = 0, method = "Jaro-Winkler string similarity")
  if (la == 0L || lb == 0L) return(none)
  win <- max(max(la, lb) %/% 2L - 1L, 0L)
  fa <- rep(FALSE, la)
  fb <- rep(FALSE, lb)
  m <- 0L
  for (i in seq_len(la)) {
    lo <- max(1L, i - win)
    hi <- min(lb, i + win)
    if (hi < lo) next
    for (j in lo:hi) {
      if (!fb[j] && a[i] == b[j]) { fa[i] <- TRUE
      fb[j] <- TRUE
      m <- m + 1L
      break }
    }
  }
  if (m == 0L) return(none)
  ka <- a[fa]
  kb <- b[fb]
  t_ <- sum(ka != kb) / 2
  jaro <- (m / la + m / lb + (m - t_) / m) / 3
  l <- 0L
  for (i in seq_len(min(max_prefix, la, lb))) {
    if (a[i] == b[i]) l <- l + 1L else break
  }
  .t1_result(estimate = jaro + l * p * (1 - jaro), jaro = jaro, matches = m,
             transpositions = t_, prefix = l,
             method = "Jaro-Winkler string similarity")
}
