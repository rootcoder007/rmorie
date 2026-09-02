# HOT SAX: the time-series discord (most unusual subsequence).
# Source: Keogh, E., Lin, J. and Fu, A. (2005), HOT SAX: efficiently
# finding the most unusual time series subsequence, ICDM 2005, 226-233:
# the brute-force definition of Table 1, the heuristic reordering of
# Table 2 (outer loop over subsequences whose SAX word is rarest,
# inner loop over subsequences sharing that word), and the early
# abandon of their line 9 / Observation 1.
#
# Native implementation mirroring Python morie.fn.hot exactly: same
# self-match exclusion |p - q| < n, same tie-breaking by index, same
# early-abandon condition.

#' .mor_hot_znorm
#'
#' A step of the hot_native implementation. Called by \code{morie_hot}.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param seg A vector; its length is taken.
#' @return A numeric value.
#' @export
.mor_hot_znorm <- function(seg) {
  m <- mean(seg)
  sdv <- sqrt(mean((seg - m)^2))
  if (sdv < 1e-12) return(rep(0, length(seg)))
  (seg - m) / sdv
}

#' HOT SAX discord discovery
#'
#' Finds the subsequence whose distance to its nearest non-self-match
#' is largest -- the discord of Keogh, Lin and Fu (2005).  The
#' ordering heuristic of their Table 2 makes the search fast without
#' changing the answer: it is exact, not approximate, because the
#' early abandon only discards candidates that provably cannot win.
#'
#' @param x Numeric series.
#' @param window Subsequence length; needs \code{2 <= window <=
#'   length(x)/2}.
#' @param alphabet SAX alphabet size for the ordering heuristic,
#'   default 3.
#' @param word_length SAX word length; \code{NULL} (default) picks the
#'   largest of 3 then 2 that divides \code{window}, else 1.
#' @return A list with \code{location} (1-based start of the
#'   discord), \code{distance}, \code{neighbor} (1-based start of its
#'   nearest neighbour), \code{window}, \code{estimate}, \code{n},
#'   \code{method}.
#' @references Keogh, E., Lin, J. and Fu, A. (2005). HOT SAX:
#'   efficiently finding the most unusual time series subsequence.
#'   ICDM 2005, 226-233.
#' @export
#' @examples
#' morie_hot(x = c(1, 2, 3, 4, 5, 6, 7, 8), window = 3L)
morie_hot <- function(x, window, alphabet = 3L, word_length = NULL) {
  xs <- as.numeric(x)
  m <- length(xs)
  n <- as.integer(window)
  if (n < 2L || n > m %/% 2L) stop("need 2 <= window <= len(x)/2")
  nsub <- m - n + 1L
  subs <- lapply(seq_len(nsub), function(p) .mor_hot_znorm(xs[p:(p + n - 1L)]))
  if (is.null(word_length)) {
    word_length <- 1L
    for (w in c(3L, 2L)) if (n %% w == 0L) { word_length <- w; break }
  }
  word_length <- as.integer(word_length)
  words <- vapply(seq_len(nsub), function(p)
    morie_saxR(xs[p:(p + n - 1L)], word_length, alphabet)$word, character(1))
  cnt <- table(words)
  wcount <- as.numeric(cnt[words])
  outer_order <- order(wcount, seq_len(nsub))
  best_dist <- -1
  # 0 = "none found", matching the Python arm's -1 + 1 convention
  best_loc <- 0L
  best_nb <- 0L
  for (p in outer_order) {
    nnd <- Inf
    nnq <- -1L
    same <- which(words == words[p])
    inner <- c(same, which(words != words[p]))
    for (q in inner) {
      if (abs(p - q) < n) next
      d <- sqrt(sum((subs[[p]] - subs[[q]])^2))
      if (d < best_dist) {
        # Observation 1 / line 9 of their Table 2: p cannot be the
        # discord, so abandon this inner loop immediately
        nnd <- -Inf
        break
      }
      if (d < nnd) { nnd <- d; nnq <- q }
    }
    if (nnd > best_dist && nnq >= 1L) {
      best_dist <- nnd
      best_loc <- p
      best_nb <- nnq
    }
  }
  list(location = best_loc, distance = best_dist, neighbor = best_nb,
       window = n, estimate = best_loc, n = m,
       method = "HOT SAX discord (Keogh-Lin-Fu 2005)")
}
