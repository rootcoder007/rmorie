# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hidden Markov part-of-speech tagging by the Viterbi algorithm
#'
#' Viterbi dynamic programming finds the best tag sequence in
#' O(T |S|^2) rather than the |S|^T sequences a brute-force search would
#' visit.  Working in logs keeps the recursion safe for long sentences.
#' The tests check the returned path against exhaustive enumeration on a
#' short sentence, which is the only way to be sure the back-pointers
#' are right.
#'
#' Formula: argmax_y prod_t P(y_t | y_{t-1}) P(x_t | y_t).
#'
#' @param X Word indices, 0-based.
#' @param tagset Tag labels; only the count is used.
#' @param start Initial tag distribution.
#' @param trans Tag transition matrix.
#' @param emit Emission matrix, tags by vocabulary.
#' @return List with \code{estimate} (best log probability),
#'   \code{path} (1-based tag indices), \code{logprob}, \code{n},
#'   \code{method}.
#' @references Charniak (1993), Statistical Language Learning, MIT
#'   Press, ch. 3; Viterbi (1967), IEEE Transactions on Information
#'   Theory 13(2):260-269. \doi{10.1109/TIT.1967.1054010}
#' @export
#' @examples
#' X <- c(0, 1, 2, 1, 0)
#' tagset <- c("N", "V")
#' start <- c(0.5, 0.5)
#' trans <- matrix(c(0.7, 0.3, 0.4, 0.6), 2, 2, byrow = TRUE)
#' emit <- matrix(c(0.3, 0.4, 0.3, 0.2, 0.3, 0.5), 2, 3, byrow = TRUE)
#' HmmTag(X, tagset, start, trans, emit)
HmmTag <- function(X, tagset, start = NULL, trans = NULL, emit = NULL) {
  xs <- as.integer(.s03vec(X))
  T <- length(xs)
  if (T == 0L) stop("hmm_pos: X is empty")
  S <- length(tagset)
  if (S < 1L) stop("hmm_pos: tagset is empty")
  if (is.null(start) || is.null(trans) || is.null(emit))
    stop("hmm_pos: start, trans and emit must be supplied")
  pi0 <- .s03vec(start); A <- .s03mat(trans); B <- .s03mat(emit)
  if (length(pi0) != S || nrow(A) != S || nrow(B) != S)
    stop("hmm_pos: start, trans and emit must match the tagset size")
  V <- ncol(B)
  if (any(xs < 0L | xs >= V)) stop("hmm_pos: observation index out of range")
  lg <- function(p) if (p > 0) log(p) else -Inf
  delta <- matrix(-Inf, T, S); psi <- matrix(0L, T, S)
  for (s in seq_len(S)) delta[1, s] <- lg(pi0[s]) + lg(B[s, xs[1] + 1L])
  if (T > 1L) for (t in 2:T) for (s in seq_len(S)) {
    best <- -Inf; arg <- 1L
    for (r in seq_len(S)) {
      v <- delta[t - 1L, r] + lg(A[r, s])
      if (v > best) { best <- v; arg <- r }
    }
    delta[t, s] <- best + lg(B[s, xs[t] + 1L])
    psi[t, s] <- arg
  }
  end <- 1L
  for (s in seq_len(S)) if (delta[T, s] > delta[T, end]) end <- s
  path <- integer(T); path[T] <- end
  if (T > 1L) for (t in seq(T - 1L, 1L)) path[t] <- psi[t + 1L, path[t + 1L]]
  .t1_result(estimate = delta[T, end], path = path, logprob = delta[T, end],
             n = T,
             method = "Viterbi maximisation of prod P(y_t|y_{t-1}) P(x_t|y_t), Charniak (1993) ch. 3")
}
