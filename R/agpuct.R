# SPDX-License-Identifier: AGPL-3.0-or-later
#' PUCT action selection for AlphaZero-style tree search
#'
#' Rosin (2011), "Multi-armed bandits with episode context", Annals of
#' Mathematics and Artificial Intelligence 61(3), 203-230,
#' doi:10.1007/s10472-011-9258-6, which introduces the predictor form of UCB,
#' and Silver et al. (2017), "Mastering the game of Go without human
#' knowledge", Nature 550, 354-359, doi:10.1038/nature24270, whose search
#' selects a_t = argmax_a (Q(s, a) + U(s, a)) with
#' U(s, a) = c_puct P(s, a) sqrt(sum_b N(s, b)) / (1 + N(s, a)).
#'
#' The shape of U is the whole point.  The numerator grows with the square root
#' of the parent visit count, so exploration decays only slowly as the tree is
#' searched; the denominator is 1 + N, so a child that has never been visited
#' still gets a finite bonus rather than an infinite one, and the prior P is
#' what breaks the tie among them.  Replacing 1 + N with N gives division by
#' zero at the root and is the classic transcription error.
#'
#' At sum_b N(s, b) = 0 the exploration term vanishes for every action and
#' selection falls back to Q alone, which is why AlphaZero seeds the root with
#' one evaluation before selecting.  That degenerate case is checked as an
#' anchor, as is c_puct = 0, which must reduce to greedy Q.  Ties in the argmax
#' go to the lowest index, so the rule is a function and both arms agree.
#'
#' @param P prior probabilities over the actions; non-negative.
#' @param N visit counts; non-negative.
#' @param Q action values.
#' @param c_puct exploration constant; non-negative.
#' @return list: score, estimate, U, action, n_parent, sqrt_n_parent, c_puct,
#'   k, method.
#' @keywords internal
#' @examples
#' Agpuct(c(0.5, 0.3, 0.2), c(3, 1, 0), c(0.1, 0.4, 0.0), 1.5)$action
#' @export
Agpuct <- function(P, N, Q, c_puct = 1) {
  p <- as.numeric(.s03vec(P))
  n <- as.numeric(.s03vec(N))
  q <- as.numeric(.s03vec(Q))
  k <- length(p)
  if (k == 0L) stop("alphazero_puct: no actions")
  if (length(n) != k || length(q) != k) {
    stop("alphazero_puct: P, N and Q must have the same length")
  }
  if (any(p < 0)) stop("alphazero_puct: priors must be non-negative")
  if (any(n < 0)) stop("alphazero_puct: visit counts must be non-negative")
  c <- as.numeric(c_puct)
  if (c < 0) stop("alphazero_puct: c_puct must be non-negative")
  tot <- 0
  for (v in n) tot <- tot + v
  rt <- sqrt(tot)
  U <- numeric(k)
  sc <- numeric(k)
  for (i in seq_len(k)) {
    U[i] <- c * p[i] * rt / (1 + n[i])
    sc[i] <- q[i] + U[i]
  }
  best <- 1L
  for (i in seq_len(k)) if (sc[i] > sc[best]) best <- i
  list(
    score = sc, estimate = sc[best], U = U, action = best - 1L, n_parent = tot,
    sqrt_n_parent = rt, c_puct = c, k = k,
    method = "Q + c_puct P sqrt(sum_b N_b)/(1 + N); Rosin (2011), Silver et al. (2017)"
  )
}
