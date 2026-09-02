# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero temperature decay for move selection
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED), and Silver et al.
#' (2017), Nature 550, 354-359: during self-play a move is sampled in
#' proportion to N(s,a)^(1/tau), with tau = 1 for the first `threshold`
#' moves (AlphaGo Zero uses 30) and tau -> 0 thereafter, which makes the
#' selection greedy.  Passing tau to zero would divide by zero, so the
#' greedy branch is taken directly; ties break to the lowest index.
#'
#' @param move_count zero-based index of the move about to be played.
#' @param threshold number of opening moves played at tau = 1.
#' @param N optional root visit counts; the policy pi is then returned.
#' @return list: estimate (tau), tau, greedy, pi, threshold, method.
#' @keywords internal
#' @examples
#' Tempdecay(5, 30, c(10, 20, 5))$pi
#' @export
Tempdecay <- function(move_count, threshold = 30L, N = NULL) {
  mc <- as.integer(move_count)
  th <- as.integer(threshold)
  greedy <- mc >= th
  tau <- if (greedy) 0 else 1
  pi_ <- numeric(0)
  if (!is.null(N)) {
    n <- .s03vec(N)
    if (greedy) {
      best <- 1L
      if (length(n) > 1L) for (a in seq(2L, length(n))) if (n[a] > n[best]) best <- a
      pi_ <- as.numeric(seq_along(n) == best)
    } else {
      tot <- 0
      for (x in n) tot <- tot + x
      pi_ <- if (tot > 0) n / tot else rep(0, length(n))
    }
  }
  list(
    estimate = tau, tau = tau, greedy = greedy, pi = pi_, threshold = th,
    method = "AlphaZero temperature schedule (tau = 1 then greedy)"
  )
}
