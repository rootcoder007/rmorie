# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero MCTS expansion of a newly reached state
#'
#' Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED), appendix B
#' ("Expansion"), and Silver et al. (2017), Nature 550, 354-359: when a
#' simulation reaches a state not yet in the tree the network is
#' evaluated once, (p, v) = f_theta(s), each edge gets P(s,a) = p_a, and
#' N = W = Q = 0.  Illegal moves are masked and the remaining priors
#' renormalised, since the policy head is over the whole action space.
#'
#' @param state the state being expanded; carried through untouched.
#' @param policy_net a function s -> list(p, v) or s -> p, or the prior
#'   vector itself.
#' @param legal legal-move mask over the action space.
#' @param logits treat the output as logits and softmax it first.
#' @return list: estimate (the value v), value, p, n, w, q, state, method.
#' @keywords internal
#' @examples
#' Mctsexpand(0, c(0.2, 0.5, 0.3))$p
#' @export
Mctsexpand <- function(state, policy_net, legal = NULL, logits = FALSE) {
  out <- if (is.function(policy_net)) policy_net(state) else policy_net
  v <- NaN
  if (is.list(out) && length(out) == 2L) {
    raw <- .s03vec(out[[1]]); v <- as.numeric(out[[2]])
  } else {
    raw <- .s03vec(out)
  }
  if (logits) raw <- .s03softmax(raw)
  m <- length(raw)
  mask <- if (is.null(legal)) rep(1, m) else as.numeric(as.logical(legal))
  masked <- raw * mask
  tot <- 0
  for (x in masked) tot <- tot + x
  if (tot > 0) {
    p <- masked / tot
  } else {
    live <- 0
    for (x in mask) live <- live + x
    p <- if (live > 0) mask / live else rep(0, m)
  }
  list(estimate = v, value = v, p = p, n = numeric(m), w = numeric(m),
       q = numeric(m), state = state,
       method = "AlphaZero MCTS expansion via the policy network")
}
