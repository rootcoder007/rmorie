# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaZero MCTS node initialisation
#'
#' Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED), appendix B,
#' and Silver et al. (2017), Nature 550, 354-359: a fresh node stores, per
#' edge, N = 0, W = 0, Q = 0 and P = p_a.  Everything beyond the priors is
#' zero, which is the point -- AlphaZero carries no rollout statistics and
#' no heuristic, so a node is fully described by the policy prior until
#' the first backup reaches it.
#'
#' @param p prior probabilities from the policy head.
#' @param action_space optional action-space size; p is zero-padded or
#'   truncated to it.
#' @return list: estimate (edge count), p, n, w, q, prior_sum, method.
#' @keywords internal
#' @examples
#' Mctsnode(c(0.2, 0.8))$p
#' @export
Mctsnode <- function(p, action_space = NULL) {
  pr <- .s03vec(p)
  if (!is.null(action_space)) {
    m <- as.integer(action_space)
    pr <- if (length(pr) < m) c(pr, rep(0, m - length(pr))) else pr[seq_len(m)]
  }
  m <- length(pr)
  tot <- 0
  for (x in pr) tot <- tot + x
  if (tot > 0) { pr <- pr / tot; tot <- 1 }
  list(estimate = as.numeric(m), p = pr, n = numeric(m), w = numeric(m),
       q = numeric(m), prior_sum = tot,
       method = "AlphaZero MCTS node initialisation (N=W=Q=0, P=p)")
}
