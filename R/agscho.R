# SPDX-License-Identifier: AGPL-3.0-or-later
#' Search-horizon control: truncate the search and bootstrap the value
#'
#' Schrittwieser et al. (2020), arXiv:1911.08265 (FETCHED), appendix B,
#' writes the backup for a search truncated at depth l as
#' G^k = sum_{tau = 0}^{l - 1 - k} gamma^tau r_(k+1+tau) + gamma^(l-k) v^l:
#' the discounted rewards down to the truncation depth plus the discounted
#' network value at the truncated leaf.  Silver et al. (2018),
#' arXiv:1712.01815 (FETCHED), uses the undiscounted two-player special
#' case.  This is the n-step return of Sutton and Barto (2018),
#' Reinforcement Learning: An Introduction, 2nd ed., eq. (7.1) (FETCHED
#' from incompleteideas.net), with the bootstrap supplied by the value
#' network.
#'
#' @param depth_limit truncation depth l.
#' @param state carried through untouched.
#' @param rewards rewards along the principal variation.
#' @param values value estimates per depth; v^l is the bootstrap.
#' @param gamma discount.
#' @param k_start depth the return is computed from.
#' @return list: estimate, bootstrap, reward_part, depth, state, method.
#' @keywords internal
#' @examples
#' Searchhoriz(2, 0, c(1, 1, 1), c(0, 0, 5))$estimate
#' @export
Searchhoriz <- function(depth_limit, state, rewards = NULL, values = NULL,
                        gamma = 1, k_start = 0) {
  r <- if (!is.null(rewards)) .s03vec(rewards) else numeric(0)
  v <- if (!is.null(values)) .s03vec(values) else numeric(0)
  l <- as.integer(depth_limit)
  if (l > length(r)) l <- if (length(r)) length(r) else l
  kk <- as.integer(k_start)
  g <- as.numeric(gamma)
  part <- 0; tau <- 0L
  while (kk + tau < l) {
    part <- part + (g^tau) * r[kk + tau + 1L]
    tau <- tau + 1L
  }
  boot <- if (length(v)) {
    idx <- if (l < length(v)) l else length(v) - 1L
    (g^(l - kk)) * v[idx + 1L]
  } else 0
  list(estimate = part + boot, bootstrap = boot, reward_part = part,
       depth = l, state = state,
       method = "Truncated search return: discounted rewards + bootstrapped value")
}
