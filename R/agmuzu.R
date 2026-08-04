# SPDX-License-Identifier: AGPL-3.0-or-later
#' MuZero world model: representation plus dynamics rollout.
#'
#' s^0 = h(o_1..o_t); r^k, s^k = g(s^{k-1}, a^k) for k = 1..K.
#'
#' @param observation Observation history passed to representation.
#' @param actions Actions a^1..a^K.
#' @param representation h, returning the root hidden state.
#' @param dynamics g, returning list(reward, state).
#'
#' @return List with states, rewards, root, K.
#' @references Schrittwieser et al. (2020), arXiv:1911.08265, Equation
#'   (1).  Read from the ar5iv rendering of the arXiv source.
#' @export
Mzworld <- function(observation, actions, representation, dynamics) {
  s0 <- representation(observation)
  s <- s0
  states <- list(s0)
  rewards <- numeric(0)
  for (i in seq_along(actions)) {
    out <- dynamics(s, actions[[i]])
    rewards <- c(rewards, as.numeric(out[[1]]))
    s <- out[[2]]
    states[[length(states) + 1L]] <- s
  }
  .t1_result(states = states, rewards = rewards, root = s0,
             K = length(rewards),
             method = "MuZero world-model rollout (Schrittwieser et al. 2020 eq. 1)")
}
