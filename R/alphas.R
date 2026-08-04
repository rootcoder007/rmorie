# SPDX-License-Identifier: AGPL-3.0-or-later
#' One AlphaZero self-play game and its training targets
#'
#' Silver et al. (2018), arXiv:1712.01815 (FETCHED): a game is played by
#' running MCTS at every move and playing a_t ~ pi_t, the normalised root
#' visit count; the record is (s_t, pi_t, z) with z the outcome from the
#' point of view of the player to move at t, and those triples are the
#' targets for l = (z - v)^2 - pi' log p + c ||theta||^2.
#'
#' Determinism: selection is greedy once the temperature has decayed
#' (AlphaGo Zero: after 30 moves) and, while tau = 1, uses the inverse CDF
#' of pi at a van der Corput point rather than a draw.  The rollout budget
#' is a fixed simulation count.
#'
#' @param state starting state id.
#' @param policy function s -> list(p, v), or s -> p with `value` given.
#' @param value optional function s -> v.
#' @param mcts_iter simulations per move.
#' @param step,terminal,outcome environment callbacks.
#' @param max_moves hard cap on game length.
#' @param temp_threshold moves played at tau = 1.
#' @param c_puct exploration constant.
#' @return list: estimate, states, pis, actions, zs, moves, final_state,
#'   method.
#' @keywords internal
#' @examples
#' Azselfplay(0, function(s) list(c(0.5, 0.5), 0), mcts_iter = 4,
#'            max_moves = 1)$moves
#' @export
Azselfplay <- function(state, policy, value = NULL, mcts_iter = 16,
                       step = NULL, terminal = NULL, outcome = NULL,
                       max_moves = 32, temp_threshold = 30, c_puct = 1.25) {
  net <- function(s) {
    out <- policy(s)
    if (is.list(out) && length(out) == 2L) return(out)
    list(out, if (!is.null(value)) value(s) else 0)
  }
  s <- state
  states <- list(); pis <- list(); acts <- integer(0)
  m <- 0L
  while (m < as.integer(max_moves)) {
    if (!is.null(terminal) && terminal(s)) break
    res <- Azsearch(s, net, mcts_iter, step = step, c_puct = c_puct,
                    terminal = terminal)
    pi_ <- res$pi
    states[[length(states) + 1L]] <- s
    pis[[length(pis) + 1L]] <- pi_
    if (m >= as.integer(temp_threshold)) {
      a <- res$action
    } else {
      u <- .s03vdc(m, 2L)
      cc <- 0; a <- length(pi_) - 1L
      for (j in seq_along(pi_)) {
        cc <- cc + pi_[j]
        if (u < cc) { a <- j - 1L; break }
      }
    }
    acts <- c(acts, as.integer(a))
    if (is.null(step)) break
    s <- step(s, a)
    m <- m + 1L
  }
  z <- if (!is.null(outcome)) as.numeric(outcome(s)) else 0
  zs <- numeric(length(states)); sign <- 1
  if (length(states) > 0L) for (i in seq_along(states)) { zs[i] <- z * sign; sign <- -sign }
  list(estimate = z, states = states, pis = pis, actions = acts, zs = zs,
       moves = length(states), final_state = s,
       method = "AlphaZero self-play game producing (s, pi, z) targets")
}
