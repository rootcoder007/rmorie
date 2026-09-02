# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Dueling network architecture (Wang et al. 2016) with a Double-DQN
# target (van Hasselt et al. 2016). Bit-identical mirror of
# src/morie/fn/dueldqn.py.

# ---------------------------------------------------------------------------
# Dueling aggregation: V(s) + (A(s,a) - anchor), anchor = mean | max | 0
# ---------------------------------------------------------------------------
# Eq. (9) of Wang et al. (2016) is the default: subtract the mean of the
# advantage vector. Eq. (8) subtracts the max instead. The naive form
# (mode = "naive") is unidentifiable -- it is provided so that the
# failure can be demonstrated rather than described.

.dueldqn_AGGS <- c("mean", "max", "naive")

#' .dueldqn_check_mode
#'
#' A step of the dueldqn_native implementation. Called by \code{dueling_aggregate}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param mode Passed to \code{\%in\%}.
#' @return One of two values, depending on the branch taken.
#' @export
.dueldqn_check_mode <- function(mode) {
  if (!(mode %in% .dueldqn_AGGS))
    stop(sprintf("duel: mode must be one of %s, got '%s'",
                 paste(.dueldqn_AGGS, collapse = ", "), mode),
         call. = FALSE)
}

#' .dueld_anchor
#'
#' A step of the dueldqn_native implementation. Called by \code{dueling_aggregate}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a A vector; its length is taken and its elements indexed.
#' @param mode One of \code{"max"}, \code{"mean"}.
#' @return One of two values, depending on the branch taken.
#' @export
.dueld_anchor <- function(a, mode) {
  if (mode == "mean") {
    sum(a) / length(a)
  } else if (mode == "max") {
    mx <- a[1]
    for (i in seq_along(a)[-1L]) if (a[i] > mx) mx <- a[i]
    mx
  } else {
    0
  }
}

#' Duel aggregation: combine a value and an advantage vector into Q(s, .)
#'
#' Implements the recombination rules of Wang et al. (2016) for the
#' dueling network architecture.  Eq. (9) (the default) subtracts the
#' mean of the advantage vector; Eq. (8) subtracts the max; the naive
#' form \code{Q = V + A} is unidentifiable and is exposed under
#' \code{mode = "naive"} only so its failure can be demonstrated.
#'
#' @param value Scalar value V(s).
#' @param advantage Numeric vector of advantages A(s, .).
#' @param mode One of \code{"mean"} (eq. 9, default), \code{"max"}
#'   (eq. 8) or \code{"naive"}.
#' @return Numeric vector of Q(s, .).
#' @references Wang, Z., Schaul, T., Hessel, M., van Hasselt, H.,
#'   Lanctot, M. & de Freitas, N. (2016). Dueling Network Architectures
#'   for Deep Reinforcement Learning. ICML / PMLR 48, 1995-2003,
#'   arXiv:1511.06581, eq. (7)-(9).
#' @export
dueling_aggregate <- function(value, advantage, mode = "mean") {
  .dueldqn_check_mode(mode)
  a <- as.numeric(advantage)
  if (length(a) == 0L) stop("duel: no actions", call. = FALSE)
  v <- as.numeric(value)
  c <- .dueld_anchor(a, mode)
  v + (a - c)
}

#' Duel aggregation across a batch of states
#'
#' Applies \code{\link{dueling_aggregate}} row-wise.
#'
#' @param values Numeric vector of state values V(s).
#' @param advantages List of numeric advantage vectors A(s, .), one
#'   per state.
#' @param mode One of \code{"mean"}, \code{"max"}, \code{"naive"}.
#' @return List of numeric Q(s, .) vectors.
#' @export
dueling_q <- function(values, advantages, mode = "mean") {
  if (length(values) != length(advantages))
    stop(sprintf("duel: %d values but %d advantage rows",
                 length(values), length(advantages)),
         call. = FALSE)
  out <- vector("list", length(values))
  for (i in seq_along(values)) {
    out[[i]] <- dueling_aggregate(values[i], advantages[[i]],
                                  mode = mode)
  }
  out
}

# Aliases per ledger / naming map (compact alias + public names).
duelingq <- dueling_q
dueling_dqn <- dueling_q
duelingdqn <- dueling_q

# ---------------------------------------------------------------------------
# Double-DQN target: the ONLINE net picks the action, the TARGET net
# values it. Using one net for both over-estimates because the max of a
# noisy estimate is biased upward and the same noise supplies the value.
# ---------------------------------------------------------------------------
#' .dueldqn_argmax
#'
#' Double-DQN target: the ONLINE net picks the action, the TARGET net
#' values it. Using one net for both over-estimates because the max of a
#' noisy estimate is biased upward and the same noise supplies the
#' value.
#'
#' @param x A vector; its length is taken and its elements indexed.
#' @return The value of \code{b}, as built in the body.
#' @export
.dueldqn_argmax <- function(x) {
  b <- 1L
  if (length(x) > 1L) {
    for (i in 2L:length(x)) if (x[i] > x[b]) b <- i
  }
  b
}

#' Double-DQN target
#'
#' Computes the Double-DQN target: the online net's argmax selects the
#' action, and the target net values that action. When \code{done} is
#' \code{TRUE} the target collapses to the immediate reward.
#'
#' @param reward Scalar immediate reward.
#' @param gamma Scalar discount factor.
#' @param q_online_next Numeric vector Q_online(s', .).
#' @param q_target_next Numeric vector Q_target(s', .).
#' @param done Logical; if \code{TRUE}, the target is the immediate
#'   reward alone (terminal state).
#' @return Scalar target value.
#' @references van Hasselt, H., Guez, A. & Silver, D. (2016). Deep
#'   Reinforcement Learning with Double Q-learning. AAAI 30(1),
#'   arXiv:1509.06461.
#' @export
double_q_target <- function(reward, gamma, q_online_next, q_target_next,
                            done = FALSE) {
  if (length(q_online_next) != length(q_target_next))
    stop("duel: online and target action counts differ", call. = FALSE)
  if (isTRUE(done)) return(as.numeric(reward))
  a <- .dueldqn_argmax(q_online_next)
  as.numeric(reward) + as.numeric(gamma) * q_target_next[a]
}

#' Temporal-difference residual
#'
#' Returns \code{target - q_sa}, the scalar residual that the
#' squared-error Bellman loss minimises.
#'
#' @param q_sa Scalar current Q(s, a).
#' @param target Scalar TD target.
#' @return Scalar difference \code{target - q_sa}.
#' @export
td_error <- function(q_sa, target) {
  as.numeric(target) - as.numeric(q_sa)
}

# ---------------------------------------------------------------------------
# One dueling + double-Q update's worth of quantities
# ---------------------------------------------------------------------------
# Aggregates the current state under the chosen mode, aggregates the
# next state under both the online and target net, builds the Double-DQN
# target, and returns the TD residual, the greedy action, and the
# decomposition. The result shape mirrors the Python RichResult payload.
#' One dueling + Double-DQN update
#'
#' Aggregates the current state, aggregates the next state under both
#' the online and the target net, builds the Double-DQN target, and
#' returns the TD residual, the greedy action and the decomposition.
#'
#' @param value Scalar V(s).
#' @param advantage Numeric vector A(s, .).
#' @param action 0-based action index taken in s.
#' @param reward Scalar immediate reward.
#' @param gamma Scalar discount factor.
#' @param next_value Scalar V_online(s').
#' @param next_advantage Numeric vector A_online(s', .).
#' @param next_target_value Scalar V_target(s').
#' @param next_target_advantage Numeric vector A_target(s', .).
#' @param mode Aggregation mode: \code{"mean"} (eq. 9), \code{"max"}
#'   (eq. 8) or \code{"naive"}.
#' @param done Logical; if \code{TRUE}, next-state values are dropped
#'   and the target collapses to the immediate reward.
#' @return Named list with keys matching the Python RichResult payload:
#'   \code{estimate}, \code{td_error}, \code{q}, \code{q_taken},
#'   \code{target}, \code{greedy_action}, \code{value}, \code{advantage},
#'   \code{mode}, \code{n_actions}, \code{method}.
#' @references Wang, Z. et al. (2016). Dueling Network Architectures
#'   for Deep Reinforcement Learning, arXiv:1511.06581.  van Hasselt,
#'   H., Guez, A. & Silver, D. (2016). Deep Reinforcement Learning with
#'   Double Q-learning, arXiv:1509.06461.
#' @export
dueling_step <- function(value, advantage, action, reward, gamma,
                         next_value, next_advantage, next_target_value,
                         next_target_advantage, mode = "mean",
                         done = FALSE) {
  q <- dueling_aggregate(value, advantage, mode = mode)
  if (!(action >= 0L && action < length(q)))
    stop(sprintf("duel: action %d out of range", action), call. = FALSE)
  q_next_online <- dueling_aggregate(next_value, next_advantage,
                                     mode = mode)
  q_next_target <- dueling_aggregate(next_target_value,
                                     next_target_advantage, mode = mode)
  tgt <- double_q_target(reward, gamma, q_next_online, q_next_target,
                         done = done)
  e <- td_error(q[action + 1L], tgt)
  list(
    estimate = e,
    td_error = e,
    q = unname(q),
    q_taken = unname(q[action + 1L]),
    target = tgt,
    greedy_action = .dueldqn_argmax(q) - 1L,
    value = as.numeric(value),
    advantage = unname(as.numeric(advantage)),
    mode = mode,
    n_actions = length(q),
    method = paste0("dueling aggregation eq. (9) with a Double-DQN ",
                    "target, Wang et al. (2016)")
  )
}

#' @rdname dueling_aggregate
#' @export
morie_dueldqn <- dueling_aggregate






















