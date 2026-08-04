# SPDX-License-Identifier: AGPL-3.0-or-later
#' AlphaGo's fast rollout policy and its mixed leaf evaluation
#'
#' Silver et al. (2016), Mastering the game of Go with deep neural
#' networks and tree search, Nature 529, 484-489.  The Nature paper is
#' paywalled, but the equation it turns on is quoted identically
#' everywhere and in its successors (Silver et al. 2017, Nature 550,
#' 354-359; Silver et al. 2018, arXiv:1712.01815 -- FETCHED, which
#' describes AlphaGo Zero as removing exactly this rollout):
#' V(s_L) = (1 - lambda) v_theta(s_L) + lambda z_L, a convex mixture of
#' the value network and the outcome of a fast rollout played out with
#' pi_rollout.  AlphaGo used lambda = 0.5.
#'
#' Determinism: the rollout is not sampled from a generator.  Actions come
#' from a caller-supplied uniform stream or, by default, from the inverse
#' CDF of pi_rollout at van der Corput points.  The horizon is a fixed
#' number of plies.
#'
#' @param state leaf state id.
#' @param rollout_net function s -> p.
#' @param horizon maximum plies.
#' @param step,terminal,outcome environment callbacks.
#' @param value_net optional function s -> v.
#' @param lam mixing weight.
#' @param stream optional uniforms driving the rollout.
#' @return list: estimate, z, v_theta, lam, plies, trajectory, actions,
#'   method.
#' @keywords internal
#' @examples
#' Rolloutmc(0, function(s) c(0.5, 0.5), horizon = 2)$plies
#' @export
Rolloutmc <- function(state, rollout_net, horizon = 16, step = NULL,
                      terminal = NULL, outcome = NULL, value_net = NULL,
                      lam = 0.5, stream = NULL) {
  s <- state
  traj <- list(s); acts <- integer(0); i <- 0L
  while (i < as.integer(horizon)) {
    if (!is.null(terminal) && terminal(s)) break
    p <- .s03vec(rollout_net(s))
    tot <- 0
    for (x in p) tot <- tot + x
    if (tot > 0) p <- p / tot
    u <- if (!is.null(stream) && i < length(stream)) as.numeric(stream[i + 1L]) else .s03vdc(i, 2L)
    cc <- 0; a <- length(p) - 1L
    for (j in seq_along(p)) {
      cc <- cc + p[j]
      if (u < cc) { a <- j - 1L; break }
    }
    acts <- c(acts, as.integer(a))
    if (is.null(step)) break
    s <- step(s, a)
    traj[[length(traj) + 1L]] <- s
    i <- i + 1L
  }
  z <- if (!is.null(outcome)) as.numeric(outcome(s)) else 0
  if (is.null(value_net)) {
    vt <- NaN; val <- z
  } else {
    vt <- as.numeric(value_net(state))
    L <- as.numeric(lam)
    val <- (1 - L) * vt + L * z
  }
  list(estimate = val, z = z, v_theta = vt, lam = as.numeric(lam), plies = i,
       trajectory = traj, actions = acts,
       method = "AlphaGo mixed leaf value (1-lambda) v_theta + lambda z_L")
}
