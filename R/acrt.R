# SPDX-License-Identifier: AGPL-3.0-or-later
#' One-step actor-critic
#'
#' Sutton and Barto (2018), Reinforcement Learning: An Introduction, 2nd
#' ed. (FETCHED from incompleteideas.net), section 13.5, equations
#' (13.12)-(13.14): theta <- theta + alpha (R + gamma vhat(S') - vhat(S))
#' grad ln pi(A|S, theta), paired with semi-gradient TD(0) for the critic,
#' w <- w + alpha_w delta grad vhat(S).  The episodic pseudocode carries
#' the discount factor I (I <- gamma I each step), included as
#' `discount_actor`.  Gradients are supplied by the caller, since they
#' belong to the policy's own parameterisation.
#'
#' @param env the rewards R_1..R_T.
#' @param actor,critic alternative slots for grad_logpi and values.
#' @param rewards,values rewards and vhat(S_0..S_T).
#' @param grad_logpi grad ln pi per step, one row per step.
#' @param grad_v grad vhat per step.
#' @param alpha_theta,alpha_w step sizes.
#' @param gamma discount.
#' @param theta,w starting parameters.
#' @param discount_actor carry the book's I factor.
#' @return list: estimate, deltas, theta, w, n, method.
#' @keywords internal
#' @examples
#' Actorcrit(c(1, 0), values = c(0, 0.5, 1))$deltas
#' @export
Actorcrit <- function(env, actor = NULL, critic = NULL, rewards = NULL,
                      values = NULL, grad_logpi = NULL, grad_v = NULL,
                      alpha_theta = 0.1, alpha_w = 0.1, gamma = 0.99,
                      theta = NULL, w = NULL, discount_actor = TRUE) {
  R <- .s03vec(if (!is.null(rewards)) rewards else env)
  V <- .s03vec(if (!is.null(values)) values else if (!is.null(critic)) critic else numeric(0))
  Tn <- length(R)
  G <- if (!is.null(grad_logpi)) .s03mat(grad_logpi) else if (!is.null(actor)) .s03mat(actor) else matrix(1, Tn, 1)
  Gv <- if (!is.null(grad_v)) .s03mat(grad_v) else matrix(1, Tn, 1)
  th <- if (!is.null(theta)) .s03vec(theta) else numeric(ncol(G))
  ww <- if (!is.null(w)) .s03vec(w) else numeric(ncol(Gv))
  g <- as.numeric(gamma)
  deltas <- numeric(Tn)
  I <- 1
  for (t in seq_len(Tn)) {
    vt <- if (t <= length(V)) V[t] else 0
    vn <- if (t + 1L <= length(V)) V[t + 1L] else 0
    d <- R[t] + g * vn - vt
    deltas[t] <- d
    for (j in seq_along(th)) th[j] <- th[j] + as.numeric(alpha_theta) * I * d * G[t, j]
    for (j in seq_along(ww)) ww[j] <- ww[j] + as.numeric(alpha_w) * d * Gv[t, j]
    if (discount_actor) I <- I * g
  }
  list(
    estimate = if (Tn) .s03mean(deltas) else NaN, deltas = deltas,
    theta = th, w = ww, n = Tn,
    method = "One-step actor-critic (Sutton and Barto 2018, eqs. 13.12-13.14)"
  )
}
