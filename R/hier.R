# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hierarchical RL: the options framework
#'
#' Sutton, Precup and Singh (1999), Between MDPs and semi-MDPs, Artificial
#' Intelligence 112(1-2), 181-211.  An option is omega = (I, pi_omega,
#' beta_omega) -- initiation set, internal policy, termination condition
#' -- and, because an option occupies k steps, the backup is the SMDP one:
#' Q(s, omega) <- Q(s, omega) + alpha [r + gamma^k max_omega' Q(s',
#' omega') - Q(s, omega)] with r = R_1 + gamma R_2 + ... + gamma^(k-1)
#' R_k.  The gamma^k in place of gamma is the whole content of the
#' semi-Markov generalisation.  The AIJ paper is paywalled; the definition
#' and the backup are quoted in their standard published form and are
#' reproduced identically in Sutton and Barto (2018) section 17.2 (FETCHED
#' from incompleteideas.net).
#'
#' Determinism: option termination compares the caller's beta against van
#' der Corput points rather than a random draw.
#'
#' @param env the rewards received while the option ran.
#' @param options termination probabilities beta per step.
#' @param meta the current Q(s, omega).
#' @param rewards explicit rewards, overriding env.
#' @param gamma discount.
#' @param alpha step size.
#' @param Q alternative slot for the current Q(s, omega).
#' @param q_next Q(s', omega') at the arrival state.
#' @param k_steps force the option duration.
#' @return list: estimate, r_option, k, target, td_error, method.
#' @keywords internal
#' @examples
#' Optionshrl(c(1, 1, 1), k_steps = 3, q_next = c(2, 1))$estimate
#' @export
Optionshrl <- function(env, options = NULL, meta = NULL, rewards = NULL,
                       gamma = 0.99, alpha = 0.1, Q = NULL, q_next = NULL,
                       k_steps = NULL) {
  R <- .s03vec(if (!is.null(rewards)) rewards else env)
  g <- as.numeric(gamma)
  if (!is.null(k_steps)) {
    kk <- as.integer(k_steps)
  } else if (!is.null(options)) {
    beta <- .s03vec(options)
    kk <- length(R)
    for (t in seq_along(beta) - 1L) {
      if (.s03vdc(t, 2L) < beta[t + 1L]) { kk <- t + 1L; break }
    }
  } else {
    kk <- length(R)
  }
  if (kk > length(R)) kk <- length(R)
  r <- 0
  if (kk > 0L) for (t in seq_len(kk) - 1L) r <- r + (g^t) * R[t + 1L]
  q0 <- as.numeric(if (!is.null(Q)) Q else if (!is.null(meta)) meta else 0)
  qn <- if (!is.null(q_next)) .s03vec(q_next) else numeric(0)
  mx <- if (length(qn)) max(qn) else 0
  target <- r + (g^kk) * mx
  td <- target - q0
  list(estimate = q0 + as.numeric(alpha) * td, r_option = r, k = kk,
       target = target, td_error = td,
       method = "SMDP option-value backup (Sutton, Precup and Singh 1999)")
}
