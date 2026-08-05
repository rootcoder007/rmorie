# SPDX-License-Identifier: AGPL-3.0-or-later
#' The n-step TD return and its update
#'
#' Sutton and Barto (2018), 2nd ed. (FETCHED from incompleteideas.net),
#' equation (7.1): G_(t:t+n) = R_(t+1) + gamma R_(t+2) + ... +
#' gamma^(n-1) R_(t+n) + gamma^n V_(t+n-1)(S_(t+n)) "for all n, t such
#' that n >= 1 and 0 <= t < T - n", with G_(t:t+n) = G_t once t + n >= T,
#' i.e. the bootstrap term is dropped past the end of the episode.
#' Equation (7.2) gives the update V(S_t) <- V(S_t) + alpha \[G_(t:t+n) -
#' V(S_t)].  n = 1 recovers TD(0) and n >= T recovers Monte Carlo.
#'
#' @param traj the rewards R_1..R_T.
#' @param V current value estimates.
#' @param n steps before bootstrapping.
#' @param gamma discount.
#' @param alpha step size.
#' @param states zero-based state index at each time 0..T.
#' @return list: estimate, returns, v_new, bootstrapped, n, method.
#' @keywords internal
#' @examples
#' Nsteptd(c(1, 1, 1), c(0, 0, 0, 5), 2, 0.9)$returns
#' @export
Nsteptd <- function(traj, V, n = 1, gamma = 0.99, alpha = 0.1, states = NULL) {
  R <- .s03vec(traj); v <- .s03vec(V)
  Tn <- length(R); nn <- as.integer(n); g <- as.numeric(gamma)
  idx <- if (!is.null(states)) as.integer(states) else seq_len(Tn + 1L) - 1L
  G <- numeric(Tn); booted <- numeric(Tn)
  for (t in seq_len(Tn) - 1L) {
    h <- t + nn
    acc <- 0; j <- t
    while (j < h && j < Tn) {
      acc <- acc + (g^(j - t)) * R[j + 1L]
      j <- j + 1L
    }
    if (h < Tn && h < length(idx)) {
      si <- idx[h + 1L]
      acc <- acc + (g^nn) * (if (si < length(v)) v[si + 1L] else 0)
      booted[t + 1L] <- 1
    } else {
      booted[t + 1L] <- 0
    }
    G[t + 1L] <- acc
  }
  vn <- v
  for (t in seq_len(Tn) - 1L) {
    si <- if (t < length(idx)) idx[t + 1L] else t
    if (si < length(vn)) vn[si + 1L] <- vn[si + 1L] + as.numeric(alpha) * (G[t + 1L] - vn[si + 1L])
  }
  list(estimate = if (Tn) G[1] else NaN, returns = G, v_new = vn,
       bootstrapped = booted, n = Tn,
       method = "n-step TD return and update (Sutton and Barto 2018, eqs. 7.1-7.2)")
}
