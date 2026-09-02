# SPDX-License-Identifier: AGPL-3.0-or-later
#' Linear Thompson sampling for contextual bandits
#'
#' Formula: B = I_d + sum b b'; muhat = B^-1 f; mutilde ~ N(muhat, v^2 B^-1); a = argmax_i b_i' mutilde
#'
#' @param contexts Context vector per arm at the current round.
#' @param played Context vectors of the arms played so far.
#' @param rewards Rewards observed for those plays.
#' @param R Sub-Gaussian parameter of the reward noise.
#' @param delta Confidence parameter.
#' @param horizon Time horizon T; the number of plays so far if omitted.
#' @param z Standard normal draw used for the posterior sample.

#' @param contexts See Usage.
#' @param played See Usage.
#' @param rewards See Usage.
#' @param R See Usage.
#' @param delta See Usage.
#' @param horizon See Usage.
#' @param z See Usage.
#' @return List with ``arm``, ``scores``, ``mu_hat``, ``mu_tilde``, ``v``, ``d``.
#' @references Agrawal and Goyal (2013), Thompson Sampling for Contextual Bandits with Linear Payoffs, ICML/arXiv:1209.3352. Algorithm 1 and the definition v = R sqrt(9 d ln(T/delta)). Verified against the paper.
#' @export
#' @examples
#' Lints(contexts = c(1, 2, 3, 4, 5, 6, 7, 8), played = c(1, 2, 3, 4, 5, 6, 7, 8), rewards = c(1, 2, 3, 4, 5, 6, 7, 8))
Lints <- function(contexts, played, rewards, R = 0.5, delta = 0.1, horizon = NULL, z = NULL) {
  X <- as.matrix(contexts); P <- as.matrix(played); r <- .t1_vec(rewards)
  d <- ncol(X)
  B <- diag(d) + t(P) %*% P
  f <- as.numeric(t(P) %*% r)
  Binv <- solve(B)
  mu <- as.numeric(Binv %*% f)
  Tt <- if (is.null(horizon)) max(nrow(P), 1) else as.integer(horizon)
  v <- as.numeric(R) * sqrt(9 * d * log(Tt / as.numeric(delta)))
  if (is.null(z)) {
    g <- .t1_lcg(1)
    z <- vapply(seq_len(d), function(i) g$norm(), numeric(1))
  }
  z <- .t1_vec(z)
  L <- t(chol(Binv))
  mutil <- as.numeric(mu + v * (L %*% z))
  scores <- as.numeric(X %*% mutil)
  .t1_result(arm = which.max(scores) - 1L, scores = scores, mu_hat = mu,
             mu_tilde = mutil, v = v, d = d,
             method = "Linear Thompson sampling (Agrawal-Goyal)")
}
