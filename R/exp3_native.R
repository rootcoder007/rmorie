# Exp3 adversarial bandit algorithm.
# Sources: Auer, P., Cesa-Bianchi, N., Freund, Y. & Schapire, R. E.
# (2002). The nonstochastic multiarmed bandit problem. *SIAM Journal
# on Computing* 32(1), 48-77.  Algorithm: figure 1 (Section 3);
# regret bound: Theorem 3.1.
#
# Native implementation mirroring Python morie.fn.exp3 exactly:
# the same p_j(t) = (1 - gamma) w_j(t) / sum_k w_k + gamma / K
# mixed distribution, the same single-uniform inverse-CDF action
# draw in index order (the .ghc_choice_p convention used
# everywhere else in this codebase so both arms produce the same
# stream), the same importance-weighted update
# w_i = w_i exp(gamma xhat_i / K) with xhat_i = x_i / p_i on the
# drawn arm and zero elsewhere, the same gamma in (0, 1] check, the
# same zero-based 0..T-1 actions, the same 0-based estimate being
# the argmax over final weights, and the same payload keys.

morie_exp3 <- function(x, gamma_, T = NULL, seed = 0) {
  x <- as.matrix(x)
  storage.mode(x) <- "double"
  if (length(dim(x)) != 2L)
    stop("x must be a (T, K) reward table")
  rows <- nrow(x); K <- ncol(x)
  T_used <- if (is.null(T)) rows else as.integer(T)
  if (T_used > rows)
    stop(sprintf("x has only %d rows", rows))
  g <- as.numeric(gamma_)
  if (!(g > 0 && g <= 1.0))
    stop("gamma_ must be in (0, 1]")
  e <- .ghc_rng(as.numeric(seed))
  w <- rep(1.0, K)
  probs <- matrix(0, T_used, K)
  actions <- numeric(T_used)
  rewards <- numeric(T_used)
  for (t in seq_len(T_used)) {
    tot <- sum(w)
    p <- (1.0 - g) * w / tot + g / K
    probs[t, ] <- p
    u <- .ghc_unif(e, 1L)
    cs <- cumsum(p)
    i <- which(u <= cs)[1L]
    if (is.na(i)) i <- K
    r <- x[t, i]
    w[i] <- w[i] * exp(g * (r / p[i]) / K)
    actions[t] <- as.numeric(i - 1L)
    rewards[t] <- r
  }
  best <- which.max(w)[1L] - 1L
  list(estimate = as.numeric(best),
       actions = actions,
       rewards = rewards,
       probs = probs,
       weights = w,
       total_reward = sum(rewards),
       method = "Exp3 exponential-weight adversarial bandit")
}

exp3_bandit <- morie_exp3
exp3 <- morie_exp3

cheatsheet <- function() {
  "exp3(x, gamma_) -> Exp3 adversarial bandit on a (T, K) reward table (Auer et al 2002, fig 1)."
}
