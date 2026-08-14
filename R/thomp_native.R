# morie native arm -- thomp
# Thompson sampling on a Beta-Bernoulli bandit (Russo et al. 2018,
# Algorithm 3.2 BernTS; the probability-matching idea is Thompson
# 1933).
#
# Each period: sample theta_k ~ Beta(alpha_k, beta_k) for every arm,
# play argmax_k theta_k, observe a Bernoulli reward, and update that
# arm's posterior conjugately.
#
# Draw order mirrors the Python exactly -- one Beta per arm in index
# order, then one uniform for the reward -- so the two runs produce
# the same action sequence, not merely the same asymptotics. Argmax
# ties break to the lowest arm on both sides.

morie_thomp <- function(p, T, alpha0 = NULL, beta0 = NULL, seed = 0) {
  p <- as.numeric(p)
  K <- length(p)
  if (any(p < 0 | p > 1)) stop("p must lie in [0, 1]")
  T <- as.integer(T)
  a <- if (is.null(alpha0)) rep(1, K) else as.numeric(alpha0)
  b <- if (is.null(beta0)) rep(1, K) else as.numeric(beta0)
  if (length(a) != K || length(b) != K) {
    stop("alpha0/beta0 must have length K")
  }
  e <- .ghc_rng(seed)
  actions <- numeric(T); rewards <- numeric(T); counts <- numeric(K)
  for (t in seq_len(T)) {
    best <- 1L; besttheta <- -1
    for (k in seq_len(K)) {
      th <- .ghc_beta1(e, a[k], b[k])
      if (th > besttheta) { besttheta <- th; best <- k }
    }
    u <- .ghc_unif(e, 1L)
    r <- if (u < p[best]) 1 else 0
    a[best] <- a[best] + r
    b[best] <- b[best] + 1 - r
    counts[best] <- counts[best] + 1
    actions[t] <- best - 1L          # 0-based, matching Python
    rewards[t] <- r
  }
  pm <- a / (a + b)
  est <- which.max(pm) - 1L          # ties -> lowest index, as Python
  list(
    estimate = as.numeric(est),
    actions = actions, rewards = rewards,
    alpha = a, beta = b,
    post_mean = pm, counts = counts,
    total_reward = sum(rewards),
    method = "Beta-Bernoulli Thompson sampling"
  )
}
