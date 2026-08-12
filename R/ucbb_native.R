# Multi-armed bandit policies: UCB1, Exp3, Thompson sampling.
# Sources:
#   UCB1 -- Auer, P., Cesa-Bianchi, N. and Fischer, P. (2002),
#     Finite-time analysis of the multiarmed bandit problem, Machine
#     Learning 47, 235-256; policy in Figure 1, p. 237, regret bound
#     Theorem 1.
#   Exp3 -- Auer, P., Cesa-Bianchi, N., Freund, Y. and Schapire, R. E.
#     (2002), The nonstochastic multiarmed bandit problem, SIAM J.
#     Comput. 32(1), 48-77; Figure 1 and Theorem 3.1.
#   Thompson -- Russo, D. et al. (2018), A tutorial on Thompson
#     sampling, FnT ML 11(1), 1-96, Algorithm 3.2 (BernTS);
#     Thompson, W. R. (1933), Biometrika 25, 285-294.
# Mirrors Python morie.fn.ucbb / exp3 / thomp exactly, including the
# RNG stream: Exp3 consumes one uniform per trial by inverse CDF in
# index order; Thompson consumes one Beta draw per action (in index
# order) then one uniform for the Bernoulli reward, per period.

#' UCB1 bandit policy on a realized-reward table
#'
#' Plays each machine once, then always plays the machine maximising
#' xbar_j + sqrt(2 log(n) / n_j) (Auer et al. 2002, Figure 1).  The
#' policy is deterministic given the reward table, so no RNG is used.
#' Index ties break to the lowest machine.
#'
#' @param x Reward table (T x K); \code{x[t, j]} is what machine j
#'   pays at play t.
#' @param T Number of plays (default all rows; must be >= K).
#' @return A list with elements \code{estimate} (0-based machine with
#'   the highest final mean), \code{actions}, \code{rewards},
#'   \code{means}, \code{counts}, \code{index}, \code{total_reward},
#'   \code{method}.
#' @references Auer, P., Cesa-Bianchi, N. and Fischer, P. (2002).
#'   Finite-time analysis of the multiarmed bandit problem. Machine
#'   Learning, 47, 235-256.
#' @export
morie_ucbb <- function(x, T = NULL) {
  x <- as.matrix(x)
  rows <- nrow(x); K <- ncol(x)
  T <- if (is.null(T)) rows else as.integer(T)
  if (T < K) stop(sprintf("need at least K = %d plays", K))
  if (T > rows) stop(sprintf("x has only %d rows", rows))
  counts <- rep(0L, K); sums <- rep(0, K)
  actions <- numeric(T); rewards <- numeric(T)
  for (t in seq_len(T)) {
    if (t <= K) {
      j <- t
    } else {
      n <- t - 1L
      best <- 1L; bestidx <- -Inf
      for (k in seq_len(K)) {
        idx <- sums[k] / counts[k] + sqrt(2 * log(n) / counts[k])
        if (idx > bestidx) { bestidx <- idx; best <- k }
      }
      j <- best
    }
    r <- x[t, j]
    counts[j] <- counts[j] + 1L
    sums[j] <- sums[j] + r
    actions[t] <- j - 1L
    rewards[t] <- r
  }
  means <- sums / counts
  index <- means + sqrt(2 * log(T) / counts)
  best <- 1L
  for (k in seq_len(K)) if (k > 1L && means[k] > means[best]) best <- k
  list(estimate = best - 1, actions = actions, rewards = rewards,
       means = means, counts = as.numeric(counts), index = index,
       total_reward = sum(rewards),
       method = "UCB1 bandit policy (Auer et al. 2002, Figure 1)")
}

#' Exp3 bandit policy on an adversarial reward table
#'
#' Weights start at 1; each trial forms
#' p_i = (1-gamma) w_i / sum_j w_j + gamma/K, draws an action from p,
#' and updates only the chosen action's weight by
#' w_i <- w_i exp(gamma (x_i / p_i) / K) (Auer et al. 2002, Figure 1;
#' regret bound Theorem 3.1).
#'
#' @param x Reward table (T x K), entries in [0, 1].
#' @param gamma_ Mixing parameter in (0, 1].
#' @param T Number of trials (default all rows).
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{estimate}, \code{actions},
#'   \code{rewards}, \code{probs}, \code{weights},
#'   \code{total_reward}, \code{method}.
#' @references Auer, P., Cesa-Bianchi, N., Freund, Y. and Schapire,
#'   R. E. (2002). The nonstochastic multiarmed bandit problem. SIAM
#'   Journal on Computing, 32(1), 48-77.
#' @export
morie_exp3 <- function(x, gamma_, T = NULL, seed = 0) {
  x <- as.matrix(x)
  rows <- nrow(x); K <- ncol(x)
  T <- if (is.null(T)) rows else as.integer(T)
  if (T > rows) stop(sprintf("x has only %d rows", rows))
  g <- as.numeric(gamma_)
  if (!(g > 0 && g <= 1)) stop("gamma_ must be in (0, 1]")
  e <- .ghc_rng(seed)
  w <- rep(1, K)
  probs <- matrix(0, T, K)
  actions <- numeric(T); rewards <- numeric(T)
  for (t in seq_len(T)) {
    tot <- sum(w)
    p <- (1 - g) * w / tot + g / K
    probs[t, ] <- p
    u <- .ghc_unif(e, 1)
    cc <- 0; i <- K
    for (j in seq_len(K)) {
      cc <- cc + p[j]
      if (u <= cc) { i <- j; break }
    }
    r <- x[t, i]
    w[i] <- w[i] * exp(g * (r / p[i]) / K)
    actions[t] <- i - 1L
    rewards[t] <- r
  }
  best <- 1L
  for (j in seq_len(K)) if (j > 1L && w[j] > w[best]) best <- j
  list(estimate = best - 1, actions = actions, rewards = rewards,
       probs = probs, weights = w, total_reward = sum(rewards),
       method = "Exp3 bandit policy (Auer et al. 2002, Figure 1)")
}

#' Thompson sampling on a Bernoulli bandit with Beta priors
#'
#' Algorithm 3.2 (BernTS) of Russo et al. (2018): each period samples
#' theta_k ~ Beta(alpha_k, beta_k) for every action, plays the argmax,
#' observes a Bernoulli reward and updates the chosen action's
#' posterior by (alpha, beta) <- (alpha + r, beta + 1 - r).  Argmax
#' ties break to the lowest action.
#'
#' @param p True Bernoulli success probabilities (length K).
#' @param T Number of periods.
#' @param alpha0,beta0 Beta prior parameters (default all 1).
#' @param seed SplitMix64 seed (mirrors the Python arm).
#' @return A list with elements \code{estimate}, \code{actions},
#'   \code{rewards}, \code{alpha}, \code{beta}, \code{post_mean},
#'   \code{counts}, \code{total_reward}, \code{method}.
#' @references Russo, D., Van Roy, B., Kazerouni, A., Osband, I. and
#'   Wen, Z. (2018). A tutorial on Thompson sampling. Foundations and
#'   Trends in Machine Learning, 11(1), 1-96. Thompson, W. R. (1933).
#'   Biometrika, 25, 285-294.
#' @export
morie_thomp <- function(p, T, alpha0 = NULL, beta0 = NULL, seed = 0) {
  p <- as.numeric(p); K <- length(p)
  if (any(p < 0 | p > 1)) stop("p must lie in [0, 1]")
  T <- as.integer(T)
  a <- if (is.null(alpha0)) rep(1, K) else as.numeric(alpha0)
  b <- if (is.null(beta0)) rep(1, K) else as.numeric(beta0)
  if (length(a) != K || length(b) != K)
    stop("alpha0/beta0 must have length K")
  e <- .ghc_rng(seed)
  actions <- numeric(T); rewards <- numeric(T); counts <- rep(0, K)
  for (t in seq_len(T)) {
    best <- 1L; besttheta <- -1
    for (k in seq_len(K)) {
      th <- .ghc_beta1(e, a[k], b[k])
      if (th > besttheta) { besttheta <- th; best <- k }
    }
    u <- .ghc_unif(e, 1)
    r <- if (u < p[best]) 1 else 0
    a[best] <- a[best] + r
    b[best] <- b[best] + 1 - r
    counts[best] <- counts[best] + 1
    actions[t] <- best - 1L
    rewards[t] <- r
  }
  pm <- a / (a + b)
  est <- 1L
  for (k in seq_len(K)) if (k > 1L && pm[k] > pm[est]) est <- k
  list(estimate = est - 1, actions = actions, rewards = rewards,
       alpha = a, beta = b, post_mean = pm, counts = counts,
       total_reward = sum(rewards),
       method = "Thompson sampling, BernTS (Russo et al. 2018, Alg. 3.2)")
}
