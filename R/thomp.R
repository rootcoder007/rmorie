# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Beta-Bernoulli Thompson sampling (Thomp). Bit-identical mirror of
# src/morie/fn/thomp.py, driving the same SplitMix64 stream through
# the .ghc_beta1 Marsaglia-Tsang mirror.

#' Beta-Bernoulli Thompson sampling
#'
#' Algorithm 3.2 (BernTS) of Russo et al. (2018): at each period sample
#' \eqn{\hat\theta_k \sim Beta(\alpha_k, \beta_k)} for every action,
#' apply \eqn{x_t = \arg\max_k \hat\theta_k}, observe the Bernoulli
#' reward and update the chosen action conjugately,
#' \eqn{(\alpha_x, \beta_x) \leftarrow (\alpha_x + r_t, \beta_x + 1 - r_t).}
#' The probability-matching idea is Thompson (1933).  Each period
#' consumes one Beta draw per action in index order and then one
#' uniform for the reward, bit-exactly as the Python arm; argmax ties
#' break to the lowest action.
#'
#' @param p True Bernoulli success probabilities (the simulated
#'   environment), length K.
#' @param T Number of periods.
#' @param alpha0 Beta prior alpha, length K (default all 1).
#' @param beta0 Beta prior beta, length K (default all 1).
#' @param seed SplitMix64 seed.
#' @return List with \code{estimate} (0-based arm with the largest
#'   posterior mean), \code{actions}, \code{rewards}, \code{alpha},
#'   \code{beta}, \code{post_mean}, \code{counts},
#'   \code{total_reward}, \code{method}.
#' @references Russo, D. J., Van Roy, B., Kazerouni, A., Osband, I. and
#'   Wen, Z. (2018). A tutorial on Thompson sampling. Foundations and
#'   Trends in Machine Learning 11(1), 1-96 (arXiv:1707.02038).
#'   Algorithm 3.2 (BernTS), Section 3 conjugate update.  Local source:
#'   fetched-wave3/russo-etal-2018-thompson-sampling-tutorial-arxiv1707.02038.pdf.
#'   Thompson, W. R. (1933). On the likelihood that one unknown
#'   probability exceeds another in view of the evidence of two
#'   samples. Biometrika 25(3-4), 285-294.
#' @examples
#' Thomp(c(0.8, 0.2), 20, seed = 1)$counts
#' @export
Thomp <- function(p, T, alpha0 = NULL, beta0 = NULL, seed = 0) {
  p <- as.numeric(p)
  K <- length(p)
  if (any(p < 0) || any(p > 1)) stop("p must lie in [0, 1]", call. = FALSE)
  T <- as.integer(T)
  a <- if (is.null(alpha0)) rep(1, K) else as.numeric(alpha0)
  b <- if (is.null(beta0)) rep(1, K) else as.numeric(beta0)
  if (length(a) != K || length(b) != K) {
    stop("alpha0/beta0 must have length K", call. = FALSE)
  }
  e <- .ghc_rng(seed)
  actions <- numeric(T)
  rewards <- numeric(T)
  counts <- numeric(K)
  for (t in seq_len(T)) {
    best <- 1L
    besttheta <- -1
    for (k in seq_len(K)) {
      th <- .ghc_beta1(e, a[k], b[k])
      if (th > besttheta) {
        besttheta <- th
        best <- k
      }
    }
    u <- .ghc_unif(e, 1L)
    r <- if (u < p[best]) 1 else 0
    a[best] <- a[best] + r
    b[best] <- b[best] + 1 - r
    counts[best] <- counts[best] + 1
    actions[t] <- as.numeric(best - 1L)
    rewards[t] <- r
  }
  pm <- a / (a + b)
  est <- 1L
  if (K > 1L) for (k in seq(2L, K)) if (pm[k] > pm[est]) est <- k
  list(estimate = as.numeric(est - 1L), actions = actions,
       rewards = rewards, alpha = a, beta = b, post_mean = pm,
       counts = counts, total_reward = sum(rewards),
       method = "Beta-Bernoulli Thompson sampling")
}
