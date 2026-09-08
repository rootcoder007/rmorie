# SPDX-License-Identifier: AGPL-3.0-or-later
#' Posterior stick-breaking weights
#'
#' Ishwaran and James (2001), Gibbs sampling methods for stick-breaking
#' priors, JASA 96(453), 161-173: given cluster counts n_1..n_K in stick
#' order, V_k | data ~ Beta(1 + n_k, alpha + sum_(j>k) n_j), the conjugate
#' update of Sethuraman's Beta(1, alpha) prior, with the weights following
#' by the same product.  The paper is paywalled; the conditional is quoted
#' in its standard published form.  The posterior MEAN stick fraction is
#' used, not a draw, so the result is a point summary and no generator is
#' consulted.
#'
#' @param partition cluster labels per observation, or counts in stick order.
#' @param alpha the DP concentration.
#' @return list: estimate, pi, V, counts, remainder, method.
#' @keywords internal
#' @examples
#' Stickpost(c(1, 1, 2, 3, 3, 3), 1)$pi
#' @export
Stickpost <- function(partition, alpha = 1) {
  v <- .s03vec(partition)
  ints <- all(abs(v - round(v)) < 1e-12 & v >= 0)
  labs <- unique(v)
  if (ints && length(labs) == length(v) && length(v) > 1L) {
    counts <- v
  } else {
    labs <- sort(unique(v))
    counts <- vapply(labs, function(cc) sum(v == cc), 0)
  }
  K <- length(counts)
  a <- as.numeric(alpha)
  Vs <- numeric(K)
  for (i in seq_len(K)) {
    tail <- 0
    if (i < K) for (j in seq(i + 1L, K)) tail <- tail + counts[j]
    Vs[i] <- (1 + counts[i]) / (1 + counts[i] + a + tail)
  }
  pi_ <- numeric(K)
  rest <- 1
  for (i in seq_len(K)) { pi_[i] <- Vs[i] * rest
  rest <- rest * (1 - Vs[i]) }
  list(estimate = if (K) pi_[1] else NaN, pi = pi_, V = Vs, counts = counts,
       remainder = rest,
       method = "Ishwaran and James (2001) conjugate stick-breaking posterior, at its mean")
}
