# SPDX-License-Identifier: AGPL-3.0-or-later

#' Blackwell-MacQueen predictive rule
#'
#' Formula: P(z_n = k | z_{1:n-1}) = n_k / (n - 1 + alpha)
#'
#' with the remaining alpha/(n - 1 + alpha) going to a new block.  The
#' rule is exchangeable: the probability of a partition depends only on
#' the block sizes, through the EPPF
#' alpha^K prod (n_k - 1)! / (alpha)_n, and the expected number of
#' blocks is sum_{i=1..n} alpha/(alpha + i - 1).
#'
#' @param partition Block label of each of the n observations seated.
#' @param alpha Concentration, strictly positive.
#' @return List with \code{estimate} (probability of a new block),
#'   \code{probs}, \code{p_new}, \code{counts}, \code{K},
#'   \code{log_eppf}, \code{expected_K}, \code{n}, \code{method}.
#' @references Blackwell & MacQueen (1973), Ann. Statist. 1(2):353-355;
#'   Pitman (2006), Combinatorial Stochastic Processes, Springer, ch. 3.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Dpedt(V)
Dpedt <- function(partition, alpha = 1) {
  if (!(alpha > 0)) stop("alpha must be strictly positive")
  lab <- partition
  n <- length(lab)
  if (n == 0L) stop("empty input: partition has no observations")
  keys <- unique(lab)
  counts <- vapply(keys, function(k) sum(lab == k), 0L)
  K <- length(keys)
  denom <- n + alpha
  probs <- counts / denom
  p_new <- alpha / denom
  log_eppf <- K * log(alpha) + sum(lgamma(counts)) + lgamma(alpha) -
    lgamma(alpha + n)
  expected_K <- 0
  for (i in 0:(n - 1L)) expected_K <- expected_K + alpha / (alpha + i)
  .t1_result(estimate = p_new, probs = probs, p_new = p_new,
             counts = counts, K = K, log_eppf = log_eppf,
             expected_K = expected_K, n = n,
             method = "Blackwell-MacQueen predictive rule of the DP")
}
