# Sampling-importance-resampling (the weighted bootstrap).
# Sources: Rubin, D. B. (1988), Using the SIR algorithm to simulate
# posterior distributions, in Bayesian Statistics 3, 395-402; Smith,
# A. F. M. and Gelfand, A. E. (1992), Bayesian statistics without
# tears: a sampling-resampling perspective, The American Statistician
# 46(2), 84-88.  Draws from a proposal are reweighted by
# w = p(x) / g(x) and resampled with probability proportional to w,
# so the resample is approximately from p.
#
# Native implementation mirroring Python morie.fn.bayisr exactly: the
# same max-shifted weights, the same UNNORMALISED cumulative scan
# (u ~ U(0, sum w), first index with u <= cumsum), and hence the same
# indices from the shared generator.

#' Importance resampling (SIR)
#'
#' Computes importance weights \eqn{w_i = p(x_i)/g(x_i)} for draws
#' from a proposal and resamples \code{m} of them with probability
#' proportional to the weights (Rubin 1988; Smith and Gelfand 1992).
#' The reported effective sample size
#' \eqn{1/\sum \bar w_i^2} is the usual diagnostic: it falls towards 1
#' when a single draw dominates, which is the signal that the
#' proposal is too far from the target for the resample to be usable.
#'
#' @param samples List of draws from the proposal.
#' @param log_target,log_proposal Functions giving the unnormalised
#'   log target and the log proposal density.
#' @param m Number of resampled draws.
#' @param seed Seed for the generator shared with the Python arm.
#' @return A list with \code{resample}, \code{indices} (0-based),
#'   \code{weights} (normalised), \code{ess}, \code{n}, \code{m},
#'   \code{seed}, \code{method}.
#' @references Smith, A. F. M. and Gelfand, A. E. (1992). Bayesian
#'   statistics without tears. The American Statistician, 46(2),
#'   84-88.
#' @export
morie_bayisr <- function(samples, log_target, log_proposal, m, seed = 0) {
  xs <- as.list(samples)
  n <- length(xs)
  if (n == 0L) stop("`samples` must be non-empty")
  m <- as.integer(m)
  if (m < 1L) stop("m must be a positive integer")
  logw <- vapply(xs, function(z) log_target(z) - log_proposal(z), numeric(1))
  w <- exp(logw - max(logw))
  tot <- sum(w)
  wbar <- w / tot
  ess <- 1 / sum(wbar * wbar)
  e <- .ghc_rng(seed)
  cum <- cumsum(w)
  idx <- integer(m)
  for (k in seq_len(m)) {
    u <- .ghc_unif(e, 1L) * tot
    j <- n - 1L
    for (t in seq_len(n)) if (u <= cum[t]) { j <- t - 1L; break }
    idx[k] <- j
  }
  list(resample = lapply(idx, function(j) xs[[j + 1L]]), indices = idx,
       weights = wbar, ess = ess, n = n, m = m, seed = as.integer(seed),
       method = "SIR weighted bootstrap (Rubin 1988; Smith-Gelfand 1992)")
}
