# SPDX-License-Identifier: AGPL-3.0-or-later
#' Kullback-Leibler diagnostic between an MCMC chain and its target
#'
#' Convergence is the agreement between the sampled distribution and the
#' target, and the Kullback-Leibler divergence over a common binning is
#' the sharpest such summary.  Bins with no chain mass contribute
#' nothing; a bin with chain mass but no target mass makes the
#' divergence infinite, which is a genuine failure and is reported
#' rather than smoothed away.
#'
#' Formula: D(p || q) = sum_k p_k log(p_k / q_k) over equal-width bins.
#'
#' @param chain Numeric draws.
#' @param target Density function evaluated at the bin midpoints, or a
#'   vector of one value per bin.
#' @param bins Number of equal-width bins.
#' @param lo,hi Optional binning range; the chain range by default.
#' @return List with \code{estimate}, \code{kl}, \code{p}, \code{q},
#'   \code{unsupported_bins}, \code{n}, \code{method}.
#' @references Brooks and Gelman (1998), General methods for monitoring
#'   convergence of iterative simulations, Journal of Computational and
#'   Graphical Statistics 7(4):434-455.
#'   \doi{10.1080/10618600.1998.10474787}
#' @export
Klmcmc <- function(chain, target, bins = 20, lo = NULL, hi = NULL) {
  x <- .s03vec(chain)
  n <- length(x)
  if (n < 2L) stop("kl_mcmc_diagnostic: chain needs at least two draws")
  B <- as.integer(bins)
  if (B < 2L) stop("kl_mcmc_diagnostic: need at least two bins")
  a <- if (is.null(lo)) min(x) else as.numeric(lo)
  b <- if (is.null(hi)) max(x) else as.numeric(hi)
  if (!(b > a)) stop("kl_mcmc_diagnostic: the binning range is degenerate")
  w <- (b - a) / B
  k <- pmin(pmax(as.integer(floor((x - a) / w)), 0L), B - 1L)
  cnt <- tabulate(k + 1L, nbins = B)
  p <- cnt / n
  mid <- a + (seq_len(B) - 0.5) * w
  q <- if (is.function(target)) vapply(mid, function(m) as.numeric(target(m)), 0) else .s03vec(target)
  if (length(q) != B) stop("kl_mcmc_diagnostic: target must give one value per bin")
  if (any(q < 0)) stop("kl_mcmc_diagnostic: target must be non-negative")
  if (sum(q) <= 0) stop("kl_mcmc_diagnostic: target has no mass on the binning range")
  q <- q / sum(q)
  kl <- 0
  empty <- 0L
  for (i in seq_len(B)) {
    if (p[i] <= 0) next
    if (q[i] <= 0) { kl <- Inf
    empty <- empty + 1L
    next }
    kl <- kl + p[i] * log(p[i] / q[i])
  }
  .t1_result(estimate = kl, kl = kl, p = p, q = q, unsupported_bins = empty,
             n = n,
             method = "binned D(p_chain || p_target), Brooks & Gelman (1998) sect. 3")
}
