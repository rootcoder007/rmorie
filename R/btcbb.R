# SPDX-License-Identifier: AGPL-3.0-or-later
#' Circular block bootstrap: wrap the series, then resample blocks
#'
#' Politis, D. N. and Romano, J. P. (1992), "A circular block-resampling
#' procedure for stationary data", in R. LePage and L. Billard (eds),
#' Exploring the Limits of Bootstrap, Wiley, 263-270.  The block-length theory
#' used alongside it is Politis, D. N. and White, H. (2004), "Automatic
#' Block-Length Selection for the Dependent Bootstrap", Econometric Reviews
#' 23(1), 53-70, whose Lemma 3.1 treats the circular and moving-block cases
#' together (see Btblen).
#'
#' The moving-block bootstrap has a defect that is easy to miss: with
#' n - ell + 1 starting positions, the observations at the two ends of the
#' series appear in fewer blocks than those in the middle, so the resample
#' mean is a WEIGHTED mean of the data and the bootstrap is biased.  Wrapping
#' the series into a circle gives all n starting positions equal footing,
#' every observation appears in exactly ell blocks, and that bias vanishes:
#' E*[xbar*] = xbar exactly.  That exactness is the anchor and it is a genuine
#' discriminator -- the same check applied to the moving-block bootstrap
#' fails, which is the entire reason this variant exists.  ebar_star reports
#' the exact conditional mean, computed combinatorially rather than by
#' simulation.
#'
#' Anchor 2: ell = n makes every circular block a full rotation of the series,
#' so for any permutation-invariant statistic every replicate equals the
#' statistic on the whole sample and the spread is exactly zero.
#'
#' @param x the series, in time order.
#' @param block_len block length ell; NULL uses max(floor(n^(1/3)), 1).
#' @param stat statistic of a series; NULL uses the mean.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, estimate, se, lo, hi, block_len, n_blocks,
#'   ebar_star, n, B, method.
#' @keywords internal
#' @examples
#' Btcbb(sin(1:60 / 3), 4, NULL, 50)$ebar_star
#' @export
Btcbb <- function(x, block_len = NULL, stat = NULL, B = 200, seed = 1, alpha = 0.05) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_circular_block: need at least two observations")
  if (is.null(block_len)) block_len <- max(as.integer(n^(1 / 3)), 1L)
  ell <- as.integer(block_len)
  if (!(ell >= 1L && ell <= n)) stop("boot_circular_block: block_len must lie in 1..n")
  if (as.integer(B) < 2L) stop("boot_circular_block: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_circular_block: alpha must lie strictly between 0 and 1")
  f <- if (is.null(stat)) .s03mean else stat
  bl <- .btmbb_reps(xx, ell, f, B, seed, TRUE)
  theta <- bl$theta; k <- bl$k
  tot <- 0
  for (s in 0:(n - 1L)) for (t in 0:(ell - 1L)) tot <- tot + xx[((s + t) %% n) + 1L]
  list(theta_b = theta, estimate = as.numeric(f(xx)), se = .s03sd(theta, 1L),
       lo = .s03quantile7(theta, a / 2), hi = .s03quantile7(theta, 1 - a / 2),
       block_len = ell, n_blocks = k, ebar_star = tot / (n * ell),
       n = n, B = as.integer(B),
       method = "Politis and Romano (1992) in Exploring the Limits of Bootstrap, 263-270")
}
