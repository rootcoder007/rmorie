# SPDX-License-Identifier: AGPL-3.0-or-later
#' Stationary bootstrap: geometric block lengths on a wrapped series
#'
#' Politis, D. N. and Romano, J. P. (1994), "The Stationary Bootstrap",
#' Journal of the American Statistical Association 89(428), 1303-1313,
#' doi:10.1080/01621459.1994.10476870 (verified against Crossref).
#'
#' Fixed-length blocks make the resampled series NON-stationary: whether
#' observation t* is followed by its true successor depends on the position of
#' t* within its block.  Politis and Romano remove that by randomising the
#' block length: start at a uniform position on the wrapped series and at each
#' subsequent step continue to the next observation with probability 1 - p, or
#' jump to a fresh uniform position with probability p.  Block lengths are
#' then iid Geometric(p) with mean 1/p and the resampled series is genuinely
#' stationary conditionally on the data -- what the paper's title claims and
#' what fixed blocks cannot deliver.  The expected block length 1/p plays the
#' role ell plays in the moving-block bootstrap, and Btblen estimates the
#' optimal one.
#'
#' Anchors, both exact and neither an asymptotic claim: p = 1 makes every
#' block length 1, so the procedure collapses to the ordinary iid bootstrap;
#' and the number of block starts is exactly 1 + Binomial(n - 1, p) per
#' replicate, because the first step always starts a block and each of the
#' remaining n - 1 steps restarts independently with probability p, so
#' E[n_runs] = B (1 + (n-1) p) exactly -- a closed form free of the censoring
#' that makes the realised block LENGTHS a biased sample of the Geometric law.
#' mean_block is reported as n B / n_runs and is stated to be that ratio, not
#' a Geometric mean.
#'
#' @param x the series, in time order.
#' @param p restart probability in (0, 1]; mean block length 1/p; NULL uses
#'   n^(-1/3).
#' @param stat statistic of a series; NULL uses the mean.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, estimate, se, lo, hi, p, exp_block, mean_block,
#'   exp_runs, n_runs, n, B, method.
#' @keywords internal
#' @examples
#' Btsbb(sin(1:60 / 3), 0.25, NULL, 50)$exp_runs
#' @export
Btsbb <- function(x, p = NULL, stat = NULL, B = 200, seed = 1, alpha = 0.05) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_stationary_block: need at least two observations")
  if (is.null(p)) p <- n^(-1 / 3)
  p <- as.numeric(p)
  if (!(p > 0 && p <= 1)) stop("boot_stationary_block: p must lie in (0, 1]")
  if (as.integer(B) < 2L) stop("boot_stationary_block: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_stationary_block: alpha must lie strictly between 0 and 1")
  f <- if (is.null(stat)) .s03mean else stat
  g <- .t1_lcg(seed)
  theta <- numeric(as.integer(B)); runs <- 0L
  for (b in seq_len(as.integer(B))) {
    smp <- numeric(n); j <- 0L
    for (t in seq_len(n)) {
      if (t == 1L || g$unif() < p) {
        runs <- runs + 1L
        j <- as.integer(g$unif() * n)
        if (j >= n) j <- n - 1L
      } else {
        j <- (j + 1L) %% n
      }
      smp[t] <- xx[j + 1L]
    }
    theta[b] <- as.numeric(f(smp))
  }
  list(theta_b = theta, estimate = as.numeric(f(xx)), se = .s03sd(theta, 1L),
       lo = .s03quantile7(theta, a / 2), hi = .s03quantile7(theta, 1 - a / 2),
       p = p, exp_block = 1 / p, mean_block = (n * as.integer(B)) / runs,
       exp_runs = as.integer(B) * (1 + (n - 1) * p), n_runs = runs,
       n = n, B = as.integer(B),
       method = "Politis and Romano (1994) JASA 89(428):1303-1313")
}
