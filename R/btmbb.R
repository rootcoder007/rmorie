# SPDX-License-Identifier: AGPL-3.0-or-later
#' Moving-block bootstrap for a stationary series (Kuensch)
#'
#' Kuensch, H. R. (1989), "The Jackknife and the Bootstrap for General
#' Stationary Observations", The Annals of Statistics 17(3), 1217-1241,
#' doi:10.1214/aos/1176347265 (verified against Crossref).
#'
#' The iid bootstrap destroys the dependence that defines a time series and so
#' understates every standard error.  Kuensch's fix is to resample OVERLAPPING
#' blocks of ell consecutive observations: there are n - ell + 1 possible
#' starting positions, k = ceiling(n / ell) blocks are drawn uniformly from
#' them with replacement and concatenated, and the result truncated back to n.
#'
#' Overlapping blocks are the point of the moving version.  The
#' non-overlapping variant has only floor(n / ell) resampling units and the
#' overlapping one has n - ell + 1, which is why it is the more efficient of
#' the two -- at the cost that neighbouring blocks share observations and are
#' therefore not independent.  The last block is truncated whenever ell does
#' not divide n; n_blocks and truncated report this rather than leaving it
#' implicit.
#'
#' Anchor: ell = 1 makes the moving-block bootstrap the ordinary iid
#' bootstrap exactly, whose variance of the mean is the closed form
#' sum (x - xbar)^2 / n^2, reported as var_iid.
#'
#' @param x the series, in time order.
#' @param block_len block length ell; NULL uses max(floor(n^(1/3)), 1).
#' @param stat statistic of a series; NULL uses the mean.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, estimate, se, lo, hi, block_len, n_blocks, n_starts,
#'   truncated, var_iid, n, B, method.
#' @keywords internal
#' @examples
#' Btmbb(sin(1:60 / 3), 4, NULL, 50)$se
#' @export
Btmbb <- function(x, block_len = NULL, stat = NULL, B = 200, seed = 1, alpha = 0.05) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_moving_block: need at least two observations")
  if (is.null(block_len)) block_len <- max(as.integer(n^(1 / 3)), 1L)
  ell <- as.integer(block_len)
  if (!(ell >= 1L && ell <= n)) stop("boot_moving_block: block_len must lie in 1..n")
  if (as.integer(B) < 2L) stop("boot_moving_block: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_moving_block: alpha must lie strictly between 0 and 1")
  f <- if (is.null(stat)) .s03mean else stat
  bl <- .btmbb_reps(xx, ell, f, B, seed, FALSE)
  theta <- bl$theta
  k <- bl$k
  xb <- .s03mean(xx)
  list(theta_b = theta, estimate = as.numeric(f(xx)), se = .s03sd(theta, 1L),
       lo = .s03quantile7(theta, a / 2), hi = .s03quantile7(theta, 1 - a / 2),
       block_len = ell, n_blocks = k, n_starts = n - ell + 1L,
       truncated = k * ell - n, var_iid = sum((xx - xb)^2) / (n * n),
       n = n, B = as.integer(B),
       method = "Kuensch (1989) Ann. Statist. 17(3):1217-1241")
}

#' @noRd
.btmbb_reps <- function(x, ell, stat, B, seed, circular) {
  n <- length(x)
  k <- as.integer(ceiling(n / ell))
  starts <- if (circular) n else n - ell + 1L
  g <- .t1_lcg(seed)
  out <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B))) {
    smp <- numeric(k * ell)
    q <- 0L
    for (j in seq_len(k)) {
      s <- as.integer(g$unif() * starts)
      if (s >= starts) s <- starts - 1L
      for (t in 0:(ell - 1L)) {
        q <- q + 1L
        smp[q] <- if (circular) x[((s + t) %% n) + 1L] else x[s + t + 1L]
      }
    }
    out[b] <- as.numeric(stat(smp[seq_len(n)]))
  }
  list(theta = out, k = k)
}
