# SPDX-License-Identifier: AGPL-3.0-or-later
#' m-out-of-n bootstrap: resample m < n points with replacement
#'
#' Bickel, P. J., Goetze, F. and van Zwet, W. R. (1997), "Resampling fewer
#' than n observations: gains, losses, and remedies for losses", Statistica
#' Sinica 7(1), 1-31.
#'
#' The ordinary bootstrap fails when the statistic is not smooth in the
#' empirical measure -- boundary parameters, extremes, shrinkage at a kink.
#' Drawing m << n points with replacement restores consistency because the
#' resample no longer resolves the non-smooth feature, at the cost of a slower
#' rate.  The rescaling that makes the replicates usable is the one the paper
#' is built on: it is the law of sqrt(m) (theta*_m - theta_hat) that
#' approximates the law of sqrt(n) (theta_hat - theta), so the implied
#' standard error for theta_hat is sqrt(m/n) times the standard deviation of
#' the replicates, and the interval is centred on theta_hat with half-widths
#' shrunk by the same factor.  Both are returned: se_raw is the raw replicate
#' spread and se the rescaled one.  m = n makes the two identical, which is
#' the degenerate anchor.
#'
#' @param x the observed sample.
#' @param m resample size; NULL uses floor(sqrt(n)).
#' @param stat statistic of a sample; NULL uses the mean.
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, theta_hat, se_raw, se, lo, hi, m, n, B, estimate,
#'   method.
#' @keywords internal
#' @examples
#' Btmoutn(c(1, 2, 3, 4, 5, 6, 7, 8, 9), B = 50)$se
#' @export
Btmoutn <- function(x, m = NULL, stat = NULL, B = 200, seed = 1, alpha = 0.05) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_m_out_of_n: need at least two observations")
  if (is.null(m)) m <- floor(sqrt(n))
  m <- as.integer(m)
  if (!(m >= 1L && m <= n)) stop("boot_m_out_of_n: m must lie in 1..n")
  if (as.integer(B) < 2L) stop("boot_m_out_of_n: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_m_out_of_n: alpha must lie strictly between 0 and 1")
  f <- if (is.null(stat)) .s03mean else stat
  th <- as.numeric(f(xx))
  g <- .t1_lcg(seed)
  theta <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B))) {
    idx <- .btmoutn_idx(g, n, m)
    theta[b] <- as.numeric(f(xx[idx]))
  }
  raw <- .s03sd(theta, 1L)
  r <- sqrt(m / n)
  qlo <- .s03quantile7(theta, a / 2)
  qhi <- .s03quantile7(theta, 1 - a / 2)
  list(theta_b = theta, theta_hat = th, se_raw = raw, se = r * raw,
       lo = th + r * (qlo - th), hi = th + r * (qhi - th),
       m = m, n = n, B = as.integer(B), estimate = th,
       method = "Bickel, Goetze and van Zwet (1997) Statist. Sinica 7(1):1-31")
}

#' @noRd
.btmoutn_idx <- function(g, n, m) {
  out <- integer(m)
  for (i in seq_len(m)) {
    j <- as.integer(g$unif() * n)
    if (j >= n) j <- n - 1L
    out[i] <- j + 1L
  }
  out
}
