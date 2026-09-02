# SPDX-License-Identifier: AGPL-3.0-or-later
#' Subsampling: size-m subsets drawn WITHOUT replacement
#'
#' Politis, D. N., Romano, J. P. and Wolf, M. (1999), Subsampling, Springer
#' Series in Statistics.
#'
#' Subsampling is not a bootstrap.  It needs only that the normalised
#' statistic have SOME limit law; it never assumes that law is estimable by
#' resampling with replacement, so it stays consistent where both Efron's
#' bootstrap and the m-out-of-n bootstrap fail.  Draw subsets of size m
#' without replacement, form the roots tau_m (theta_hat_m,b - theta_hat) with
#' tau_m = sqrt(m), and read the interval off their quantiles rescaled by
#' tau_n = sqrt(n):
#'   \[ theta_hat - q_(1-alpha/2)/tau_n , theta_hat - q_(alpha/2)/tau_n \].
#' Note the inversion: the UPPER quantile of the root sets the LOWER endpoint.
#' Getting this backwards is the classical sign error here and it is invisible
#' on a symmetric fixture, so the module is anchored on an asymmetric one.
#'
#' Anchors, neither of which runs through the resampling loop: with m = n
#' every subsample is the whole sample, so for any permutation-invariant
#' statistic every replicate equals theta_hat and the root distribution
#' collapses to a point mass at zero; and for the mean the exact
#' without-replacement variance of a subsample mean is (s^2/m)(n-m)/n with s^2
#' the ddof=1 sample variance, which var_closed reports.  With m = 1 that must
#' reduce to the POPULATION variance sum(x-xbar)^2/n; the (n-m)/(n-1) form
#' found in some notes does not, and an exhaustive enumeration of all C(9,4)
#' subset means is what caught it.
#'
#' @param x the observed sample.
#' @param m subsample size; NULL uses floor(sqrt(n)).
#' @param stat statistic of a sample; NULL uses the mean.
#' @param B number of subsamples.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, roots, theta_hat, lo, hi, se, var_closed, m, n, B,
#'   estimate, method.
#' @keywords internal
#' @examples
#' Btsubs(c(1, 2, 3, 4, 5, 6, 7, 8, 9), m = 4, B = 50)$var_closed
#' @export
Btsubs <- function(x, m = NULL, stat = NULL, B = 200, seed = 1, alpha = 0.05) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_subsampling: need at least two observations")
  if (is.null(m)) m <- floor(sqrt(n))
  m <- as.integer(m)
  if (!(m >= 1L && m <= n)) stop("boot_subsampling: m must lie in 1..n")
  if (as.integer(B) < 2L) stop("boot_subsampling: need at least two subsamples")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_subsampling: alpha must lie strictly between 0 and 1")
  f <- if (is.null(stat)) .s03mean else stat
  th <- as.numeric(f(xx))
  g <- .t1_lcg(seed)
  theta <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B))) {
    idx <- .btsubs_idx(g, n, m)
    theta[b] <- as.numeric(f(xx[idx]))
  }
  tm <- sqrt(m)
  tn <- sqrt(n)
  roots <- tm * (theta - th)
  qlo <- .s03quantile7(roots, a / 2)
  qhi <- .s03quantile7(roots, 1 - a / 2)
  vc <- if (is.null(stat) && n > 1L && m > 0L)
    (.s03var(xx, 1L) / m) * (n - m) / n else NaN
  list(theta_b = theta, roots = roots, theta_hat = th,
       lo = th - qhi / tn, hi = th - qlo / tn,
       se = if (length(roots) > 1L) .s03sd(roots, 1L) / tn else NaN,
       var_closed = vc, m = m, n = n, B = as.integer(B), estimate = th,
       method = "Politis, Romano and Wolf (1999) Subsampling, Springer")
}

#' @noRd
.btsubs_idx <- function(g, n, m) {
  p <- seq_len(n) - 1L
  for (i in seq_len(m)) {
    k <- (i - 1L) + as.integer(g$unif() * (n - (i - 1L)))
    if (k > n - 1L) k <- n - 1L
    tmp <- p[i]
    p[i] <- p[k + 1L]
    p[k + 1L] <- tmp
  }
  p[seq_len(m)] + 1L
}
