# SPDX-License-Identifier: AGPL-3.0-or-later
#' Smoothed bootstrap: resample, then perturb by a kernel draw
#'
#' Silverman, B. W. and Young, G. A. (1987), "The bootstrap: to smooth or not
#' to smooth?", Biometrika 74(3), 469-479, doi:10.1093/biomet/74.3.469
#' (verified against Crossref).
#'
#' Efron's bootstrap resamples the empirical distribution, which is a step
#' function; for statistics that depend on the local shape of F (a density
#' ordinate, a quantile, a mode) that atomicity is the dominant source of
#' error.  The smoothed bootstrap resamples from a kernel density estimate
#' instead, which for a Gaussian kernel is exactly
#'   x*_i = x_(I_i) + h eps_i,  I_i ~ U\{1..n\},  eps_i ~ N(0, 1),
#' so no density has to be evaluated.
#'
#' The bandwidth is the whole trade-off and the paper's title is the warning:
#' smoothing helps only when the functional is sensitive to the fine structure
#' of F, and h > 0 inflates the variance of anything that is not.  Both facts
#' are visible in the closed form for the mean, which is this module's anchor:
#' the conditional variance of the smoothed bootstrap mean is exactly
#' (sigma_hat^2 + h^2)/n with sigma_hat^2 = sum (x_i - xbar)^2 / n, i.e. the
#' ordinary bootstrap variance plus h^2/n, with no benefit.  var_closed
#' reports it and does not run through the resampling loop.  h = 0 reduces the
#' procedure to the ordinary bootstrap exactly, which is the second anchor.
#'
#' @param x the observed sample.
#' @param stat statistic of a sample; NULL uses the mean.
#' @param h kernel bandwidth; NULL uses 0.9 min(s, IQR/1.34) n^(-1/5).
#' @param B replicates.
#' @param seed seed for the shared deterministic stream.
#' @param alpha two-sided error rate.
#' @return list: theta_b, estimate, se, lo, hi, h, var_closed, n, B, method.
#' @keywords internal
#' @examples
#' Btsmth(c(1, 2, 3, 4, 5, 6, 7, 8), h = 0, B = 50)$var_closed
#' @export
Btsmth <- function(x, stat = NULL, h = NULL, B = 200, seed = 1, alpha = 0.05) {
  xx <- .s03vec(x)
  n <- length(xx)
  if (n < 2L) stop("boot_smoothed: need at least two observations")
  if (as.integer(B) < 2L) stop("boot_smoothed: need at least two replicates")
  a <- as.numeric(alpha)
  if (!(a > 0 && a < 1)) stop("boot_smoothed: alpha must lie strictly between 0 and 1")
  if (is.null(h)) {
    s <- .s03sd(xx, 1L)
    iqr <- .s03quantile7(xx, 0.75) - .s03quantile7(xx, 0.25)
    spread <- if (iqr <= 0 || s < iqr / 1.34) s else iqr / 1.34
    h <- 0.9 * spread * n^(-0.2)
  }
  h <- as.numeric(h)
  if (h < 0) stop("boot_smoothed: h must be non-negative")
  f <- if (is.null(stat)) .s03mean else stat
  th <- as.numeric(f(xx))
  g <- .t1_lcg(seed)
  theta <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B))) {
    smp <- numeric(n)
    for (i in seq_len(n)) {
      j <- as.integer(g$unif() * n)
      if (j >= n) j <- n - 1L
      smp[i] <- xx[j + 1L] + h * g$norm()
    }
    theta[b] <- as.numeric(f(smp))
  }
  xb <- .s03mean(xx)
  s2 <- sum((xx - xb)^2) / n
  list(theta_b = theta, estimate = th, se = .s03sd(theta, 1L),
       lo = .s03quantile7(theta, a / 2), hi = .s03quantile7(theta, 1 - a / 2),
       h = h, var_closed = if (is.null(stat)) (s2 + h * h) / n else NaN,
       n = n, B = as.integer(B),
       method = "Silverman and Young (1987) Biometrika 74(3):469-479")
}
