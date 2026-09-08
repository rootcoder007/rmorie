# SPDX-License-Identifier: AGPL-3.0-or-later

#' Naive kernel-smoothed Kolmogorov-Smirnov statistic (Eq. 5.3)
#'
#' Eq. (5.3): \deqn{\widehat{KS} = \sup_x |\hat F_X(x) - F(x)|,}{KShat = sup_x |Fhat_X(x) - F(x)|,}
#' with `Fhat_X` the NAIVE kernel distribution function estimator,
#' `n^-1 sum_i W((x - X_i)/h)`.
#'
#' Because `Fhat_X` is continuous the supremum is no longer attained at a jump,
#' so unlike the empirical version it genuinely needs a grid. The grid is fixed
#' and deterministic: `ngrid` equally spaced points spanning the sample
#' extended by `pad` bandwidths on each side, since the smoothed estimator has
#' not reached 0 and 1 at the extreme order statistics.
#'
#' This module previously carried a copy of the empirical KS body, which
#' ignored the bandwidth entirely. It now smooths.
#'
#' Theorem 5.1 says `|KS_n - KShat| ->_p 0`, so the Kolmogorov critical values
#' still apply and the p-value uses them. Smoothing buys not a different limit
#' but better small-sample calibration, per Sec. 5.1.
#'
#' @param x Sample.
#' @param cdf The null distribution `F(t)`.
#' @param h Bandwidth; defaults to the distribution-function rule.
#' @param ngrid Grid size for the supremum; fixed, never adapted.
#' @param pad How many bandwidths to extend the grid beyond the sample range.
#' @return Named list with ``statistic``, ``p_value``, ``argmax``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (5.3), Theorem 5.1.
#' @examples
#' Kernks(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
#' @export
Kernks <- function(x, cdf, h = NULL, ngrid = 2001L, pad = 4) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least two observations.")
  if (!is.function(cdf)) stop("cdf must be a function F(t).")
  if (is.null(h)) h <- .morie_kdfe_h(x)
  if (h <= 0) stop("bandwidth must be positive.")
  grid <- seq(min(x) - pad * h, max(x) + pad * h, length.out = ngrid)
  khat <- vapply(grid, function(t) mean(stats::pnorm((t - x) / h)), numeric(1))
  fv <- vapply(grid, function(t) as.numeric(cdf(t)), numeric(1))
  d <- abs(khat - fv)
  k <- which.max(d)
  stat <- d[k]
  lam <- (sqrt(n) + 0.12 + 0.11 / sqrt(n)) * stat
  j <- seq_len(100L)
  pval <- min(1, max(0, 2 * sum((-1)^(j - 1) * exp(-2 * j^2 * lam^2))))
  list(statistic = stat, p_value = pval, argmax = grid[k], h = h, n = n,
       method = "naive kernel-smoothed Kolmogorov-Smirnov statistic (Eq. 5.3)")
}

# CANONICAL TEST
# r <- Kernks(c(0.1, 0.3, 0.5, 0.7, 0.9), cdf = function(t) pmin(pmax(t, 0), 1))
# stopifnot(r$statistic >= 0, r$statistic <= 1)

#' @rdname Kernks
#' @keywords internal
#' @export
morie_fauzi_naive_kernel_ks <- Kernks
