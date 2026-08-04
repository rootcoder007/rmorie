# SPDX-License-Identifier: AGPL-3.0-or-later

#' Naive kernel-smoothed Cramer-von Mises statistic (Eq. 5.4)
#'
#' Eq. (5.4): \deqn{\widehat{CvM} = n\int [\hat F_X(x)-F(x)]^2 dF(x),}{CvMhat = n int [Fhat_X(x) - F(x)]^2 dF(x),}
#' with `Fhat_X` the naive kernel distribution function estimator.
#'
#' Unlike the empirical `CvM_n` this has no finite-sum closed form: `Fhat_X` is
#' smooth, so the integral does not collapse onto the order statistics. It is
#' evaluated by substituting `u = F(x)` and integrating over `u` in `(0,1)` on
#' a fixed equally spaced grid -- which turns `dF(x)` into `du` and needs no
#' density, only the quantile function.
#'
#' This module previously carried a copy of the empirical KS body. It now
#' computes a Cramer-von Mises statistic, and a smoothed one.
#'
#' Theorem 5.1 gives `|CvM_n - CvMhat| ->_p 0`, so the Cramer-von Mises
#' critical values still apply.
#'
#' @param x Sample.
#' @param quantile The null quantile function `F^-1(u)` for `u` in `(0,1)`.
#' @param h Bandwidth; defaults to the distribution-function rule.
#' @param ngrid Number of `u`-nodes; fixed, never adapted.
#' @return Named list with ``statistic``, ``p_value``, ``h``, ``n``, ``method``.
#' @references Fauzi and Maesono (2023), Eq. (5.4), Theorem 5.1.
#' @examples
#' Kerncvm(c(0.1, 0.3, 0.5, 0.7, 0.9), quantile = function(u) u)
#' @export
Kerncvm <- function(x, quantile, h = NULL, ngrid = 2001L) {
  x <- as.numeric(x)
  n <- length(x)
  if (n < 2L) stop("need at least two observations.")
  if (!is.function(quantile)) stop("quantile must be a function F^-1(u).")
  if (is.null(h)) h <- .morie_kdfe_h(x)
  if (h <= 0) stop("bandwidth must be positive.")
  m <- as.integer(ngrid)
  u <- (seq_len(m) - 0.5) / m
  integrand <- vapply(u, function(uu) {
    t <- as.numeric(quantile(uu))
    (mean(stats::pnorm((t - x) / h)) - uu)^2
  }, numeric(1))
  stat <- n * mean(integrand)
  pval <- 1
  if (stat > 0) {
    k <- 0:99
    acc <- sum(exp(-((4 * k + 1)^2) * pi^2 / (8 * stat)))
    pval <- min(1, max(0, 1 - acc * sqrt(2 / stat)))
  }
  list(statistic = stat, p_value = pval, h = h, n = n,
       method = "naive kernel-smoothed Cramer-von Mises statistic (Eq. 5.4)")
}

# CANONICAL TEST
# r <- Kerncvm(c(0.1, 0.3, 0.5, 0.7, 0.9), quantile = function(u) u)
# stopifnot(r$statistic > 0)

#' @rdname Kerncvm
#' @keywords internal
#' @export
morie_fauzi_naive_kernel_cvm <- Kerncvm
