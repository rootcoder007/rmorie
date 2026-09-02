# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hoeffding's inequality
#'
#' The bound is distribution-free, which is its point; it is vacuous
#' until the deviation exceeds (b - a) sqrt(log 2 / (2n)), and that
#' threshold is reported so the caller can see when the bound starts to
#' say anything.
#'
#' Formula: P(|mean - E mean| >= t) <= 2 exp(-2 n t^2 / (b - a)^2).
#'
#' @param a Lower end of the support.
#' @param b Upper end of the support, greater than a.
#' @param n Sample size, at least one.
#' @param t Non-negative deviation.
#' @return List with \code{estimate} (bound capped at 1), \code{bound},
#'   \code{one_sided}, \code{informative}, \code{t_min}, \code{n},
#'   \code{method}.
#' @references Hoeffding (1963), Probability inequalities for sums of
#'   bounded random variables, JASA 58(301):13-30, Theorem 2.
#'   \doi{10.1080/01621459.1963.10500830}
#' @export
#' @examples
#' Hffdsg(a = 0, b = 1, n = 100, t = 0.1)
Hffdsg <- function(a, b, n, t) {
  av <- as.numeric(a)
  bv <- as.numeric(b)
  if (!(bv > av)) stop("hoeffding_inequality: need a < b")
  nn <- as.integer(n)
  if (nn < 1L) stop("hoeffding_inequality: n must be at least 1")
  tv <- as.numeric(t)
  if (tv < 0) stop("hoeffding_inequality: t must be non-negative")
  rng <- bv - av
  ex <- -2 * nn * tv * tv / (rng * rng)
  bound <- 2 * exp(ex)
  .t1_result(estimate = min(bound, 1), bound = bound, one_sided = exp(ex),
             informative = as.integer(bound < 1),
             t_min = rng * sqrt(log(2) / (2 * nn)), n = nn,
             method = "2 exp(-2 n t^2 / (b - a)^2), Hoeffding (1963) Theorem 2")
}
