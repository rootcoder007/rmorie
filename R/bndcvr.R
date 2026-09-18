# SPDX-License-Identifier: AGPL-3.0-or-later
#' Empirical coverage of replicated intervals, tested against nominal
#'
#' Coverage is the property a confidence construction claims and the one
#' thing a simulation can check directly. Under-coverage is the failure
#' that matters, so the test is one-sided: the number of covering
#' replications is binomial with the nominal success probability and the
#' reported p-value is its exact lower tail. A two-sided test would flag
#' conservative procedures, which are not wrong in the same way.
#'
#' Formula: \code{coverage = mean(lower <= theta <= upper)} and
#' \code{p = P(Bin(R, 1 - alpha) <= n_covered)}.
#'
#' @param lower,upper Replicated interval endpoints, same length.
#' @param theta_true The data-generating parameter value.
#' @param alpha Nominal miss probability, default 0.05.
#' @return List with \code{coverage}, \code{nominal}, \code{n_covered},
#'   \code{R}, \code{p_value}, \code{reject}, \code{mean_width}.
#' @references The coverage requirements distinguished here are equations
#'   (4.11) to (4.14) of Molinari, F. (2021), Handbook of Econometrics 7A
#'   (arXiv:2004.11751 pp. 97-100). Andrews, D. W. K. and Soares, G.
#'   (2010), Econometrica 78(1), 119-157, \doi{10.3982/ECTA7502}, is the
#'   stub's attribution.
#' @export
#' @examples
#' Bndcvr(lower = c(1, 2, 3, 4, 5, 6, 7, 8), upper = c(1, 2, 3, 4, 5, 6, 7, 8),
#' theta_true = c(1, 2, 3, 4, 5, 6, 7, 8))
Bndcvr <- function(lower, upper, theta_true, alpha = 0.05) {
  lo <- as.numeric(unlist(lower))
  hi <- as.numeric(unlist(upper))
  R <- length(lo)
  if (R == 0L) stop("Bndcvr: lower is empty")
  if (length(hi) != R)
    stop("Bndcvr: lower and upper must have the same length")
  a <- as.numeric(alpha)[1]
  if (!(a > 0 && a < 1)) stop("Bndcvr: alpha must lie in (0, 1)")
  if (any(hi < lo)) stop("Bndcvr: upper is below lower at some replicate")
  t <- as.numeric(theta_true)[1]
  k <- sum(lo <= t & t <= hi)
  p <- 1 - a
  term <- (1 - p)^R
  tail <- term
  if (k >= 1L) for (j in seq_len(k)) {
    term <- term * p * (R - j + 1) / ((1 - p) * j)
    tail <- tail + term
  }
  if (tail > 1) tail <- 1
  .t1_result(coverage = k / R, nominal = p, n_covered = k, R = R,
             p_value = tail, reject = if (tail < a) 1 else 0,
             mean_width = mean(hi - lo),
             method = "Coverage probability check")
}
