#' Mann-Kendall trend test
#'
#' \eqn{S = \sum_{i < j} sign(x_j - x_i)} with the tie-corrected null variance
#' \eqn{var(S) = (n(n-1)(2n+5) - \sum_t t(t-1)(2t+5)) / 18} over the
#' multiplicities t of tied values, and \eqn{z = sign(S)(|S| - 1)/\sqrt{var(S)}}
#' under the continuity correction (\eqn{S / \sqrt{var(S)}} without it); the
#' p-value is two-sided normal. Kendall's tau uses the tie-adjusted
#' denominator (tau-b against an untied time index). Identical to the Python
#' arm \code{morie.fn.mannK.mann_kendall}.
#'
#' @param x Series in time order.
#' @param continuity Apply the continuity correction.
#' @return List: statistic (z), p_value, S, varS, tau, n, method.
#' @references Mann, H. B. (1945). Nonparametric tests against trend.
#'   Econometrica 13, 245-259.
#'
#'   Kendall, M. G. (1975). Rank Correlation Methods, 4th edn. Griffin.
#' @examples
#' MannKendall(c(1.2, 0.8, 1.9, 2.4, 2.1, 3.3, 3.0, 4.1))
#' @export
MannKendall <- function(x, continuity = TRUE) {
  x <- as.numeric(unlist(x))
  n <- length(x)
  if (n < 3) stop("need at least 3 observations")
  s <- 0
  for (j in seq_len(n)) {
    for (i in seq_len(j)) {
      d <- x[j] - x[i]
      s <- s + (if (d > 0) 1 else if (d < 0) -1 else 0)
    }
  }
  tt <- as.numeric(table(x))
  vars <- (n * (n - 1) * (2 * n + 5) - sum(tt * (tt - 1) * (2 * tt + 5))) / 18
  den <- sqrt(0.5 * n * (n - 1) - 0.5 * sum(tt * (tt - 1))) * sqrt(0.5 * n * (n - 1))
  tau <- if (den > 0) s / den else NaN
  z <- if (vars <= 0) {
    NaN
  } else if (continuity) {
    sign(s) * (abs(s) - 1) / sqrt(vars)
  } else {
    s / sqrt(vars)
  }
  p <- 2 * min(0.5, stats::pnorm(abs(z), lower.tail = FALSE))
  list(statistic = z, p_value = p, S = s, varS = vars, tau = tau, n = n, method = "Mann-Kendall trend test")
}
