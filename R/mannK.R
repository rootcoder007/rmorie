# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mann-Kendall rank test for monotone trend
#'
#' Formula: \eqn{S = \sum_{i<j} \mathrm{sign}(x_j - x_i)} with null
#' variance \eqn{\[n(n-1)(2n+5) - \sum t(t-1)(2t+5)\]/18} over the tie
#' multiplicities \eqn{t}, and \eqn{z = \mathrm{sign}(S)(|S|-1)/
#' \sqrt{\mathrm{var} S}} under the continuity correction.  Kendall's
#' tau uses the tie-adjusted denominator.
#'
#' @param x Series in time order.
#' @param continuity Apply the \eqn{|S|-1} continuity correction.
#' @return List with \code{statistic} (z), \code{p_value}, \code{S},
#'   \code{varS}, \code{tau}, \code{n}, \code{method}.
#' @references Mann (1945), Econometrica 13:245-259; Kendall (1975), Rank Correlation Methods.  Both paywalled; the coded form was read from Pohlert's CRAN package trend (R/mk.test.R, R/utilfn.R), whose .varmk and .Dfn give the tie corrections verbatim.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Mktest(V)
Mktest <- function(x, continuity = TRUE) {
  x <- .t4_vec(x)
  n <- length(x)
  if (n < 3) stop("need at least 3 observations")
  S <- 0
  for (j in seq_len(n)) S <- S + sum(sign(x[j] - x[1:j]))
  tt <- .t4_tiecounts(x)
  varS <- (n * (n - 1) * (2 * n + 5) - sum(tt * (tt - 1) * (2 * tt + 5))) / 18
  den <- sqrt(0.5 * n * (n - 1) - 0.5 * sum(tt * (tt - 1))) * sqrt(0.5 * n * (n - 1))
  tau <- if (den > 0) S / den else NaN
  z <- if (varS <= 0) NaN else if (continuity) sign(S) * (abs(S) - 1) / sqrt(varS) else S / sqrt(varS)
  p <- 2 * min(0.5, stats::pnorm(abs(z), lower.tail = FALSE))
  .t4_result(statistic = z, p_value = p, S = S, varS = varS, tau = tau,
             n = as.integer(n), method = "Mann-Kendall trend test")
}
