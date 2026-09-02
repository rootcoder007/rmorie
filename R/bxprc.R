# SPDX-License-Identifier: AGPL-3.0-or-later
#' Box-Pierce Q test for autocorrelation.
#'
#' Formula: \eqn{Q = n \sum_{k=1}^{m} r_k^2}, chi-square on
#' \code{lags - fitdf} degrees of freedom.  This is the uncorrected
#' portmanteau statistic; \code{Ljungbox} applies the small-sample
#' variance correction to the same sum.
#'
#' @param x Series, normally a residual series.
#' @param lags Number of lags entering the sum.
#' @param fitdf Parameters fitted to obtain \code{x}.
#' @return List with \code{statistic}, \code{p_value}, \code{df},
#'   \code{acf}, \code{n}, \code{method}.
#' @references Box and Pierce (1970), JASA 65:1509-1526.  Paywalled; the statistic was taken from R's stats::Box.test, which codes the Box-Pierce branch as n*sum(obs^2).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Boxpierce(V)
Boxpierce <- function(x, lags = 1, fitdf = 0) {
  x <- .t4_vec(x); n <- length(x); m <- as.integer(lags); fitdf <- as.integer(fitdf)
  if (m < 1 || n <= m) stop("need 1 <= lags < length(x)")
  r <- .t4_acfbiased(x, m)
  q <- n * sum(r^2)
  df <- m - fitdf
  p <- if (df > 0) stats::pchisq(q, df, lower.tail = FALSE) else NaN
  .t4_result(statistic = q, p_value = p, df = as.integer(df), acf = r,
             n = as.integer(n), method = "Box-Pierce Q test")
}
