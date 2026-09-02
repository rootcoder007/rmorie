# SPDX-License-Identifier: AGPL-3.0-or-later
#' Ljung-Box Q test for autocorrelation.
#'
#' Formula: \eqn{Q = n(n+2) \sum_{k=1}^{m} r_k^2 / (n-k)}, referred to
#' chi-square on \code{lags - fitdf} degrees of freedom.  The
#' \eqn{(n+2)/(n-k)} factor is a variance correction for \eqn{r_k},
#' whose exact null variance is \eqn{(n-k)/(n(n+2))} rather than
#' \eqn{1/n}; \eqn{r_k} uses the biased normalisation of
#' \code{stats::acf}.
#'
#' @param y Series, normally a residual series.
#' @param lags Number of lags entering the sum.
#' @param fitdf Parameters fitted to obtain \code{y}.
#' @return List with \code{statistic}, \code{p_value}, \code{df},
#'   \code{acf}, \code{n}, \code{method}.
#' @references Ljung and Box (1978), On a measure of lack of fit in time series models, Biometrika 65:297-303.  Paywalled at JSTOR (HTTP 403); the statistic was taken from R's own stats::Box.test (src/library/stats/R/ts-tests.R), the canonical reference implementation.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Ljungbox(V)
Ljungbox <- function(y, lags = 1, fitdf = 0) {
  y <- .t4_vec(y); n <- length(y); m <- as.integer(lags); fitdf <- as.integer(fitdf)
  if (m < 1 || n <= m) stop("need 1 <= lags < length(y)")
  r <- .t4_acfbiased(y, m)
  q <- n * (n + 2) * sum(r^2 / (n - seq_len(m)))
  df <- m - fitdf
  p <- if (df > 0) stats::pchisq(q, df, lower.tail = FALSE) else NaN
  .t4_result(statistic = q, p_value = p, df = as.integer(df), acf = r,
             n = as.integer(n), method = "Ljung-Box Q test")
}
