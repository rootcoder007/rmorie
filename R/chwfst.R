# SPDX-License-Identifier: AGPL-3.0-or-later
#' Chow forecast (predictive-failure) test (Chow 1960)
#'
#' Source: Chow, G. C. (1960), Tests of equality between sets of
#' coefficients in two linear regressions, Econometrica 28, 591-605.
#' The 1960 paper is paywalled here and could not be retrieved; the
#' second of the two tests in that paper, the forecast test, is quoted
#' in its standard published form
#' \code{F = ((RSS_c - RSS_1)/n2) / (RSS_1/(n1 - k))} on
#' \code{(n2, n1 - k)} degrees of freedom, where RSS_1 fits the first
#' \code{n1} observations alone and RSS_c fits all \code{n1 + n2}.
#'
#' @param y Numeric response of length n.
#' @param X Numeric n x p regressor matrix.
#' @param split Number n1 of leading observations used for estimation.
#' @param add_intercept Prepend a column of ones.  Default TRUE.
#' @return list: statistic, p_value, df1, df2, rss1, rss_pooled, n1, n2,
#'   k, method.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(60), 30, 2)
#' Chowfc(X[, 1] + rnorm(30), X, 20)$statistic
#' @export
Chowfc <- function(y, X, split, add_intercept = TRUE) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  n <- length(y)
  if (nrow(X) != n) X <- t(X)
  n1 <- as.integer(split)
  n2 <- n - n1
  D <- if (add_intercept) cbind(1, X) else X
  k <- ncol(D)
  if (n2 < 1 || n1 - k < 1) stop("need 1 <= n2 and n1 > k")
  rss <- function(Dm, yy) {
    b <- qr.solve(Dm, yy)
    r <- yy - Dm %*% b
    sum(r * r)
  }
  rss1 <- rss(D[seq_len(n1), , drop = FALSE], y[seq_len(n1)])
  rssc <- rss(D, y)
  df1 <- n2
  df2 <- n1 - k
  stat <- ((rssc - rss1) / df1) / (rss1 / df2)
  list(
    statistic = stat, p_value = stats::pf(stat, df1, df2, lower.tail = FALSE),
    df1 = as.integer(df1), df2 = as.integer(df2),
    rss1 = rss1, rss_pooled = rssc, n1 = n1, n2 = n2, k = k,
    method = "Chow (1960) forecast test"
  )
}
