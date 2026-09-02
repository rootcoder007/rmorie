# SPDX-License-Identifier: AGPL-3.0-or-later
#' Sample correlation, its t test, and a Fisher-z interval
#'
#' The t test is exact under bivariate normality with df = n - 2; the
#' interval comes from Fisher's variance-stabilising transform, which is
#' why it is not symmetric about r.
#'
#' Formula: rhat = Sxy / sqrt(Sxx Syy);
#'   t = rhat sqrt(n - 2)/sqrt(1 - rhat^2) on n - 2 df;
#'   z = atanh(rhat), se_z = 1/sqrt(n - 3)
#'
#' @param x,y Paired samples of the same length, n >= 3.
#' @param level Confidence level.
#' @return List with \code{estimate}, \code{statistic}, \code{p_value},
#'   \code{df}, \code{z}, \code{se_z}, \code{ci_lower}, \code{ci_upper},
#'   \code{n}.
#' @references Wasserman (2004), All of Statistics, Example 7.13, which
#'   derives the plug-in sample correlation. Fetched as the full text of
#'   the book. The t test on n - 2 degrees of freedom and Fisher's z
#'   transform are NOT in that section; they are the standard published
#'   forms (Fisher, 1915, Biometrika 10(4), 507-521).
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Pearsonr(V, V)
Pearsonr <- function(x, y, level = 0.95) {
  x <- .t1_vec(x); y <- .t1_vec(y); n <- length(x)
  if (length(y) != n) stop("x and y must have the same length")
  if (n < 3L) stop("n must be at least 3 for the z interval")
  mx <- mean(x); my <- mean(y)
  sxy <- sum((x - mx) * (y - my))
  sxx <- sum((x - mx)^2); syy <- sum((y - my)^2)
  if (sxx <= 0 || syy <= 0)
    stop("a sample with zero variance has no correlation")
  r <- min(1, max(-1, sxy / sqrt(sxx * syy)))
  df <- n - 2L
  if (abs(r) >= 1) { t <- sign(r) * Inf; p <- 0 } else {
    t <- r * sqrt(df) / sqrt(1 - r^2)
    p <- 2 * stats::pt(abs(t), df, lower.tail = FALSE)
  }
  z <- if (abs(r) < 1) 0.5 * log((1 + r) / (1 - r)) else sign(r) * Inf
  sez <- 1 / sqrt(n - 3)
  zc <- stats::qnorm((1 + level) / 2)
  .t1_result(estimate = r, statistic = t, p_value = p, df = as.numeric(df),
             z = z, se_z = sez, ci_lower = tanh(z - zc * sez),
             ci_upper = tanh(z + zc * sez), n = n,
             method = "Sample correlation, Wasserman Example 7.13")
}
