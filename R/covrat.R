# SPDX-License-Identifier: AGPL-3.0-or-later

#' COVRATIO effect of deleting obs i on the precision of beta-hat
#'
#' Formula: COVRATIO_i = (s_(i)^2 / s^2)^p / (1 - h_ii)
#'
#' It is the ratio of generalised variances
#' det(s_(i)^2 (X_(i)'X_(i))^-1) / det(s^2 (X'X)^-1).  Values far from 1
#' mark observations that change the precision; the Belsley-Kuh-Welsch
#' cut-off is |COVRATIO - 1| > 3p/n.
#'
#' @param y Response vector, length n.
#' @param X Design matrix with n rows.
#' @param intercept Whether to prepend a column of ones.
#' @return List with \code{estimate} (max |COVRATIO - 1|),
#'   \code{covratio}, \code{threshold}, \code{flagged},
#'   \code{n_influential}, \code{n}, \code{p}, \code{method}.
#' @references Belsley, Kuh & Welsch (1980), Regression Diagnostics,
#'   Wiley, ch. 2.
#' @export
Covrat <- function(y, X, intercept = TRUE) {
  f <- .bkw_influence(y, X, intercept)
  n <- f$n; p <- f$p
  s2 <- f$sse / (n - p)
  out <- numeric(n)
  for (i in seq_len(n)) {
    si2 <- .bkw_sdel(f$sse, f$e[i], f$h[i], n, p)^2
    out[i] <- if (f$h[i] >= 1 || s2 <= 0) NaN else
      (si2 / s2)^p / (1 - f$h[i])
  }
  thr <- 3 * p / n
  flagged <- as.integer(!is.nan(out) & abs(out - 1) > thr)
  dev <- abs(out[!is.nan(out)] - 1)
  .t1_result(estimate = if (length(dev)) max(dev) else NaN,
             covratio = out, threshold = thr, flagged = flagged,
             n_influential = sum(flagged), n = n, p = p,
             method = "COVRATIO deletion effect on the covariance of beta-hat")
}
