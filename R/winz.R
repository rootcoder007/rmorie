# SPDX-License-Identifier: AGPL-3.0-or-later
#' Mean after pulling the tails in to the quantiles
#'
#' Trimming discards extreme observations; winsorizing caps them. The
#' difference matters for the variance: the winsorized sample still has n
#' observations, so the estimator has a smaller standard error than the
#' trimmed mean at the same alpha.
#'
#' Formula: replace values below the alpha quantile with it and above the
#' \code{1 - alpha} quantile with that, then take the mean.
#'
#' @param x Sample.
#' @param alpha Fraction winsorized at each tail.
#' @return List with \code{estimate}, \code{lower}, \code{upper},
#'   \code{n_changed}, \code{n}.
#' @references Dixon, W. J. (1960). Ann Math Statist 31:385-391, where
#'   Winsor rule is set out and attributed to him.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Winz(V)
Winz <- function(x, alpha = 0.1) {
  v <- as.numeric(unlist(x))
  n <- length(v)
  lo <- .s4_quantile7(v, alpha)
  hi <- .s4_quantile7(v, 1 - alpha)
  w <- pmin(pmax(v, lo), hi)
  .t1_result(estimate = sum(w) / n, lower = lo, upper = hi,
             n_changed = sum(w != v), n = n, method = "Winsorized mean")
}
