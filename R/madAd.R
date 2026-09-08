# SPDX-License-Identifier: AGPL-3.0-or-later
#' Iglewicz-Hoaglin modified z-score anomaly scores
#'
#' M_i = 0.6745 (x_i - median(x)) / MAD with MAD = median(|x - median(x)|),
#' and the book's rule "outlier when |M_i| > 3.5".  Source consulted:
#' Iglewicz and Hoaglin (1993), How to Detect and Handle Outliers, chapter 5.
#'
#' @param x sample.
#' @param threshold flagging cut-off on |M|.
#' @param constant numerator constant, qnorm(3/4).
#' @return list: estimate, scores, outlier, center, mad, n_outliers, n, method.
#' @keywords internal
#' @examples
#' madAd(c(2.1, 3.4, 1.9, 5.6, 9.9))$n_outliers
#' @export
madAd <- function(x, threshold = 3.5, constant = 0.6745) {
  v <- as.numeric(x)
  ctr <- stats::median(v)
  mad0 <- stats::median(abs(v - ctr))
  m <- if (mad0 > 0) constant * (v - ctr) / mad0 else rep(0, length(v))
  flag <- abs(m) > threshold
  list(estimate = max(abs(m)), scores = m, outlier = flag, center = ctr,
       mad = mad0, n_outliers = as.integer(sum(flag)), n = length(v),
       method = "Modified z-score outlier labelling (Iglewicz & Hoaglin 1993, ch. 5)")
}

# CANONICAL TEST
# r <- madAd(c(2.1,3.4,1.9,5.6,2.8,3.1,9.9,2.5,3.3,2.7))
# stopifnot(r$n_outliers == 2L, abs(r$mad - 0.45) < 1e-14)

#' @rdname madAd
#' @keywords internal
#' @export
morie_madAd <- madAd
