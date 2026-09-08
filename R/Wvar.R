# SPDX-License-Identifier: AGPL-3.0-or-later
#' Weighted variance and weighted mean
#'
#' The weights are reliability weights: an observation of weight w counts
#' as w observations, so the denominator is sum(w) - 1 rather than n - 1.
#' With every weight equal to one this is exactly the unbiased sample
#' variance.
#'
#' Formula: ybar_w = sum(w y) / sum(w),
#'   s2_w = sum(w (y - ybar_w)^2) / (sum(w) - 1).
#'
#' @param y Numeric observations.
#' @param weights Optional non-negative weights; equal weights if NULL.
#' @return List with \code{estimate} (weighted variance), \code{mean},
#'   \code{sd}, \code{se}, \code{sumw}, \code{n}, \code{method}.
#' @references Lohr, S. L. (2010). Sampling: Design and Analysis, 2nd ed.
#'   Brooks/Cole, section 7.2.
#' @examples
#' Wvar(c(1, 2, 3, 10))
#' @export
Wvar <- function(y, weights = NULL) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n == 0L) stop("weighted_variance: y is empty")
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  if (length(w) != n) stop("weighted_variance: y and weights differ in length")
  if (any(w < 0)) stop("weighted_variance: weights must be non-negative")
  sw <- sum(w)
  if (sw <= 0) stop("weighted_variance: weights sum to zero")
  mu <- sum(w * yy) / sw
  ss <- sum(w * (yy - mu)^2)
  s2 <- if (sw > 1) ss / (sw - 1) else NaN
  sd <- if (!is.nan(s2) && s2 >= 0) sqrt(s2) else NaN
  sw2 <- sum(w * w)
  se <- if (!is.nan(s2)) sqrt(s2 * sw2 / (sw * sw)) else NaN
  list(estimate = as.numeric(s2), mean = as.numeric(mu), sd = as.numeric(sd),
       se = as.numeric(se), sumw = as.numeric(sw), n = as.integer(n),
       method = "weighted variance, sum(w (y-ybar_w)^2)/(sum(w)-1) [Lohr 2010]")
}

# CANONICAL TEST
# r <- Wvar(c(1, 2, 3, 10))
# stopifnot(abs(r$estimate - var(c(1, 2, 3, 10))) < 1e-12)
