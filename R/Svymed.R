# SPDX-License-Identifier: AGPL-3.0-or-later
#' Survey median
#'
#' The weighted CDF inverted at one half, delegated to \code{Svyqtl} so
#' the estimator and its Woodruff interval are shared rather than
#' reimplemented.  The weighted mean is reported alongside so the
#' skewness of the weighted distribution is visible.
#'
#' Formula: M = inf\{t : F_w(t) >= 1/2\}.
#'
#' @param y Numeric observations.
#' @param weights Optional design weights; equal weights if NULL.
#' @return List with \code{estimate}, \code{se}, \code{lower},
#'   \code{upper}, \code{F}, \code{mean}, \code{sumw}, \code{n},
#'   \code{method}.
#' @references Francisco, C. A. and Fuller, W. A. (1991). Quantile
#'   estimation with a complex survey design. The Annals of Statistics
#'   19(1):454-469. \doi{10.1214/aos/1176347993}
#' @examples
#' Svymed(c(1, 2, 3, 4, 5))
#' @export
Svymed <- function(y, weights = NULL) {
  r <- Svyqtl(y, weights, 0.5)
  yy <- .s03vec(y)
  n <- length(yy)
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  tot <- sum(w)
  list(estimate = as.numeric(r$estimate), se = as.numeric(r$se),
       lower = as.numeric(r$lower), upper = as.numeric(r$upper),
       F = as.numeric(r$F), mean = as.numeric(sum(w * yy) / tot),
       sumw = as.numeric(tot), n = as.integer(n),
       method = "weighted median, F_w inverted at 1/2 [Francisco & Fuller 1991]")
}

# CANONICAL TEST
# stopifnot(abs(Svymed(c(1, 2, 3, 4, 5))$estimate - 3) < 1e-12)
