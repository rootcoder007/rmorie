# SPDX-License-Identifier: AGPL-3.0-or-later
#' Survey quantile by inversion of the weighted CDF
#'
#' The estimator inverts the Horvitz-Thompson estimator of the
#' distribution function, F_w(t) = sum_\{y_i <= t\} w_i / sum_i w_i.  With
#' equal weights it is exactly the type-1 sample quantile.  The interval
#' is Woodruff's: invert the CDF at p -/+ z times the linearized standard
#' error of F_w at the point estimate.
#'
#' Formula: Q_p = inf\{t : F_w(t) >= p\}.
#'
#' @param y Numeric observations.
#' @param weights Optional design weights; equal weights if NULL.
#' @param quantile Probability in (0, 1).
#' @return List with \code{estimate}, \code{se}, \code{lower},
#'   \code{upper}, \code{p}, \code{F}, \code{sumw}, \code{neff},
#'   \code{n}, \code{method}.
#' @references Francisco, C. A. and Fuller, W. A. (1991). Quantile
#'   estimation with a complex survey design. The Annals of Statistics
#'   19(1):454-469. \doi{10.1214/aos/1176347993}
#' @examples
#' Svyqtl(c(4, 1, 3, 2, 5), NULL, 0.5)
#' @export
Svyqtl <- function(y, weights = NULL, quantile = 0.5) {
  yy <- .s03vec(y)
  n <- length(yy)
  if (n == 0L) stop("survey_quantile: y is empty")
  w <- if (is.null(weights)) rep(1, n) else .s03vec(weights)
  if (length(w) != n) stop("survey_quantile: y and weights differ in length")
  p <- as.numeric(quantile)
  if (!(p > 0 && p < 1)) stop("survey_quantile: quantile must lie in (0, 1)")
  ord <- order(yy)
  xs <- yy[ord]
  ws <- w[ord]
  tot <- sum(ws)
  if (tot <= 0) stop("survey_quantile: weights sum to zero")
  cum <- cumsum(ws) / tot
  q <- .svyqtl_inv(xs, cum, p)
  F <- sum(w[yy <= q]) / tot
  ind <- as.numeric(yy <= q)
  var <- sum(w * w * (ind - F)^2) / (tot * tot)
  se <- if (var > 0) sqrt(var) else 0
  z <- 1.959963984540054
  lo <- .svyqtl_inv(xs, cum, min(max(p - z * se, 1e-12), 1 - 1e-12))
  hi <- .svyqtl_inv(xs, cum, min(max(p + z * se, 1e-12), 1 - 1e-12))
  list(estimate = as.numeric(q), se = as.numeric(se), lower = as.numeric(lo),
       upper = as.numeric(hi), p = p, F = as.numeric(F),
       sumw = as.numeric(tot), neff = as.numeric(tot * tot / sum(w * w)),
       n = as.integer(n),
       method = "weighted CDF inversion with Woodruff interval [Francisco & Fuller 1991]")
}

#' .svyqtl_inv
#'
#' A step of the Svyqtl implementation. Called by \code{Svyqtl}.
#' See the file header for the source the module follows.
#' it follows.
#'
#' @param xs A vector; its length is taken and its elements indexed.
#' @param cum Passed to \code{>=}.
#' @param p Passed to \code{>=}.
#' @return One of two values, depending on the branch taken.
#' @export
.svyqtl_inv <- function(xs, cum, p) {
  i <- which(cum >= p)
  if (length(i)) xs[i[1L]] else xs[length(xs)]
}

# CANONICAL TEST
# stopifnot(abs(Svyqtl(c(4, 1, 3, 2, 5), NULL, 0.5)$estimate -
#               as.numeric(quantile(c(4, 1, 3, 2, 5), 0.5, type = 1))) < 1e-12)
