# SPDX-License-Identifier: AGPL-3.0-or-later
#' Influence function of an estimator at a point
#'
#' \deqn{IF(x; T, F) = \lim_{e \to 0} [T((1-e)F + e \delta_x) - T(F)]/e.}{IF(x; T, F) = lim_{e -> 0} [T((1-e)F + e delta_x) - T(F)]/e.}
#'
#' Hampel, F. R. (1974), "The influence curve and its role in robust
#' estimation", \emph{Journal of the American Statistical Association} 69(346),
#' 383-393, doi:10.1080/01621459.1974.10482962, defines the influence curve as
#' this limit; the paper is closed access with no open copy in any repository
#' (Unpaywall reports oa_status "closed"), and the definition used here is the
#' one already written in this module's own stub docstring, which matches every
#' later statement of it (e.g. Hampel, Ronchetti, Rousseeuw and Stahel 1986,
#' Section 2.1).
#'
#' F is a sample, taken as the empirical distribution putting mass 1/n on each
#' point, so the contaminated mixture is exactly representable: the same points
#' with weight (1-e)/n and x with weight e.  T must therefore be a functional
#' of a weighted sample, T(values, weights).  Three named ones are supplied:
#' "mean" (weighted mean), "var" (weighted variance, sum w (v-mu)^2 / sum w)
#' and "median" (lower weighted median).
#'
#' The quotient is evaluated at eps and at eps/2 and combined by Richardson
#' extrapolation, 2 Q(eps/2) - Q(eps), which removes the O(eps) term.  For the
#' mean the quotient is exactly x - mean(F) at every eps, since the mean is
#' linear in the mixing weight; for the variance the limit is
#' (x - mu)^2 - sigma^2.
#'
#' The median shows what an empirical F cannot do.  For odd n the lower
#' weighted median does not move at all under a small added weight, so the
#' quotient is exactly 0 whatever x is -- including x far outside the data.
#' For even n it is the opposite: the unweighted lower median sits exactly at
#' the halfway crossing, so any weight added above it pushes the crossing to
#' the next order statistic and the quotient is
#' (x_(n/2+1) - x_(n/2))/eps, which diverges as eps -> 0.  Neither is the
#' sign-based population formula IF = sign(x - m)/(2 f(m)); that one needs a
#' density, which an empirical distribution does not have.
#'
#' @param estimator function(values, weights), or "mean", "var", "median".
#' @param F The sample standing for F.
#' @param x Where to evaluate the influence; a single point.
#' @param eps Contamination weight used for the difference quotient.
#' @return list: estimate (Richardson-extrapolated), raw, half, tf, eps, n,
#'   method.
#' @keywords internal
#' @examples
#' Infcrv("mean", c(2, 4, 4, 5, 7), 12)$estimate
#' @export
Infcrv <- function(estimator, F, x, eps = 1e-3) {
  T <- .if_resolve(estimator, "influence_function")
  v <- .s03vec(F)
  n <- length(v)
  if (n == 0L) stop("influence_function: F is empty")
  xs <- .s03vec(x)
  if (length(xs) != 1L) stop("influence_function: x must be a single point")
  x0 <- xs[1L]
  e <- as.numeric(eps)
  if (!(e > 0 && e < 1)) {
    stop("influence_function: eps must lie strictly between 0 and 1")
  }
  base <- T(v, rep(1 / n, n))
  quot <- function(h) (T(c(v, x0), c(rep((1 - h) / n, n), h)) - base) / h
  q1 <- quot(e); q2 <- quot(e / 2)
  list(estimate = 2 * q2 - q1, raw = q1, half = q2, tf = base, eps = e,
       n = n, method = "Influence function")
}

.if_wmean <- function(v, w) sum(w * v) / sum(w)

.if_wvar <- function(v, w) {
  m <- .if_wmean(v, w)
  sum(w * (v - m)^2) / sum(w)
}

# Lower weighted median: the smallest value whose cumulative weight reaches
# half the total.
.if_wmedian <- function(v, w) {
  o <- order(v)
  tot <- sum(w)
  acc <- 0
  for (i in o) {
    acc <- acc + w[i]
    if (acc >= 0.5 * tot) return(v[i])
  }
  v[o[length(o)]]
}

.if_resolve <- function(estimator, who) {
  if (is.function(estimator)) return(estimator)
  if (is.character(estimator) && length(estimator) == 1L) {
    if (estimator == "mean") return(.if_wmean)
    if (estimator == "var") return(.if_wvar)
    if (estimator == "median") return(.if_wmedian)
  }
  stop(paste0(who, ": estimator must be a function or one of 'mean', 'var', 'median'"))
}
