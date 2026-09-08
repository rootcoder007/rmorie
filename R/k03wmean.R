# SPDX-License-Identifier: AGPL-3.0-or-later

#' Horvitz--Thompson weighted (Hajek) survey mean
#'
#' With design weights \eqn{w_i = 1/\pi_i} the Horvitz--Thompson total is
#' \eqn{\hat t_y = \sum_i w_i y_i} and the estimated population size is
#' \eqn{\hat N = \sum_i w_i}. Their ratio
#' \eqn{\bar y_w = \sum_i w_i y_i / \sum_i w_i} is the Hajek mean. It is a
#' ratio of two estimated totals rather than a linear statistic, so its
#' variance comes from linearisation rather than directly.
#'
#' The variance estimator is the standard with-replacement linearisation
#' of that ratio,
#' \deqn{v(\bar y_w) = \frac{n}{(n-1)(\sum_i w_i)^2}
#'   \sum_i w_i^2 (y_i - \bar y_w)^2,}
#' the residual technique of Sarndal, Swensson & Wretman (1992,
#' Section 5.6) applied to the Hajek ratio. This is the same quantity
#' \code{survey::svymean} reports for a design declared with
#' \code{ids = ~1}; the identity was checked numerically against that
#' package when this function was written.
#'
#' \code{weights} are design weights \eqn{1/\pi_i}, not frequencies. Zero
#' weights drop a unit; negative weights are rejected.
#'
#' Mirrors \code{morie.fn.wmeansr} on the Python side.
#'
#' @param y Numeric vector of observed values for the sampled units.
#' @param weights Numeric vector of design weights \eqn{w_i = 1/\pi_i},
#'   the same length as \code{y}.
#' @return Named list with \code{estimate}, \code{se},
#'   \code{sum_weights}, \code{n}, \code{method}.
#' @references Horvitz D G & Thompson D J (1952). A generalization of
#'   sampling without replacement from a finite universe. \emph{JASA}
#'   47(260), 663--685.
#'
#'   Thompson S K (2012). \emph{Sampling}, 3rd ed. Wiley, Chapter 6.
#'
#'   Sarndal C-E, Swensson B & Wretman J (1992). \emph{Model Assisted
#'   Survey Sampling}. Springer, Section 5.6.
#' @examples
#' Wmeansr(c(3.1, 5.2, 2.8, 9.4), c(1.5, 2, 0.5, 3))$se
#' @export
Wmeansr <- function(y, weights) {
  yv <- as.numeric(y)
  wv <- as.numeric(weights)
  n <- length(yv)
  if (length(wv) != n) {
    stop("y and weights must have the same length", call. = FALSE)
  }
  if (n < 2L) stop("need at least two sampled units", call. = FALSE)
  if (any(!is.finite(wv)) || any(wv < 0)) {
    stop("design weights must be finite and non-negative", call. = FALSE)
  }
  sw <- 0
  for (v in wv) sw <- sw + v
  if (sw <= 0) {
    stop("design weights must have positive total", call. = FALSE)
  }

  num <- 0
  for (i in seq_len(n)) num <- num + wv[i] * yv[i]
  est <- num / sw

  ss <- 0
  for (i in seq_len(n)) {
    r <- yv[i] - est
    ss <- ss + wv[i] * wv[i] * r * r
  }
  vr <- n / ((n - 1) * sw * sw) * ss
  list(estimate = est,
       se = sqrt(vr),
       sum_weights = sw,
       n = n,
       method = "Hajek weighted survey mean (Horvitz-Thompson weights)")
}
