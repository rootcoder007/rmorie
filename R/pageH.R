# SPDX-License-Identifier: AGPL-3.0-or-later
#' Page-Hinkley sequential test for a shift in the mean
#'
#' Formula: with \eqn{\bar x_T} the running mean of the first \eqn{T}
#' observations, \eqn{m_T = \sum_{t\le T}(x_t - \bar x_t - \delta)},
#' \eqn{M_T = \min_{t \le T} m_t}, \eqn{PH_T = m_T - M_T}, and a change
#' is flagged at the first \eqn{T} with \eqn{PH_T > \lambda}.  The
#' mirrored form detects a decrease.  \eqn{\delta} is the magnitude
#' below which a deviation is treated as noise; \code{threshold} trades
#' detection delay against false alarms.
#'
#' @param x Stream in arrival order.
#' @param threshold Alarm level lambda.
#' @param delta Tolerated magnitude of drift.
#' @param direction "increase" or "decrease".
#' @return List with \code{statistic}, \code{detected},
#'   \code{changepoint} (1-based, 0 if none), \code{ph}, \code{n},
#'   \code{method}.
#' @references Page (1954), Biometrika 41:100-115; Hinkley (1971), Biometrika 58:509-523.
#'  Both paywalled at JSTOR (HTTP 403); the running-mean form used here is the one
#' standardised in Gama, Zliobaite, Bifet, Pechenizkiy and Bouchachia (2014), A survey on
#' concept drift adaptation, ACM Computing Surveys 46(4):44, sec. 4.2.
#' @export
#' @examples
#' Pagehink(x = c(1, 2, 3, 4, 5, 6, 7, 8), threshold = 0.5)
Pagehink <- function(x, threshold, delta = 0.005, direction = "increase") {
  x <- .t4_vec(x)
  n <- length(x)
  if (n == 0L) stop("empty stream")
  if (!direction %in% c("increase", "decrease")) stop("direction must be 'increase' or 'decrease'")
  sgn <- if (direction == "increase") 1 else -1
  delta <- as.numeric(delta)
  run <- 0
  m <- 0
  ext <- 0
  path <- numeric(n)
  detected <- FALSE
  cp <- 0L
  for (t in seq_len(n)) {
    run <- run + x[t]
    xbar <- run / t
    m <- m + sgn * (x[t] - xbar) - delta
    if (t == 1L || m < ext) ext <- m
    ph <- m - ext
    path[t] <- ph
    if (!detected && ph > threshold) { detected <- TRUE
    cp <- t }
  }
  .t4_result(statistic = path[n], detected = detected,
             changepoint = as.integer(cp), ph = path, n = as.integer(n),
             method = "Page-Hinkley change detector")
}
