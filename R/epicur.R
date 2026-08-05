# SPDX-License-Identifier: AGPL-3.0-or-later

#' Epidemic curve smoothing
#'
#' Formula: robust locally weighted regression of daily counts on time.
#' At each day x0 the fit is the intercept of the weighted least-squares
#' line of \code{cases} on \code{dates - x0} with tricube neighbourhood
#' weights w_i = (1 - |x_i - x0|^3 / h^3)^3 for |x_i - x0| < h
#' (Cleveland 1979 eq. 2), followed by \code{iterations} robustness
#' passes weighting each observation by the bisquare of its residual
#' scaled by six times the median absolute residual,
#' delta_i = (1 - (r_i / (6 s))^2)^2 for |r_i| < 6 s (eq. 5-7).  A fixed
#' bandwidth in day units replaces Cleveland's nearest-neighbour span.
#'
#' @param dates Day index of each observation.
#' @param cases Counts observed on those days.
#' @param bandwidth Neighbourhood half-width h in date units (> 0).
#' @param iterations Robustness passes (0 gives plain local linear).
#' @return List with \code{estimate}, \code{fitted}, \code{peak_date},
#'   \code{peak_value}, \code{total}, \code{n}, \code{method}.
#' @references Cleveland (1979), JASA 74(368):829-836,
#'   doi:10.1080/01621459.1979.10481038.
#' @export
Epicur <- function(dates, cases, bandwidth, iterations = 2) {
  .tri <- function(u) {
    a <- abs(u)
    if (a >= 1) return(0)
    t <- 1 - a * a * a
    t * t * t
  }
  .bis <- function(u) {
    a <- abs(u)
    if (a >= 1) return(0)
    t <- 1 - a * a
    t * t
  }
  x <- as.numeric(dates); y <- as.numeric(cases)
  n <- length(x)
  if (n == 0L) stop("empty input: dates has no observations")
  if (length(y) != n) stop("dates and cases must have the same length")
  h <- as.numeric(bandwidth)
  if (h <= 0) stop("bandwidth must be positive")
  it <- as.integer(iterations)
  if (it < 0L) stop("iterations must be non-negative")
  delta <- rep(1, n)
  fit <- numeric(n)
  for (.p in seq_len(it + 1L)) {
    for (k in seq_len(n)) {
      x0 <- x[k]
      s0 <- 0; s1 <- 0; s2 <- 0; t0 <- 0; t1 <- 0
      for (i in seq_len(n)) {
        d <- x[i] - x0
        w <- .tri(d / h) * delta[i]
        if (w == 0) next
        s0 <- s0 + w
        s1 <- s1 + w * d
        s2 <- s2 + w * d * d
        t0 <- t0 + w * y[i]
        t1 <- t1 + w * d * y[i]
      }
      det <- s0 * s2 - s1 * s1
      fit[k] <- if (s0 <= 0) y[k]
                else if (abs(det) < 1e-12 * (1 + abs(s0 * s2))) t0 / s0
                else (t0 * s2 - t1 * s1) / det
    }
    r <- y - fit
    s <- .s03median(abs(r))
    if (s <= 0) break
    delta <- vapply(r, function(v) .bis(v / (6 * s)), 0)
  }
  pk <- 1L
  if (n > 1L) for (i in 2:n) if (fit[i] > fit[pk]) pk <- i
  .t1_result(estimate = fit[pk], fitted = fit, peak_date = x[pk],
             peak_value = fit[pk], total = sum(y), n = n,
             method = "Epidemic curve smoothing")
}
