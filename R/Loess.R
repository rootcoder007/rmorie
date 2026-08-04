# SPDX-License-Identifier: AGPL-3.0-or-later
#' LOWESS: locally weighted scatterplot smoothing with robustness iterations
#'
#' At each x_i the floor(f n) nearest points get tricube weights
#' (1 - |u|^3)^3, u = (x_j - x_i)/d_i, a weighted line is fitted and evaluated
#' at x_i, and `iterations` further passes reweight by the bisquare of the
#' residual over six times the median absolute residual.  Source consulted:
#' Cleveland (1979), JASA 74(368), 829-836.  Follows the lowest/clowess step
#' sequence including the h9/h1 and c9/c1 guards, so it reproduces
#' stats::lowess exactly at delta = 0.  Named Loess, not loess, so that
#' library(morie) cannot mask stats::loess -- the signatures differ.
#'
#' @param x,y predictor and response.
#' @param span fraction of points per neighbourhood.
#' @param iterations robustness iterations.
#' @return list: estimate, x, fitted, residuals, robustness_weights, span,
#'   iterations, n, method.
#' @keywords internal
#' @examples
#' Loess(1:10, c(1.2, 2.3, 2.9, 4.1, 5.2, 5.8, 7.3, 8.1, 8.9, 10.2), 0.5)$fitted
#' @export
Loess <- function(x, y, span = 2 / 3, iterations = 3L) {
  xa <- as.numeric(x); ya <- as.numeric(y)
  ordr <- base::order(xa)
  xv <- xa[ordr]; yv <- ya[ordr]
  n <- length(xv)
  ys <- numeric(n); rw <- rep(1, n); res <- numeric(n); w <- numeric(n)
  ns <- max(2L, min(n, as.integer(n * span + 1e-7)))
  lowest <- function(xs, nleft, nright, userw) {
    rng <- xv[n] - xv[1]
    h <- max(xs - xv[nleft], xv[nright] - xs)
    h9 <- 0.999 * h; h1 <- 0.001 * h
    a <- 0; j <- nleft
    while (j <= n) {
      w[j] <<- 0
      r <- abs(xv[j] - xs)
      if (r <= h9) {
        w[j] <<- if (r <= h1) 1 else (1 - (r / h)^3)^3
        if (userw) w[j] <<- w[j] * rw[j]
        a <- a + w[j]
      } else if (xv[j] > xs) break
      j <- j + 1L
    }
    nrt <- j - 1L
    if (a <= 0) return(NULL)
    idx <- nleft:nrt
    w[idx] <<- w[idx] / a
    if (h > 0) {
      aa <- sum(w[idx] * xv[idx])
      b <- xs - aa
      cc <- sum(w[idx] * (xv[idx] - aa)^2)
      if (sqrt(cc) > 0.001 * rng) {
        b <- b / cc
        w[idx] <<- w[idx] * (b * (xv[idx] - aa) + 1)
      }
    }
    sum(w[idx] * yv[idx])
  }
  for (it in 0:as.integer(iterations)) {
    nleft <- 1L; nright <- ns
    for (i in seq_len(n)) {
      while (nright < n) {
        d1 <- xv[i] - xv[nleft]; d2 <- xv[nright + 1L] - xv[i]
        if (d1 <= d2) break
        nleft <- nleft + 1L; nright <- nright + 1L
      }
      val <- lowest(xv[i], nleft, nright, it > 0L)
      ys[i] <- if (is.null(val)) yv[i] else val
    }
    res <- yv - ys
    if (it == as.integer(iterations)) break
    absr <- sort(abs(res))
    m1 <- n %/% 2L
    m2 <- n - m1 - 1L
    cmad <- 3 * (absr[m1 + 1L] + absr[m2 + 1L])
    c9 <- 0.999 * cmad; c1 <- 0.001 * cmad
    for (i in seq_len(n)) {
      r <- abs(res[i])
      rw[i] <- if (r <= c1) 1 else if (r <= c9) (1 - (r / cmad)^2)^2 else 0
    }
  }
  list(estimate = ys, x = xv, fitted = ys, residuals = res,
       robustness_weights = rw, span = as.numeric(span),
       iterations = as.integer(iterations), n = n,
       method = "LOWESS locally weighted regression (Cleveland 1979)")
}

# CANONICAL TEST
# r <- Loess(1:10, c(1.2,2.3,2.9,4.1,5.2,5.8,7.3,8.1,8.9,10.2), 0.5, 3L)
# stopifnot(abs(r$fitted[1] - 1.25178975416042) < 1e-10)

#' @rdname Loess
#' @keywords internal
#' @export
morie_loess <- Loess
