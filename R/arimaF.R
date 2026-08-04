# SPDX-License-Identifier: AGPL-3.0-or-later
#' Conditional sum of squares for an ARIMA(p, d, q) model.
#'
#' e_t = (w_t - mu) - sum_i phi_i (w_{t-i} - mu) - sum_j theta_j e_{t-j}
#' on w = (1-B)^d y with zero presample innovations; CSS = sum e_t^2 and
#' logLik = -(m/2)(1 + log(2 pi) + log(CSS/m)).
#'
#' @param y Observed series.
#' @param phi Autoregressive coefficients.
#' @param theta Moving-average coefficients (Box-Jenkins +theta sign).
#' @param d Order of differencing.
#' @param mu Mean of the differenced series.
#'
#' @return List with css, sigma2, loglik, aic, resid, diff, m, p, q, d, n.
#' @references Box and Jenkins (1970), Time Series Analysis: Forecasting
#'   and Control, Chapters 4 and 7.  Standard published form; the
#'   monograph is not in the local corpus and was not read.
#' @export
Arimacss <- function(y, phi = numeric(0), theta = numeric(0), d = 0,
                     mu = 0) {
  y <- .t1_vec(y); n <- length(y)
  ph <- as.numeric(phi); th <- as.numeric(theta)
  p <- length(ph); q <- length(th); d <- as.integer(d)
  if (d < 0L) stop("d must be non-negative")
  if (n <= d + p) stop("series too short for the requested orders")
  w <- y
  if (d > 0L) for (i in seq_len(d)) w <- diff(w)
  z <- w - as.numeric(mu)
  nw <- length(z)
  e <- rep(0, nw); css <- 0; m <- 0L
  if (p < nw) for (t in (p + 1L):nw) {
    v <- z[t]
    if (p > 0L) for (i in seq_len(p)) v <- v - ph[i] * z[t - i]
    if (q > 0L) for (j in seq_len(q)) if (t - j >= 1L) v <- v - th[j] * e[t - j]
    e[t] <- v
    css <- css + v * v
    m <- m + 1L
  }
  s2 <- css / m
  ll <- -0.5 * m * (1 + log(2 * pi) + log(s2))
  k <- p + q + 1L
  .t1_result(css = css, sigma2 = s2, loglik = ll, aic = -2 * ll + 2 * k,
             resid = e[(p + 1L):nw], diff = w, m = m, p = p, q = q,
             d = d, n = n,
             method = "ARIMA conditional sum of squares (Box-Jenkins 1970)")
}
