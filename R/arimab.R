# SPDX-License-Identifier: AGPL-3.0-or-later
#' Hannan-Rissanen estimation of an ARIMA(p, d, q) model.
#'
#' Stage one fits a long AR(m) by OLS and keeps its residuals as
#' innovation proxies; stage two regresses w_t on p own lags and q lagged
#' proxies.  Two OLS solves, no iteration.
#'
#' @param y Observed series.
#' @param p,q Autoregressive and moving-average orders.
#' @param d Order of differencing.
#' @param m Order of the stage-one autoregression; NULL uses
#'   max(p + q + 1, floor(sqrt(n)) + 1).
#'
#' @return List with phi, theta, intercept, sigma2, resid, m, p, q, d,
#'   nobs.
#' @references Hannan and Rissanen (1982), Biometrika 69(1), 81-94; the
#'   ARIMA model is Box, Jenkins and Reinsel (1994), Chapter 4.  Standard
#'   published form; neither source is in the local corpus and neither
#'   was read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Arimahr(V)
Arimahr <- function(y, p = 1, q = 1, d = 0, m = NULL) {
  y <- .t1_vec(y)
  p <- as.integer(p); q <- as.integer(q); d <- as.integer(d)
  if (p < 0L || q < 0L || d < 0L) stop("orders must be non-negative")
  w <- y
  if (d > 0L) for (i in seq_len(d)) w <- diff(w)
  nw <- length(w)
  if (is.null(m)) m <- max(p + q + 1L, as.integer(sqrt(nw)) + 1L)
  m <- as.integer(m)
  if (nw <= m + max(p, q) + 1L)
    stop("series too short for the requested orders")
  idx <- (m + 1L):nw
  Xa <- cbind(1, sapply(seq_len(m), function(i) w[idx - i]))
  dim(Xa) <- c(length(idx), m + 1L)
  fa <- .t1_lstsq(Xa, w[idx])
  eh <- rep(0, nw)
  eh[idx] <- fa$resid
  s <- m + max(p, q)
  jdx <- (s + 1L):nw
  Xb <- matrix(1, nrow = length(jdx), ncol = 1L)
  if (p > 0L) Xb <- cbind(Xb, sapply(seq_len(p), function(i) w[jdx - i]))
  if (q > 0L) Xb <- cbind(Xb, sapply(seq_len(q), function(j) eh[jdx - j]))
  dim(Xb) <- c(length(jdx), 1L + p + q)
  fb <- .t1_lstsq(Xb, w[jdx])
  b <- fb$beta; res <- fb$resid
  nobs <- length(jdx); k <- p + q + 1L
  .t1_result(phi = if (p > 0L) b[2:(1L + p)] else numeric(0),
             theta = if (q > 0L) b[(2L + p):(1L + p + q)] else numeric(0),
             intercept = b[1],
             sigma2 = sum(res^2) / max(nobs - k, 1L), resid = res,
             m = m, p = p, q = q, d = d, nobs = nobs,
             method = "Hannan-Rissanen ARMA estimation (Hannan-Rissanen 1982)")
}
