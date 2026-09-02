# SPDX-License-Identifier: AGPL-3.0-or-later
#' ARIMAX: exogenous regression with ARMA errors
#'
#' (1-B)^d y = X beta + n with phi(B) n = theta(B) e.  Stage one is OLS
#' of the differenced response on the differenced covariates; stage two
#' is the Hannan-Rissanen two-stage regression on the residual series.
#'
#' @param y Observed response.
#' @param X Exogenous regressors, differenced internally.
#' @param p,q ARMA orders for the disturbance.
#' @param d Order of differencing.
#' @param m Stage-one long-autoregression order, or NULL.
#'
#' @return List with beta, phi, theta, intercept, sigma2, noise, resid,
#'   p, q, d, r, nobs.
#' @references Box and Jenkins (1976), Chapters 10-11 (transfer-function
#'   models); disturbance estimation by Hannan and Rissanen (1982),
#'   Biometrika 69(1), 81-94.  Standard published form; neither source is
#'   in the local corpus and neither was read.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Arimaxhr(V, V)
Arimaxhr <- function(y, X, p = 1, q = 1, d = 0, m = NULL) {
  y <- .t1_vec(y); Xm <- .t1_mat(X); n <- length(y)
  if (nrow(Xm) != n) stop("X must have one row per observation")
  r <- ncol(Xm)
  p <- as.integer(p); q <- as.integer(q); d <- as.integer(d)
  if (p < 0L || q < 0L || d < 0L) stop("orders must be non-negative")
  w <- y; Xd <- Xm
  if (d > 0L) for (i in seq_len(d)) {
    w <- diff(w)
    Xd <- apply(Xd, 2L, diff)
    dim(Xd) <- c(length(w), r)
  }
  Xr <- cbind(1, Xd)
  dimnames(Xr) <- NULL
  f1 <- .t1_lstsq(Xr, w)
  nz <- f1$resid; nw <- length(nz)
  if (is.null(m)) m <- max(p + q + 1L, as.integer(sqrt(nw)) + 1L)
  m <- as.integer(m)
  if (nw <= m + max(p, q) + 1L)
    stop("series too short for the requested orders")
  idx <- (m + 1L):nw
  Xa <- cbind(1, sapply(seq_len(m), function(i) nz[idx - i]))
  dim(Xa) <- c(length(idx), m + 1L)
  fa <- .t1_lstsq(Xa, nz[idx])
  eh <- rep(0, nw); eh[idx] <- fa$resid
  s <- m + max(p, q)
  jdx <- (s + 1L):nw
  Xb <- matrix(1, nrow = length(jdx), ncol = 1L)
  if (p > 0L) Xb <- cbind(Xb, sapply(seq_len(p), function(i) nz[jdx - i]))
  if (q > 0L) Xb <- cbind(Xb, sapply(seq_len(q), function(j) eh[jdx - j]))
  dim(Xb) <- c(length(jdx), 1L + p + q)
  fb <- .t1_lstsq(Xb, nz[jdx])
  b <- fb$beta; res <- fb$resid
  nobs <- length(jdx); k <- p + q + 1L + r
  .t1_result(beta = f1$beta[-1], phi = if (p > 0L) b[2:(1L + p)] else numeric(0),
             theta = if (q > 0L) b[(2L + p):(1L + p + q)] else numeric(0),
             intercept = f1$beta[1],
             sigma2 = sum(res^2) / max(nobs - k, 1L), noise = nz,
             resid = res, p = p, q = q, d = d, r = r, nobs = nobs,
             method = "ARIMAX by OLS plus Hannan-Rissanen errors (Box-Jenkins 1976)")
}
