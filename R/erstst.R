# SPDX-License-Identifier: AGPL-3.0-or-later

#' Elliott-Rothenberg-Stock GLS-detrended ADF (DF-GLS)
#'
#' Formula: local-to-unity GLS detrending at abar = 1 + cbar / T with
#' cbar = -7 (mean case) or cbar = -13.5 (trend case).  Quasi-difference
#' both the series and the deterministic terms,
#' \preformatted{
#'   ytil_1 = y_1,  ytil_t = y_t - abar y_{t-1}
#'   ztil_1 = z_1,  ztil_t = z_t - abar z_{t-1}
#' }
#' regress ytil on ztil to get psi, form y^d = y - z psi, and run the ADF
#' regression without deterministic terms,
#' dy^d_t = rho y^d_{t-1} + sum_j c_j dy^d_{t-j} + e_t.  The DF-GLS
#' statistic is the t-ratio on rho.
#'
#' @param x Series.
#' @param lags Number of lagged first differences p (>= 0).
#' @param trend Detrend against (1, t) rather than the constant alone.
#' @return List with \code{estimate}, \code{statistic}, \code{rho},
#'   \code{se}, \code{abar}, \code{lags}, \code{nobs}, \code{n},
#'   \code{method}.
#' @references Elliott, Rothenberg & Stock (1996), Econometrica
#'   64(4):813-836, doi:10.2307/2171846.
#' @export
Erstst <- function(x, lags = 1, trend = FALSE) {
  y <- as.numeric(x)
  n <- length(y)
  p <- as.integer(lags)
  if (p < 0L) stop("lags must be non-negative")
  if (n < p + 3L) stop(sprintf("series too short for %d lags", p))
  cbar <- if (isTRUE(as.logical(trend))) -13.5 else -7
  abar <- 1 + cbar / n
  k <- if (isTRUE(as.logical(trend))) 2L else 1L
  Z <- if (k == 2L) cbind(rep(1, n), seq_len(n)) else matrix(1, n, 1L)
  yt <- y
  if (n > 1L) yt[2:n] <- y[2:n] - abar * y[1:(n - 1L)]
  Zt <- Z
  if (n > 1L) Zt[2:n, ] <- Z[2:n, , drop = FALSE] - abar * Z[1:(n - 1L), , drop = FALSE]
  psi <- .s03lstsq(Zt, yt, 0)
  yd <- as.numeric(y - Z %*% psi)
  dy <- yd[2:n] - yd[1:(n - 1L)]
  nd <- length(dy)
  idx <- (p + 1L):nd
  nobs <- length(idx)
  kk <- p + 1L
  if (nobs <= kk) stop(sprintf("series too short for %d lags", p))
  rows <- matrix(0, nobs, kk)
  rhs <- numeric(nobs)
  for (r in seq_len(nobs)) {
    i <- idx[r]
    rows[r, 1L] <- yd[i]
    if (p > 0L) for (j in seq_len(p)) rows[r, j + 1L] <- dy[i - j]
    rhs[r] <- dy[i]
  }
  b <- .s03lstsq(rows, rhs, 0)
  resid <- rhs - as.numeric(rows %*% b)
  s2 <- sum(resid * resid) / (nobs - kk)
  XtX <- matrix(0, kk, kk)
  for (a in seq_len(kk)) for (c in seq_len(kk))
    XtX[a, c] <- sum(rows[, a] * rows[, c])
  e1 <- c(1, rep(0, kk - 1L))
  inv <- .s03cholsolve(XtX, e1)
  se <- sqrt(s2 * inv[1])
  stat <- b[1] / se
  .t1_result(estimate = stat, statistic = stat, rho = b[1], se = se,
             abar = abar, lags = p, nobs = nobs, n = n,
             method = "Elliott-Rothenberg-Stock GLS-detrended ADF")
}
